use crate::{
    bytecode::{Chunk, ConstantAddr, Instruction, OpCode},
    value::{StringTable, Value},
};
use std::{collections::HashMap, convert::TryFrom, io, ops::Range};
use thiserror::Error;

#[derive(Clone)]
pub(crate) struct Runtime<'io> {
    stdout: &'io dyn io::Write,
}

#[derive(Clone, Debug, Error)]
pub enum RuntimeError {
    #[error("(line {line}): Binary '{operator}' is not applicable to {lhs} and {rhs}")]
    BinaryOperatorNotApplicable {
        lhs: Value,
        rhs: Value,
        operator: char,
        line: usize,
    },

    #[error("(line {line}): Unary '{operator}' is not applicable to {operand}")]
    UnaryOperatorNotApplicable {
        operator: char,
        operand: Value,
        line: usize,
    },
}

const STACK_SIZE: usize = 256;

pub(crate) struct VmState<'c, 'st> {
    ip: *const u8,
    end_of_code: *const u8, // One past chunk end.

    // Ensure that the chunk can't be modified so ip isn't invalidated.
    pub(crate) chunk: &'c Chunk,

    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    strings: &'st mut StringTable,
}

impl<'c, 'st> VmState<'c, 'st> {
    fn new(chunk: &'c Chunk, strings: &'st mut StringTable) -> VmState<'c, 'st> {
        let Range { start, end } = chunk.code.as_ptr_range();
        VmState {
            ip: start,
            end_of_code: end,
            chunk,
            stack: Vec::with_capacity(STACK_SIZE),
            globals: HashMap::default(),
            strings,
        }
    }

    #[inline]
    pub(crate) fn line_number(&self) -> usize {
        let offset = self.offset();
        let location = self
            .chunk
            .line_numbers
            .binary_search_by_key(&offset, |e| e.0);

        match location {
            Ok(index) => self.chunk.line_numbers[index].1,
            Err(insert_index) => {
                debug_assert!(insert_index > 0, "Line numbers out of sync");
                self.chunk.line_numbers[insert_index - 1].1
            }
        }
    }

    #[inline]
    pub(crate) fn offset(&self) -> usize {
        self.ip as usize - self.chunk.code.as_ptr() as usize
    }

    #[inline]
    fn constant_at(&self, address: ConstantAddr) -> &Value {
        unsafe {
            // Safety: A ConstantAddr can only be obtained by adding
            // the constant to the constant table, so it must be present.
            self.chunk.constants.get_unchecked(address.0 as usize)
        }
    }

    #[inline]
    fn resolve_string(&self, sym: usize) -> &str {
        self.strings.resolve(sym).expect("Missing interned string")
    }

    #[inline]
    fn stack_pop(&mut self) -> Value {
        self.stack.pop().expect("Popped empty stack")
    }

    #[inline]
    fn stack_peek(&mut self) -> &mut Value {
        self.stack.last_mut().expect("Peeked at empty stack")
    }
}

macro_rules! binary_operator {
    ($vm:ident, $op:tt, $($type:tt)|+ -> $ret:tt) => {{
        let rhs = $vm.stack_pop();
        let lhs = $vm.stack_pop();
        let result = match (&lhs, &rhs) {
            $( ($type(l), $type(r)) => $ret(l $op r), )*

            _ => return Err(RuntimeError::BinaryOperatorNotApplicable {
                lhs,
                rhs,
                operator: stringify!($op).chars().next().unwrap(),
                line: $vm.line_number()
            })
        };
        $vm.stack.push(result);
    }}
}

impl<'io> Runtime<'io> {
    pub fn new(stdout: &'io dyn io::Write) -> Runtime<'io> {
        Runtime { stdout }
    }

    pub fn execute(
        &mut self,
        chunk: &Chunk,
        strings: &mut StringTable,
    ) -> Result<(), RuntimeError> {
        let mut vm = VmState::new(chunk, strings);
        self.run(&mut vm)
    }

    fn run(&mut self, vm: &mut VmState) -> Result<(), RuntimeError> {
        // Safety: We use a raw pointer as the instruction pointer for
        // performance. We could represent the instruction stream as
        // an iterator, but this is a poor fit given that traversal is
        // non-linear and we need to change chunks. We would also
        // incur a check for Option::None on every instruction.
        //
        // Safety is enforced in 2 ways.
        // 1) We ensure a well formed instruction stream by requiring
        //    all writes to a chunk to go through its type-safe
        //    interface.
        // 2) The VmState holds a shared reference to the current
        //    chunk, which guarantees that the underlying vector cannot
        //    be modified during execution.
        loop {
            debug_assert!(vm.ip < vm.end_of_code, "Instruction pointer overrun");
            // Safety: Safe as long as we always bump vm.ip by the
            // size of the last instruction.
            let instruction = unsafe { decode(vm.ip) };

            #[cfg(feature = "trace")]
            {
                use crate::disassemble;
                disassemble::stack(&vm.stack);
                disassemble::instruction(&vm.chunk, vm.offset(), &instruction, vm.strings);
            }

            use Instruction::*;
            use Value::*;
            match instruction {
                False => vm.stack.push(Boolean(false)),
                True => vm.stack.push(Boolean(true)),
                Instruction::Nil => vm.stack.push(Value::Nil),
                Constant { address } => {
                    let constant = vm.constant_at(address).clone();
                    vm.stack.push(constant);
                }
                DefineGlobal { address } => {
                    let global_name = match vm.constant_at(address) {
                        String(sym) => vm.resolve_string(*sym).to_owned(),
                        _ => unreachable!("Non-string global name"),
                    };
                    // Only pop after adding to globals map to ensure
                    // that the value is still available in the event
                    // of a GC while this is happening.
                    let value = vm.stack_peek().clone(); // Cheap, always an interend string
                    vm.globals.insert(global_name, value);
                    vm.stack_pop();
                }
                Add => {
                    let rhs = vm.stack_pop();
                    let lhs = vm.stack_pop();
                    let result = match (lhs, rhs) {
                        (Number(a), Number(b)) => Number(a + b),
                        (String(sym1), String(sym2)) => {
                            let str1 = vm.resolve_string(sym1);
                            let str2 = vm.resolve_string(sym2);
                            let cat = str1.to_owned() + str2;
                            let sym = vm.strings.get_or_intern(cat);
                            String(sym)
                        }
                        (lhs, rhs) => {
                            return Err(RuntimeError::BinaryOperatorNotApplicable {
                                lhs,
                                rhs,
                                operator: '+',
                                line: vm.line_number(),
                            })
                        }
                    };
                    vm.stack.push(result);
                }
                Divide => binary_operator!(vm, /, Number -> Number),
                Multiply => binary_operator!(vm, *, Number -> Number),
                Subtract => binary_operator!(vm, -, Number -> Number),
                Greater => binary_operator!(vm, >, Number -> Boolean),
                Less => binary_operator!(vm, <, Number -> Boolean),
                Equal => {
                    let b = vm.stack_pop();
                    let a = vm.stack_pop();
                    vm.stack.push(Boolean(a == b));
                }
                Negate => {
                    let value = vm.stack_peek();
                    if let Number(number) = value {
                        *number = -(*number);
                    } else {
                        return Err(RuntimeError::UnaryOperatorNotApplicable {
                            operator: '-',
                            operand: value.clone(),
                            line: vm.line_number(),
                        });
                    }
                }
                Not => {
                    let value = vm.stack_peek();
                    *value = Boolean(value.is_falsey());
                }
                Pop => {
                    vm.stack_pop();
                }
                Print => {
                    let value = vm.stack_pop();
                    if let Value::String(sym) = value {
                        let string = vm.resolve_string(sym);
                        println!("{}", string);
                    } else {
                        println!("{}", value);
                    }
                }
                Return => return Ok(()),
            }
            // Safety: Safe as long as the instruction stream is well
            // formed and the underlying vector doesn't change.
            vm.ip = unsafe { vm.ip.add(instruction.size()) };
        }
    }
}

pub(crate) unsafe fn decode(address: *const u8) -> Instruction {
    // Safety: 'address' must point to an opcode in a valid chunk
    // Speed: It should be safe to replace this with a mem::transmute()
    //        as long as we keep the chunk writing interface typesafe.
    let opcode = OpCode::try_from(*address).expect("Invalid opcode");

    #[rustfmt::skip]
    match opcode {
        OpCode::Constant => Instruction::Constant {
            // Safety: Safe as long as the instruction stream is well formed.
            address: ConstantAddr(*address.add(1)),
        },
        OpCode::DefineGlobal => Instruction::DefineGlobal {
            address: ConstantAddr(*address.add(1)),
        },
        OpCode::Add      => Instruction::Add,
        OpCode::Divide   => Instruction::Divide,
        OpCode::Equal    => Instruction::Equal,
        OpCode::False    => Instruction::False,
        OpCode::Greater  => Instruction::Greater,
        OpCode::Less     => Instruction::Less,
        OpCode::Multiply => Instruction::Multiply,
        OpCode::Negate   => Instruction::Negate,
        OpCode::Nil      => Instruction::Nil,
        OpCode::Not      => Instruction::Not,
        OpCode::Pop      => Instruction::Pop,
        OpCode::Print    => Instruction::Print,
        OpCode::Return   => Instruction::Return,
        OpCode::Subtract => Instruction::Subtract,
        OpCode::True     => Instruction::True,
    }
}
