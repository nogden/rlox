use std::{
    ops::Range,
    convert::TryFrom,
    io,
    collections::HashMap,
};

use thiserror::Error;

use crate::{
    bytecode::{Chunk, OpCode, Instruction, ConstantAddr},
    value::{Value, RefType},
};

#[derive(Clone)]
pub(crate) struct Runtime<'io> {
    stdout: &'io dyn io::Write,
}

#[derive(Clone, Debug, Error)]
pub enum RuntimeError {
    #[error("(line {line}): Binary '{operator}' is not applicable \
             to {lhs} and {rhs}")]
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
    }
}

const STACK_SIZE: usize = 256;

struct VmState<'c> {
    ip: *const u8,
    end_of_code: *const u8, // One past chunk end.

    // Ensure that the chunk can't be modified so ip isn't invalidated.
    chunk: &'c Chunk,

    stack: Vec<Value>,
    globals: HashMap<String, Value>,
}

impl<'c> VmState<'c> {
    fn new(chunk: &Chunk) -> VmState {
        let Range { start, end } = chunk.code.as_ptr_range();
        VmState {
            ip: start,
            end_of_code: end,
            chunk,
            stack: Vec::with_capacity(STACK_SIZE),
            globals: HashMap::default(),
        }
    }

    #[inline]
    fn line_number(&self) -> usize {
        let offset = self.offset();
        match self.chunk.line_numbers.binary_search_by_key(&offset, |e| e.0) {
            Ok(index) => self.chunk.line_numbers[index].1,
            Err(insert_index) => {
                debug_assert!(insert_index > 0, "Line numbers out of sync");
                self.chunk.line_numbers[insert_index - 1].1
            }
        }
    }

    #[inline]
    fn offset(&self) -> usize {
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

    pub fn execute(&mut self, chunk: &Chunk) -> Result<(), RuntimeError> {
        let mut vm = VmState::new(chunk);
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

            #[cfg(trace)] {
                use crate::disassemble;
                disassemble::stack(&vm.stack);
                disassemble::instruction(vm.chunk, vm.offset(), &instruction);
            }

            use Instruction::*;
            use Value::*;
            match instruction {
                False            => vm.stack.push(Boolean(false)),
                True             => vm.stack.push(Boolean(true)),
                Instruction::Nil => vm.stack.push(Value::Nil),
                Constant { address } => {
                    let constant = vm.constant_at(address).clone();
                    vm.stack.push(constant);
                }
                DefineGlobal { address } => {
                    let global_name = match vm.constant_at(address) {
                        Value::Ref(box RefType::String(s)) => s.clone(),
                        _ => unreachable!("Non-string global name")
                    };
                    let value = vm.stack_pop();
                    vm.globals.insert(global_name, value);
                }
                Add => {
                    let rhs = vm.stack_pop();
                    let lhs = vm.stack_pop();
                    let result = match (lhs, rhs) {
                        (Number(a), Number(b)) => Number(a + b),
                        (Ref(box RefType::String(s1)),
                         Ref(box RefType::String(s2))) => Value::string(s1 + &s2),
                        (lhs, rhs) => return Err(RuntimeError::BinaryOperatorNotApplicable {
                            lhs, rhs, operator: '+', line: vm.line_number()
                        })
                    };
                    vm.stack.push(result);
                }
                Divide   => binary_operator!(vm, /, Number -> Number),
                Multiply => binary_operator!(vm, *, Number -> Number),
                Subtract => binary_operator!(vm, -, Number -> Number),
                Greater  => binary_operator!(vm, >, Number -> Boolean),
                Less     => binary_operator!(vm, <, Number -> Boolean),
                Equal    => {
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
                            line: vm.line_number()
                        })
                    }
                }
                Not => {
                    let value = vm.stack_peek();
                    *value = Boolean(value.is_falsey());
                }
                Pop => { vm.stack_pop(); }
                Print => {
                    println!("{}", vm.stack_pop());
                }
                Return => {
                    return Ok(())
                }
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

    match opcode {
        OpCode::Constant => Instruction::Constant {
            // Safety: Safe as long as the instruction stream is well formed.
            address: ConstantAddr(*address.add(1))
        },
        OpCode::DefineGlobal => Instruction::DefineGlobal {
            address: ConstantAddr(*address.add(1))
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
