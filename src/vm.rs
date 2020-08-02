use std::{
    ops::Range,
    convert::TryFrom,
};

use thiserror::Error;

use crate::{
    bytecode::{Chunk, OpCode, Instruction, ConstantAddr},
    value::Value,
};

#[derive(Clone, Debug)]
pub struct VirtualMachine;

#[derive(Clone, Debug, Error)]
pub enum Error {
    #[error("Compiler error")]
    CompileError,

    #[error("Runtime error")]
    RuntimeError,
}

const STACK_SIZE: usize = 256;

struct VmState<'c> {
    ip: *const u8,
    end_of_code: *const u8, // One past chunk end.

    // Ensure that the chunk can't be modified so ip isn't invalidated.
    chunk: &'c Chunk,

    stack: Vec<Value>,
}

impl<'c> VmState<'c> {
    fn new(chunk: &Chunk) -> VmState {
        let Range { start, end } = chunk.code.as_ptr_range();
        VmState {
            ip: start,
            end_of_code: end,
            chunk,
            stack: Vec::with_capacity(STACK_SIZE),
        }
    }

    #[cfg(trace)]
    fn offset(&self) -> usize {
        self.ip as usize - self.chunk.code.as_ptr() as usize
    }
}

macro_rules! binary_operator {
    ($vm:ident, $operator:tt) => {{
        let b = $vm.stack.pop().expect("Empty stack (Binary Op)");
        let a = $vm.stack.pop().expect("Empty stack (Binary Op)");
        $vm.stack.push(a $operator b);
    }}
}

impl VirtualMachine {
    pub fn execute(&mut self, chunk: &Chunk) -> Result<(), Error> {
        let mut vm = VmState::new(chunk);
        self.run(&mut vm)
    }

    fn run(&mut self, vm: &mut VmState) -> Result<(), Error> {
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
            match instruction {
                Constant { address } => {
                    let constant = unsafe {
                        // Safety: This is guaranteed to be ok since a
                        // ConstantAddr can only be obtained by adding
                        // the constant, so it has to be present.
                        vm.chunk.constants.get_unchecked(address.0 as usize)
                    };
                    vm.stack.push(*constant);
                }
                Add      => binary_operator!(vm, +),
                Divide   => binary_operator!(vm, /),
                Multiply => binary_operator!(vm, *),
                Subtract => binary_operator!(vm, -),
                Negate => {
                    let value = vm.stack.last_mut().expect("Empty stack (Negate)");
                    *value = -(*value);
                }
                Return => {
                    println!("{}", vm.stack.pop().expect("Empty stack (Return)"));
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
        OpCode::Add      => Instruction::Add,
        OpCode::Divide   => Instruction::Divide,
        OpCode::Multiply => Instruction::Multiply,
        OpCode::Negate   => Instruction::Negate,
        OpCode::Return   => Instruction::Return,
        OpCode::Subtract => Instruction::Subtract,
    }
}
