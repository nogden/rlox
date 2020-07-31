use std::{
    ops::Range,
    convert::TryFrom,
};

use thiserror::Error;

use crate::{
    bytecode::{Chunk, OpCode, Instruction, ConstantAddr},
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

struct VmState<'c> {
    ip: *const u8,
    end_of_code: *const u8, // One past chunk end.

    // Holding this reference ensures that ip is always valid
    chunk: &'c Chunk,
}

impl<'c> VmState<'c> {
    fn new(chunk: &Chunk) -> VmState {
        let Range { start, end } = chunk.code.as_ptr_range();
        VmState {
            ip: start,
            end_of_code: end,
            chunk
        }
    }
}

impl VirtualMachine {
    pub fn execute(&mut self, chunk: &Chunk) -> Result<(), Error> {
        let mut vm = VmState::new(chunk);
        self.run(&mut vm)
    }

    fn run(&mut self, vm: &mut VmState) -> Result<(), Error> {
        loop {
            debug_assert!(vm.ip < vm.end_of_code, "Instruction pointer overrun");
            let instruction = unsafe { decode(vm.ip) };

            #[cfg(trace)]
            debug::disassemble(chunk, offset, instruction);

            use Instruction::*;
            match instruction {
                Constant { address } => {
                    let constant = unsafe {
                        // Safety: This is guaranteed to be ok since
                        // `address` can only be obtained by adding
                        // the constant, enforeced by the type system.
                        vm.chunk.constants.get_unchecked(address.0 as usize)
                    };
                    println!("{}", constant);
                }
                Return => { return Ok(()) }
            }
            // Safety: As long as instruction.size() is accurate this is
            // guaranteed to be safe.
            vm.ip = unsafe { vm.ip.add(instruction.size()) };
        }
    }
}

pub(crate) unsafe fn decode(address: *const u8) -> Instruction {
    // Safety: address must point to an opcode in a valid chunk
    let opcode = OpCode::try_from(*address).expect("Invalid opcode");

    match opcode {
        OpCode::Constant => Instruction::Constant {
            // Safety: This should be safe as the writing code
            // guarantees that instructions are written as a unit.
            address: ConstantAddr(*address.add(1))
        },
        OpCode::Return => Instruction::Return,
    }
}
