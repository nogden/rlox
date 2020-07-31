use num_enum::{IntoPrimitive, TryFromPrimitive};
use thiserror::Error;

use crate::{
    value::Value,
};

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    Constant { address: ConstantAddr },
    Return,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, IntoPrimitive, TryFromPrimitive)]
pub enum OpCode {
    Constant,
    Return,
}

#[derive(Debug, Clone, Copy, Error)]
#[error("Byte {opcode} at {offset:#04x} does not match any known op-code")]
pub struct UnknownOpCode {
    offset: usize,
    opcode: u8,
}

#[derive(Debug, Clone, Default)]
pub struct Chunk {
    pub(crate) code: Vec<u8>,
    pub(crate) constants: Vec<Value>,
    pub(crate) line_numbers: Vec<(usize, u32)>,
}

#[derive(Debug, Clone, Copy)]
pub struct ConstantAddr(pub(crate) u8);

impl Instruction {
    pub fn size(&self) -> usize {
        use Instruction::*;

        match self {
            Constant { .. } => 2,
            Return          => 1,
        }
    }
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk::default()
    }

    pub fn write(&mut self, instruction: &Instruction, line: u32) {
        use Instruction::*;

        self.record_line_number(line);
        match instruction {
            Constant { address } => {
                self.code.push(OpCode::Constant.into());
                self.code.push(address.0);
            }
            Return => self.code.push(OpCode::Return.into())
        }
    }

    pub fn add_constant(&mut self, constant: Value) -> Instruction {
        let location = self.constants.len();
        self.constants.push(constant);
        Instruction::Constant { address: ConstantAddr(location as u8) }
    }

    fn record_line_number(&mut self, line: u32) {
        let offset = self.code.len();
        if let Some((_, last_line)) = self.line_numbers.last() {
            if line != *last_line {
                self.line_numbers.push((offset, line));
            }
        } else {
            self.line_numbers.push((offset, line));
        }
    }
}
