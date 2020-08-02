use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::{
    value::Value,
};

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    Add,
    Constant { address: ConstantAddr },
    Divide,
    Multiply,
    Negate,
    Return,
    Subtract,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, IntoPrimitive, TryFromPrimitive)]
pub enum OpCode {
    Add,
    Constant,
    Divide,
    Multiply,
    Negate,
    Return,
    Subtract,
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
            Add             => 1,
            Constant { .. } => 2,
            Divide          => 1,
            Multiply        => 1,
            Negate          => 1,
            Return          => 1,
            Subtract        => 1,
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
            Add => self.code.push(OpCode::Add.into()),
            Constant { address } => {
                self.code.push(OpCode::Constant.into());
                self.code.push(address.0);
            }
            Divide => self.code.push(OpCode::Divide.into()),
            Multiply => self.code.push(OpCode::Multiply.into()),
            Negate => self.code.push(OpCode::Negate.into()),
            Return => self.code.push(OpCode::Return.into()),
            Subtract => self.code.push(OpCode::Subtract.into()),
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
