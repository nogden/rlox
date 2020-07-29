use std::{fmt, convert::TryFrom};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use thiserror::Error;

use crate::value::Value;

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

#[derive(Clone, Default)]
pub struct Chunk {
    instructions: Vec<u8>,
    constants: Vec<Value>,
    line_numbers: Vec<(usize, u32)>,
}

#[derive(Clone, Copy)]
pub struct ConstantAddr(u8);

impl OpCode {
    pub fn operand_size(&self) -> u8 {
        match self {
            OpCode::Constant => 1,
            OpCode::Return => 0,
        }
    }
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk::default()
    }

    pub fn write_return(&mut self, line: u32) {
        self.record_line_number(line);
        self.instructions.push(OpCode::Return.into());
    }

    pub fn write_constant(&mut self, constant: ConstantAddr, line: u32) {
        self.record_line_number(line);
        self.instructions.push(OpCode::Constant.into());
        self.instructions.push(constant.0);
    }

    pub fn add_constant(&mut self, constant: Value) -> ConstantAddr {
        let location = self.constants.len();
        self.constants.push(constant);
        ConstantAddr(location as u8)
    }

    fn record_line_number(&mut self, line: u32) {
        let offset = self.instructions.len();
        if let Some((_, last_line)) = self.line_numbers.last() {
            if line != *last_line {
                self.line_numbers.push((offset, line));
            }
        } else {
            self.line_numbers.push((offset, line));
        }
    }

    fn fmt_instruction<'b>(
        &self,
        opcode: OpCode,
        offset: usize,
        f: &mut fmt::Formatter,
        mut bytes: impl Iterator<Item = (usize, &'b u8)>
    ) -> fmt::Result {
        write!(f, "{:#04x}  ", offset)?;

        match self.line_numbers.binary_search_by_key(&offset, |e| e.0) {
            Ok(index) => {
                let line_number = self.line_numbers.get(index)
                    .expect("Binary search is broken").1;
                write!(f, "{:4}  {:?} ", line_number, opcode)?;
            }
            Err(_) => write!(f, "   |  {:?} ", opcode)?
        }

        match opcode {
            OpCode::Constant => {
                let (_, constant_addr) = bytes.next()
                    .expect("Unexpected end of chunk");
                let constant_value = self.constants.get(*constant_addr as usize)
                    .expect("Missing expected constant");
                write!(f, "{:6} '{}'", constant_addr, constant_value)?;
            }
            OpCode::Return => {}
        }

        writeln!(f)
    }
}

impl fmt::Debug for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut bytes = self.instructions.iter().enumerate();
        while let Some((offset, byte)) = bytes.next() {
            if let Ok(opcode) = OpCode::try_from(*byte) {
                self.fmt_instruction(opcode, offset, f, &mut bytes)?;
            } else {
                writeln!(f, "{:#04x}  <unknown opcode> {:#04x}", offset, byte)?;
            }
        }

        Ok(())
    }
}
