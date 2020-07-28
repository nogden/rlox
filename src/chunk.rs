use std::{fmt, slice, iter, convert::TryFrom};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use thiserror::Error;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, IntoPrimitive, TryFromPrimitive)]
pub enum OpCode {
    Return,
}

#[derive(Debug, Clone, Copy, Error)]
#[error("Byte {opcode} at {offset:#04x} does not match any known op-code")]
pub struct UnknownOpCode {
    offset: usize,
    opcode: u8,
}

#[derive(Clone)]
pub struct Chunk {
    memory: Vec<u8>,
}

impl OpCode {
    pub fn operand_size(&self) -> u8 {
        match self {
            OpCode::Return => 0,
        }
    }
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk { memory: Vec::new() }
    }

    pub fn write(&mut self, operation: OpCode) {
        self.memory.push(operation.into());
    }

    pub fn instructions(&self) -> Instructions {
        Instructions { memory: self.memory.iter().enumerate() }
    }
}

pub struct Instructions<'m> {
    memory: iter::Enumerate<slice::Iter<'m, u8>>
}

impl<'m> Iterator for Instructions<'m> {
    type Item = Result<(usize, OpCode), UnknownOpCode>;

    fn next(&mut self) -> Option<Self::Item> {
        let (offset, byte) = self.memory.next()?;
        let opcode = match OpCode::try_from(*byte) {
            Ok(opcode) => opcode,
            Err(_) => return Some(Err(UnknownOpCode { offset, opcode: *byte }))
        };

        for _ in 0..opcode.operand_size() {
            self.memory.next();
        }

        Some(Ok((offset, opcode)))
    }
}

impl fmt::Debug for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for instruction in self.instructions() {
            match instruction {
                Ok((offset, operation)) =>
                    write!(f, "{:04}  {:?}", offset, operation)?,
                Err(UnknownOpCode { offset, opcode }) =>
                    write!(f, "{:04}  {:#04x} <unknown opcode>", offset, opcode)?
            }

        }

        Ok(())
    }
}
