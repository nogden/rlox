use std::fmt;

use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Boolean(bool),
    Number(f64),
    Nil,
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    Add,
    Constant { address: ConstantAddr },
    Divide,
    Equal,
    False,
    Greater,
    Less,
    Multiply,
    Negate,
    Nil,
    Not,
    Return,
    Subtract,
    True,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, IntoPrimitive, TryFromPrimitive)]
pub enum OpCode {
    Add,
    Constant,
    Divide,
    Equal,
    False,
    Greater,
    Less,
    Multiply,
    Negate,
    Nil,
    Not,
    Return,
    Subtract,
    True,
}

#[derive(Debug, Clone, Default)]
pub struct IncompleteChunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    line_numbers: Vec<(Offset, LineNumber)>,
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub(crate) code: Vec<u8>,
    pub(crate) constants: Vec<Value>,
    pub(crate) line_numbers: Vec<(Offset, LineNumber)>,
}

type Offset = usize;
type LineNumber = usize;

#[derive(Debug, Clone, Copy)]
pub struct ConstantAddr(pub(crate) u8);

impl Value {
    pub fn is_falsey(&self) -> bool {
        use Value::*;

        match self {
            Boolean(false) | Nil => true,
            _ => false,
        }
    }
}

impl Instruction {
    pub fn size(&self) -> usize {
        use Instruction::*;

        match self {
            Constant { .. } => 2,
            _ => 1
        }
    }
}

impl Chunk {
    pub fn new() -> IncompleteChunk {
        IncompleteChunk::default()
    }
}

impl IncompleteChunk {
    pub fn write(&mut self, instruction: &Instruction, line: LineNumber) {
        use Instruction::*;

        self.record_line_number(line);
        match instruction {
            Constant { address } => {
                self.code.push(OpCode::Constant.into());
                self.code.push(address.0);
            }
            Add      => self.code.push(OpCode::Add.into()),
            Divide   => self.code.push(OpCode::Divide.into()),
            Equal    => self.code.push(OpCode::Equal.into()),
            False    => self.code.push(OpCode::False.into()),
            Greater  => self.code.push(OpCode::Greater.into()),
            Less     => self.code.push(OpCode::Less.into()),
            Multiply => self.code.push(OpCode::Multiply.into()),
            Negate   => self.code.push(OpCode::Negate.into()),
            Nil      => self.code.push(OpCode::Nil.into()),
            Not      => self.code.push(OpCode::Not.into()),
            Return   => self.code.push(OpCode::Return.into()),
            Subtract => self.code.push(OpCode::Subtract.into()),
            True     => self.code.push(OpCode::True.into()),
        }
    }

    pub fn add_constant(&mut self, constant: Value) -> Option<Instruction> {
        let location = self.constants.len();
        if location < u8::MAX.into() {
            self.constants.push(constant);
            Some(Instruction::Constant { address: ConstantAddr(location as u8) })
        } else {
            None
        }
    }

    pub fn complete(mut self) -> Chunk {
        self.code.push(OpCode::Return.into());

        Chunk {
            code: self.code,
            constants: self.constants,
            line_numbers: self.line_numbers
        }
    }

    fn record_line_number(&mut self, line: LineNumber) {
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;

        match self {
            Boolean(b) => write!(f, "{}", b),
            Nil        => write!(f, "nil"),
            Number(n)  => write!(f, "{}", n),
        }
    }
}
