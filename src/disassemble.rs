use std::ops::Range;

use crate::{
    bytecode::{Chunk, OpCode, Instruction, Value},
    runtime,
    disassemble,
};

pub fn chunk(chunk: &Chunk, name: &str) {
    eprintln!("== {} ==", name);
    let Range { start, end } = chunk.code.as_ptr_range();
    let mut ip = start;
    while ip < end {
        // Safety: Since we're iterating between the code block
        // bounds, this is guaranteed to be safe.
        let instruction = unsafe { runtime::decode(ip) };
        let offset = ip as usize - start as usize;
        disassemble::instruction(chunk, offset, &instruction);
        // Safety: As long as instruction.size() returns the right
        // this is guaranteed to be safe.
        ip = unsafe { ip.add(instruction.size()) };
    }
}

macro_rules! op_code {
    ($instruction:tt) => { eprint!("{:?}", OpCode::$instruction) }
}

pub fn instruction(chunk: &Chunk, offset: usize, instruction: &Instruction) {
    use Instruction::*;

    match chunk.line_numbers.binary_search_by_key(&offset, |e| e.0) {
        Ok(index) => {
            let line_number = chunk.line_numbers[index].1;
            eprint!("{:#04x} {:4}  ", offset, line_number);
        }
        Err(_) => eprint!("{:#04x}    |  ", offset)
    }

    match instruction {
        Constant { address } => {
            let constant = chunk.constants[address.0 as usize];
            eprint!("{:?}  {:16}  '{}'", OpCode::Constant, address.0, constant);
        },
        Add      => op_code!(Add),
        Divide   => op_code!(Divide),
        False    => op_code!(False),
        Multiply => op_code!(Multiply),
        Negate   => op_code!(Negate),
        Nil      => op_code!(Nil),
        Not      => op_code!(Not),
        Return   => op_code!(Return),
        Subtract => op_code!(Subtract),
        True     => op_code!(True),
    }
    eprintln!();
}

pub fn stack(stack: &[Value]) {
    eprint!("        ");
    for item in stack.iter() {
        eprint!("[ {} ]", item)
    }
    eprintln!();
}
