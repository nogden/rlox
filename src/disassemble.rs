use crate::{
    bytecode::{Chunk, Instruction, OpCode},
    disassemble, runtime,
    value::{StringTable, Value},
};
use std::ops::Range;

pub(crate) fn chunk(chunk: &Chunk, name: &str, strings: &StringTable) {
    eprintln!("== {} ==", name);
    let Range { start, end } = chunk.code.as_ptr_range();
    let mut ip = start;
    while ip < end {
        // Safety: Since we're iterating between the code block
        // bounds, this is guaranteed to be safe.
        let instruction = unsafe { runtime::decode(ip) };
        let offset = ip as usize - start as usize;
        disassemble::instruction(chunk, offset, &instruction, strings);
        // Safety: As long as instruction.size() returns the right
        // this is guaranteed to be safe.
        ip = unsafe { ip.add(instruction.size()) };
    }
}

macro_rules! op_code {
    ($instruction:tt) => {
        eprint!("{:?}", OpCode::$instruction)
    };
}

pub fn instruction(chunk: &Chunk, offset: usize, instruction: &Instruction, strings: &StringTable) {
    use Instruction::*;

    match chunk.line_numbers.binary_search_by_key(&offset, |e| e.0) {
        Ok(index) => {
            let line_number = chunk.line_numbers[index].1;
            eprint!("{:#04x} {:4}  ", offset, line_number);
        }
        Err(_) => eprint!("{:#04x}    |  ", offset),
    }

    #[rustfmt::skip]
    match instruction {
        Constant { address } => {
            let constant = &chunk.constants[address.0 as usize];
            if let Value::String(sym) = constant {
                let string = strings.resolve(*sym).expect("Missing string");
                eprint!("{:?}  {:16}  '{}'", OpCode::Constant, address.0, string);
            } else {
                eprint!("{:?}  {:16}  '{}'", OpCode::Constant, address.0, constant);
            }
        }
        DefineGlobal { address } => {
            let global = &chunk.constants[address.0 as usize];
            if let Value::String(sym) = global {
                let string = strings.resolve(*sym).expect("Missing string");
                eprint!("{:?}  {:16}  '{}'", OpCode::DefineGlobal, address.0, string);
            } else {
                eprint!("{:?}  {:16}  '{}'", OpCode::DefineGlobal, address.0, global);
            }
        }
        ResolveGlobal { address } => {
            let global = &chunk.constants[address.0 as usize];
            if let Value::String(sym) = global {
                let string = strings.resolve(*sym).expect("Missing string");
                eprint!("{:?}  {:16}  '{}'", OpCode::ResolveGlobal, address.0, string);
            } else {
                eprint!("{:?}  {:16}  '{}'", OpCode::ResolveGlobal, address.0, global);
            }
        }
        Add =>      op_code!(Add),
        Divide =>   op_code!(Divide),
        Equal =>    op_code!(Equal),
        False =>    op_code!(False),
        Greater =>  op_code!(Greater),
        Less =>     op_code!(Less),
        Multiply => op_code!(Multiply),
        Negate =>   op_code!(Negate),
        Nil =>      op_code!(Nil),
        Not =>      op_code!(Not),
        Pop =>      op_code!(Pop),
        Print =>    op_code!(Print),
        Return =>   op_code!(Return),
        Subtract => op_code!(Subtract),
        True =>     op_code!(True),
    }
    eprintln!();
}

pub(crate) fn stack(stack: &[Value]) {
    eprint!("        |  [");
    for item in stack.iter() {
        eprint!(" {:?} ", item)
    }
    eprintln!("]");
}
