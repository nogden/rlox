#![allow(dead_code)]
#![feature(or_patterns)]

#[cfg(test)]
mod linkedlist;
mod scanner;
mod parser;
mod interpreter;

use std::{
    io::self,
    path::Path,
    fmt::{self, Display, Formatter},
};

use thiserror::Error;

pub type LoxResult<T> = std::result::Result<T, Error>;

pub struct Lox;

impl Lox {
    pub fn new() -> Lox {
        Lox {}
    }

    pub fn run(&mut self, _file: &Path, source_code: &str) -> LoxResult<Status> {
        use scanner::Scanner;
        use interpreter::Evaluate;

        let ast = match parser::parse(source_code.tokens()) {
            Ok(ast) => ast,
            Err(syntax_errors) => {
                for error in syntax_errors { println!("{}", error) }
                return Ok(Status::AwaitingInput)
            }
        };

        match ast.evaluate() {
            Ok(value) => println!("{}", value),
            Err(runtime_error) => println!("{}", runtime_error)
        }

        Ok(Status::AwaitingInput)
    }
}

fn error(location: &Location, message: &str) {
    print_now(location, message);
}

#[allow(dead_code)]
fn print_now(location: &Location, message: &str) {
    eprintln!("error[{}]: {}", location, message);
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Could not read source file")]
    BadFile(#[from] io::Error),
}

#[derive(Clone, Copy, Debug)]
pub enum Status {
    AwaitingInput,
    Terminated(i32),
}

#[derive(Clone, Copy, Debug)]
pub struct Location<'p> {
    file: &'p Path,
    line: usize,
    column: usize,
}

impl Display for Location<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}:{}:{}", self.file, self.line, self.column)
    }
}
