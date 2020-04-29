#![allow(dead_code)]
#![feature(or_patterns)]

#[cfg(test)]
mod linkedlist;
mod token;
mod scanner;
mod parser;
mod interpreter;
mod error;

use std::{
    io::self,
    path::Path,
};

use thiserror::Error;

pub type LoxResult<T> = std::result::Result<T, Error>;

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
                for error in syntax_errors {
                    println!("ERROR {}", error)
                }
                return Ok(Status::AwaitingInput)
            }
        };

        match ast.evaluate() {
            Ok(value) => println!("{}", value),
            Err(runtime_error) => println!("ERROR {}", runtime_error)
        }

        Ok(Status::AwaitingInput)
    }
}
