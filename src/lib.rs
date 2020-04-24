#![allow(dead_code)]
#![feature(or_patterns)]

#[cfg(test)]
mod linkedlist;
mod scanner;
mod parser;

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
        // TODO(nick): Pass error reporter for error handling
        let tokens = source_code.tokens();

        for token in tokens.clone() {
            println!("{:?}", token);
        }

        let ast = parser::parse(tokens)?;
        println!("{:?}", ast);

        // println!("{}", Binary(&Unary(&tokens[0], &Literal(&tokens[1])), &tokens[2], &Grouping(&Literal(&tokens[4]))));

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
    #[error("Could not read file")]
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
