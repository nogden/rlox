#![allow(dead_code)]

#[cfg(test)]
mod linkedlist;
mod scanner;

use std::{
    io::self,
    path::Path,
    fmt::{self, Display, Formatter},
};

use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

pub struct Lox;

impl Lox {
    pub fn new() -> Lox {
        Lox {}
    }

    pub fn run(&mut self, source: &str) -> Result<Status> {
        let tokens = scanner::tokens(source);

        for token in tokens {
            println!("{:?}", token);
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
    #[error("Could not read file")]
    BadFile(#[from] io::Error),
}

#[derive(Clone, Copy, Debug)]
pub enum Status {
    AwaitingInput,
    Terminated(i32),
}

#[derive(Clone, Debug)]
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
