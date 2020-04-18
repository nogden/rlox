#[cfg(test)]
mod linkedlist;

use std::{
    io::self,
    path::Path,
    fmt::{self, Display, Formatter},
};

use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

pub struct Lox<'r> {
    pub report: &'r dyn Fn(&Location, &str),
}

impl<'r> Lox<'r> {
    pub fn new() -> Lox<'r> { Lox { report: &print_now } }

    pub fn run(&self, source: &str) -> Result<Status> {
        let tokens = scanner::tokens(source);

        for token in tokens {
            println!("{:?}", token);
        }

        let loc = Location {
            file: Path::new("test.lox"),
            line: 102,
            column: 23,
        };
        self.error(&loc, "This is a test error");

        Ok(Status::AwaitingInput)
    }

    fn error(&self, location: &Location, message: &str) {
        (self.report)(location, message);
    }

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

mod scanner {
    pub fn tokens(_source: &str) -> Vec<Token> {
        Vec::new()
    }

    #[derive(Clone, Copy, Debug)]
    pub struct Token;
}
