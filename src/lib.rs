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
    fmt,
    path::Path,
    error::Error
};

pub type LoxResult<'s, T> = std::result::Result<T, LoxError<'s>>;

#[derive(Debug)]
pub enum LoxError<'s> {
    ParseErrors(Vec<error::ParseError<'s>>),
    RuntimeError(interpreter::RuntimeError<'s>)
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

    pub fn run<'s>(
        &mut self, _file: &Path, source_code: &'s str
    ) -> LoxResult<'s, (interpreter::Value, Status)> {
        use scanner::Scanner;
        use interpreter::Evaluate;

        let ast = parser::parse(source_code.tokens())?;
        let value = ast.evaluate()?;

        Ok((value, Status::AwaitingInput))
    }
}

impl<'s> Error for LoxError<'s> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl<'s> fmt::Display for LoxError<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use LoxError::*;

        match self {
            ParseErrors(parse_errors) => {
                for error in parse_errors { write!(f, "{}", error)? }
                Ok(())
            },

            RuntimeError(runtime_error) => write!(f, "{}", runtime_error)
        }
    }
}

impl<'s> From<Vec<error::ParseError<'s>>> for LoxError<'s> {
    fn from(errors: Vec<error::ParseError<'s>>) -> Self {
        Self::ParseErrors(errors)
    }
}

impl<'s> From<interpreter::RuntimeError<'s>> for LoxError<'s> {
    fn from(error: interpreter::RuntimeError<'s>) -> Self {
        Self::RuntimeError(error)
    }
}
