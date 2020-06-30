#![feature(or_patterns)]

mod token;
mod scanner;
mod parser;
mod interpreter;
mod error;
mod native_functions;

use std::{
    fmt, io,
    path::Path,
    error::Error,
};

pub use interpreter::{Value, NativeFn};

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

pub struct Lox<'io> {
    interpreter: interpreter::Interpreter<'io>
}

impl<'io> Lox<'io> {
    pub fn new(stdout: &'io mut dyn io::Write) -> Lox<'io> {
        let mut interpreter = interpreter::Interpreter::new(stdout);
        interpreter.expose("elapsed_time", native_functions::elapsed_time);

        Lox { interpreter }
    }

    pub fn run<'s>(
        &mut self,
        _file: &Path,
        source_code: &'s str,
    ) -> LoxResult<'s, (Option<Value>, Status)> {
        use scanner::Scanner;

        let ast = parser::parse(source_code.tokens())?;
        let value = self.interpreter.evaluate(&ast)?;

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
                for error in parse_errors { writeln!(f, "ERROR {}", error)? }
                Ok(())
            },

            RuntimeError(runtime_error) => write!(f, "ERROR {}", runtime_error)
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
