#![allow(dead_code)]
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
    collections::HashMap,
};

pub use interpreter::Value;

pub type LoxResult<'s, T> = std::result::Result<T, LoxError<'s>>;

#[derive(Debug)]
pub enum LoxError<'s> {
    ParseErrors(Vec<error::ParseError<'s>>),
    RuntimeError(interpreter::RuntimeError<'s>)
}

#[derive(Clone, Debug)]
pub enum NativeError {
    ArityMismatch(usize),
    TypeMismatch(&'static str, Value),
    Failed(String),
}

#[derive(Clone, Copy, Debug)]
pub enum Status {
    AwaitingInput,
    Terminated(i32),
}

pub type NativeFn = fn(&Vec<Value>) -> Result<Value, NativeError>;

pub struct Lox<'io> {
    pub stdout: &'io mut dyn io::Write,
    bindings: HashMap<String, Value>,
    native_functions: Vec<NativeFn>
}

impl<'io> Lox<'io> {
    pub fn new(stdout: &'io mut dyn io::Write) -> Lox<'io> {
        let mut interpreter = Lox {
            stdout,
            bindings: HashMap::new(),
            native_functions: vec![]
        };

        interpreter.expose("elapsed_time", native_functions::elapsed_time);

        interpreter
    }

    pub fn run<'s>(
        &mut self,
        _file: &Path,
        source_code: &'s str,
    ) -> LoxResult<'s, (Option<Value>, Status)> {
        use interpreter::Evaluate;
        use scanner::Scanner;

        let ast = parser::parse(source_code.tokens())?;
        let value = ast.evaluate(self)?;

        Ok((value, Status::AwaitingInput))
    }

    pub fn expose<T: AsRef<str>>(&mut self, name: T, function: NativeFn) {
        let index = self.native_functions.len();
        self.native_functions.push(function);
        self.bindings.insert(
            name.as_ref().to_owned(), Value::native_fn(index)
        );
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
