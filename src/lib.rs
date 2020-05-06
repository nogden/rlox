#![allow(dead_code)]
#![feature(or_patterns)]

mod token;
mod scanner;
mod parser;
mod interpreter;
mod error;

use std::{
    fmt, io,
    path::Path,
    error::Error,
    collections::HashMap,
};

use interpreter::RuntimeError;

pub use interpreter::Value;

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
    pub stdout: &'io mut dyn io::Write,
   bindings: HashMap<String, Value>,
}

impl<'io> Lox<'io> {
    pub fn new(stdout: &'io mut dyn io::Write) -> Lox<'io> {
        Lox { stdout, bindings: HashMap::new() }
    }

    pub fn run<'s>(
        &mut self, _file: &Path, source_code: &'s str
    ) -> LoxResult<'s, (Option<Value>, Status)> {
        use scanner::Scanner;
        use interpreter::Evaluate;

        let ast = parser::parse(source_code.tokens())?;
        let value = ast.evaluate(self)?;

        Ok((value, Status::AwaitingInput))
    }
}

impl<'io> interpreter::Environment for Lox<'io> {
    fn stdout(&mut self) -> &mut dyn io::Write {
        self.stdout
    }

    fn define(&mut self, identifier: &str, value: Value) {
        let _ = self.bindings.insert(identifier.to_owned(), value);
    }

    fn assign<'s>(
        &mut self, identifier: &token::Token<'s>, value: Value
    ) -> Result<(), RuntimeError<'s>> {
        if let Some(bound_value) = self.bindings.get_mut(identifier.lexeme) {
            *bound_value = value;
            Ok(())
        } else {
            Err(RuntimeError::UnresolvedIdentifier(*identifier))
        }
    }

    fn resolve(&self, identifier: &token::Token) -> Option<Value> {
        self.bindings.get(identifier.lexeme).cloned()
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
