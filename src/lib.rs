#![feature(or_patterns)]
#![feature(slice_ptr_range)]

mod token;
mod scanner;
mod parser;
mod resolver;
mod interpreter;
mod error;
mod native_functions;

mod value;
mod bytecode;
mod compiler;
mod runtime;
pub mod disassemble;

use std::{
    fmt, io,
    path::Path,
    error::Error,
};

pub use interpreter::{Value, NativeFn, Interpreter};

pub type LoxResult<'s, T> = std::result::Result<T, LoxError<'s>>;

#[derive(Debug)]
pub enum LoxError<'s> {
    ParseErrors(Vec<error::ParseError<'s>>),
    CompileErrors(Vec<compiler::CompileError<'s>>),
    RuntimeError(interpreter::RuntimeError<'s>),
    VmError(runtime::RuntimeError),
}

#[derive(Clone, Copy, Debug)]
pub enum Status {
    AwaitingInput,
    Terminated(i32),
}

pub struct VirtualMachine<'io> {
    runtime: runtime::Runtime<'io>,
}

impl<'io> VirtualMachine<'io> {
    pub fn new(stdout: &'io mut dyn io::Write) -> VirtualMachine<'io> {
        VirtualMachine { runtime: runtime::Runtime::new(stdout) }
    }

    pub fn execute<'s>(
        &mut self, _file: &Path, source_code: &'s str
    ) -> Result<(), LoxError<'s>> {
        use scanner::Scanner;

        let ast = parser::parse(source_code.tokens())?;
        let bytecode = compiler::compile(&ast)?;
        Ok(self.runtime.execute(&bytecode)?)
    }
}

pub struct Lox<'io> {
    interpreter: Interpreter<'io>
}

impl<'io> Lox<'io> {
    pub fn new(stdout: &'io mut dyn io::Write) -> Lox<'io> {
        let mut interpreter = Interpreter::new(stdout);
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
        let reference_table = resolver::resolve_references(&ast)?;
        let value = self.interpreter.evaluate(&ast, &reference_table)?;

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
            },

            CompileErrors(compiler_errors) => {
                for error in compiler_errors { writeln!(f, "ERROR {}", error)? }
            }

            RuntimeError(runtime_error) => write!(f, "ERROR {}", runtime_error)?,

            VmError(runtime_error) => write!(f, "ERROR {}", runtime_error)?
        }

        Ok(())
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

impl<'s> From<Vec<compiler::CompileError<'s>>> for LoxError<'s> {
    fn from(errors: Vec<compiler::CompileError<'s>>) -> Self {
        Self::CompileErrors(errors)
    }
}

impl<'s> From<runtime::RuntimeError> for LoxError<'s> {
    fn from(error: runtime::RuntimeError) -> Self {
        Self::VmError(error)
    }
}
