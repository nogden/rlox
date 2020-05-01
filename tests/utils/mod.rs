#![allow(dead_code)]

extern crate rlox;

use std::path::Path;

use rlox::{Lox, Value};

#[macro_export]
macro_rules! lox {
    ( $( $code:tt )+ ) => { result_of(stringify!($( $code )*)) }
}

#[macro_export]
macro_rules! lox_stdout {
    ( $( $code:tt )+ ) => { output_of(stringify!($( $code )*)) }
}

pub fn result_of(code_snippet: &str) -> Option<Value> {
    let mut lox = Lox::default();

    match lox.run(Path::new("test_code"), code_snippet) {
        Ok((value, _)) => value,
        Err(error) => panic!(
            "Lox code '{}' raised error: '{}'", code_snippet, error
        )
    }
}

pub fn output_of(code_snippet: &str) -> String {
    let mut output_buffer: Vec<u8> = Vec::new();
    let mut lox = Lox { stdout: &mut output_buffer };

    match lox.run(Path::new("test_code"), code_snippet) {
        Ok(_) => String::from_utf8(output_buffer)
            .expect("Output contained invalid UTF-8"),
        Err(error) => panic!(
            "Lox code '{}' raised error: '{}'", code_snippet, error
        )
    }
}

pub fn string(string: &str) -> Value {
    Value::String(string.to_owned())
}
