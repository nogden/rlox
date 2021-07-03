#![allow(dead_code)]

extern crate rlox;

use std::{io, path::Path};

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
    let stdout = &mut io::stdout();
    let mut lox = Lox::new(stdout);

    match lox.run(Path::new("test_code"), code_snippet) {
        Ok((value, _)) => value,
        Err(error) => panic!("Lox code '{}' raised error: '{}'", code_snippet, error),
    }
}

pub fn output_of(code_snippet: &str) -> String {
    let mut fake_std_out: Vec<u8> = Vec::new();
    let mut lox = Lox::new(&mut fake_std_out);

    match lox.run(Path::new("test_code"), code_snippet) {
        Ok(_) => String::from_utf8(fake_std_out).expect("Output contained invalid UTF-8"),
        Err(error) => panic!("Lox code '{}' raised error: '{}'", code_snippet, error),
    }
}

pub fn string(string: &str) -> Value {
    Value::String(string.to_owned())
}
