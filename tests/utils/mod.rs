#![allow(dead_code)]

extern crate rlox;

use std::path::Path;

use rlox::{Lox, Value, Value::*};

#[macro_export]
macro_rules! lox {
    ( $( $code:tt)+ ) => { result_of(stringify!($( $code )*)) }
}

pub fn result_of(code_snippet: &str) -> Option<Value> {
    let mut lox = Lox;

    match lox.run(Path::new("test_code"), code_snippet) {
        Ok((value, _)) => value,
        Err(error) => panic!(
            "Lox code '{}' raised error: '{}'", code_snippet, error
        )
    }
}

pub fn string(string: &str) -> Value {
    String(string.to_owned())
}
