use std::{
    io::self,
    io::prelude::*,
    path::Path,
};

use rlox::Status;

fn main() -> rlox::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    let exit_status = match args.as_slice() {
        [_]              => run_prompt()?,
        [_, script_file] => run_file(&script_file)?,
        [..]             => {
            println!("Usage: lox [script]");
            64
        }
    };

    std::process::exit(exit_status);
}

fn run_prompt() -> rlox::Result<i32> {
    let mut input = String::new();
    let stdin = io::stdin();
    let mut input_stream = stdin.lock();

    println!("RLox 1.0 (interpreted mode)");
    loop {
        print!("> ");
        io::stdout().flush()?;
        input_stream.read_line(&mut input)?;
        if let Status::Terminated(exit_status) = rlox::run(&input)? {
            return Ok(exit_status)
        }
    }
}

fn run_file<P: AsRef<Path>>(file: P) -> rlox::Result<i32> {
    let script = std::fs::read_to_string(file)?;
    if let Status::Terminated(exit_status) = rlox::run(&script)? {
        Ok(exit_status)
    } else {
        Ok(0)
    }
}
