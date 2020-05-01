use std::{
    io::self,
    io::prelude::*,
    path::Path,
};

use rlox::{Status};

type ExitStatus = i32;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let exit_status = match args.as_slice() {
        [_]              => repl(),
        [_, script_file] => run_script(&script_file),
        [..]             => {
            println!("Usage: lox [script]");
            64
        }
    };

    std::process::exit(exit_status);
}

fn repl() -> ExitStatus {
    let mut input = String::new();
    let stdin = io::stdin();
    let mut input_stream = stdin.lock();
    let mut lox = rlox::Lox::default();
    let path = Path::new("(REPL)");

    println!("RLox 1.0 (interactive mode)");
    loop {
        print!("> ");
        io::stdout().flush().expect("Unable to flush stdout");
        input_stream.read_line(&mut input).expect("Unable to read from stdin");
        match lox.run(&path, &input) {
            Ok((result, status)) => {
                if let Some(value) = result {
                    println!("{}", value);
                }

                if let Status::Terminated(exit_status) = status {
                    return exit_status
                }
            }
            Err(error) => println!("{}", error),
        }
        input.clear();
    }
}

fn run_script<P: AsRef<Path>>(file: P) -> ExitStatus {
    let script = match std::fs::read_to_string(file.as_ref()) {
        Ok(contents) => contents,
        Err(error) => {
            println!(
                "ERROR: Unable to read source file \"{}\": {}",
                file.as_ref().display(),
                error
            );
            return 64
        }
    };

    let mut lox = rlox::Lox::default();
    match lox.run(file.as_ref(), &script) {
        Ok((_, Status::Terminated(exit_status))) => exit_status,
        Ok(_)                                    => 0,
        Err(error) => {
            println!("{}", error);
            65
        }
    }
}
