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
        [_]                         => repl(),
        [_, t] if t == "-t"         => slow_repl(),
        [_, script]                 => run_script(&script),
        [_, t, script] if t == "-t" => slow_run_script(&script),
        _                           => {
            println!("Usage: lox [-c] [script]");
            64
        }
    };

    std::process::exit(exit_status);
}

fn slow_repl() -> ExitStatus {
    use rlox::Lox;

    let mut input = String::with_capacity(50);
    let stdin = io::stdin();
    let mut input_stream = stdin.lock();
    let stdout = &mut io::stdout();
    let mut lox = Lox::new(stdout);
    let path = Path::new("(REPL)");

    println!("RLox 1.0 Treewalk (interactive mode)");
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

fn slow_run_script<P: AsRef<Path>>(script_file: P) -> ExitStatus {
    let script = match std::fs::read_to_string(script_file.as_ref()) {
        Ok(contents) => contents,
        Err(error) => {
            println!(
                "ERROR: Unable to read source file \"{}\": {}",
                script_file.as_ref().display(),
                error
            );
            return 64
        }
    };

    let stdout = &mut io::stdout();
    let mut lox = rlox::Lox::new(stdout);
    match lox.run(script_file.as_ref(), &script) {
        Ok((_, Status::Terminated(exit_status))) => exit_status,
        Ok(_)                                    => 0,
        Err(error) => {
            println!("{}", error);
            65
        }
    }
}

fn repl() -> ExitStatus {
    use rlox::VirtualMachine;

    let mut input = String::with_capacity(50);
    let stdin = io::stdin();
    let mut input_stream = stdin.lock();
    let stdout = &mut io::stdout();
    let path = Path::new("(REPL)");
    let mut vm = VirtualMachine::new(stdout);

    println!("RLox VM 1.0 (interactive mode)");
    loop {
        print!("> ");
        io::stdout().flush().expect("Unable to flush stdout");
        input_stream.read_line(&mut input).expect("Unable to read from stdin");
        vm.execute(&path, &input);
        input.clear();
    }
}

fn run_script<P: AsRef<Path>>(script_file: P) -> ExitStatus {
    use rlox::VirtualMachine;

    let script = match std::fs::read_to_string(script_file.as_ref()) {
        Ok(contents) => contents,
        Err(error) => {
            println!(
                "ERROR: Unable to read source file \"{}\": {}",
                script_file.as_ref().display(),
                error
            );
            return 64
        }
    };

    let stdout = &mut io::stdout();
    let mut vm = VirtualMachine::new(stdout);
    vm.execute(&script_file.as_ref(), &script);

    0
}
