use std::{
//    io::BufReader,
//    fs::File,
    path::Path,
};

type Result<T> = anyhow::Result<T>;

fn main() -> Result<()> {
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

fn run_prompt() -> Result<i32> {
    println!("Run prompt");
    Ok(0)
}

fn run_file<P: AsRef<Path>>(file: P) -> Result<i32> {
    println!("Run script: {:?}", file.as_ref());
    Ok(0)
}
