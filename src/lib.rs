#[cfg(test)]
mod linkedlist;

use std::{
    io::self
};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Could not read file")]
    BadFile(#[from] io::Error),
}

#[derive(Clone, Copy, Debug)]
pub enum Status {
    AwaitingInput,
    Terminated(i32),
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn run(_source: &str) -> Result<Status> {
    Ok(Status::AwaitingInput)
}
