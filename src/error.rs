use thiserror::Error;

use crate::token::Token;

#[derive(Clone, Debug, Error)]
pub enum ParseError<'s> {
    #[error("(line {line}): Unexpected character '{character}'")]
    UnexpectedCharacter {
        character: &'s str,
        line: usize
    },

    #[error("Unexpected end of file")]
    UnexpectedEndOfFile,

    #[error("(line {}): Unterminated string {0}", .0.line)]
    UnterminatedString(Token<'s>),

    #[error("(line {}): Missing closing delimiter for \
             '{opening_delimiter}', found {token}", token.line)]
    UnmatchedDelimiter {
        token: Token<'s>,
        opening_delimiter: Token<'s>
    },

    #[error("(line {}): Expected {}, found {token}", token.line, expected)]
    UnexpectedToken {
        token: Token<'s>,
        expected: &'static str
    }
}
