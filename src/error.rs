use thiserror::Error;

use crate::token::Token;

#[derive(Clone, Debug, Error)]
pub enum ParseError<'s> {
    #[error("(line {line}): Unexpected character '{character}'")]
    UnexpectedCharacter {
        character: &'s str,
        line: usize
    },

    #[error("(line {}): Unexpected end of file", .0.line)]
    UnexpectedEndOfFile(Token<'s>),

    #[error("(line {}): Unterminated string {0}", .0.line)]
    UnterminatedString(Token<'s>),

    #[error("(line {}): Missing closing delimiter for \
             '{opening_delimiter}', found {token}", token.line)]
    UnmatchedDelimiter {
        token: Token<'s>,
        opening_delimiter: Token<'s>
    },

    #[error("(line {}): Expected expression, found {0}", .0.line)]
    ExpectedExpression(Token<'s>)
}
