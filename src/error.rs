use thiserror::Error;

use crate::token::Token;

#[derive(Clone, Debug, Error)]
pub enum ParseError<'s> {
    #[error("ERROR (line {}): Unexpected token {0}", .0.line)]
    UnexpectedToken(Token<'s>),

    #[error("ERROR (line {}): Unexpected end of file", .0.line)]
    UnexpectedEndOfFile(Token<'s>),

    #[error("ERROR (line {}): Unterminated string {0}", .0.line)]
    UnterminatedString(Token<'s>),

    #[error("ERROR (line {}): Missing delimiter to close '{opening_delimiter}', \
             found {token}", token.line)]
    UnmatchedDelimiter {
        token: Token<'s>,
        opening_delimiter: Token<'s>
    },

    #[error("ERROR (line {}): Expected expression, found {0}", .0.line)]
    ExpectedExpression(Token<'s>)
}
