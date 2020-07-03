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
        opening_delimiter: Token<'s>,
    },

    #[error("(line {}): Expected {}, found '{token}'", token.line, expected)]
    UnexpectedToken {
        token: Token<'s>,
        expected: &'static str,
    },

    #[error("(line {}): Invalid assignment target '{0}'", .0.line)]
    InvalidAssignmentTarget(Token<'s>),

    #[error("(line {}): Function takes more than the maximum number \
             of arguments (255)", .0.line)]
    TooManyArguments(Token<'s>),

    #[error("(line {}): Definition of variable references itself", .0.line)]
    RecursiveDefinition(Token<'s>),

    #[error("(line {}): Redeclaration of variable '{0}')", .0.line)]
    Redeclaration(Token<'s>),
}
