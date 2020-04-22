use std::{
    fmt,
};

use crate::scanner::{Token, TokenType};

#[derive(Clone, Debug)]
pub enum Expression<'s> {
    Binary(&'s Expression<'s>, &'s Token<'s>, &'s Expression<'s>),
    Unary(&'s Token<'s>, &'s Expression<'s>),
    Grouping(&'s Expression<'s>),
    Literal(&'s Token<'s>),
}

impl<'s> fmt::Display for Expression<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expression::*;

        match self {
            Binary(left, operator, right) => {
                write!(f, "({} {} {})", operator, left, right)
            },
            Unary(token, expression) => {
                write!(f, "({} {})", token, expression)
            },
            Grouping(expression) => {
                write!(f, "(group {})", expression)
            },
            Literal(token) => {
                use TokenType::*;

                match token.token_type {
                    Number(n)     => write!(f, "{}", n),
                    String(s)     => write!(f, "{}", s),
                    Identifier(i) => write!(f, "{}", i),
                    _             => panic!("Literal contained non-literal token")
                }
            }
        }
    }
}
