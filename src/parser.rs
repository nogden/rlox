use std::{
    fmt, iter
};

use crate::{
    Result,
    scanner::{Token, TokenType},
};

use Expression::*;

#[derive(Clone, Debug)]
pub struct Ast;

pub fn parse<'s, 'i>(
    tokens: impl IntoIterator<Item = Token<'s>> + 'i
) -> Result<Ast> {
    let mut parser = Parser {
        tokens: tokens.into_iter().peekable(),
        nodes: Vec::new()
    };

    parser.parse()
}

struct Parser<'s, I: Iterator<Item = Token<'s>>> {
    tokens: iter::Peekable<I>,
    nodes: Vec<Expression<'s>>,
}

impl<'s, I: Iterator<Item = Token<'s>>> Parser<'s, I> {
    fn parse(&mut self) -> Result<Ast> {
        Ok(Ast)
    }

    fn add(&mut self, expression: Expression<'s>) -> ExprIndex {
        let index = self.nodes.len();
        self.nodes.push(expression);
        index
    }

    fn expression(&mut self) -> ExprIndex { self.equality() }

    fn equality(&mut self) -> ExprIndex {
        use TokenType::*;
        let mut expr = self.comparison();

        while let Some(token @ Token {
            token_type: EqualEqual | BangEqual, ..
        }) = self.tokens.peek() {
            let operator = *token;
            self.advance();
            expr = self.add(Binary(expr, operator, self.comparison()))
        }

        expr
    }

    fn comparison(&self) -> ExprIndex {
        unimplemented!()
    }

    fn advance(&mut self) { let _ = self.tokens.next(); }
}

type ExprIndex = usize;  // A reference to another Expression in the Ast

#[derive(Clone, Debug)]
pub enum Expression<'s> {
    Binary(ExprIndex, Token<'s>, ExprIndex),
    Unary(Token<'s>, ExprIndex),
    Grouping(ExprIndex),
    Literal(Token<'s>),
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
