use std::{
    fmt, iter
};

use crate::{
    LoxResult,
    scanner::{Token, TokenType, TokenType::*}
};

use Expression::*;

#[derive(Clone, Debug)]
pub struct Ast;

pub fn parse<'s, 'i>(
    tokens: impl IntoIterator<Item = Token<'s>> + 'i
) -> LoxResult<Ast> {
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
    fn parse(&mut self) -> LoxResult<Ast> {
        Ok(Ast)
    }

    fn advance(&mut self) { let _ = self.tokens.next(); }

    fn consume(&mut self, expected_token: TokenType) -> Result<(), Token<'s>> {
        match self.tokens.peek() {
            Some(Token { token_type: t, .. }) if *t == expected_token
                => Ok(self.advance()),

            Some(token) => Err(*token),

            None => panic!("Ran out of tokens to parse (should have hit Eof)")
        }
    }

    fn found(&mut self, expression: Expression<'s>) -> ExprIndex {
        let index = self.nodes.len();
        self.nodes.push(expression);
        index
    }

    fn expression(&mut self) -> ExprIndex { self.equality() }

    fn equality(&mut self) -> ExprIndex {
        let mut expr = self.comparison();

        while let Some(token @ Token {
            token_type: EqualEqual | BangEqual, ..
        }) = self.tokens.peek() {
            let operator = *token;
            self.advance();
            let right = self.comparison();
            expr = self.found(Binary(expr, operator, right))
        }

        expr
    }

    fn comparison(&mut self) -> ExprIndex {
        let mut expr = self.addition();

        while let Some(token @ Token {
            token_type: Greater | GreaterEqual | Less | LessEqual, ..
        }) = self.tokens.peek() {
            let operator = *token;
            self.advance();
            let right = self.addition();
            expr = self.found(Binary(expr, operator, right))
        }

        expr
    }

    fn addition(&mut self) -> ExprIndex {
        let mut expr = self.multiplication();

        while let Some(token @ Token {
            token_type: Minus | Plus, ..
        }) = self.tokens.peek() {
            let operator = *token;
            self.advance();
            let right = self.multiplication();
            expr = self.found(Binary(expr, operator, right))
        }

        expr
    }

    fn multiplication(&mut self) -> ExprIndex {
        let mut expr = self.unary();

        while let Some(token @ Token {
            token_type: Slash | Star, ..
        }) = self.tokens.peek() {
            let operator = *token;
            self.advance();
            let right = self.unary();
            expr = self.found(Binary(expr, operator, right))
        }

        expr
    }

    fn unary(&mut self) -> ExprIndex {
        if let Some(token @ Token {
            token_type: Bang | Minus, ..
        }) = self.tokens.peek() {
            let operator = *token;
            self.advance();
            let right = self.unary();
            return self.found(Unary(operator, right))
        }

        self.primary()
    }

    fn primary(&mut self) -> ExprIndex {
        match self.tokens.peek() {
            Some(token @ Token {
                token_type: False | True | Nil | Number(_) | String(_), ..
            }) => {
                let literal = *token;
                self.found(Literal(literal))
            },

            Some(_token @ Token {token_type: LeftParen, ..}) => {
                self.advance();
                let expr = self.expression();
                if let Err(token) = self.consume(RightParen) {
                    panic!("Expected ')' after expression, found {}", token)
                }
                self.found(Grouping(expr))
            },

            _ => unreachable!()
        }
    }
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
