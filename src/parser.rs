use std::{fmt, iter};

use crate::scanner::{Token, TokenType, TokenType::*};
use Expression::*;
use thiserror::Error;

#[derive(Clone, Debug)]
pub struct Ast<'s> {
    root: ExprIndex,
    nodes: Vec<Expression<'s>>
}

#[derive(Error, Debug)]
pub enum ParseError<'s> {
    #[error("ERROR (line {}): Missing delimiter to close '{opening_delimiter}', \
             found {token}", token.line)]
    UnmatchedDelimiter {
        token: Token<'s>,
        opening_delimiter: Token<'s>
    },

    #[error("ERROR (line {}): Expected expression, got {0}", .0.line)]
    ExpectedExpression(Token<'s>)
}

pub fn parse<'s, 'i>(
    tokens: impl IntoIterator<Item = Token<'s>> + 'i
) -> Result<Ast<'s>, Vec<ParseError<'s>>> {
    let parser = Parser {
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
    fn parse(mut self) -> Result<Ast<'s>, Vec<ParseError<'s>>> {
        match self.expression() {
            Ok(root_index) => Ok(Ast { root: root_index, nodes: self.nodes }),
            Err(error) => Err(vec![error])
        }
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

    fn synchronise(&mut self) {
        while let Some(token) =  self.tokens.next() {
            if let Semicolon = token.token_type {
               return
            }

            if let Some(Token {
                token_type: Class | Fun | Var | For | If | While | Print | Return, ..
            }) = self.tokens.peek() {
                return
            }
        }
    }

    fn found(&mut self, expression: Expression<'s>) -> ExprIndex {
        let index = self.nodes.len();
        self.nodes.push(expression);
        index
    }

    fn expression(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        self.equality()
    }

    fn equality(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        let mut expr = self.comparison()?;

        while let Some(token @ Token {
            token_type: EqualEqual | BangEqual, ..
        }) = self.tokens.peek() {
            let operator = *token;
            self.advance();
            let right = self.comparison()?;
            expr = self.found(Binary(expr, operator, right))
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        let mut expr = self.addition()?;

        while let Some(token @ Token {
            token_type: Greater | GreaterEqual | Less | LessEqual, ..
        }) = self.tokens.peek() {
            let operator = *token;
            self.advance();
            let right = self.addition()?;
            expr = self.found(Binary(expr, operator, right))
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        let mut expr = self.multiplication()?;

        while let Some(token @ Token {
            token_type: Minus | Plus, ..
        }) = self.tokens.peek() {
            let operator = *token;
            self.advance();
            let right = self.multiplication()?;
            expr = self.found(Binary(expr, operator, right))
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        let mut expr = self.unary()?;

        while let Some(token @ Token {
            token_type: Slash | Star, ..
        }) = self.tokens.peek() {
            let operator = *token;
            self.advance();
            let right = self.unary()?;
            expr = self.found(Binary(expr, operator, right))
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        if let Some(token @ Token {
            token_type: Bang | Minus, ..
        }) = self.tokens.peek() {
            let operator = *token;
            self.advance();
            let right = self.unary()?;
            return Ok(self.found(Unary(operator, right)))
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        match self.tokens.peek() {
            Some(token @ Token {
                token_type: False | True | Nil | Number(_) | String(_), ..
            }) => {
                let literal = *token;
                self.advance();
                Ok(self.found(Literal(literal)))
            },

            Some(token @ Token {token_type: LeftParen, ..}) => {
                let opening_delimiter = *token;
                self.advance();
                let expr = self.expression()?;
                if let Err(token) = self.consume(RightParen) {
                    Err(ParseError::UnmatchedDelimiter {
                        token, opening_delimiter
                    })
                } else {
                    Ok(self.found(Grouping(expr)))
                }
            },

            Some(token) => Err(ParseError::ExpectedExpression(*token)),

            None => unreachable!("Should have terminated at Eof")
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

impl<'s> Ast<'s> {
    pub fn node(&self, node_index: ExprIndex) -> &Expression<'s> {
        &self.nodes[node_index]
    }
}

impl<'s> fmt::Display for Ast<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn print<'s>(
            f: &mut fmt::Formatter,
            node: &Expression<'s>,
            ast: &Ast
        ) -> fmt::Result {
            match node {
                Binary(left, operator, right) => {
                    write!(f, "({} ", operator)?;
                    print(f, ast.node(*left), ast)?;
                    write!(f, " ")?;
                    print(f, ast.node(*right), ast)?;
                    write!(f, ")")
                },
                Unary(token, expression) => {
                    write!(f, "({} ", token)?;
                    print(f, ast.node(*expression), ast)?;
                    write!(f, ")")
                },
                Grouping(expression) => {
                    write!(f, "(group ")?;
                    print(f, ast.node(*expression), ast)?;
                    write!(f, ")")
                },
                Literal(token) => match token.token_type {
                    Number(n)     => write!(f, "{}", n),
                    String(s)     => write!(f, "{}", s),
                    Identifier(i) => write!(f, "{}", i),
                    _ => unreachable!("Literal contained non-literal token")
                }
            }
        }

        if let Some(root_node) = self.nodes.get(self.root) {
            print(f, root_node, self)
        } else {
            Ok(())  // Empty Ast
        }
    }
}
