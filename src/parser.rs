use std::{fmt, iter};

use crate::{
    error::{ParseError, ParseError::*},
    token::{Token, TokenType, TokenType::*}
};

use Expression::*;

#[derive(Clone, Debug)]
pub struct Ast<'s> {
    statements: Vec<Statement<'s>>,
    expressions: Vec<Expression<'s>>
}

#[derive(Clone, Debug)]
pub enum Statement<'s> {
    Expression(ExprIndex),
    Print(ExprIndex),
    Var(Token<'s>, Option<ExprIndex>),
    Block(Vec<StmtIndex>),
}

#[derive(Clone, Debug)]
pub enum Expression<'s> {
    Binary(ExprIndex, Token<'s>, ExprIndex),
    Unary(Token<'s>, ExprIndex),
    Grouping(ExprIndex),
    Literal(Token<'s>),
    Variable(Token<'s>),
    Assign(Token<'s>, ExprIndex),
}

#[derive(Clone, Copy, Debug)]
pub struct ExprIndex(usize);  // A reference to an expression in the Ast

#[derive(Clone, Copy, Debug)]
pub struct StmtIndex(usize);  // A reference to a statement in the Ast

type StmtResult<'s> = Result<Option<Statement<'s>>, ParseError<'s>>;

pub fn parse<'s, 'i>(
    tokens: impl IntoIterator<Item = Result<Token<'s>, ParseError<'s>>> + 'i
) -> Result<Ast<'s>, Vec<ParseError<'s>>> {
    let parser = Parser {
        tokens: tokens.into_iter().peekable(),
        expressions: Vec::new()
    };

    parser.parse()
}

impl<'s> Ast<'s> {
    pub fn expression(&self, index: ExprIndex) -> &Expression<'s> {
        &self.expressions[index.0]
    }

    pub fn statement(&self, index: StmtIndex) -> &Statement<'s> {
        &self.statements[index.0]
    }

    pub fn statements(&self) -> impl Iterator<Item = &Statement<'s>> {
        self.statements.iter()
    }
}

struct Parser<'s, I: Iterator<Item = Result<Token<'s>, ParseError<'s>>>> {
    tokens: iter::Peekable<I>,
    expressions: Vec<Expression<'s>>,
}

impl<'s, I: Iterator<Item = Result<Token<'s>, ParseError<'s>>>> Parser<'s, I> {
    fn parse(mut self) -> Result<Ast<'s>, Vec<ParseError<'s>>> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        loop {
            match self.declaration() {
                Ok(Some(statement)) => statements.push(statement),

                Err(error) => {
                    errors.push(error);
                    self.synchronise()
                },

                Ok(None) => if errors.is_empty() {
                    return Ok(Ast {
                        statements: statements,
                        expressions: self.expressions
                    })
                } else {
                    return Err(errors)
                }
            }
        }
    }

    fn peek(&mut self) -> Result<Option<&Token<'s>>, ParseError<'s>> {
        match self.tokens.peek() {
            Some(Ok(token)) => Ok(Some(token)),
            Some(Err(error)) => Err(error.clone()),
            None => Ok(None)
        }
    }

    fn advance(&mut self) { let _ = self.tokens.next(); }

    fn consume(&mut self, expected_token: TokenType) -> Result<(), Token<'s>> {
        match self.tokens.peek() {
            Some(Ok(Token { token_type: t, .. })) if *t == expected_token
                => Ok(self.advance()),

            Some(Ok(token)) => Err(*token),  // Unexpected token

            _ => panic!("Ran out of tokens to parse (should have hit Eof)")
        }
    }

    fn consume_semicolon(&mut self) -> Result<(), ParseError<'s>> {
        match self.consume(Semicolon) {
            Ok(_) => Ok(()),
            Err(Token { token_type: Eof, .. }) => Ok(()),
            Err(unexpected_token) => Err(UnexpectedToken {
                token: unexpected_token,
                expected: "';'"
            })
        }
    }

    fn synchronise(&mut self) {
        while let Some(Ok(token)) = self.tokens.next() {
            if let Semicolon = token.token_type {
               return
            }

            if let Some(Ok(Token {
                token_type: Class | Fun | Var | For | If | While | Print | Return, ..
            })) = self.tokens.peek() {
                return
            }
        }
    }

    fn found(&mut self, expression: Expression<'s>) -> ExprIndex {
        let index = self.expressions.len();
        self.expressions.push(expression);
        ExprIndex(index)
    }

    fn declaration(& mut self) -> StmtResult<'s> {
        if let Some(Token { token_type: Var, .. }) = self.peek()? {
            self.advance();
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> StmtResult<'s> {
        let ident = match self.peek()? {
            Some(ident @ Token { token_type: Identifier(_), .. }) => *ident,
            Some(unexpected_token) => return Err(UnexpectedToken {
                token: *unexpected_token,
                expected: "identifier"
            }),
            None => unreachable!("Should have hit Eof")
        };

        self.advance();
        let initialiser = if let Some(Token {
            token_type: Equal, ..
        }) = self.peek()? {
            self.advance();
            Some(self.expression()?)
        } else {
            None
        };

        self.consume_semicolon()?;

        Ok(Some(Statement::Var(ident, initialiser)))
    }

    fn statement(&mut self) -> StmtResult<'s> {
        match self.peek()? {
            Some(Token { token_type: Print, .. }) => {
                self.advance();
                self.print_statement()
            },

            Some(Token { token_type: Eof, .. }) => Ok(None),

            Some(_) => self.expression_statement(),

            None => unreachable!("Should have hit Eof")
        }
    }

    fn print_statement(&mut self) -> StmtResult<'s> {
        let expression = self.expression()?;
        self.consume_semicolon()?;

        Ok(Some(Statement::Print(expression)))
    }

    fn expression_statement(&mut self) -> StmtResult<'s> {
        let expression = self.expression()?;
        self.consume_semicolon()?;

        Ok(Some(Statement::Expression(expression)))
    }

    fn expression(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        let expression = self.equality()?;

        if let Some(token @ Token { token_type: Equal, .. }) = self.peek()? {
            let assignment = *token;
            self.advance();
            let value = self.assignment()?;

            if let Variable(ident) = self.expressions[expression.0] {
                return Ok(self.found(Assign(ident, value)))
            }

            return Err(InvalidAssignmentTarget(assignment))
        }

        Ok(expression)
    }

    fn equality(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        let mut expr = self.comparison()?;

        while let Some(token @ Token {
            token_type: EqualEqual | BangEqual, ..
        }) = self.peek()? {
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
        }) = self.peek()? {
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
        }) = self.peek()? {
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
        }) = self.peek()? {
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
        }) = self.peek()? {
            let operator = *token;
            self.advance();
            let right = self.unary()?;
            return Ok(self.found(Unary(operator, right)))
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        match self.peek()? {
            Some(token @ Token {
                token_type: False | True | Nil | Number(_) | String(_), ..
            }) => {
                let literal = *token;
                self.advance();
                Ok(self.found(Literal(literal)))
            },

            Some(token @ Token { token_type: LeftParen, .. }) => {
                let opening_delimiter = *token;
                self.advance();
                let expr = self.expression()?;
                if let Err(token) = self.consume(RightParen) {
                    Err(UnmatchedDelimiter { token, opening_delimiter })
                } else {
                    Ok(self.found(Grouping(expr)))
                }
            },

            Some(token @ Token { token_type: Identifier(_), .. }) => {
                let ident = *token;
                self.advance();
                Ok(self.found(Variable(ident)))
            }

            Some(token) => Err(UnexpectedToken {
                token: *token,
                expected: "expression"
            }),

            None => unreachable!("Should have hit Eof")
        }
    }
}

impl<'s> fmt::Display for Ast<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn print_expression<'s>(
            f: &mut fmt::Formatter,
            expression: &Expression<'s>,
            ast: &Ast
        ) -> fmt::Result {
            match expression {
                Binary(left, operator, right) => {
                    write!(f, "({} ", operator)?;
                    print_expression(f, ast.expression(*left), ast)?;
                    write!(f, " ")?;
                    print_expression(f, ast.expression(*right), ast)?;
                    write!(f, ")")
                },
                Unary(token, expression) => {
                    write!(f, "({} ", token)?;
                    print_expression(f, ast.expression(*expression), ast)?;
                    write!(f, ")")
                },
                Grouping(expression) => {
                    write!(f, "(group ")?;
                    print_expression(f, ast.expression(*expression), ast)?;
                    write!(f, ")")
                },
                Literal(token) => match token.token_type {
                    Number(n)     => write!(f, "{}", n),
                    String(s)     => write!(f, "\"{}\"", s),
                    Nil           => write!(f, "nil"),
                    True          => write!(f, "true"),
                    False         => write!(f, "false"),
                    _             => write!(f, "<unprintable>")
                },
                Variable(token) => write!(f, "{}", token),
                Assign(target, expression) => {
                    write!(f, "(set {} ", target)?;
                    print_expression(f, ast.expression(*expression), ast)?;
                    write!(f, ")")
                }
            }
        }

        fn print_statement(
            f: &mut fmt::Formatter, statement: &Statement, ast: &Ast
        ) -> fmt::Result {
            use Statement::*;

            match statement {
                Expression(expression) => {
                    print_expression(f, ast.expression(*expression), ast)?;
                },
                Print(expression) => {
                    write!(f, "(print ")?;
                    print_expression(f, ast.expression(*expression), ast)?;
                    write!(f, ")")?;
                },
                Var(ident, initialiser) => {
                    write!(f, "(define {}", ident)?;
                    if let Some(expr) = initialiser {
                        write!(f, " ")?;
                        print_expression(f, ast.expression(*expr), ast)?;
                    }
                    write!(f, ")")?;
                },
                Block(statements) => {
                    writeln!(f, "(scope ")?;
                    for statement in statements {
                        print_statement(f, ast.statement(*statement), ast)?;
                    }
                    write!(f, ")")?;
                }
            }
            write!(f, "\n")
        }

        for statement in &self.statements {
            print_statement(f, statement, self)?;
        }
        Ok(())
    }
}
