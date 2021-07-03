use std::{fmt, iter};

use crate::{
    error::{ParseError, ParseError::*},
    token::{Token, TokenType, TokenType::*},
};

use Expression::*;

#[derive(Clone, Debug)]
pub struct Ast<'s> {
    top_level_statements: Vec<StmtIndex>,
    statements: Vec<Statement<'s>>,
    expressions: Vec<Expression<'s>>,
}

#[derive(Clone, Debug)]
pub enum Statement<'s> {
    Block(Vec<StmtIndex>),
    Class(Token<'s>, Option<ExprIndex>, Vec<StmtIndex>),
    Expression(ExprIndex, Token<'s>),
    Fun(Token<'s>, Vec<Token<'s>>, Vec<StmtIndex>),
    If(ExprIndex, StmtIndex, Option<StmtIndex>),
    Print(Token<'s>, ExprIndex),
    Return(Token<'s>, Option<ExprIndex>),
    Var(Token<'s>, Option<ExprIndex>),
    While(ExprIndex, StmtIndex),
}

#[derive(Clone, Debug)]
pub enum Expression<'s> {
    Access(ExprIndex, Token<'s>),
    Assign(Token<'s>, ExprIndex),
    Binary(ExprIndex, Token<'s>, ExprIndex),
    Call(ExprIndex, Token<'s>, Vec<ExprIndex>),
    Grouping(ExprIndex),
    Literal(Token<'s>),
    Logical(ExprIndex, Token<'s>, ExprIndex),
    Mutate(ExprIndex, Token<'s>, ExprIndex),
    SelfRef(Token<'s>),
    SuperRef(Token<'s>, Token<'s>),
    Unary(Token<'s>, ExprIndex),
    Variable(Token<'s>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ExprIndex(usize); // A reference to an expression in the Ast

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct StmtIndex(usize); // A reference to a statement in the Ast

type StmtResult<'s> = Result<Option<StmtIndex>, ParseError<'s>>;

pub fn parse<'s, 'i>(
    tokens: impl IntoIterator<Item = Result<Token<'s>, ParseError<'s>>> + 'i,
) -> Result<Ast<'s>, Vec<ParseError<'s>>> {
    let parser = Parser {
        tokens: tokens.into_iter().peekable(),
        statements: Vec::new(),
        expressions: Vec::new(),
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

    pub fn top_level_statements(&self) -> impl Iterator<Item = &StmtIndex> {
        self.top_level_statements.iter()
    }
}

struct Parser<'s, I: Iterator<Item = Result<Token<'s>, ParseError<'s>>>> {
    tokens: iter::Peekable<I>,
    statements: Vec<Statement<'s>>,
    expressions: Vec<Expression<'s>>,
}

impl<'s, I: Iterator<Item = Result<Token<'s>, ParseError<'s>>>> Parser<'s, I> {
    fn parse(mut self) -> Result<Ast<'s>, Vec<ParseError<'s>>> {
        let mut top_level_statements = Vec::new();
        let mut errors = Vec::new();

        loop {
            match self.declaration() {
                Ok(Some(statement)) => top_level_statements.push(statement),

                Err(error) => {
                    errors.push(error);
                    self.synchronise()
                }

                Ok(None) => {
                    if errors.is_empty() {
                        return Ok(Ast {
                            top_level_statements,
                            statements: self.statements,
                            expressions: self.expressions,
                        });
                    } else {
                        return Err(errors);
                    }
                }
            }
        }
    }

    fn peek(&mut self) -> Result<Option<&Token<'s>>, ParseError<'s>> {
        match self.tokens.peek() {
            Some(Ok(token)) => Ok(Some(token)),
            Some(Err(error)) => Err(error.clone()),
            None => Ok(None),
        }
    }

    fn match_token(&mut self, expected: TokenType) -> Result<bool, ParseError<'s>> {
        match self.peek()? {
            Some(token) if token.token_type == expected => {
                self.advance();
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn advance(&mut self) {
        let _ = self.tokens.next();
    }

    fn consume(&mut self, expected: TokenType) -> Result<Token<'s>, ParseError<'s>> {
        match self.peek()? {
            Some(token) if token.token_type == expected => {
                let found = *token;
                self.advance();
                Ok(found)
            }

            Some(other_token) => Err(UnexpectedToken {
                token: *other_token,
                expected: expected.symbol(),
            }),

            None => unreachable!("Should have hit Eof (in consume)"),
        }
    }

    fn consume_terminator(&mut self) -> Result<Token<'s>, ParseError<'s>> {
        match self.consume(Semicolon) {
            Ok(semicolon) => Ok(semicolon),

            // We accept EOF as the final statement terminator, but don't
            // consume it as it needs to be present to stop the parsing loop.
            Err(UnexpectedToken { token, .. }) if token.token_type == Eof => Ok(token),

            Err(unexpected_token) => Err(unexpected_token),
        }
    }

    fn synchronise(&mut self) {
        loop {
            match self.peek() {
                Ok(Some(Token {
                    token_type: Semicolon,
                    ..
                })) => return self.advance(),

                Ok(Some(Token {
                    token_type: Class | Fun | Var | For | If | While | Print | Return | Eof,
                    ..
                })) => return,

                Ok(Some(_)) => self.advance(),

                Ok(None) => return,

                Err(_) => return self.advance(), // TODO(nick): Is this right?
            }
        }
    }

    fn add_stmt(&mut self, statement: Statement<'s>) -> StmtIndex {
        let index = self.statements.len();
        self.statements.push(statement);
        StmtIndex(index)
    }

    fn add_expr(&mut self, expression: Expression<'s>) -> ExprIndex {
        let index = self.expressions.len();
        self.expressions.push(expression);
        ExprIndex(index)
    }

    fn declaration(&mut self) -> StmtResult<'s> {
        if self.match_token(Var)? {
            self.var_declaration()
        } else if self.match_token(Fun)? {
            Ok(Some(self.function()?))
        } else if self.match_token(Class)? {
            self.class_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> StmtResult<'s> {
        let ident = self.consume(Identifier)?;
        let initialiser = if self.match_token(Equal)? {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume_terminator()?;

        Ok(Some(self.add_stmt(Statement::Var(ident, initialiser))))
    }

    fn function(&mut self) -> Result<StmtIndex, ParseError<'s>> {
        let name = self.consume(Identifier)?;
        self.consume(LeftParen)?;

        let mut parameters = Vec::new();
        if !self.match_token(RightParen)? {
            loop {
                let parameter_name = self.consume(Identifier)?;
                parameters.push(parameter_name);

                if !self.match_token(Comma)? {
                    break;
                }
            }
            self.consume(RightParen)?;
        };

        let opening_brace = self.consume(LeftBrace)?;
        let body = self.block(opening_brace)?;

        Ok(self.add_stmt(Statement::Fun(name, parameters, body)))
    }

    fn class_declaration(&mut self) -> StmtResult<'s> {
        let name = self.consume(Identifier)?;
        let super_class = if self.match_token(Less)? {
            let super_class_name = self.consume(Identifier)?;
            Some(self.add_expr(Variable(super_class_name)))
        } else {
            None
        };
        self.consume(LeftBrace)?;

        let mut methods = Vec::new();
        while !self.match_token(RightBrace)? {
            methods.push(self.function()?);
        }

        Ok(Some(self.add_stmt(Statement::Class(
            name,
            super_class,
            methods,
        ))))
    }

    fn statement(&mut self) -> StmtResult<'s> {
        #[rustfmt::skip]
        match self.peek()? {
            Some(token) if token.token_type == If => {
                self.advance();
                self.if_statement()
            },

            Some(token) if token.token_type == For => {
                self.advance();
                self.for_statement()
            },

            Some(token) if token.token_type == While => {
                self.advance();
                self.while_statement()
            }

            Some(token) if token.token_type == Print => {
                let keyword = *token;
                self.advance();
                self.print_statement(keyword)
            },

            Some(token) if token.token_type == Return => {
                let keyword = *token;
                self.advance();
                self.return_statement(keyword)
            },

            Some(token) if token.token_type == LeftBrace => {
                let opening_brace = *token;
                self.advance();
                let block_contents = self.block(opening_brace)?;
                Ok(Some(self.add_stmt(Statement::Block(block_contents))))
            },

            Some(token) if token.token_type == Eof => Ok(None),

            _ => self.expression_statement(),
        }
    }

    fn if_statement(&mut self) -> StmtResult<'s> {
        let condition = self.expression()?;
        let opening_brace = self.consume(LeftBrace)?;
        let block_contents = self.block(opening_brace)?;
        let then_branch = self.add_stmt(Statement::Block(block_contents));

        let else_branch = if self.match_token(Else)? {
            let opening_brace = self.consume(LeftBrace)?;
            let block_contents = self.block(opening_brace)?;
            Some(self.add_stmt(Statement::Block(block_contents)))
        } else {
            None
        };

        Ok(Some(self.add_stmt(Statement::If(
            condition,
            then_branch,
            else_branch,
        ))))
    }

    fn for_statement(&mut self) -> StmtResult<'s> {
        let initialiser = if self.match_token(Semicolon)? {
            None
        } else if self.match_token(Var)? {
            self.var_declaration()?
        } else {
            self.expression_statement()?
        };

        let condition = match self.peek()? {
            Some(token) if token.token_type == Semicolon => {
                // Empty conditions are always true
                let literal_true = Token {
                    token_type: True,
                    ..*token
                };
                self.advance();
                self.add_expr(Literal(literal_true))
            }
            _ => self.expression()?,
        };

        let terminator = self.consume(Semicolon)?;

        let increment = if self.match_token(LeftBrace)? {
            None
        } else {
            Some(self.expression()?)
        };

        let opening_brace = self.consume(LeftBrace)?;
        let block_contents = self.block(opening_brace)?;
        let mut body = self.add_stmt(Statement::Block(block_contents));

        // Desugar to a while loop

        // Wrap the body in a new block with the increment as the last statement
        if let Some(expression) = increment {
            let increment = self.add_stmt(Statement::Expression(expression, terminator));
            body = self.add_stmt(Statement::Block(vec![body, increment]));
        }

        // Create a while loop with the new block as it's body
        body = self.add_stmt(Statement::While(condition, body));

        // Place the while loop in a block that runs the initialiser first
        if let Some(initialiser) = initialiser {
            body = self.add_stmt(Statement::Block(vec![initialiser, body]))
        }

        Ok(Some(body))
    }

    fn while_statement(&mut self) -> StmtResult<'s> {
        let condition = self.expression()?;
        let opening_brace = self.consume(LeftBrace)?;
        let block_contents = self.block(opening_brace)?;
        let body = self.add_stmt(Statement::Block(block_contents));

        Ok(Some(self.add_stmt(Statement::While(condition, body))))
    }

    fn print_statement(&mut self, keyword: Token<'s>) -> StmtResult<'s> {
        let expression = self.expression()?;
        self.consume_terminator()?;

        Ok(Some(self.add_stmt(Statement::Print(keyword, expression))))
    }

    fn return_statement(&mut self, keyword: Token<'s>) -> StmtResult<'s> {
        let return_value = if self.match_token(Semicolon)? {
            None
        } else {
            let expression = self.expression()?;
            self.consume_terminator()?;
            Some(expression)
        };

        Ok(Some(
            self.add_stmt(Statement::Return(keyword, return_value)),
        ))
    }

    fn block(&mut self, opening_brace: Token<'s>) -> Result<Vec<StmtIndex>, ParseError<'s>> {
        let mut statements = Vec::new();

        loop {
            match self.peek()? {
                Some(token) if token.token_type == RightBrace => {
                    self.advance();
                    return Ok(statements);
                }
                Some(token) if token.token_type == Eof => {
                    return Err(UnmatchedDelimiter {
                        opening_delimiter: opening_brace,
                        token: *token,
                    })
                }
                _ => {
                    if let Some(statement) = self.declaration()? {
                        statements.push(statement);
                    }
                }
            }
        }
    }

    fn expression_statement(&mut self) -> StmtResult<'s> {
        let expression = self.expression()?;
        let terminator = self.consume_terminator()?;

        Ok(Some(
            self.add_stmt(Statement::Expression(expression, terminator)),
        ))
    }

    fn expression(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        let expression = self.or()?;

        if let Ok(token) = self.consume(Equal) {
            let assignment = token;
            let value = self.assignment()?;

            match self.expressions[expression.0] {
                Variable(ident) => return Ok(self.add_expr(Assign(ident, value))),
                Access(object, field) => return Ok(self.add_expr(Mutate(object, field, value))),
                _ => return Err(InvalidAssignmentTarget(assignment)),
            }
        }

        Ok(expression)
    }

    fn or(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        let mut expression = self.and()?;

        while let Ok(operator) = self.consume(Or) {
            let right = self.and()?;
            expression = self.add_expr(Logical(expression, operator, right))
        }

        Ok(expression)
    }

    fn and(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        let mut expression = self.equality()?;

        while let Ok(operator) = self.consume(And) {
            let right = self.equality()?;
            expression = self.add_expr(Logical(expression, operator, right))
        }

        Ok(expression)
    }

    fn equality(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        let mut expression = self.comparison()?;

        #[rustfmt::skip]
        while let Some(
            token @ Token { token_type: EqualEqual | BangEqual, ..}
        ) = self.peek()? {
            let operator = *token;
            self.advance();
            let right = self.comparison()?;
            expression = self.add_expr(Binary(expression, operator, right))
        }

        Ok(expression)
    }

    fn comparison(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        let mut expr = self.addition()?;

        #[rustfmt::skip]
        while let Some(
            token @ Token { token_type: Greater | GreaterEqual | Less | LessEqual, .. }
        ) = self.peek()? {
            let operator = *token;
            self.advance();
            let right = self.addition()?;
            expr = self.add_expr(Binary(expr, operator, right))
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        let mut expr = self.multiplication()?;

        #[rustfmt::skip]
        while let Some(
            token @ Token { token_type: Minus | Plus, .. }
        ) = self.peek()? {
            let operator = *token;
            self.advance();
            let right = self.multiplication()?;
            expr = self.add_expr(Binary(expr, operator, right))
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        let mut expr = self.unary()?;

        #[rustfmt::skip]
        while let Some(
            token @ Token { token_type: Slash | Star, .. }
        ) = self.peek()? {
            let operator = *token;
            self.advance();
            let right = self.unary()?;
            expr = self.add_expr(Binary(expr, operator, right))
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        #[rustfmt::skip]
        if let Some(
            token @ Token { token_type: Bang | Minus, ..}
        ) = self.peek()? {
            let operator = *token;
            self.advance();
            let right = self.unary()?;
            return Ok(self.add_expr(Unary(operator, right)));
        }

        self.call()
    }

    fn call(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        let mut expression = self.primary()?;

        loop {
            if self.match_token(LeftParen)? {
                expression = self.finish_call(expression)?;
            } else if self.match_token(Dot)? {
                let name = self.consume(Identifier)?;
                expression = self.add_expr(Access(expression, name));
            } else {
                break;
            }
        }

        Ok(expression)
    }

    fn finish_call(&mut self, callee: ExprIndex) -> Result<ExprIndex, ParseError<'s>> {
        let mut arguments = Vec::new();

        let closing_paren = if let Ok(paren) = self.consume(RightParen) {
            paren
        } else {
            loop {
                arguments.push(self.expression()?);

                if !self.match_token(Comma)? {
                    break;
                }
            }
            self.consume(RightParen)?
        };

        Ok(self.add_expr(Call(callee, closing_paren, arguments)))
    }

    fn primary(&mut self) -> Result<ExprIndex, ParseError<'s>> {
        #[rustfmt::skip]
        match self.peek()? {
            Some(token @ Token {
                token_type: False | True | Nil | Number(_) | String(_), ..
            }) => {
                let literal = *token;
                self.advance();
                Ok(self.add_expr(Literal(literal)))
            }

            Some(token) if token.token_type == LeftParen => {
                let opening_delimiter = *token;
                self.advance();
                let expr = self.expression()?;
                if let Err(UnexpectedToken {
                    token: unexpected_token,
                    ..
                }) = self.consume(RightParen) {
                    Err(UnmatchedDelimiter {
                        token: unexpected_token,
                        opening_delimiter,
                    })
                } else {
                    Ok(self.add_expr(Grouping(expr)))
                }
            }

            Some(token) if token.token_type == This => {
                let keyword = *token;
                self.advance();
                Ok(self.add_expr(SelfRef(keyword)))
            }

            Some(token) if token.token_type == Super => {
                let keyword = *token;
                self.advance();
                self.consume(Dot)?;
                let method = self.consume(Identifier)?;
                Ok(self.add_expr(SuperRef(keyword, method)))
            }

            Some(token) if token.token_type == Identifier => {
                let ident = *token;
                self.advance();
                Ok(self.add_expr(Variable(ident)))
            }

            Some(token) => Err(UnexpectedToken {
                token: *token,
                expected: "expression",
            }),

            None => unreachable!("Should have hit Eof (in primary)"),
        }
    }
}

impl<'s> fmt::Display for Ast<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn print_expression<'s>(
            f: &mut fmt::Formatter,
            expression: ExprIndex,
            ast: &Ast,
        ) -> fmt::Result {
            match ast.expression(expression) {
                Access(object, field) => {
                    write!(f, "(get ")?;
                    print_expression(f, *object, ast)?;
                    write!(f, " {})", field)
                }

                Assign(target, expression) => {
                    write!(f, "(set {} ", target)?;
                    print_expression(f, *expression, ast)?;
                    write!(f, ")")
                }

                Binary(left, operator, right) => {
                    write!(f, "({} ", operator)?;
                    print_expression(f, *left, ast)?;
                    write!(f, " ")?;
                    print_expression(f, *right, ast)?;
                    write!(f, ")")
                }

                Call(callee, _token, arguments) => {
                    write!(f, "(")?;
                    print_expression(f, *callee, ast)?;
                    for argument in arguments {
                        write!(f, " ")?;
                        print_expression(f, *argument, ast)?;
                    }
                    write!(f, ")")
                }

                Grouping(expression) => {
                    write!(f, "(group ")?;
                    print_expression(f, *expression, ast)?;
                    write!(f, ")")
                }

                Literal(token) => match token.token_type {
                    Number(n) => write!(f, "{}", n),
                    String(s) => write!(f, "\"{}\"", s),
                    Nil => write!(f, "nil"),
                    True => write!(f, "true"),
                    False => write!(f, "false"),
                    _ => write!(f, "<unprintable>"),
                },

                Logical(left, operator, right) => {
                    write!(f, "({} ", operator)?;
                    print_expression(f, *left, ast)?;
                    write!(f, " ")?;
                    print_expression(f, *right, ast)?;
                    write!(f, ")")
                }

                Mutate(object, field, value) => {
                    write!(f, "(set ")?;
                    print_expression(f, *object, ast)?;
                    write!(f, " {} ", field)?;
                    print_expression(f, *value, ast)?;
                    write!(f, ")")
                }

                SelfRef(_keyword) => write!(f, "this"),

                SuperRef(_keyword, _method) => write!(f, "super"),

                Unary(token, expression) => {
                    write!(f, "({} ", token)?;
                    print_expression(f, *expression, ast)?;
                    write!(f, ")")
                }

                Variable(token) => write!(f, "{}", token),
            }
        }

        fn print_statement(f: &mut fmt::Formatter, statement: StmtIndex, ast: &Ast) -> fmt::Result {
            use Statement::*;

            match ast.statement(statement) {
                Block(statements) => {
                    writeln!(f, "(scope ")?;
                    for statement in statements {
                        print_statement(f, *statement, ast)?;
                    }
                    write!(f, ")")
                }

                Class(name, _super_class, methods) => {
                    write!(f, "(defclass {}", name)?;
                    for method in methods {
                        print_statement(f, *method, ast)?;
                    }
                    write!(f, ")")
                }

                Expression(expression, _terminator) => print_expression(f, *expression, ast),

                Fun(name, parameters, body) => {
                    write!(f, "(defn {} [", name)?;
                    for parameter in parameters {
                        write!(f, " {}", parameter)?;
                    }
                    write!(f, " ] ")?;
                    for statement in body {
                        print_statement(f, *statement, ast)?;
                    }
                    write!(f, ")")
                }

                If(condition, then_block, optional_else_block) => {
                    write!(f, "(if ")?;
                    print_expression(f, *condition, ast)?;
                    write!(f, " ")?;
                    print_statement(f, *then_block, ast)?;
                    if let Some(else_block) = optional_else_block {
                        write!(f, " ")?;
                        print_statement(f, *else_block, ast)?;
                    }
                    write!(f, ")")
                }

                Print(_keyword, expression) => {
                    write!(f, "(print ")?;
                    print_expression(f, *expression, ast)?;
                    write!(f, ")")
                }

                Return(_location, expression) => {
                    write!(f, "(return")?;
                    if let Some(expression) = expression {
                        print_expression(f, *expression, ast)?;
                    }
                    write!(f, ")")
                }

                Var(ident, initialiser) => {
                    write!(f, "(def {}", ident)?;
                    if let Some(expr) = initialiser {
                        write!(f, " ")?;
                        print_expression(f, *expr, ast)?;
                    }
                    write!(f, ")")
                }

                While(condition, body) => {
                    write!(f, "(while ")?;
                    print_expression(f, *condition, ast)?;
                    write!(f, " ")?;
                    print_statement(f, *body, ast)?;
                    write!(f, ")")
                }
            }
        }

        for statement in self.top_level_statements() {
            print_statement(f, *statement, self)?;
        }

        Ok(())
    }
}
