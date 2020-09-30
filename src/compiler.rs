use thiserror::Error;

use crate::{
    parser::{Ast, ExprIndex, StmtIndex, Expression, Statement},
    token::{Token, TokenType},
    bytecode::{IncompleteChunk, Chunk, Instruction},
    value::Value,
};

#[derive(Debug, Clone, Error)]
pub enum CompileError<'s> {
    #[error("(line {}) Too many constants inone chunk", .0.line)]
    TooManyConstants(Token<'s>),
}

pub fn compile<'s>(ast: &Ast<'s>) -> Result<Chunk, Vec<CompileError<'s>>> {
    let mut compiler = Compiler {
        bytecode: Chunk::new(),
    };

    let mut errors = Vec::new();
    for statement in ast.top_level_statements() {
        if let Err(error) = compiler.compile_statement(*statement, ast) {
            errors.push(error)
        }
    }
    let bytecode = compiler.bytecode.complete();

    if errors.is_empty() { Ok(bytecode) } else { Err(errors) }
}

struct Compiler {
    bytecode: IncompleteChunk,
}

impl Compiler {
    fn compile_statement<'s>(
        &mut self, statement: StmtIndex, ast: &Ast<'s>
    ) -> Result<(), CompileError<'s>> {
        use Statement::*;

        match ast.statement(statement) {
            Expression(expr, termintaor) => {
                self.compile_expression(*expr, ast)?;
                self.bytecode.write(&Instruction::Pop, termintaor.line)
            }
            Print(keyword, expr) => {
                self.compile_expression(*expr, ast)?;
                self.bytecode.write(&Instruction::Print, keyword.line)
            }
            Var(name, optional_initialiser) => {
                let address = self.bytecode.add_constant(
                    Value::string(name.lexeme.to_owned())
                ).ok_or(CompileError::TooManyConstants(*name))?;

                if let Some(initialiser) = optional_initialiser {
                    self.compile_expression(*initialiser, ast)?;
                } else {
                    self.bytecode.write(&Instruction::Nil, name.line)
                }

                self.bytecode.write(
                    &Instruction::DefineGlobal { address }, name.line
                )
            }
            _ => todo!()
        }

        Ok(())
    }

    fn compile_expression<'s>(
        &mut self, expression: ExprIndex, ast: &Ast<'s>
    ) -> Result<(), CompileError<'s>> {
        use Expression::*;
        use Instruction::*;

        match ast.expression(expression) {
            Binary(lhs, operator, rhs) => {
                use TokenType as Token;

                self.compile_expression(*lhs, ast)?;
                self.compile_expression(*rhs, ast)?;
                let instruction = match operator.token_type {
                    Token::Plus       => Add,
                    Token::Minus      => Subtract,
                    Token::Star       => Multiply,
                    Token::Slash      => Divide,
                    Token::EqualEqual => Equal,
                    Token::Greater    => Greater,
                    Token::Less       => Less,
                    _ => unreachable!("Invalid binary operator")
                };
                self.bytecode.write(&instruction, operator.line);
            }

            Grouping(expr) => self.compile_expression(*expr, ast)?,

            Literal(token) => {
                use TokenType as Token;

                match token.token_type {
                    Token::False => self.bytecode.write(&False, token.line),
                    Token::True  => self.bytecode.write(&True, token.line),
                    Token::Nil   => self.bytecode.write(&Nil, token.line),
                    Token::Number(number) => {
                        let address = self.bytecode.add_constant(
                            Value::Number(number)
                        ).ok_or(CompileError::TooManyConstants(*token))?;
                        self.bytecode.write(&Constant { address }, token.line)
                    }
                    Token::String(s) => {
                        let address = self.bytecode.add_constant(
                            Value::string(s.to_owned())
                        ).ok_or(CompileError::TooManyConstants(*token))?;
                        self.bytecode.write(&Constant { address }, token.line)
                    }
                    _ => todo!()
                }
            }

            Unary(token, expr) => {
                use TokenType as Token;

                self.compile_expression(*expr, ast)?;
                match token.token_type {
                    Token::Minus => self.bytecode.write(&Negate, token.line),
                    Token::Bang  => self.bytecode.write(&Not, token.line),
                    _ => unreachable!()
                }
            }
            _ => todo!()
        }

        Ok(())
    }
}
