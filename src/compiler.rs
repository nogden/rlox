use thiserror::Error;

use crate::{
    parser::{Ast, ExprIndex, StmtIndex, Expression, Statement},
    token::{Token, TokenType},
    bytecode::{IncompleteChunk, Chunk, Instruction, Value},
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
            Expression(expr) => self.compile_expression(*expr, ast)?,
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
                use TokenType::*;

                self.compile_expression(*lhs, ast)?;
                self.compile_expression(*rhs, ast)?;
                let instruction = match operator.token_type {
                    Plus  => Add,
                    Minus => Subtract,
                    Star  => Multiply,
                    Slash => Divide,
                    _ => unreachable!("Invalid binary operator")
                };
                self.bytecode.write(&instruction, operator.line);
            }

            Grouping(expr) => self.compile_expression(*expr, ast)?,

            Literal(token) => {
                use TokenType::*;

                match token.token_type {
                    Number(number) => {
                        let num = Value::Number(number);
                        if let Some(constant) = self.bytecode.add_constant(num) {
                            self.bytecode.write(&constant, token.line);
                        } else {
                            return Err(CompileError::TooManyConstants(*token))
                        }
                    }
                    _ => todo!()
                }
            }

            Unary(token, expr) => {
                use TokenType::*;

                self.compile_expression(*expr, ast)?;
                match token.token_type {
                    Minus => self.bytecode.write(&Negate, token.line),
                    _ => unreachable!()
                }
            }
            _ => todo!()
        }

        Ok(())
    }
}
