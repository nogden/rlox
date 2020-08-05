use thiserror::Error;

use crate::{
    parser::{Ast, ExprIndex, StmtIndex, Expression, Statement},
    token::{Token, TokenType},
    bytecode::{Chunk, Instruction},
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

    if errors.is_empty() { Ok(compiler.bytecode) } else { Err(errors) }
}

struct Compiler {
    bytecode: Chunk,
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
            Grouping(expr) => self.compile_expression(*expr, ast)?,

            Literal(token) => {
                use TokenType::*;

                match token.token_type {
                    Number(number) => {
                        if let Some(constant) = self.bytecode.add_constant(number) {
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
