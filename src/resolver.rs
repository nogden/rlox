use crate::{
    parser::{Ast, ExprIndex, StmtIndex},
    token::{Token},
    error::ParseError,
};

use std::collections::HashMap;

pub type ReferenceTable = HashMap<ExprIndex, usize>;

pub fn resolve_references<'s>(
    ast: &Ast<'s>
) -> Result<ReferenceTable, ParseError<'s>> {
    let mut resolver = Resolver {
        scopes: Vec::new(),
        resolved: ReferenceTable::new(),
    };

    for statement in ast.top_level_statements() {
        resolver.resolve_statement(*statement, ast)?;
    }

    Ok(resolver.resolved)
}

struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    resolved: ReferenceTable,
}

impl Resolver {
    fn resolve_statement<'s>(
        &mut self, statement: StmtIndex, ast: &Ast<'s>
    ) -> Result<(), ParseError<'s>> {
        use crate::parser::Statement::*;

        match ast.statement(statement) {
            Expression(expr) => self.resolve_expression(*expr, ast)?,

            Fun(name, parameters, body) => {
                self.declare(name.lexeme);
                self.define(name.lexeme);
                self.resolve_fn(parameters, body, ast)?;
            },

            If(condition, then_block, optional_else_block) => {
                self.resolve_expression(*condition, ast)?;
                self.resolve_statement(*then_block, ast)?;
                if let Some(else_block) = optional_else_block {
                    self.resolve_statement(*else_block, ast)?;
                }
            },

            While(condition, body) => {
                self.resolve_expression(*condition, ast)?;
                self.resolve_statement(*body, ast)?;
            }

            Print(expr) => self.resolve_expression(*expr, ast)?,

            Return(_token, optional_expression) => {
                if let Some(expression) = optional_expression {
                    self.resolve_expression(*expression, ast)?;
                }
            },

            Var(identifier, optional_initialiser) => {
                self.declare(identifier.lexeme);
                if let Some(initialiser) = optional_initialiser {
                    self.resolve_expression(*initialiser, ast)?;
                }
                self.define(identifier.lexeme);
            },

            Block(statements) => {
                self.scopes.push(HashMap::new());
                for stmt in statements {
                    self.resolve_statement(*stmt, ast)?;
                }
                let _ = self.scopes.pop();
            }
        }

        Ok(())
    }

    fn resolve_expression<'s>(
        &mut self, expression: ExprIndex, ast: &Ast<'s>
    ) -> Result<(), ParseError<'s>> {
        use crate::parser::Expression::*;

        match ast.expression(expression) {
            Literal(_token_type) => { /* Nothing to resolve */ },

            Grouping(expr) => self.resolve_expression(*expr, ast)?,

            Unary(_token, rhs) => self.resolve_expression(*rhs, ast)?,

            Binary(lhs, _token, rhs) => {
                self.resolve_expression(*lhs, ast)?;
                self.resolve_expression(*rhs, ast)?;
            },

            Logical(lhs, _token, rhs) => {
                self.resolve_expression(*lhs, ast)?;
                self.resolve_expression(*rhs, ast)?;
            },

            Variable(identifier) => {
                if let Some(scope) = self.scopes.last() {
                    if let Some(initialised) = scope.get(identifier.lexeme) {
                        if !initialised {
                            return Err(ParseError::RecursiveDefinition(*identifier))
                        }
                    }
                }

                self.resolve(expression, identifier.lexeme);
            }

            Assign(variable, expr) => {
                self.resolve_expression(*expr, ast)?;
                self.resolve(*expr, variable.lexeme)
            },

            Call(callee, _token, args) => {
                self.resolve_expression(*callee, ast)?;
                for argument in args.iter() {
                    self.resolve_expression(*argument, ast)?;
                }
            }
        }

        Ok(())
    }

    fn resolve_fn<'s>(
        &mut self, parameters: &Vec<Token<'s>>, body: &Vec<StmtIndex>, ast: &Ast<'s>
    ) -> Result<(), ParseError<'s>> {
        self.scopes.push(HashMap::new());
        for parameter in parameters.iter() {
            self.declare(parameter.lexeme);
            self.define(parameter.lexeme);
        }
        for statement in body {
            self.resolve_statement(*statement, ast)?;
        }
        let _ = self.scopes.pop();
        Ok(())
    }

    fn declare(&mut self, identifier: &str) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(identifier.to_owned(), false);
        }
    }

    fn define(&mut self, identifier: &str) {
        if let Some(current_scope) = self.scopes.last_mut() {
            let _ = current_scope.insert(identifier.to_owned(), true);
        }
    }

    fn resolve(&mut self, expression: ExprIndex, identifier: &str) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(identifier) {
                let _ = self.resolved.insert(expression, i);
                return
            }
        }

        // Not found, assume it's in the global scope.
    }
}
