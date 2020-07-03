use crate::{
    parser::{Ast, ExprIndex, StmtIndex},
    token::Token,
    error::ParseError,
};

use std::collections::HashMap;

pub type ReferenceTable = HashMap<ExprIndex, usize>;

pub fn resolve_references<'s>(
    ast: &Ast<'s>
) -> Result<ReferenceTable, Vec<ParseError<'s>>> {
    let mut resolver = Resolver {
        scopes: Vec::new(),
        resolved: ReferenceTable::new(),
        scope_type: ScopeType::Global,
    };

    let mut errors = Vec::new();
    for statement in ast.top_level_statements() {
        if let Err(error) = resolver.resolve_statement(*statement, ast) {
            errors.push(error)
        }
    }

    if errors.is_empty() {
        Ok(resolver.resolved)
    } else {
        Err(errors)
    }
}

struct Resolver<'s> {
    scopes: Vec<HashMap<&'s str, bool>>,
    resolved: ReferenceTable,
    scope_type: ScopeType,
}

#[derive(Clone, Copy, PartialEq)]
enum ScopeType { Global, Function }

impl<'s> Resolver<'s> {
    fn resolve_statement(
        &mut self, statement: StmtIndex, ast: &Ast<'s>
    ) -> Result<(), ParseError<'s>> {
        use crate::parser::Statement::*;

        match ast.statement(statement) {
            Expression(expr) => self.resolve_expression(*expr, ast)?,

            Fun(name, parameters, body) => {
                self.declare(name)?;
                self.define(name);
                self.resolve_fn(parameters, body, ast, ScopeType::Function)?;
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

            Return(token, optional_expression) => {
                if self.scope_type == ScopeType::Global {
                    return Err(ParseError::TopLevelReturn(*token))
                }

                if let Some(expression) = optional_expression {
                    self.resolve_expression(*expression, ast)?;
                }
            },

            Var(identifier, optional_initialiser) => {
                self.declare(identifier)?;
                if let Some(initialiser) = optional_initialiser {
                    self.resolve_expression(*initialiser, ast)?;
                }
                self.define(identifier);
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

    fn resolve_expression(
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

                self.resolve(expression, identifier);
            }

            Assign(variable, expr) => {
                self.resolve_expression(*expr, ast)?;
                self.resolve(*expr, variable)
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

    fn resolve_fn(
        &mut self,
        parameters: &Vec<Token<'s>>,
        body: &Vec<StmtIndex>,
        ast: &Ast<'s>,
        fn_type: ScopeType
    ) -> Result<(), ParseError<'s>> {
        let enclosing_scope_type = self.scope_type;
        self.scope_type = fn_type;

        self.scopes.push(HashMap::new());
        for parameter in parameters.iter() {
            self.declare(parameter)?;
            self.define(parameter);
        }
        for statement in body {
            self.resolve_statement(*statement, ast)?;
        }
        let _ = self.scopes.pop();

        self.scope_type = enclosing_scope_type;
        Ok(())
    }

    fn declare(&mut self, identifier: &Token<'s>) -> Result<(), ParseError<'s>> {
        if let Some(current_scope) = self.scopes.last_mut() {
            if current_scope.contains_key(identifier.lexeme) {
                return Err(ParseError::Redeclaration(*identifier))
            }
            current_scope.insert(identifier.lexeme, false);
        }

        Ok(())
    }

    fn define(&mut self, identifier: &Token<'s>) {
        if let Some(current_scope) = self.scopes.last_mut() {
            let _ = current_scope.insert(identifier.lexeme, true);
        }
    }

    fn resolve(&mut self, expression: ExprIndex, identifier: &Token<'s>) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(identifier.lexeme) {
                let _ = self.resolved.insert(expression, i);
                return
            }
        }

        // Not found, assume it's in the global scope.
    }
}
