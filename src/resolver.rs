use crate::{
    error::ParseError,
    parser::{Ast, ExprIndex, Expression::*, StmtIndex},
    token::Token,
};

use std::collections::HashMap;

pub type ReferenceTable = HashMap<ExprIndex, usize>;

pub fn resolve_references<'s>(ast: &Ast<'s>) -> Result<ReferenceTable, Vec<ParseError<'s>>> {
    let mut resolver = Resolver {
        scopes: Vec::new(),
        resolved: ReferenceTable::new(),
        scope_type: ScopeType::Global,
        class_type: ClassType::None,
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
    class_type: ClassType,
}

#[derive(Clone, Copy, PartialEq)]
enum ScopeType {
    Global,
    Function,
    Constructor,
    Method,
}

#[derive(Clone, Copy, PartialEq)]
enum ClassType {
    None,
    Class,
    SubClass,
}

impl<'s> Resolver<'s> {
    fn resolve_statement(
        &mut self,
        statement: StmtIndex,
        ast: &Ast<'s>,
    ) -> Result<(), ParseError<'s>> {
        use crate::parser::Statement::*;

        match ast.statement(statement) {
            Block(statements) => {
                self.scopes.push(HashMap::new());
                for stmt in statements {
                    self.resolve_statement(*stmt, ast)?;
                }
                self.scopes.pop();
            }

            Class(name, optional_super_class, methods) => {
                let enclosing_class_type = self.class_type;
                self.class_type = ClassType::Class;

                self.declare(name)?;
                self.define(name);

                if let Some(super_class) = optional_super_class {
                    if let Variable(super_name) = ast.expression(*super_class) {
                        if super_name.lexeme == name.lexeme {
                            return Err(ParseError::InheritanceCycle(*super_name));
                        }
                    }
                    self.class_type = ClassType::SubClass;
                    self.resolve_expression(*super_class, ast)?;

                    let mut super_class_scope = HashMap::new();
                    super_class_scope.insert("super", true);
                    self.scopes.push(super_class_scope);
                }

                let mut object_scope = HashMap::new();
                object_scope.insert("this", true);
                self.scopes.push(object_scope);

                for method in methods {
                    if let Fun(name, params, body) = ast.statement(*method) {
                        let scope_type = if name.lexeme == "init" {
                            ScopeType::Constructor
                        } else {
                            ScopeType::Method
                        };
                        self.resolve_fn(params, body, ast, scope_type)?;
                    } else {
                        unreachable!("Class method that isn't a Fun statement")
                    }
                }

                self.scopes.pop();
                if optional_super_class.is_some() {
                    self.scopes.pop();
                }
                self.class_type = enclosing_class_type;
            }

            Expression(expr, _terminator) => self.resolve_expression(*expr, ast)?,

            Fun(name, parameters, body) => {
                self.declare(name)?;
                self.define(name);
                self.resolve_fn(parameters, body, ast, ScopeType::Function)?;
            }

            If(condition, then_block, optional_else_block) => {
                self.resolve_expression(*condition, ast)?;
                self.resolve_statement(*then_block, ast)?;
                if let Some(else_block) = optional_else_block {
                    self.resolve_statement(*else_block, ast)?;
                }
            }

            Print(_keyword, expr) => self.resolve_expression(*expr, ast)?,

            Return(token, optional_expression) => {
                if self.scope_type == ScopeType::Global {
                    return Err(ParseError::TopLevelReturn(*token));
                }

                if let Some(expression) = optional_expression {
                    if self.scope_type == ScopeType::Constructor {
                        return Err(ParseError::ValueReturnedFromConstructor(*token));
                    }
                    self.resolve_expression(*expression, ast)?;
                }
            }

            Var(identifier, optional_initialiser) => {
                self.declare(identifier)?;
                if let Some(initialiser) = optional_initialiser {
                    self.resolve_expression(*initialiser, ast)?;
                }
                self.define(identifier);
            }

            While(condition, body) => {
                self.resolve_expression(*condition, ast)?;
                self.resolve_statement(*body, ast)?;
            }
        }

        Ok(())
    }

    fn resolve_expression(
        &mut self,
        expression: ExprIndex,
        ast: &Ast<'s>,
    ) -> Result<(), ParseError<'s>> {
        use crate::parser::Expression::*;

        match ast.expression(expression) {
            Access(object, _field) => self.resolve_expression(*object, ast)?,

            Assign(variable, expr) => {
                self.resolve_expression(*expr, ast)?;
                self.resolve(*expr, variable)
            }

            Binary(lhs, _token, rhs) => {
                self.resolve_expression(*lhs, ast)?;
                self.resolve_expression(*rhs, ast)?;
            }

            Call(callee, _token, args) => {
                self.resolve_expression(*callee, ast)?;
                for argument in args.iter() {
                    self.resolve_expression(*argument, ast)?;
                }
            }

            Grouping(expr) => self.resolve_expression(*expr, ast)?,

            Literal(_token_type) => { /* Nothing to resolve */ }

            Logical(lhs, _token, rhs) => {
                self.resolve_expression(*lhs, ast)?;
                self.resolve_expression(*rhs, ast)?;
            }

            Mutate(object, _field, value) => {
                self.resolve_expression(*value, ast)?;
                self.resolve_expression(*object, ast)?;
            }

            SelfRef(keyword) => {
                if self.class_type == ClassType::None {
                    return Err(ParseError::SelfRefOutsideObject(*keyword));
                }

                self.resolve(expression, keyword);
            }

            SuperRef(keyword, _method) => {
                if self.class_type != ClassType::SubClass {
                    return Err(ParseError::SuperOutsideSubClass(*keyword));
                }
                self.resolve(expression, keyword);
            }

            Unary(_token, rhs) => self.resolve_expression(*rhs, ast)?,

            Variable(identifier) => {
                if let Some(scope) = self.scopes.last() {
                    if let Some(initialised) = scope.get(identifier.lexeme) {
                        if !initialised {
                            return Err(ParseError::RecursiveDefinition(*identifier));
                        }
                    }
                }

                self.resolve(expression, identifier);
            }
        }

        Ok(())
    }

    fn resolve_fn(
        &mut self,
        parameters: &Vec<Token<'s>>,
        body: &Vec<StmtIndex>,
        ast: &Ast<'s>,
        fn_type: ScopeType,
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

        self.scopes.pop();
        self.scope_type = enclosing_scope_type;

        Ok(())
    }

    fn declare(&mut self, identifier: &Token<'s>) -> Result<(), ParseError<'s>> {
        if let Some(current_scope) = self.scopes.last_mut() {
            if current_scope.contains_key(identifier.lexeme) {
                return Err(ParseError::Redeclaration(*identifier));
            }
            current_scope.insert(identifier.lexeme, false);
        }

        Ok(())
    }

    fn define(&mut self, identifier: &Token<'s>) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(identifier.lexeme, true);
        }
    }

    fn resolve(&mut self, expression: ExprIndex, identifier: &Token<'s>) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(identifier.lexeme) {
                self.resolved.insert(expression, i);
                return;
            }
        }

        // Not found, assume it's in the global scope.
    }
}
