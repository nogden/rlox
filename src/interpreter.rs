use std::{io, collections::HashMap};

use thiserror::Error;

use crate::{
    token::{Token, TokenType},
    parser::{Ast, Expression, Expression::*, Statement, Statement::*}
};

pub trait Evaluate<'s> {
    fn evaluate(&self, env: &mut dyn Environment) -> EvalResult<'s>;
}

pub trait Environment {
    fn stdout(&mut self) -> &mut dyn io::Write;

    fn define(&mut self, identifier: &str, value: Value);

    fn assign<'s>(
        &mut self, identifier: &Token<'s>, value: Value
    ) -> Result<(), RuntimeError<'s>>;

    fn resolve(&self, identifier: &Token) -> Option<Value>;
}

type EvalResult<'s> = Result<Option<Value>, RuntimeError<'s>>;
type ExprResult<'s> = Result<Value, RuntimeError<'s>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Clone, Debug, Error)]
pub enum RuntimeError<'s> {
    #[error("(line {}): Unary '{operator}' is not applicable to {value}",
            operator.line)]
    UnaryOperatorNotApplicable {
        value: Value,
        operator: Token<'s>
    },

    #[error("(line {}): Binary '{operator}' is not appicable to {lhs} \
             and {rhs}", operator.line)]
    BinaryOperatorNotApplicable {
        lhs: Value,
        rhs: Value,
        operator: Token<'s>
    },

    #[error("(line {}): Unresolved identifier '{0}'", .0.line)]
    UnresolvedIdentifier(Token<'s>),
}

impl Value {
    fn is_truthy(&self) -> bool {
        match self {
            Value::Nil | Value::Boolean(false) => false,
            _                                  => true
        }
    }
}

use std::fmt;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;

        match self {
            Number(n)  => write!(f, "{}", n),
            String(s)  => write!(f, "\"{}\"", s),
            Boolean(b) => write!(f, "{}", b),
            Nil        => write!(f, "nil"),
        }
    }
}

impl<'s> Evaluate<'s> for Ast<'s> {
    fn evaluate(&self, env: &mut dyn Environment) -> EvalResult<'s> {
        let mut last_value = None;
        for statement in self.top_level_statements() {
            last_value = eval_statement(self.statement(*statement), self, env)?;
        }

        Ok(last_value)
    }
}

struct Scope<'p> {
    parent: &'p mut dyn Environment,
    bindings: HashMap<String, Value>,
}

impl<'p> Scope<'p> {
    fn new(parent_scope: &'p mut dyn Environment) -> Scope<'p> {
        Scope { parent: parent_scope, bindings: HashMap::new() }
    }
}

impl<'p> Environment for Scope<'p> {
    fn stdout(&mut self) -> &mut dyn io::Write {
        self.parent.stdout()
    }

    fn define(&mut self, identifier: &str, value: Value) {
        let _ = self.bindings.insert(identifier.to_owned(), value);
    }

    fn assign<'s>(
        &mut self, identifier: &Token<'s>, value: Value
    ) -> Result<(), RuntimeError<'s>> {
        if let Some(bound_value) = self.bindings.get_mut(identifier.lexeme) {
            *bound_value = value;
            Ok(())
        } else {
            self.parent.assign(identifier, value)
        }
    }

    fn resolve(&self, identifier: &Token) -> Option<Value> {
        self.bindings.get(identifier.lexeme).cloned().or_else(||
            self.parent.resolve(identifier)
        )
    }
}

fn eval_statement<'s>(
    statement: &Statement<'s>, ast: &Ast<'s>, env: &mut dyn Environment
) -> EvalResult<'s> {
    use Value::*;

    match statement {
        Expression(expression) =>
            eval_expression(ast.expression(*expression), ast, env)
            .map(|v| Some(v)),

        If(condition, then_block, optional_else_block) => {
            let condition_result = eval_expression(
                ast.expression(*condition), ast, env
            )?;

            if condition_result.is_truthy() {
                eval_statement(ast.statement(*then_block), ast, env)?;
            } else if let Some(else_block) = optional_else_block {
                eval_statement(ast.statement(*else_block), ast, env)?;
            }

            Ok(None)
        },

        Print(expression) => {
            match eval_expression(ast.expression(*expression), ast, env)? {
                String(s) => writeln!(env.stdout(), "{}", s),
                result    => writeln!(env.stdout(), "{}", result)
            }.expect("Failed to write to stdout");

            Ok(None)
        },

        Var(ident, initialiser) => {
            let value = if let Some(initialiser) = initialiser {
                eval_expression(ast.expression(*initialiser), ast, env)?
            } else {
                Nil
            };
            env.define(ident.lexeme, value);

            Ok(None)
        },

        Block(statements) => {
            let mut scope = Scope::new(env);
            let mut last_value = None;

            for statement in statements {
                last_value = eval_statement(
                    ast.statement(*statement), ast, &mut scope
                )?;
            }

            Ok(last_value)
        }
    }
}

fn eval_expression<'s>(
    expression: &Expression<'s>, ast: &Ast<'s>, env: &mut dyn Environment
) -> ExprResult<'s> {
    use Value::*;
    use TokenType as TT;

    match expression {
        Literal(Token { token_type: t, .. }) => match *t {
            TT::Number(n)  => Ok(Number(n)),
            TT::String(s)  => Ok(String(s.to_owned())),
            TT::True       => Ok(Boolean(true)),
            TT::False      => Ok(Boolean(false)),
            TT::Nil        => Ok(Nil),
            _ => unreachable!("Literal other than number or string")
        },
        Grouping(expr) => {
            eval_expression(ast.expression(*expr), ast, env)
        },
        Unary(token, rhs) => {
            let value = eval_expression(ast.expression(*rhs), ast, env)?;

            match token.token_type {
                TT::Minus => negate(&value, token),
                TT::Bang  => Ok(Boolean(!value.is_truthy())),
                _ => unreachable!("Unary operation other than (!|-)")
            }
        },
        Binary(lhs, token, rhs) => {
            let left  = eval_expression(ast.expression(*lhs), ast, env)?;
            let right = eval_expression(ast.expression(*rhs), ast, env)?;

            match token.token_type {
                TT::Greater      => greater(&left, &right, token),
                TT::GreaterEqual => greater_eq(&left, &right, token),
                TT::Less         => less(&left, &right, token),
                TT::LessEqual    => less_eq(&left, &right, token),
                TT::EqualEqual   => Ok(Boolean(left == right)),
                TT::BangEqual    => Ok(Boolean(left != right)),
                TT::Minus        => minus(&left, &right, token),
                TT::Slash        => divide(&left, &right, token),
                TT::Star         => multiply(&left, &right, token),
                TT::Plus         => plus(&left, &right, token),
                _ => unreachable!("Binary operator other than (+|-|*|/)")
            }
        },
        Variable(identifier) => match env.resolve(identifier) {
            Some(value) => Ok(value),
            None        => Err(RuntimeError::UnresolvedIdentifier(*identifier))
        },
        Assign(variable, expression) => {
            let value = eval_expression(ast.expression(*expression), ast, env)?;
            env.assign(variable, value.clone())?;
            Ok(value)
        }
    }
}

macro_rules! binary_operator (
    ($name:ident, $op:tt, $($type:tt)|+ -> $ret:tt) => {
        fn $name<'s>(lhs: &Value, rhs: &Value, operator: &Token<'s>) -> ExprResult<'s> {
            use Value::*;

            match (lhs, rhs) {
                $( ($type(l), $type(r)) => Ok($ret(l $op r)), )*

                _ => Err(RuntimeError::BinaryOperatorNotApplicable {
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                    operator: *operator
                })
            }
        }
    }
);

binary_operator!(greater,    >,  Number | String | Boolean -> Boolean);
binary_operator!(greater_eq, >=, Number | String | Boolean -> Boolean);
binary_operator!(less,       <,  Number | String | Boolean -> Boolean);
binary_operator!(less_eq,    <=, Number | String | Boolean -> Boolean);
binary_operator!(minus,      -,  Number -> Number);
binary_operator!(multiply,   *,  Number -> Number);
binary_operator!(divide,     /,  Number -> Number);

fn negate<'s>(value: &Value, operator: &Token<'s>) -> ExprResult<'s> {
    use Value::*;

    match value {
        Number(n) => Ok(Number(-n)),
        _ => Err(RuntimeError::UnaryOperatorNotApplicable {
            value: value.clone(),
            operator: *operator
        })
    }
}

fn plus<'s>(lhs: &Value, rhs: &Value, operator: &Token<'s>) -> ExprResult<'s> {
    use Value::*;

    match (lhs, rhs) {
        (Number(l), Number(r)) => Ok(Number(l + r)),
        (String(l), String(r)) => Ok(String(l.clone() + r)),
        _ => Err(RuntimeError::BinaryOperatorNotApplicable{
            lhs: lhs.clone(),
            rhs: rhs.clone(),
            operator: *operator
        })
    }
}
