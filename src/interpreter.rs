use std::{io, collections::HashMap};

use thiserror::Error;

use crate::{
    token::{Token, TokenType},
    parser::{Ast, Expression, Expression::*, Statement, Statement::*, StmtIndex},
    NativeError,
};

pub trait Evaluate<'s> {
    fn evaluate(&self, env: &mut dyn Environment<'s>) -> EvalResult<'s>;
}

pub trait Environment<'s> {
    fn stdout(&mut self) -> &mut dyn io::Write;

    fn global_scope(&mut self) -> &mut dyn Environment<'s>;

    fn define(&mut self, identifier: &str, value: Value);

    fn assign(
        &mut self, identifier: &Token<'s>, value: Value
    ) -> Result<(), RuntimeError<'s>>;

    fn resolve(&self, identifier: &Token) -> Option<Value>;

    fn call_native_fn(
        &mut self, function: NativeFnIndex, arguments: &Vec<Value>
    ) -> Result<Value, NativeError>;
}

type EvalResult<'s> = Result<Option<Value>, RuntimeError<'s>>;
type ExprResult<'s> = Result<Value, RuntimeError<'s>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    String(String),
    Boolean(bool),
    Function(Vec<String>, StmtIndex),
    NativeFunction(NativeFnIndex),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NativeFnIndex(usize);

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

    #[error("(line {}): Type mismatch, expected {}, found {} '{}'",
            location.line, expected, provided.value_type(), provided)]
    TypeMismatch {
        expected: &'static str,
        provided: Value,
        location: Token<'s>
    },

    #[error("(line {}): Arity mismatch, expected {} parameters, found {}",
            location.line, expected, provided)]
    ArityMismatch {
        expected: usize,
        provided: usize,
        location: Token<'s>
    },

    #[error("Unhandled stack unwind")]
    StackUnwind(Option<Value>),
}

impl Value {
    fn is_truthy(&self) -> bool {
        match self {
            Value::Nil | Value::Boolean(false) => false,
            _                                  => true
        }
    }

    fn is_falsey(&self) -> bool {
        ! self.is_truthy()
    }

    pub fn value_type(&self) -> &'static str {
        use Value::*;

        match self {
            Nil               => "nil",
            Number(_)         => "number",
            String(_)         => "string",
            Boolean(_)        => "boolean",
            Function(_, _)    => "function",
            NativeFunction(_) => "native function"
        }
    }

    pub fn native_fn(index: usize) -> Value {
        Value::NativeFunction(NativeFnIndex(index))
    }
}

use std::fmt;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;

        match self {
            Nil               => write!(f, "nil"),
            Number(n)         => write!(f, "{}", n),
            String(s)         => write!(f, "\"{}\"", s),
            Boolean(b)        => write!(f, "{}", b),
            Function(_, _)    => write!(f, "<fn>"),
            NativeFunction(_) => write!(f, "<native fn>"),
        }
    }
}


impl<'s> Evaluate<'s> for Ast<'s> {
    fn evaluate(&self, env: &mut dyn Environment<'s>) -> EvalResult<'s> {
        let mut last_value = None;
        for statement in self.top_level_statements() {
            last_value = eval_statement(self.statement(*statement), self, env)?;
        }

        Ok(last_value)
    }
}

struct Scope<'p, 's> {
    parent: &'p mut dyn Environment<'s>,
    bindings: HashMap<String, Value>,
}

impl<'p, 's> Scope<'p, 's> {
    fn new(parent_scope: &'p mut dyn Environment<'s>) -> Scope<'p, 's> {
        Scope { parent: parent_scope, bindings: HashMap::new() }
    }
}

impl<'p, 's> Environment<'s> for Scope<'p, 's> {
    fn stdout(&mut self) -> &mut dyn io::Write {
        self.parent.stdout()
    }

    fn global_scope(&mut self) -> &mut dyn Environment<'s> {
        self.parent.global_scope()
    }

    fn define(&mut self, identifier: &str, value: Value) {
        let _ = self.bindings.insert(identifier.to_owned(), value);
    }

    fn assign(
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

    fn call_native_fn(
        &mut self, function: NativeFnIndex, arguments: &Vec<Value>
    ) -> Result<Value, NativeError> {
        self.global_scope().call_native_fn(function, arguments)
    }
}

impl<'io, 's> Environment<'s> for crate::Lox<'io> {
    fn stdout(&mut self) -> &mut dyn io::Write {
        self.stdout
    }

    fn global_scope(&mut self) -> &mut dyn Environment<'s> {
        self
    }

    fn define(&mut self, identifier: &str, value: Value) {
        let _ = self.bindings.insert(identifier.to_owned(), value);
    }

    fn assign(
        &mut self, identifier: &Token<'s>, value: Value,
    ) -> Result<(), RuntimeError<'s>> {
        if let Some(bound_value) = self.bindings.get_mut(identifier.lexeme) {
            *bound_value = value;
            Ok(())
        } else {
            Err(RuntimeError::UnresolvedIdentifier(*identifier))
        }
    }

    fn resolve(&self, identifier: &Token) -> Option<Value> {
        self.bindings.get(identifier.lexeme).cloned()
    }

    fn call_native_fn(
        &mut self, function: NativeFnIndex, arguments: &Vec<Value>
    ) -> Result<Value, NativeError> {
        let f = self.native_functions[function.0];
        f(arguments)
    }
}

fn eval_statement<'s>(
    statement: &Statement<'s>, ast: &Ast<'s>, env: &mut dyn Environment<'s>
) -> EvalResult<'s> {
    use Value::*;

    match statement {
        Expression(expression) =>
            eval_expression(ast.expression(*expression), ast, env)
            .map(|v| Some(v)),

        Fun(name, parameters, body) => {
            let params = parameters.iter()
                .map(|t| t.lexeme.to_owned())
                .collect();
            let function = Function(params, *body);
            env.define(name.lexeme, function);

            Ok(None)
        },

        If(expression, then_block, optional_else_block) => {
            let condition = eval_expression(
                ast.expression(*expression), ast, env
            )?;

            if condition.is_truthy() {
                eval_statement(ast.statement(*then_block), ast, env)?;
            } else if let Some(else_block) = optional_else_block {
                eval_statement(ast.statement(*else_block), ast, env)?;
            }

            Ok(None)
        },

        While(expression, statement) => {
            let condition = ast.expression(*expression);
            let body = ast.statement(*statement);

            while eval_expression(condition, ast, env)?.is_truthy() {
                eval_statement(body, ast, env)?;
            }

            Ok(None)
        }

        Print(expression) => {
            match eval_expression(ast.expression(*expression), ast, env)? {
                String(s) => writeln!(env.stdout(), "{}", s),
                result    => writeln!(env.stdout(), "{}", result)
            }.expect("Failed to write to stdout");

            Ok(None)
        },

        Return(_token, expression) => {
            let return_value = if let Some(expression) = expression {
                Some(eval_expression(ast.expression(*expression), ast, env)?)
            } else {
                None
            };

            // This is a small abuse of the error handling, but it's the exact
            // unwind semantics that we want and by far the simplest way.
            Err(RuntimeError::StackUnwind(return_value))
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

            for statement in statements {
                eval_statement(ast.statement(*statement), ast, &mut scope)?;
            }

            Ok(None)
        }
    }
}

fn eval_expression<'s>(
    expression: &Expression<'s>, ast: &Ast<'s>, env: &mut dyn Environment<'s>
) -> ExprResult<'s> {
    use Value::*;
    use TokenType as TT;

    match expression {
        Literal(token_type) => match *token_type {
            TT::Number(n)  => Ok(Number(n)),
            TT::String(s)  => Ok(String(s.to_owned())),
            TT::True       => Ok(Boolean(true)),
            TT::False      => Ok(Boolean(false)),
            TT::Nil        => Ok(Nil),
            _ => unreachable!(
                "Literal other than (number | string | true | false | nil)"
            )
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
        Logical(lhs, token, rhs) => {
            let left = eval_expression(ast.expression(*lhs), ast, env)?;

            match token.token_type {
                TT::And => if left.is_falsey() { return Ok(left) },
                TT::Or  => if left.is_truthy() { return Ok(left) },
                _ => unreachable!("Logical operator other than (and | or)")
            }

            eval_expression(ast.expression(*rhs), ast, env)
        },
        Variable(identifier) => match env.resolve(identifier) { // TODO(nick): Replace with ok_or()?
            Some(value) => Ok(value),
            None        => Err(RuntimeError::UnresolvedIdentifier(*identifier))
        },
        Assign(variable, expression) => {
            let value = eval_expression(ast.expression(*expression), ast, env)?;
            env.assign(variable, value.clone())?;
            Ok(value)
        },
        Call(callee, token, arguments) => {
            match eval_expression(ast.expression(*callee), ast, env)? {
                Function(parameters, body) => {   // body is always a block
                    if arguments.len() != parameters.len() {
                        return Err(RuntimeError::ArityMismatch {
                            expected: parameters.len(),
                            provided: arguments.len(),
                            location: *token
                        })
                    }

                    let mut bindings = HashMap::new();
                    let arg_param_pairs = arguments.iter().zip(parameters.iter());
                    for (arg, param) in arg_param_pairs {
                        let argument = eval_expression(
                            ast.expression(*arg), ast, env
                        )?;
                        bindings.insert(param.clone(), argument);
                    }

                    let mut fn_env = Scope { bindings, parent: env };
                    match eval_statement(ast.statement(body), ast, &mut fn_env) {
                        Ok(return_value)
                            => Ok(return_value.unwrap_or(Nil)),
                        Err(RuntimeError::StackUnwind(return_value))
                            => Ok(return_value.unwrap_or(Nil)),
                        Err(a_real_error)
                            => Err(a_real_error),
                    }
                },

                NativeFunction(function) => {
                    let arguments = arguments.iter()
                        .map(|a| eval_expression(ast.expression(*a), ast, env))
                        .collect::<Result<Vec<Value>, _>>()?;

                    env.call_native_fn(function, &arguments).map_err(|e| {
                        match e {
                            NativeError::ArityMismatch(expected_arity) => {
                                RuntimeError::ArityMismatch {
                                    expected: expected_arity,
                                    provided: arguments.len(),
                                    location: *token
                                }
                            }
                            NativeError::TypeMismatch(expected, provided) => {
                                RuntimeError::TypeMismatch {
                                    expected,
                                    provided,
                                    location: *token
                                }
                            }
                            _ => panic!("Native call returned error")
                        }
                    })
                }

                value => return Err(RuntimeError::TypeMismatch {
                    expected: "callable type",
                    provided: value,
                    location: *token
                })
            }
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
