use std::{io, fmt, collections::HashMap};

use thiserror::Error;

use crate::{
    token::{Token, TokenType},
    parser::{Ast, Expression::*, Statement::*, StmtIndex, ExprIndex},
};

pub struct Interpreter<'io> {
    globals: Scope,
    stack: Vec<Scope>,                // Copy the current stack and store in
    stdout: &'io mut dyn io::Write,    // the function to create a closure
    native_functions: Vec<NativeFn>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    String(String),
    Boolean(bool),
    Function(Vec<String>, StmtIndex),
    NativeFunction(NativeFnIndex),
}

pub type NativeFn = fn(&Vec<Value>) -> Result<Value, NativeError>;

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

#[derive(Clone, Debug)]
pub enum NativeError {
    ArityMismatch(usize),
    TypeMismatch(&'static str, Value),
    Failed(String),
}

type Scope = HashMap<String, Value>;
type EvalResult<'s> = Result<Option<Value>, RuntimeError<'s>>;
type ExprResult<'s> = Result<Value, RuntimeError<'s>>;

impl<'io> Interpreter<'io> {
    pub fn new(stdout: &'io mut dyn io::Write) -> Interpreter<'io> {
        Interpreter {
            globals: Scope::new(),
            stack: Vec::new(),
            stdout,
            native_functions: Vec::new(),
        }
    }

    pub fn expose<T: AsRef<str>>(&mut self, name: T, function: NativeFn) {
        let index = self.native_functions.len();
        self.native_functions.push(function);
        self.globals.insert(name.as_ref().to_owned(), Value::native_fn(index));
    }

    pub fn evaluate<'s>(&mut self, ast: &Ast<'s>) -> EvalResult<'s> {
        let mut last_value = None;
        for statement in ast.top_level_statements() {
            last_value = self.eval_statement(*statement, ast)?;
        }

         Ok(last_value)
    }

    fn push_scope(&mut self, new_scope: Scope) {
        self.stack.push(new_scope)
    }

    fn pop_scope(&mut self) -> Scope {
        self.stack.pop().expect("Attempt to pop empty stack")
    }

    fn define(&mut self, identifier: &str, value: Value) {
        if let Some(active_scope) = self.stack.last_mut() {
            let _ = active_scope.insert(identifier.to_owned(), value);
        } else {
            let _ = self.globals.insert(identifier.to_owned(), value);
        }
    }

    fn resolve(&self, identifier: &str) -> Option<Value> {
        for scope in self.stack.iter().rev() {
            if let Some(value) = scope.get(identifier) {
                return Some(value.clone())
            }
        }

        self.globals.get(identifier).cloned()
    }

    fn assign(&mut self, identifier: &str, new_value: Value) -> Option<&Value> {
        for scope in self.stack.iter_mut().rev() {
            if let Some(bound_value) = scope.get_mut(identifier) {
                *bound_value = new_value;
                return Some(bound_value)
            }
        }

        if let Some(bound_value) = self.globals.get_mut(identifier) {
            *bound_value = new_value;
            Some(bound_value)
        } else {
            None
        }
    }

    fn call_native_fn(
        &mut self, function: NativeFnIndex, arguments: &Vec<Value>
    ) -> Result<Value, NativeError> {
        let f = self.native_functions[function.0];
        f(arguments)
    }

    fn eval_statement<'s>(
        &mut self, statement: StmtIndex, ast: &Ast<'s>
    ) -> EvalResult<'s> {
        use Value::*;

        match ast.statement(statement) {
            Expression(expr) => self.eval_expression(*expr, ast).map(|v| Some(v)),

            Fun(name, parameters, body) => {
                let params = parameters.iter()
                    .map(|t| t.lexeme.to_owned())
                    .collect();
                let function = Function(params, *body);
                self.define(name.lexeme, function);

                Ok(None)
            },

            If(condition, then_block, optional_else_block) => {
                if self.eval_expression(*condition, ast)?.is_truthy() {
                    self.eval_statement(*then_block, ast)?;
                } else if let Some(else_block) = optional_else_block {
                    self.eval_statement(*else_block, ast)?;
                }

                Ok(None)
            },

            While(condition, body) => {
                while self.eval_expression(*condition, ast)?.is_truthy() {
                    self.eval_statement(*body, ast)?;
                }

                Ok(None)
            }

            Print(expr) => {
                match self.eval_expression(*expr, ast)? {
                    String(s) => writeln!(self.stdout, "{}", s),
                    result    => writeln!(self.stdout, "{}", result)
                }.expect("Failed to write to stdout");

                Ok(None)
            },

            Return(_token, optional_expression) => {
                let return_value = if let Some(expr) = optional_expression {
                    Some(self.eval_expression(*expr, ast)?)
                } else {
                    None
                };

                // This is a small abuse of the error handling, but it's the exact
                // unwind semantics that we want and by far the simplest way.
                Err(RuntimeError::StackUnwind(return_value))
            },

            Var(identifier, optional_initialiser) => {
                let value = if let Some(initialiser) = optional_initialiser {
                    self.eval_expression(*initialiser, ast)?
                } else {
                    Nil
                };
                self.define(identifier.lexeme, value);

                Ok(None)
            },

            Block(statements) => {
                self.push_scope(Scope::default());
                for statement in statements {
                    self.eval_statement(*statement, ast)?;
                }
                let _ = self.pop_scope();

                Ok(None)
            }
        }
    }

    fn eval_expression<'s>(
        &mut self, expression: ExprIndex, ast: &Ast<'s>
    ) -> ExprResult<'s> {
        use Value::*;
        use TokenType as TT;

        match ast.expression(expression) {
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

            Grouping(expr) => self.eval_expression(*expr, ast),

            Unary(token, rhs) => {
                let value = self.eval_expression(*rhs, ast)?;

                match token.token_type {
                    TT::Minus => negate(&value, token),
                    TT::Bang  => Ok(Boolean(!value.is_truthy())),
                    _ => unreachable!("Unary operation other than (!|-)")
                }
            },

            Binary(lhs, token, rhs) => {
                let left  = self.eval_expression(*lhs, ast)?;
                let right = self.eval_expression(*rhs, ast)?;

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
                let left = self.eval_expression(*lhs, ast)?;

                match token.token_type {
                    TT::And => if left.is_falsey() { return Ok(left) },
                    TT::Or  => if left.is_truthy() { return Ok(left) },
                    _ => unreachable!("Logical operator other than (and | or)")
                }

                self.eval_expression(*rhs, ast)
            },

            Variable(identifier) => self.resolve(identifier.lexeme)
                .ok_or(RuntimeError::UnresolvedIdentifier(*identifier)),

            Assign(variable, expr) => {
                let value = self.eval_expression(*expr, ast)?;
                if self.assign(variable.lexeme, value.clone()).is_some() {
                    Ok(value)
                } else {
                    Err(RuntimeError::UnresolvedIdentifier(*variable))
                }
            },

            Call(callee, token, args) => {
                match self.eval_expression(*callee, ast)? {
                    Function(params, body) => {   // body is always a block
                        if args.len() != params.len() {
                            return Err(RuntimeError::ArityMismatch {
                                expected: params.len(),
                                provided: args.len(),
                                location: *token
                            })
                        }

                        let arg_param_pairs = args.iter().zip(params.iter());
                        for (arg, parameter) in arg_param_pairs {
                            let argument = self.eval_expression(*arg, ast)?;
                            self.define(parameter, argument);
                        }

                        self.push_scope(Scope::default());
                        let return_value = self.eval_statement(body, ast);
                        let _ = self.pop_scope();

                        match return_value {
                            Ok(value)
                                => Ok(value.unwrap_or(Nil)),
                            Err(RuntimeError::StackUnwind(value))
                                => Ok(value.unwrap_or(Nil)),
                            Err(a_real_error)
                                => Err(a_real_error),
                        }
                    },

                    NativeFunction(function) => {
                        let arguments = args.iter()
                            .map(|arg| self.eval_expression(*arg, ast))
                            .collect::<Result<Vec<Value>, _>>()?;

                        self.call_native_fn(function, &arguments).map_err(|e| {
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
