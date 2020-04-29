use thiserror::Error;

use crate::{
    token::{Token, TokenType},
    parser::{Ast, Expression, Expression::*}
};

pub trait Evaluate<'s> {
    fn evaluate(&self) -> EvalResult<'s>;
}

type EvalResult<'s> = Result<Value, RuntimeError<'s>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Clone, Debug, Error)]
pub enum RuntimeError<'s> {
    #[error("ERROR (line {}): Unary '{operator}' is not applicable to {value}",
            operator.line)]
    UnaryOperatorNotApplicable {
        value: Value,
        operator: Token<'s>
    },

    #[error("ERROR (line {}): Binary '{operator}' is not appicable to {lhs} \
             and {rhs}", operator.line)]
    BinaryOperatorNotApplicable {
        lhs: Value,
        rhs: Value,
        operator: Token<'s>
    }
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
    fn evaluate(&self) -> EvalResult<'s> {
        fn eval<'s>(node: &Expression<'s>, ast: &Ast<'s>) -> EvalResult<'s> {
            use Value::*;
            use TokenType as TT;

            match node {
                Literal(Token { token_type: t, .. }) => match *t {
                    TT::Number(n) => Ok(Number(n)),
                    TT::String(s) => Ok(String(s.to_owned())),
                    _ => unreachable!("Literal other than number or string")
                },
                Grouping(expr) => {
                    eval(ast.node(*expr), ast)
                },
                Unary(token, rhs) => {
                    let value = eval(ast.node(*rhs), ast)?;

                    match token.token_type {
                        TT::Minus => negate(&value, token),
                        TT::Bang  => Ok(Boolean(!value.is_truthy())),
                        _ => unreachable!("Unary operation other than (!|-)")
                    }
                },
                Binary(lhs, token, rhs) => {
                    let left  = eval(ast.node(*lhs), ast)?;
                    let right = eval(ast.node(*rhs), ast)?;

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
                }
            }
        }

        eval(self.root(), self)
    }
}

macro_rules! binary_operator (
    ($name:ident, $op:tt, $($type:tt)|+ -> $ret:tt) => {
        fn $name<'s>(lhs: &Value, rhs: &Value, operator: &Token<'s>) -> EvalResult<'s> {
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

fn negate<'s>(value: &Value, operator: &Token<'s>) -> EvalResult<'s> {
    use Value::*;

    match value {
        Number(n) => Ok(Number(-n)),
        _ => Err(RuntimeError::UnaryOperatorNotApplicable {
            value: value.clone(),
            operator: *operator
        })
    }
}

fn plus<'s>(lhs: &Value, rhs: &Value, operator: &Token<'s>) -> EvalResult<'s> {
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
