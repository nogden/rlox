use crate::{
    scanner::{Token, TokenType},
    parser::{Ast, Expression, Expression::*}
};

type EvalResult<'s> = Result<Value, RuntimeError<'s>>;

pub trait Evaluate<'s> {
    fn evaluate(&self) -> EvalResult<'s>;
}

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl Value {
    fn is_truthy(&self) -> bool {
        match self {
            Value::Nil | Value::Boolean(false) => false,
            _                                  => true
        }
    }
}

pub enum RuntimeError<'s> {
    UnaryOperatorNotApplicable {
        value: Value,
        operator: Token<'s>
    },
    BinaryOperatorNotApplicable {
        lhs: Value,
        rhs: Value,
        operator: Token<'s>
    }
}

impl<'s> Evaluate<'s> for Ast<'s> {
    fn evaluate(&self) -> EvalResult<'s> {
        fn eval<'s>(node: &Expression<'s>, ast: &Ast<'s>) -> EvalResult<'s> {
            use Value::*;

            match node {
                Literal(Token { token_type: t, .. }) => match *t {
                    TokenType::Number(n) => Ok(Number(n)),
                    TokenType::String(s) => Ok(String(s.to_owned())),
                    _ => unreachable!("Literal other than number or string")
                },
                Grouping(expr) => {
                    eval(ast.node(*expr), ast)
                },
                Unary(token, rhs) => {
                    let value = eval(ast.node(*rhs), ast)?;

                    match token.token_type {
                        TokenType::Minus => negate(&value, token),
                        TokenType::Bang  => Ok(Boolean(!value.is_truthy())),
                        _ => unreachable!("Unary operation other than (!|-)")
                    }
                },
                Binary(lhs, token, rhs) => {
                    let left  = eval(ast.node(*lhs), ast)?;
                    let right = eval(ast.node(*rhs), ast)?;

                    match token.token_type {
                        TokenType::Minus => minus(&left, &right, token),
                        TokenType::Slash => divide(&left, &right, token),
                        TokenType::Star  => multiply(&left, &right, token),
                        TokenType::Plus  => plus(&left, &right, token),
                        _ => unreachable!("Binary operator other than (+|-|*|/)")
                    }
                }
            }
        }

        Ok(Value::Number(42.0))
    }
}

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

fn minus<'s>(lhs: &Value, rhs: &Value, operator: &Token<'s>) -> EvalResult<'s> {
    use Value::*;

    match (lhs, rhs) {
        (Number(l), Number(r)) => Ok(Number(l - r)),
        _ => Err(RuntimeError::BinaryOperatorNotApplicable{
            lhs: lhs.clone(),
            rhs: rhs.clone(),
            operator: *operator
        })
    }
}

fn multiply<'s>(lhs: &Value, rhs: &Value, operator: &Token<'s>) -> EvalResult<'s> {
    use Value::*;

    match (lhs, rhs) {
        (Number(l), Number(r)) => Ok(Number(l * r)),
        _ => Err(RuntimeError::BinaryOperatorNotApplicable{
            lhs: lhs.clone(),
            rhs: rhs.clone(),
            operator: *operator
        })
    }
}

fn divide<'s>(lhs: &Value, rhs: &Value, operator: &Token<'s>) -> EvalResult<'s> {
    use Value::*;

    match (lhs, rhs) {
        (Number(l), Number(r)) => Ok(Number(l / r)),
        _ => Err(RuntimeError::BinaryOperatorNotApplicable{
            lhs: lhs.clone(),
            rhs: rhs.clone(),
            operator: *operator
        })
    }
}
