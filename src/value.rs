use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Boolean(bool),
    Number(f64),
    Nil,
    Ref(Box<RefType>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum RefType {
    String(String),
}

impl Value {
    pub fn string(string: String) -> Value {
        Value::Ref(Box::new(RefType::String(string)))
    }

    pub fn is_truthy(&self) -> bool {
        use Value::*;

        match self {
            Boolean(false) | Nil => false,
            _ => true,
        }
    }

    pub fn is_falsey(&self) -> bool {
        ! self.is_truthy()
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;

        match self {
            Boolean(b) => write!(f, "{}", b),
            Nil        => write!(f, "nil"),
            Number(n)  => write!(f, "{}", n),
            Ref(r)     => write!(f, "{}", r),
        }
    }
}

impl fmt::Display for RefType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use RefType::*;

        match self {
            String(s) => write!(f, "{}", s),
        }
    }
}
