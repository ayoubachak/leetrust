// value.rs

use std::cmp::Ordering;
use std::ops::{Add, Sub, Mul, Div, Neg, Not};
use std::rc::Rc;

pub struct ValueArray {
    pub values: Vec<Value>,
}

impl ValueArray {
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
        }
    }

    pub fn write(&mut self, value: Value) {
        self.values.push(value);
    }

    pub fn count(&self) -> usize {
        self.values.len()
    }
}


#[derive(Clone)]
pub enum Value {
    Number(f64),
    Bool(bool),
    String(Rc<String>),
    Identifier(Rc<String>),
    None,
    Error(String),
}

impl Value {
    pub fn from_f64(n: f64) -> Self {
        Value::Number(n)
    }

    pub fn from_bool(b: bool) -> Self {
        Value::Bool(b)
    }

    pub fn from_string(s: Rc<String>) -> Self {
        Value::String(s)
    }

    pub fn from_identifier(s: Rc<String>) -> Self {
        Value::Identifier(s)
    }

    pub fn as_number(&self) -> f64 {
        if let Value::Number(n) = self {
            *n
        } else {
            panic!("Value is not a number");
        }
    }

    pub fn as_bool(&self) -> bool {
        if let Value::Bool(b) = self {
            *b
        } else {
            panic!("Value is not a bool");
        }
    }

    pub fn as_string(&self) -> Rc<String> {
        if let Value::String(s) = self {
            Rc::clone(s)
        } else {
            panic!("Value is not a string");
        }
    }

    pub fn as_identifier(&self) -> Rc<String> {
        if let Value::Identifier(s) = self {
            Rc::clone(s)
        } else {
            panic!("Value is not an identifier");
        }
    }
}

impl Add for Value {
    type Output = Result<Self, String>;

    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (Value::String(a), Value::String(b)) => Ok(Value::from_string(Rc::new((*a).clone() + &(*b)))),
            _ => Err("Add operation type mismatch".to_string()),
        }
    }
}

impl Sub for Value {
    type Output = Result<Self, String>;


    fn sub(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            _ => Err("Subtract operation type mismatch or unsupported".to_string()),
        }
    }
}

impl Neg for Value {
    type Output = Result<Self, String>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(a) => Ok(Value::Number(-a)),
            _ => Err("Negate operation type mismatch or unsupported".to_string()),
        }
    }
}

impl Mul for Value {
    type Output = Result<Self, String>;

    fn mul(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
            _ => Err("Multiply operation type mismatch or unsupported".to_string()),
        }
    }
}

impl Div for Value {
    type Output = Result<Self, String>;

    fn div(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => {
                if b == 0.0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Number(a / b))
                }
            },
            _ => Err("Divide operation type mismatch or unsupported".to_string()),
        }
    }
}

impl Not for Value {
    type Output = Result<Self, String>;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(b) => Ok(Value::Bool(!b)),
            Value::Number(n) => Ok(Value::Bool(n == 0.0)),
            _ => Err("Not operation type mismatch or unsupported".to_string()),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Identifier(a), Value::Identifier(b)) => a == b,
            _ => false,
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Identifier(s) => write!(f, "{}", s),
            Value::None => write!(f, "None"),
            Value::Error(e) => write!(f, "Error: {}", e),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a.partial_cmp(b),
            (Value::String(a), Value::String(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}


impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Identifier(s) => write!(f, "{}", s),
            Value::None => write!(f, "None"),
            Value::Error(e) => write!(f, "Error: {}", e),
        }
    }
}