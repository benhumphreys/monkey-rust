#![allow(dead_code)]

use std::fmt::{Display, Formatter};
use crate::ast::{BlockStatement, Identifier};
use crate::environment::Environment;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    StringObject(String),
    ReturnValue(Box<Object>),
    Function(Vec<Identifier>, BlockStatement, Environment),
    Error(String),
    Null,
}

pub trait ObjectType {
    fn object_type(&self) -> String;
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::StringObject(value) => write!(f, "{}", value),
            Object::ReturnValue(boxed_value) => write!(f, "{}", *boxed_value),
            Object::Function(parameters, body, _) => {
                write!(f, "fn ({}) {{\n{}\n}}",
                       parameters
                           .iter()
                           .map(|id| id.to_string())
                           .collect::<Vec<String>>()
                           .join(", "),
                       body)
            }
            Object::Error(value) => write!(f, "error: {}", value),
            Object::Null =>  write!(f, "Null"),
        }
    }
}

impl ObjectType for Object {
    fn object_type(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::StringObject(_) => {"STRING"}
            Object::ReturnValue(_) => "RETURN_VALUE",
            Object::Function(_, _, _) => "FUNCTION",
            Object::Error(_) => "ERROR",
            Object::Null => "NULL",
        }.to_string()
    }
}