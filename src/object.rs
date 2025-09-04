#![allow(dead_code)]
#![allow(unpredictable_function_pointer_comparisons)]

use std::fmt::{Display, Formatter};
use crate::ast::{BlockStatement, Identifier};
use crate::environment::Environment;

pub type BuiltinFunction = fn(Vec<Object>) -> Object;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    StringObject(String),
    ReturnValue(Box<Object>),
    Function(Vec<Identifier>, BlockStatement, Environment),
    Array(Vec<Object>),
    Builtin(BuiltinFunction),
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
            Object::Array(elements) => {
                write!(f, "[{}]",
                       elements
                           .iter()
                           .map(|id| id.to_string())
                           .collect::<Vec<String>>()
                           .join(", "))
            }
            Object::Builtin(_) => {write!(f, "builtin function")},
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
            Object::StringObject(_) => "STRING",
            Object::ReturnValue(_) => "RETURN_VALUE",
            Object::Function(_, _, _) => "FUNCTION",
            Object::Array(_) => { "ARRAY" }
            Object::Builtin(_) => "BUILTIN",
            Object::Error(_) => "ERROR",
            Object::Null => "NULL",
        }.to_string()
    }
}