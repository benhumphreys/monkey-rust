#![allow(dead_code)]

use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
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
            Object::ReturnValue(boxed_value) => write!(f, "{}", *boxed_value),
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
            Object::ReturnValue(_) => "RETURN_VALUE",
            Object::Error(_) => "ERROR",
            Object::Null => "NULL"
        }.to_string()
    }
}