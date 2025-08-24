#![allow(dead_code)]

use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Null =>  write!(f, "Null")
        }
    }
}