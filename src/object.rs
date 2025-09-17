#![allow(dead_code)]
#![allow(unpredictable_function_pointer_comparisons)]

use crate::ast::{BlockStatement, Identifier};
use crate::environment::Environment;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub type BuiltinFunction = fn(&[Object]) -> Object;

pub const OBJECT_BOOLEAN_TRUE: Object = Object::Boolean(true);
pub const OBJECT_BOOLEAN_FALSE: Object = Object::Boolean(false);
pub const OBJECT_NULL: Object = Object::Null;

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    StringObject(String),
    ReturnValue(Box<Object>),
    Function(Vec<Identifier>, BlockStatement, Rc<RefCell<Environment>>),
    Array(Vec<Object>),
    HashObject(HashMap<Object, Object>),
    Builtin(BuiltinFunction),
    Error(String),
    Null,
}

pub trait ObjectType {
    fn object_type(&self) -> String;
}

pub trait IsHashable {
    // Indicates which objects can be used as keys in a Monkey language hash
    fn is_hashable(&self) -> bool;
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
            Object::HashObject(pairs) => {
                write!(f, "{{{}}}",
                       pairs
                           .iter()
                           .map(|(key, value)| format!("{}: {}", key, value))
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
            Object::HashObject(_) => "HASH",
            Object::Builtin(_) => "BUILTIN",
            Object::Error(_) => "ERROR",
            Object::Null => "NULL",
        }.to_string()
    }
}

impl IsHashable for Object {
    fn is_hashable(&self) -> bool {
        match self {
            Object::Integer(_) => true,
            Object::Boolean(_) => true,
            Object::StringObject(_) => true,
            _ => false
        }
    }
}
impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Integer(a), Object::Integer(b)) => a == b,
            (Object::Boolean(a), Object::Boolean(b)) => a == b,
            (Object::StringObject(a), Object::StringObject(b)) => a == b,
            (Object::ReturnValue(a), Object::ReturnValue(b)) => a == b,
            (Object::Function(_, _, _), Object::Function(_, _, _)) => false,
            (Object::Array(a), Object::Array(b)) => a == b,
            (Object::HashObject(_), Object::HashObject(_)) => {
                panic!("Not implemented: HashObject ==")
            }
            (Object::Builtin(f1), Object::Builtin(f2)) => f1 == f2,
            (Object::Error(a), Object::Error(b)) => a == b,
            (Object::Null, Object::Null) => true,
            _ => false,
        }
    }
}

impl Eq for Object {
}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Integer(val) => val.hash(state),
            Object::Boolean(val) => val.hash(state),
            Object::StringObject(val) => val.hash(state),
            _ => { panic!("Hash trait not implemented for object type: {}", self.object_type())}
        }
    }
}

pub fn native_bool_to_bool_object(value: bool) -> Object {
    if value { OBJECT_BOOLEAN_TRUE } else { OBJECT_BOOLEAN_FALSE }
}