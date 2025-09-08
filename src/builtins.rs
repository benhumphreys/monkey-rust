use crate::object::{Object, ObjectType, OBJECT_NULL};
use std::collections::HashMap;

pub fn builtins() -> HashMap<String, Object>  {
    HashMap::from([
        ("len".to_string(), Object::Builtin(len)),
        ("first".to_string(), Object::Builtin(first)),
        ("last".to_string(), Object::Builtin(last)),
        ("rest".to_string(), Object::Builtin(rest)),
        ("push".to_string(), Object::Builtin(push)),
        ("puts".to_string(), Object::Builtin(puts)),
    ])
}

fn len(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()));
    }

    match &args[0] {
        Object::Array(elements) => Object::Integer(elements.len() as i64),
        Object::StringObject(s) => Object::Integer(s.len() as i64),
        obj => Object::Error(format!("argument to `len` not supported, got {}", obj.object_type()))
    }
}

fn first(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()));
    }

    if let Object::Array(elements) = &args[0] {
        if !elements.is_empty() {
            elements[0].clone()
        } else {
            OBJECT_NULL
        }
    } else {
        Object::Error(format!("argument to 'first' must be ARRAY, got {}", args[0].object_type()))
    }
}
fn last(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()));
    }

    if let Object::Array(elements) = &args[0] {
        let length = elements.len();
        if length > 0 {
            elements[length - 1].clone()
        } else {
            OBJECT_NULL
        }
    } else {
        Object::Error(format!("argument to 'last' must be ARRAY, got {}", args[0].object_type()))
    }
}

fn rest(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()));
    }

    if let Object::Array(elements) = &args[0] {
        let length = elements.len();
        if length > 0 {
            Object::Array(elements[1..length].to_vec())
        } else {
            OBJECT_NULL
        }
    } else {
        Object::Error(format!("argument to 'rest' must be ARRAY, got {}", args[0].object_type()))
    }
}

fn push(args: &[Object]) -> Object {
    if args.len() != 2 {
        return Object::Error(format!("wrong number of arguments. got={}, want=2", args.len()));
    }

    if let Object::Array(elements) = &args[0] {
        let mut new_elements = elements.to_vec();
        new_elements.push(args[1].clone());
        Object::Array(new_elements)
    } else {
        Object::Error(format!("argument to 'push' must be ARRAY, got {}", args[0].object_type()))
    }
}

fn puts(args: &[Object]) -> Object {
    for arg in args {
        println!("{}", arg);
    }
    OBJECT_NULL
}