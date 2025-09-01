use std::collections::HashMap;
use crate::object::{Object, ObjectType};

pub fn builtins() -> HashMap<String, Object>  {
    HashMap::from([
        ("len".to_string(), Object::Builtin(len))
    ])
}

fn len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()));
    }

    match &args[0] {
        Object::StringObject(s) => Object::Integer(s.len() as i64),
        obj => Object::Error(format!("argument to `len` not supported, got {}", obj.object_type()))
    }
}