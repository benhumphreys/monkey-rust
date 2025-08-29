#![allow(dead_code)]

use crate::object::Object;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed_environment(outer: &Environment) -> Environment {
        Environment {
            store: HashMap::new(),
            outer: Some(Box::new(outer.clone())),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        if let Some(obj) = self.store.get(name) {
            Some(obj.clone())
        } else if let Some(outer) = &self.outer {
            outer.get(name)
        } else {
            None
        }
    }
    
    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }
}
