use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolScope {
    GlobalScope
}

impl fmt::Display for SymbolScope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SymbolScope::GlobalScope => write!(f, "GLOBAL")
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: i32
}

pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    // TODO: Is this redundant given we can get it from the map?
    num_definitions: i32
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0
        }
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol{
            name: String::from(name),
            index: self.num_definitions,
            scope: SymbolScope::GlobalScope
        };

        self.store.insert(String::from(name), symbol.clone());
        self.num_definitions += 1;
        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        self.store.get(name).cloned()
    }
}