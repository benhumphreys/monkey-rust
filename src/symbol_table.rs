use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolScope {
    GlobalScope,
    LocalScope,
}

impl fmt::Display for SymbolScope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SymbolScope::GlobalScope => write!(f, "GLOBAL"),
            SymbolScope::LocalScope => write!(f, "LOCAL"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: i32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    // TODO: Is this redundant given we can get it from the map?
    num_definitions: i32,
    pub outer: Option<Rc<RefCell<SymbolTable>>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
            outer: None,
        }
    }

    pub fn new_enclosed_symbol_table(outer: &Rc<RefCell<SymbolTable>>) -> SymbolTable {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
            outer: Some(Rc::clone(outer)),
        }
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol{
            name: String::from(name),
            index: self.num_definitions,
            scope: if self.outer.is_none() { SymbolScope::GlobalScope } else { SymbolScope::LocalScope },
        };

        self.store.insert(String::from(name), symbol.clone());
        self.num_definitions += 1;
        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        match self.store.get(name) {
            Some(value) => Some(value.clone()),
            None => {
                if let Some(outer) = &self.outer {
                    outer.borrow().resolve(name)
                } else {
                    None
                }
            }
        }
    }
}