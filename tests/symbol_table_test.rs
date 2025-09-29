use std::collections::HashMap;
use monkey::symbol_table::{Symbol, SymbolTable};
use monkey::symbol_table::SymbolScope::GlobalScope;

#[test]
fn test_define() {
    let expected = HashMap::from([
        ("a", Symbol{ name: "a".to_string(), scope: GlobalScope, index: 0}),
        ("b", Symbol{ name: "b".to_string(), scope: GlobalScope, index: 1})
    ]);
    
    let mut global = SymbolTable::new();
    
    let a = global.define("a");
    assert_eq!(a, expected["a"]);
    
    let b = global.define("b");
    assert_eq!(b, expected["b"]);
}

#[test]
fn test_resolve_global() {
    let mut global = SymbolTable::new();
    global.define("a");
    global.define("b");
    
    let expected = vec![
        Symbol{ name: "a".to_string(), scope: GlobalScope, index: 0},
        Symbol{ name: "b".to_string(), scope: GlobalScope, index: 1},
    ];

    for expected_sym in expected.iter() {
        let resolved_sym = global.resolve(expected_sym.name.as_str())
            .unwrap_or_else(|| panic!("name {} is not resolvable", expected_sym.name));
        assert_eq!(resolved_sym, *expected_sym)
    }
}
