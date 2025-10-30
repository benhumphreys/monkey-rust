use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;
use monkey::symbol_table::{Symbol, SymbolTable};
use monkey::symbol_table::SymbolScope::{GlobalScope, LocalScope};

struct SymbolTableTestCase {
    table: SymbolTable,
    expected_symbols: Vec<Symbol>
}

#[test]
fn test_define() {
    let expected = HashMap::from([
        ("a", Symbol{ name: "a".to_string(), scope: GlobalScope, index: 0}),
        ("b", Symbol{ name: "b".to_string(), scope: GlobalScope, index: 1}),
        ("c", Symbol{ name: "c".to_string(), scope: LocalScope, index: 0}),
        ("d", Symbol{ name: "d".to_string(), scope: LocalScope, index: 1}),
        ("e", Symbol{ name: "e".to_string(), scope: LocalScope, index: 0}),
        ("f", Symbol{ name: "f".to_string(), scope: LocalScope, index: 1}),
    ]);

    let global = Rc::new(RefCell::new(SymbolTable::new()));

    let a = global.borrow_mut().define("a");
    assert_eq!(a, expected["a"]);
    
    let b = global.borrow_mut().define("b");
    assert_eq!(b, expected["b"]);

    let first_local = Rc::new(RefCell::new(SymbolTable::new_enclosed_symbol_table(&global)));
    let c = first_local.borrow_mut().define("c");
    assert_eq!(c, expected["c"]);

    let d = first_local.borrow_mut().define("d");
    assert_eq!(d, expected["d"]);

    let mut second_local = SymbolTable::new_enclosed_symbol_table(&first_local);
    let e = second_local.define("e");
    assert_eq!(e, expected["e"]);
}

#[test]
fn test_resolve_global() {
    let mut global = SymbolTable::new();
    global.define("a");
    global.define("b");
    
    let test_cases = vec![
        SymbolTableTestCase {
            table: global,
            expected_symbols: vec![
                Symbol { name: "a".to_string(), scope: GlobalScope, index: 0 },
                Symbol { name: "b".to_string(), scope: GlobalScope, index: 1 },
            ]
        },
    ];

    run_symbol_table_test(test_cases);
}

#[test]
fn test_resolve_local() {
    let global = Rc::new(RefCell::new(SymbolTable::new()));
    global.borrow_mut().define("a");
    global.borrow_mut().define("b");

    let local = Rc::new(RefCell::new(SymbolTable::new_enclosed_symbol_table(&global)));
    local.borrow_mut().define("c");
    local.borrow_mut().define("d");

    let test_cases = vec![
        SymbolTableTestCase {
            table: local.borrow().deref().clone(),
            expected_symbols: vec![
                Symbol{ name: "a".to_string(), scope: GlobalScope, index: 0},
                Symbol{ name: "b".to_string(), scope: GlobalScope, index: 1},
                Symbol{ name: "c".to_string(), scope: LocalScope, index: 0},
                Symbol{ name: "d".to_string(), scope: LocalScope, index: 1},
            ]
        },
    ];

    run_symbol_table_test(test_cases);
}

#[test]
fn test_resolve_nested_local() {
    let global = Rc::new(RefCell::new(SymbolTable::new()));
    global.borrow_mut().define("a");
    global.borrow_mut().define("b");

    let first_local = Rc::new(RefCell::new(SymbolTable::new_enclosed_symbol_table(&global)));
    first_local.borrow_mut().define("c");
    first_local.borrow_mut().define("d");

    let mut second_local = SymbolTable::new_enclosed_symbol_table(&first_local);
    second_local.define("e");
    second_local.define("f");

    let test_cases = vec![
        SymbolTableTestCase {
            table: first_local.borrow().deref().clone(),
            expected_symbols: vec![
                Symbol { name: "a".to_string(), scope: GlobalScope, index: 0 },
                Symbol { name: "b".to_string(), scope: GlobalScope, index: 1 },
                Symbol { name: "c".to_string(), scope: LocalScope, index: 0 },
                Symbol { name: "d".to_string(), scope: LocalScope, index: 1},
            ]
        },
        SymbolTableTestCase {
            table: second_local,
            expected_symbols: vec![
                Symbol { name: "a".to_string(), scope: GlobalScope, index: 0 },
                Symbol { name: "b".to_string(), scope: GlobalScope, index: 1 },
                Symbol { name: "e".to_string(), scope: LocalScope, index: 0 },
                Symbol { name: "f".to_string(), scope: LocalScope, index: 1},
            ]
        },
    ];

    run_symbol_table_test(test_cases);
}

fn run_symbol_table_test(test_cases: Vec<SymbolTableTestCase>) {
    for test_case in test_cases {
        let table = test_case.table;
        for expected_sym in test_case.expected_symbols.iter() {
            let resolved_sym = table.resolve(expected_sym.name.as_str())
                .unwrap_or_else(|| panic!("name {} is not resolvable", expected_sym.name));
            assert_eq!(resolved_sym, *expected_sym)
        }
    }
}