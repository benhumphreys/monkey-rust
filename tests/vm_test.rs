use monkey::ast::Program;
use monkey::compiler::Compiler;
use monkey::lexer::Lexer;
use monkey::object::{Object, ObjectType};
use monkey::parser::Parser;
use monkey::vm::Vm;

#[test]
fn test_integer_arithmetic() {
    let test_cases = [
        ("1", Value::Integer(1)),
        ("2", Value::Integer(2)),
        ("1 + 2", Value::Integer(3)),
        ("1 - 2", Value::Integer(-1)),
        ("1 * 2", Value::Integer(2)),
        ("4 / 2", Value::Integer(2)),
        ("50 / 2 * 2 + 10 - 5", Value::Integer(55)),
        ("5 + 5 + 5 + 5 - 10", Value::Integer(10)),
        ("2 * 2 * 2 * 2 * 2", Value::Integer(32)),
        ("5 * 2 + 10", Value::Integer(20)),
        ("5 + 2 * 10", Value::Integer(25)),
        ("5 * (2 + 10)", Value::Integer(60)),
        ("-5", Value::Integer(-5)),
        ("-10", Value::Integer(-10)),
        ("-50 + 100 + -50", Value::Integer(0)),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Value::Integer(50)),
    ];

    run_vm_test(&test_cases);
}

#[test]
fn test_boolean_expressions() {
    let test_cases = [
        ("true", Value::Boolean(true)),
        ("false", Value::Boolean(false)),
        ("1 < 2", Value::Boolean(true)),
        ("1 > 2", Value::Boolean(false)),
        ("1 < 1", Value::Boolean(false)),
        ("1 > 1", Value::Boolean(false)),
        ("1 == 1", Value::Boolean(true)),
        ("1 != 1", Value::Boolean(false)),
        ("1 == 2", Value::Boolean(false)),
        ("1 != 2", Value::Boolean(true)),
        ("true == true", Value::Boolean(true)),
        ("false == false", Value::Boolean(true)),
        ("true == false", Value::Boolean(false)),
        ("true != false", Value::Boolean(true)),
        ("false != true", Value::Boolean(true)),
        ("(1 < 2) == true", Value::Boolean(true)),
        ("(1 < 2) == false", Value::Boolean(false)),
        ("(1 > 2) == true", Value::Boolean(false)),
        ("(1 > 2) == false", Value::Boolean(true)),
        ("!true", Value::Boolean(false)),
        ("!false", Value::Boolean(true)),
        ("!5", Value::Boolean(false)),
        ("!!true", Value::Boolean(true)),
        ("!!false", Value::Boolean(false)),
        ("!!5", Value::Boolean(true)),
        ("!(if (false) { 5; })", Value::Boolean(true))
    ];

    run_vm_test(&test_cases);
}

#[test]
fn test_conditionals() {
    let test_cases = [
        ("if (true) { 10 }", Value::Integer(10)),
        ("if (true) { 10 } else { 20 }", Value::Integer(10)),
        ("if (false) { 10 } else { 20 } ", Value::Integer(20)),
        ("if (1) { 10 }", Value::Integer(10)),
        ("if (1 < 2) { 10 }", Value::Integer(10)),
        ("if (1 < 2) { 10 } else { 20 }", Value::Integer(10)),
        ("if (1 > 2) { 10 } else { 20 }", Value::Integer(20)),
        ("if (1 > 2) { 10 }", Value::Null),
        ("if (false) { 10 }", Value::Null),
        ("if ((if (false) { 10 })) { 10 } else { 20 }", Value::Integer(20)),
    ];

    run_vm_test(&test_cases);
}

#[test]
fn test_global_let_statements() {
    let test_cases = [
        ("let one = 1; one", Value::Integer(1)),
        ("let one = 1; let two = 2; one + two", Value::Integer(3)),
        ("let one = 1; let two = one + one; one + two", Value::Integer(3)),
    ];

    run_vm_test(&test_cases);
}

#[test]
fn test_string_expressions() {
    let test_cases = [
        (r#""monkey""#, Value::String("monkey".to_string())),
        (r#""mon" + "key""#, Value::String("monkey".to_string())),
        (r#""mon" + "key" + "banana""#, Value::String("monkeybanana".to_string())),
    ];

    run_vm_test(&test_cases)
}

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Null
}

fn run_vm_test(test_cases: &[(&str, Value)]) {
    for test in test_cases {
        let input = test.0.to_string();
        let expected = &test.1;

        let program = parse(input.clone());

        let mut compiler = Compiler::new();
        let result = compiler.compile(&program);
        if result.is_err() {
            panic!("compiler error: {}", result.err().unwrap());
        }

        let bytecode = compiler.bytecode();
        let mut vm = Vm::new(&bytecode);
        let vm_result = vm.run();
        if vm_result.is_err() {
            panic!("vm error: {}", vm_result.err().unwrap());
        }
        let stack_elem = vm.last_popped_stack_elem();

        assert_expected_object(&stack_elem, expected.clone(), input.as_str());
    }
}

fn assert_expected_object(actual: &Object, expected: Value, test_input: &str) {
    match expected {
        Value::Integer(expected) => assert_integer_object(actual, expected, test_input),
        Value::Boolean(expected) => assert_boolean_object(actual, expected, test_input),
        Value::String(expected) => assert_string_object(actual, expected.as_str(), test_input),
        Value::Null => assert_null_object(actual),
    }
}

fn parse(input: String) -> Program {
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer);
    parser.parse_program()
}

fn assert_integer_object(actual: &Object, expected: i64, test_input: &str) {
    if let Object::Integer(value) = actual {
        assert_eq!(*value, expected, "Test input: {}", test_input);
    } else {
        panic!("Object not Integer. Got={:?}. Test input: {}", actual, test_input);
    }
}

fn assert_boolean_object(actual: &Object, expected: bool, test_input: &str) {
    if let Object::Boolean(value) = actual {
        assert_eq!(*value, expected, "Test input: {}", test_input);
    } else {
        panic!("Object not Boolean. Got={:?}. Test input: {}", actual, test_input);
    }
}

fn assert_string_object(actual: &Object, expected: &str, test_input: &str) {
    if let Object::StringObject(value) = actual {
        assert_eq!(*value, expected, "Test input: {}", test_input);
    } else {
        panic!("Object not StringObject. Got={:?}. Test input: {}", actual, test_input);
    }
}

fn assert_null_object(object: &Object) {
    assert!(matches!(object, Object::Null), "Expected Null object, x Got: {:?}", object.object_type());
}
