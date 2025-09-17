use monkey::ast::Program;
use monkey::compiler::Compiler;
use monkey::lexer::Lexer;
use monkey::object::Object;
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
    ];

    run_vm_test(&test_cases);
}

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool)
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
        Value::Boolean(expected) => assert_boolean_object(actual, expected, test_input)
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
