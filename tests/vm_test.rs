use monkey::ast::Program;
use monkey::compiler::Compiler;
use monkey::lexer::Lexer;
use monkey::object::Object;
use monkey::parser::Parser;
use monkey::vm::Vm;

#[test]
fn test_integer_arithmetic() {
    let test_cases = vec![
        VmTestCase {
            input: "1".to_string(),
            expected: Value::Integer(1),
        },
        /*VmTestCase {
            input: "2".to_string(),
            expected: Value::Integer(2),
        },
        VmTestCase {
            input: "1 + 2".to_string(),
            expected: Value::Integer(3),
        },*/
    ];

    run_vm_test(test_cases);
}

#[derive(Debug)]
enum Value {
    Integer(i64),
}

struct VmTestCase {
    input: String,
    expected: Value
}

fn run_vm_test(test_cases: Vec<VmTestCase>) {
    for test in test_cases {
        let program = parse(test.input.clone());

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

        assert_expected_object(&stack_elem, test.expected, test.input.as_str());
    }
}

fn assert_expected_object(actual: &Object, expected: Value, test_input: &str) {
    match expected {
        Value::Integer(expected) => assert_integer_object(actual, expected, test_input),
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
