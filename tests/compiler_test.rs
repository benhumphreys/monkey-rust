#![allow(dead_code)]

use monkey::ast::Program;
use monkey::code::Opcode::OpConstant;
use monkey::code::{make, Instructions};
use monkey::compiler::Compiler;
use monkey::lexer::Lexer;
use monkey::object::Object;
use monkey::parser::Parser;

#[derive(Debug)]
enum Value {
    Integer(i64),
}

struct CompilerTestCase {
    input: String,
    expected_constants: Vec<Value>,
    expected_instructions: Vec<Instructions>
}

#[test]
fn test_integer_arithmetic() {
    let test_case = vec![
        CompilerTestCase {
            input: String::from("1 + 2"),
            expected_constants: vec![Value::Integer(1), Value::Integer(2)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpConstant, vec![1])
            ],
        }];

    run_compiler_test(test_case)
}

fn run_compiler_test(test_cases: Vec<CompilerTestCase>) {
    for test in test_cases {
        let program = parse(test.input);

        let mut compiler = Compiler::new();
        let result = compiler.compile(&program);
        if result.is_err() {
            panic!("compiler error: {}", result.err().unwrap());
        }

        let bytecode = compiler.bytecode();

        assert_instructions(bytecode.instructions, test.expected_instructions);
        assert_constants(bytecode.constants, test.expected_constants);
    }
}

fn assert_instructions(actual: Instructions, expected: Vec<Instructions>) {
    let concatted = concat_instructions(expected);

    assert_eq!(actual.len(), concatted.len(), "instruction length mismatch");
    for i in 0..actual.len() {
        assert_eq!(actual[i], concatted[i], "instruction mismatch");
    }
}

fn assert_constants(actual: Vec<Object>, expected: Vec<Value>) {
    assert_eq!(actual.len(), expected.len());

    let mut i = 0;
    for constant in expected {
        match constant {
            Value::Integer(val) => assert_integer_object(actual[i].clone(), val),
        }
        i += 1;
    }
}

fn assert_integer_object(actual: Object, expected: i64) {
    if let Object::Integer(value) = actual {
        assert_eq!(value, expected);
    } else {
        panic!("Object not Integer. Got={:?}", actual);
    }
}

fn concat_instructions(s: Vec<Instructions>) -> Instructions {
    let mut out = vec![];
    for ins in s {
        out.extend(ins);
    }
    out
}

fn parse(input: String) -> Program {
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer);
    parser.parse_program()
}
