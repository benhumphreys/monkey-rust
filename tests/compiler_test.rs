#![allow(dead_code)]

use monkey::ast::Program;
use monkey::code::Opcode::{OpAdd, OpBang, OpConstant, OpDiv, OpEqual, OpFalse, OpGreaterThan, OpJump, OpJumpNotTruthy, OpMinus, OpMul, OpNotEqual, OpPop, OpSub, OpTrue};
use monkey::code::{disassemble, make, Instructions};
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
                make(OpConstant, vec![1]),
                make(OpAdd, vec![]),
                make(OpPop, vec![]),
            ],
        },
        CompilerTestCase {
            input: String::from("1; 2"),
            expected_constants: vec![Value::Integer(1), Value::Integer(2)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpPop, vec![]),
                make(OpConstant, vec![1]),
                make(OpPop, vec![]),
            ],
        },
        CompilerTestCase {
            input: String::from("1 - 2"),
            expected_constants: vec![Value::Integer(1), Value::Integer(2)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpConstant, vec![1]),
                make(OpSub, vec![]),
                make(OpPop, vec![]),
            ],
        },
        CompilerTestCase {
            input: String::from("1 * 2"),
            expected_constants: vec![Value::Integer(1), Value::Integer(2)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpConstant, vec![1]),
                make(OpMul, vec![]),
                make(OpPop, vec![]),
            ],
        },
        CompilerTestCase {
            input: String::from("2 / 1"),
            expected_constants: vec![Value::Integer(2), Value::Integer(1)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpConstant, vec![1]),
                make(OpDiv, vec![]),
                make(OpPop, vec![]),
            ],
        },
        CompilerTestCase {
            input: String::from("-1"),
            expected_constants: vec![Value::Integer(1)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpMinus, vec![]),
                make(OpPop, vec![]),
            ],
        }
    ];

    run_compiler_test(test_case)
}

#[test]
fn test_boolean_expressions() {
    let test_cases = vec![
        CompilerTestCase {
            input: String::from("true"),
            expected_constants: vec![],
            expected_instructions: vec![
                make(OpTrue, vec![]),
                make(OpPop, vec![]),
            ],
        },
        CompilerTestCase {
            input: String::from("false"),
            expected_constants: vec![],
            expected_instructions: vec![
                make(OpFalse, vec![]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("1 > 2"),
            expected_constants: vec![Value::Integer(1), Value::Integer(2)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpConstant, vec![1]),
                make(OpGreaterThan, vec![]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("1 < 2"),
            expected_constants: vec![Value::Integer(2), Value::Integer(1)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpConstant, vec![1]),
                make(OpGreaterThan, vec![]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("1 == 2"),
            expected_constants: vec![Value::Integer(1), Value::Integer(2)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpConstant, vec![1]),
                make(OpEqual, vec![]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("1 != 2"),
            expected_constants: vec![Value::Integer(1), Value::Integer(2)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpConstant, vec![1]),
                make(OpNotEqual, vec![]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("true == false"),
            expected_constants: vec![],
            expected_instructions: vec![
                make(OpTrue, vec![]),
                make(OpFalse, vec![]),
                make(OpEqual, vec![]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("true != false"),
            expected_constants: vec![],
            expected_instructions: vec![
                make(OpTrue, vec![]),
                make(OpFalse, vec![]),
                make(OpNotEqual, vec![]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("!true"),
            expected_constants: vec![],
            expected_instructions: vec![
                make(OpTrue, vec![]),
                make(OpBang, vec![]),
                make(OpPop, vec![]),
            ]
        }
    ];

    run_compiler_test(test_cases);
}

#[test]
fn test_conditionals() {
    let test_cases = vec![
        CompilerTestCase {
            input: String::from("if (true) { 10 }; 3333;"),
            expected_constants: vec![Value::Integer(10), Value::Integer(3333)],
            expected_instructions: vec![
                // 0000
                make(OpTrue, vec![]),
                // 0001
                make(OpJumpNotTruthy, vec![7]),
                // 0004
                make(OpConstant, vec![0]),
                // 0007
                make(OpPop, vec![]),
                // 0008
                make(OpConstant, vec![1]),
                // 0011
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("if (true) { 10 } else { 20 }; 3333"),
            expected_constants: vec![Value::Integer(10), Value::Integer(20), Value::Integer(3333)],
            expected_instructions: vec![
                // 0000
                make(OpTrue, vec![]),
                // 0001
                make(OpJumpNotTruthy, vec![10]),
                // 0004
                make(OpConstant, vec![0]),
                // 0007
                make(OpJump, vec![13]),
                // 0010
                make(OpConstant, vec![1]),
                // 0013
                make(OpPop, vec![]),
                // 0014
                make(OpConstant, vec![2]),
                // 0017
                make(OpPop, vec![]),
            ]
        }
    ];

    run_compiler_test(test_cases);
}

fn run_compiler_test(test_cases: Vec<CompilerTestCase>) {
    for test in test_cases {
        let program = parse(&test.input);

        let mut compiler = Compiler::new();
        let result = compiler.compile(&program);
        if result.is_err() {
            panic!("compiler error: {}", result.err().unwrap());
        }

        let bytecode = compiler.bytecode();

        assert_instructions(bytecode.instructions, test.expected_instructions, test.input.as_str());
        assert_constants(bytecode.constants, test.expected_constants);
    }
}

fn assert_instructions(actual: Instructions, expected: Vec<Instructions>, test_case: &str) {
    let concatenated = concat_instructions(expected);

    assert_eq!(actual.len(), concatenated.len(), "wrong instruction length.\nwant={}\ngot={}\ntest_case={}",
               disassemble(&concatenated), disassemble(&actual), test_case);

    for i in 0..actual.len() {
        assert_eq!(actual[i], concatenated[i], "wrong instruction at {}.\nwant={}\ngot={}\ntest_case={}",
                   i, disassemble(&concatenated), disassemble(&actual), test_case);
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

fn parse(input: &String) -> Program {
    let mut lexer = Lexer::new(input.clone());
    let mut parser = Parser::new(&mut lexer);
    parser.parse_program()
}
