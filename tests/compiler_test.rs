use monkey::ast::Program;
use monkey::code::Opcode::{OpAdd, OpArray, OpBang, OpCall, OpConstant, OpDiv, OpEqual, OpFalse, OpGetGlobal, OpGreaterThan, OpHash, OpIndex, OpJump, OpJumpNotTruthy, OpMinus, OpMul, OpNotEqual, OpNull, OpPop, OpReturn, OpReturnValue, OpSetGlobal, OpSub, OpTrue};
use monkey::code::{disassemble, make, Instructions};
use monkey::compiler::Compiler;
use monkey::lexer::Lexer;
use monkey::object::Object;
use monkey::parser::Parser;

#[derive(Debug)]
enum Value {
    Integer(i64),
    String(String),
    Instructions(Vec<u8>)
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
                make(OpJumpNotTruthy, vec![10]),
                // 0004
                make(OpConstant, vec![0]),
                // 0007
                make(OpJump, vec![11]),
                // 0010
                make(OpNull, vec![]),
                // 0011
                make(OpPop, vec![]),
                // 0012
                make(OpConstant, vec![1]),
                // 0015
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

#[test]
fn test_global_let_statements() {
    let test_cases = vec![
        CompilerTestCase {
            input: String::from("let one = 1;
                                 let two = 2;"),
            expected_constants: vec![Value::Integer(1), Value::Integer(2)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpSetGlobal, vec![0]),
                make(OpConstant, vec![1]),
                make(OpSetGlobal, vec![1]),
            ]
        },
        CompilerTestCase {
            input: String::from("let one = 1;
                                 one;"),
            expected_constants: vec![Value::Integer(1)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpSetGlobal, vec![0]),
                make(OpGetGlobal, vec![0]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("let one = 1;
                                 let two = one;
                                 two;"),
            expected_constants: vec![Value::Integer(1)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpSetGlobal, vec![0]),
                make(OpGetGlobal, vec![0]),
                make(OpSetGlobal, vec![1]),
                make(OpGetGlobal, vec![1]),
                make(OpPop, vec![]),
            ]
        },
    ];

    run_compiler_test(test_cases)
}

#[test]
fn test_string_expressions() {
    let test_cases = vec![
        CompilerTestCase {
            input: String::from("\"monkey\""),
            expected_constants: vec![Value::String("monkey".to_string())],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("\"mon\" + \"key\""),
            expected_constants: vec![Value::String("mon".to_string()), Value::String("key".to_string())],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpConstant, vec![1]),
                make(OpAdd, vec![]),
                make(OpPop, vec![]),
            ]
        }
    ];

    run_compiler_test(test_cases)
}

#[test]
fn test_array_literals() {
    let test_cases = vec![
        CompilerTestCase {
            input: String::from("[]"),
            expected_constants: vec![],
            expected_instructions: vec![
                make(OpArray, vec![0]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("[1, 2, 3]"),
            expected_constants: vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpConstant, vec![1]),
                make(OpConstant, vec![2]),
                make(OpArray, vec![3]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("[1 + 2, 3 - 4, 5 * 6]"),
            expected_constants: vec![Value::Integer(1), Value::Integer(2), Value::Integer(3),
                                     Value::Integer(4), Value::Integer(5), Value::Integer(6)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpConstant, vec![1]),
                make(OpAdd, vec![]),
                make(OpConstant, vec![2]),
                make(OpConstant, vec![3]),
                make(OpSub, vec![]),
                make(OpConstant, vec![4]),
                make(OpConstant, vec![5]),
                make(OpMul, vec![]),
                make(OpArray, vec![3]),
                make(OpPop, vec![]),
            ]
        }
    ];

    run_compiler_test(test_cases)
}

#[test]
fn test_hash_literals() {
    let test_cases = vec![
        CompilerTestCase {
            input: String::from("{}"),
            expected_constants: vec![],
            expected_instructions: vec![
                make(OpHash, vec![0]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("{1: 2, 3: 4, 5: 6}"),
            expected_constants:vec![Value::Integer(1), Value::Integer(2), Value::Integer(3),
                                    Value::Integer(4), Value::Integer(5), Value::Integer(6)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpConstant, vec![1]),
                make(OpConstant, vec![2]),
                make(OpConstant, vec![3]),
                make(OpConstant, vec![4]),
                make(OpConstant, vec![5]),
                make(OpHash, vec![6]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("{1: 2  + 3, 4: 5 * 6}"),
            expected_constants:vec![Value::Integer(1), Value::Integer(2), Value::Integer(3),
                                    Value::Integer(4), Value::Integer(5), Value::Integer(6)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpConstant, vec![1]),
                make(OpConstant, vec![2]),
                make(OpAdd, vec![]),
                make(OpConstant, vec![3]),
                make(OpConstant, vec![4]),
                make(OpConstant, vec![5]),
                make(OpMul, vec![]),
                make(OpHash, vec![4]),
                make(OpPop, vec![]),
            ]
        }
    ];

    run_compiler_test(test_cases);
}

#[test]
fn test_index_expressions() {
    let test_cases = vec![
        CompilerTestCase {
            input: String::from("[1, 2, 3][1 + 1]"),
            expected_constants:vec![Value::Integer(1), Value::Integer(2), Value::Integer(3),
                                    Value::Integer(1), Value::Integer(1)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpConstant, vec![1]),
                make(OpConstant, vec![2]),
                make(OpArray, vec![3]),
                make(OpConstant, vec![3]),
                make(OpConstant, vec![4]),
                make(OpAdd, vec![]),
                make(OpIndex, vec![]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("{1: 2}[2 - 1]"),
            expected_constants:vec![Value::Integer(1), Value::Integer(2), Value::Integer(2),
                                    Value::Integer(1)],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpConstant, vec![1]),
                make(OpHash, vec![2]),
                make(OpConstant, vec![2]),
                make(OpConstant, vec![3]),
                make(OpSub, vec![]),
                make(OpIndex, vec![]),
                make(OpPop, vec![]),
            ]
        }
    ];

    run_compiler_test(test_cases);
}

#[test]
fn test_functions() {
    let test_cases = vec![
        CompilerTestCase {
            input: String::from("fn() { return 5 + 10 }"),
            expected_constants: vec![
                Value::Integer(5),
                Value::Integer(10),
                Value::Instructions([
                    make(OpConstant, vec![0]),
                    make(OpConstant, vec![1]),
                    make(OpAdd, vec![]),
                    make(OpReturnValue, vec![]),
                ].concat())
            ],
            expected_instructions: vec![
                make(OpConstant, vec![2]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("fn() { 5 + 10 }"),
            expected_constants: vec![
                Value::Integer(5),
                Value::Integer(10),
                Value::Instructions([
                    make(OpConstant, vec![0]),
                    make(OpConstant, vec![1]),
                    make(OpAdd, vec![]),
                    make(OpReturnValue, vec![]),
                ].concat())
            ],
            expected_instructions: vec![
                make(OpConstant, vec![2]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("fn() { 1; 2 }"),
            expected_constants: vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Instructions([
                    make(OpConstant, vec![0]),
                    make(OpPop, vec![]),
                    make(OpConstant, vec![1]),
                    make(OpReturnValue, vec![]),
                ].concat())
            ],
            expected_instructions: vec![
                make(OpConstant, vec![2]),
                make(OpPop, vec![]),
            ]
        },
    ];

    run_compiler_test(test_cases);
}

#[test]
fn test_compiler_scopes() {
    let mut compiler = Compiler::new();
    assert_eq!(compiler.scope_index, 0, "scope_index wrong");

    compiler.emit(OpMul, vec![]);
    compiler.enter_scope();
    assert_eq!(compiler.scope_index, 1, "scope_index wrong");

    compiler.emit(OpSub, vec![]);
    assert_eq!(compiler.scopes[compiler.scope_index].instructions.len(), 1, "instructions length wrong");

    let mut last = compiler.scopes[compiler.scope_index].last_instruction;
    assert_eq!(last.opcode, OpSub, "last instruction wrong");

    compiler.leave_scope();
    assert_eq!(compiler.scope_index, 0, "scope_index wrong");

    compiler.emit(OpAdd, vec![]);
    assert_eq!(compiler.scopes[compiler.scope_index].instructions.len(), 2, "instructions length wrong");


    last = compiler.scopes[compiler.scope_index].last_instruction;
    assert_eq!(last.opcode, OpAdd, "last instruction wrong");

    let previous = compiler.scopes[compiler.scope_index].previous_instruction;
    assert_eq!(previous.opcode, OpMul, "previous instruction wrong");
}

#[test]
fn test_functions_without_return_value() {
    let test_cases = vec![
        CompilerTestCase {
            input: String::from("fn() { }"),
            expected_constants: vec![
                Value::Instructions([
                    make(OpReturn, vec![]),
                ].concat())
            ],
            expected_instructions: vec![
                make(OpConstant, vec![0]),
                make(OpPop, vec![]),
            ]
        }
    ];

    run_compiler_test(test_cases);
}

#[test]
fn test_function_calls() {
    let test_cases = vec![
        CompilerTestCase {
            input: String::from("fn() { 24 }();"),
            expected_constants: vec![
                Value::Integer(24),
                Value::Instructions([
                    make(OpConstant, vec![0]),
                    make(OpReturnValue, vec![]),
                ].concat())
            ],
            expected_instructions: vec![
                make(OpConstant, vec![1]), // The compiled function
                make(OpCall, vec![]),
                make(OpPop, vec![]),
            ]
        },
        CompilerTestCase {
            input: String::from("let noArg = fn() { 24 };\n\
            noArg()"),
            expected_constants: vec![
                Value::Integer(24),
                Value::Instructions([
                    make(OpConstant, vec![0]),
                    make(OpReturnValue, vec![]),
                ].concat())
            ],
            expected_instructions: vec![
                make(OpConstant, vec![1]), // The compiled function
                make(OpSetGlobal, vec![0]),
                make(OpGetGlobal, vec![0]),
                make(OpCall, vec![]),
                make(OpPop, vec![]),
            ]
        },
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
        assert_constants(bytecode.constants, test.expected_constants, test.input.as_str());
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

fn assert_constants(actual: Vec<Object>, expected: Vec<Value>, test_case: &str) {
    assert_eq!(actual.len(), expected.len());

    let mut i = 0;
    for constant in expected {
        match constant {
            Value::Integer(val) => assert_integer_object(actual[i].clone(), val),
            Value::String(val) => assert_string_object(actual[i].clone(), val.as_str()),
            Value::Instructions(val) => {
                if let Object::CompiledFunction(compiled_function) = actual[i].clone() {
                    assert_instructions(compiled_function, vec![val], test_case)
                } else {
                    panic!("constant {} - not a function: {:?}", i, actual[i]);
                }
            }
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

fn assert_string_object(actual: Object, expected: &str) {
    if let Object::StringObject(value) = actual {
        assert_eq!(value, expected);
    } else {
        panic!("Object not StringObject. Got={:?}", actual);
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
