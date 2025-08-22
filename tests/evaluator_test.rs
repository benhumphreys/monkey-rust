#![allow(dead_code)]

use monkey::evaluator::eval_program;
use monkey::lexer::Lexer;
use monkey::object::Object;
use monkey::parser::Parser;

#[test]
fn test_eval_integer_expression() {
    let test_cases = vec![("5", 5i64), ("10", 10i64)];

    for test_case in test_cases {
        let test_input = test_case.0;
        let expected_integer = test_case.1;
        let evaluated = eval_input(test_input);
        assert_integer_object(evaluated, expected_integer)
    }
}

fn assert_integer_object(object: Object, expected: i64) {
    if let Object::Integer(value) = object {
        assert_eq!(value, expected);
    } else {
        panic!("Object not Integer. Got={:?}", object);
    }
}

fn eval_input(input: &str) -> Object {
    let mut lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();
    eval_program(&program)
}
