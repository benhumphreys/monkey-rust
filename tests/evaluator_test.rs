#![allow(dead_code)]

use monkey::evaluator::eval_program;
use monkey::lexer::Lexer;
use monkey::object::Object;
use monkey::parser::Parser;

#[test]
fn test_eval_integer_expression() {
    let test_cases = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50)
    ];

    for test_case in test_cases {
        let test_input = test_case.0;
        let expected = test_case.1 as i64;
        let evaluated = eval_input(test_input);
        assert_integer_object(evaluated, expected)
    }
}

#[test]
fn test_eval_boolean_expression() {
    let test_cases = vec![
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true)
    ];

    for test_case in test_cases {
        let test_input = test_case.0;
        let expected = test_case.1;
        let evaluated = eval_input(test_input);
        assert_boolean_object(evaluated, expected)
    }
}

#[test]
fn test_bang_operator() {
    let test_cases = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
    ];

    for test_case in test_cases {
        let test_input = test_case.0;
        let expected = test_case.1;
        let evaluated = eval_input(test_input);
        assert_boolean_object(evaluated, expected)
    }
}

#[test]
fn test_if_else_expressions() {
    let test_cases = vec![
        ("if (true) { 10 }", Some(10)),
        ("if (false) { 10 }", None),
        ("if (1) { 10 }", Some(10)),
        ("if (1 < 2) { 10 }", Some(10)),
        ("if (1 > 2) { 10 }", None),
        ("if (1 > 2) { 10 } else { 20 }", Some(20)),
        ("if (1 < 2) { 10 } else { 20 }", Some(10))
    ];

    for test_case in test_cases {
        let test_input = test_case.0;
        let expected = test_case.1;
        let evaluated = eval_input(test_input);
        match expected {
            Some(expected_int) => { assert_integer_object(evaluated, expected_int) }
            None => { assert_null_object(evaluated) }
        }
    }
}

fn assert_null_object(object: Object) {
    assert!(matches!(object, Object::Null), "Expected Null object, x Got: {:?}", object);
}

fn assert_boolean_object(object: Object, expected: bool) {
    if let Object::Boolean(value) = object {
        assert_eq!(value, expected);
    } else {
        panic!("Object not Boolean. Got={:?}", object);
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
