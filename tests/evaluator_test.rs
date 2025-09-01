#![allow(dead_code)]

use monkey::environment::Environment;
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
        assert_integer_object(evaluated, expected, test_input)
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
        assert_boolean_object(evaluated, expected, test_input)
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
        assert_boolean_object(evaluated, expected, test_input)
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
            Some(expected_int) => { assert_integer_object(evaluated, expected_int, test_input) }
            None => { assert_null_object(evaluated) }
        }
    }
}

#[test]
fn test_return_statements() {
    let test_cases = vec![
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        ("if (10 > 1) {
              if (10 > 1) {
              return 10;
         }
         return 1;", 10)
    ];

    for test_case in test_cases {
        let test_input = test_case.0;
        let expected = test_case.1 as i64;
        let evaluated = eval_input(test_input);
        assert_integer_object(evaluated, expected, test_input)
    }
}

#[test]
fn test_error_handling() {
    let test_cases = vec![
        ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
        ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
        ("-true", "unknown operator: -BOOLEAN"),
        ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
        ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
        ("if (10 > 1) { true + false; }", "unknown operator: BOOLEAN + BOOLEAN"),
        ("if (10 > 1) {
                          if (10 > 1) {
                            return true + false;
                          }

                          return 1;
                        }",
         "unknown operator: BOOLEAN + BOOLEAN"),
        ("foobar", "identifier not found: foobar"),
        (r#""hello" - "world""#, "unknown operator: STRING - STRING"),
    ];

    for test_case in test_cases {
        let test_input = test_case.0;
        let expected = test_case.1;
        let evaluated = eval_input(test_input);
        assert_error_object(evaluated, expected, test_input);
    }
}

#[test]
fn test_let_statements() {
    let test_cases = vec![
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15)
    ];

    for test_case in test_cases {
        let test_input = test_case.0;
        let expected = test_case.1;
        let evaluated = eval_input(test_input);
        assert_integer_object(evaluated, expected, test_input);
    }
}

#[test]
fn test_function_object() {
    let test_input = "fn(x) { x + 2; };";

    let object = eval_input(test_input);

    if let Object::Function(parameters, body, _) = object {
        assert_eq!(parameters.len(), 1);
        assert_eq!(parameters.get(0).unwrap().value, "x");
        assert_eq!(body.to_string(), "(x + 2)");
    } else {
        panic!("Object not Function. Got={:?}", object);
    }
}

#[test]
fn test_function_application() {
    let test_cases = vec![
        ("let identity = fn(x) { x; }; identity(5);", 5),
        ("let identity = fn(x) { return x; }; identity(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        ("fn(x) { x; }(5)", 5)
    ];

    for test_case in test_cases {
        let test_input = test_case.0;
        let expected = test_case.1;
        let evaluated = eval_input(test_input);
        assert_integer_object(evaluated, expected, test_input);
    }
}

#[test]
fn test_closures() {
    let test_input =
        "let newAdder = fn(x) {
            fn(y) { x + y };
        };

        let addTwo = newAdder(2);
        addTwo(2);";

    let evaluated = eval_input(test_input);
    assert_integer_object(evaluated, 4, test_input);
}

#[test]
fn test_string_literal() {
    let test_input = r#""Hello World!""#;

    let evaluated = eval_input(test_input);

    if let Object::StringObject(value) = evaluated {
        assert_eq!(value, "Hello World!");
    } else {
        panic!("Object not StringObject. Got={:?}", evaluated);
    }
}

#[test]
fn test_string_concatenation() {
    let test_input = r#""Hello" + " " + "World!""#;

    let evaluated = eval_input(test_input);

    if let Object::StringObject(value) = evaluated {
        assert_eq!(value, "Hello World!");
    } else {
        panic!("Object not StringObject. Got={:?}", evaluated);
    }
}

fn assert_error_object(object: Object, expected_message: &str, test_input: &str) {
    if let Object::Error(value) = object {
        assert_eq!(value, expected_message, "Test input: {}", test_input);
    } else {
        panic!("Object not Error. Got={:?}", object);
    }
}

fn assert_null_object(object: Object) {
    assert!(matches!(object, Object::Null), "Expected Null object, x Got: {:?}", object);
}

fn assert_boolean_object(object: Object, expected: bool, test_input: &str) {
    if let Object::Boolean(value) = object {
        assert_eq!(value, expected, "Test input: {}", test_input);
    } else {
        panic!("Object not Boolean. Got={:?}", object);
    }
}

fn assert_integer_object(object: Object, expected: i64, test_input: &str) {
    if let Object::Integer(value) = object {
        assert_eq!(value, expected, "Test input: {}", test_input);
    } else {
        panic!("Object not Integer. Got={:?}", object);
    }
}

fn eval_input(input: &str) -> Object {
    let mut lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();
    let mut env = Environment::new();
    eval_program(&program, &mut env)
}
