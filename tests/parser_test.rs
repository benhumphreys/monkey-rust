#![allow(dead_code)]

use std::collections::HashMap;
use monkey::ast;
use monkey::ast::Statement::ExpressionStatement;
use monkey::ast::{Expression, Node, Program, Statement};
use monkey::lexer::Lexer;
use monkey::parser::Parser;
use std::ops::Deref;
use monkey::token::Token;
use monkey::token::TokenType::StringLiteral;

#[test]
fn test_let_statements() {
    let test_cases = vec![
        ("let x = 5;", "x", Value::Integer(5)),
        ("let y = true;", "y", Value::Boolean(true)),
        ("let foobar = y;", "foobar", Value::String("y".to_string())),
    ];

    for test_case in test_cases {
        let test_input = test_case.0;
        let expected_identifier = test_case.1;
        let expected_value = test_case.2;

        // Verify identifier
        let stmt = parse_single_statement_program(test_input);
        assert_let_statement(&stmt, expected_identifier);

        // Verify value/expression
        if let Statement::LetStatement(_, _, expression) = stmt {
            assert_literal_expression(&expression, &expected_value)
        }
    }
}

#[test]
fn test_let_statement_with_errors() {
    let code = "let x 5;";
    let mut lexer = Lexer::new(String::from(code));
    let mut parser = Parser::new(&mut lexer);
    parser.parse_program();
    assert_eq!(false, parser.errors().is_empty())
}

#[test]
fn test_return_statements() {
    let test_cases = vec![
        ("return 5;", Value::Integer(5)),
        ("return true;", Value::Boolean(true)),
        ("return foobar;", Value::String("foobar".to_string())),
    ];

    for test_case in test_cases {
        let test_input = test_case.0;
        let expected_value = test_case.1;

        let stmt = parse_single_statement_program(test_input);

        if let Statement::ReturnStatement(token, expression) = stmt {
            assert_eq!(token.literal, "return");
            assert_literal_expression(&expression, &expected_value);
        } else {
            panic!("Statement not ReturnStatement. Got={:?}", stmt)
        }
    }
}

#[test]
fn test_identifier_expression() {
    let code = "foobar;";
    let expression = parse_single_expression_program(code);

    assert_identifier(&expression, "foobar");
}

#[test]
fn test_integer_literal_expression() {
    let code = "5;";
    let expression = parse_single_expression_program(code);

    assert_integer_literal(&expression, 5);
}

#[test]
fn test_parsing_prefix_expressions() {
    let test_cases = vec![
        ("!5", "!", Value::Integer(5)),
        ("-15", "-", Value::Integer(15)),
        ("!true", "!", Value::Boolean(true)),
        ("!false", "!", Value::Boolean(false)),
    ];

    for test_case in test_cases {
        let test_code = test_case.0;
        let expected_operator = test_case.1;
        let expected_value = test_case.2;

        let expression = parse_single_expression_program(test_code);
        if let Expression::PrefixExpression(_, operator, right) = expression {
            assert_eq!(operator, expected_operator);
            assert_literal_expression(&right, &expected_value);
        } else {
            panic!("Expression not PrefixExpression. Got={:?}", expression);
        }
    }
}

#[test]
fn test_parsing_infix_expressions() {
    let test_cases = vec![
        ("5 + 5;", Value::Integer(5), "+", Value::Integer(5)),
        ("5 - 5;", Value::Integer(5), "-", Value::Integer(5)),
        ("5 * 5;", Value::Integer(5), "*", Value::Integer(5)),
        ("5 / 5;", Value::Integer(5), "/", Value::Integer(5)),
        ("5 > 5;", Value::Integer(5), ">", Value::Integer(5)),
        ("5 < 5;", Value::Integer(5), "<", Value::Integer(5)),
        ("5 == 5;", Value::Integer(5), "==", Value::Integer(5)),
        ("5 != 5;", Value::Integer(5), "!=", Value::Integer(5)),
        (
            "true == true",
            Value::Boolean(true),
            "==",
            Value::Boolean(true),
        ),
        (
            "true != false",
            Value::Boolean(true),
            "!=",
            Value::Boolean(false),
        ),
        (
            "false == false",
            Value::Boolean(false),
            "==",
            Value::Boolean(false),
        ),
    ];

    for test_case in test_cases {
        let test_code = test_case.0;
        let expected_left_value = test_case.1;
        let expected_operator = test_case.2;
        let expected_right_value = test_case.3;

        let expression = parse_single_expression_program(test_code);
        assert_infix_expression(
            &expression,
            expected_left_value,
            expected_operator,
            expected_right_value,
        );
    }
}

#[test]
fn test_operator_precedence_parsing() {
    let test_cases = vec![
        ("-a * b;", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
        ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
        ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"),
        ("a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)"),
        ("add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))")
    ];

    for test_case in test_cases {
        let test_input = test_case.0;
        let expected = test_case.1;

        let program = parse_program(test_input);
        assert_eq!(program.to_string(), expected);
    }
}

#[test]
fn test_boolean_expression() {
    let test_cases = vec![("true;", true), ("false;", false)];

    for test_case in test_cases {
        let test_code = test_case.0;
        let expected_value = test_case.1;

        let expression = parse_single_expression_program(test_code);
        assert_boolean_literal(&expression, expected_value);
    }
}

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";
    let expression = parse_single_expression_program(input);

    if let Expression::IfExpression(_, condition, consequence, alternative) = expression {
        // Verify condition
        assert_infix_expression(
            condition.deref(),
            Value::String("x".to_string()),
            "<",
            Value::String("y".to_string()),
        );

        // Verify consequence
        assert_eq!(consequence.statements.len(), 1);
        let consequence_stmt = &consequence.statements[0];
        if let ExpressionStatement(_, consequence_expr) = consequence_stmt {
            assert_identifier(&consequence_expr, "x");
        } else {
            panic!("Expression not IfExpression. Got={:?}", consequence_stmt);
        }

        // Verify alternative is not present
        if alternative.is_some() {
            panic!("Alternative not empty. Got={:?}", alternative.unwrap())
        }
    } else {
        panic!("Expression not IfExpression. Got={:?}", expression);
    }
}

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y }";
    let expression = parse_single_expression_program(input);

    if let Expression::IfExpression(_, condition, consequence, maybe_alternative) = expression {
        // Verify condition
        assert_infix_expression(
            condition.deref(),
            Value::String("x".to_string()),
            "<",
            Value::String("y".to_string()),
        );

        // Verify consequence
        assert_eq!(consequence.statements.len(), 1);
        let consequence_stmt = &consequence.statements[0];
        if let ExpressionStatement(_, consequence_expr) = consequence_stmt {
            assert_identifier(&consequence_expr, "x");
        } else {
            panic!(
                "Expression not ExpressionStatement. Got={:?}",
                consequence_stmt
            );
        }

        // Verify alternative
        let alternative = maybe_alternative.expect("Alternative is empty");
        assert_eq!(alternative.statements.len(), 1);
        let alternative_stmt = &alternative.statements[0];
        if let ExpressionStatement(_, alternative_expr) = alternative_stmt {
            assert_identifier(&alternative_expr, "y");
        } else {
            panic!(
                "Expression not ExpressionStatement. Got={:?}",
                alternative_stmt
            );
        }
    } else {
        panic!("Expression not IfExpression. Got={:?}", expression);
    }
}

#[test]
fn test_function_literal_parsing() {
    let input = "fn(x, y) { x + y; }";
    let expression = parse_single_expression_program(input);

    if let Expression::FunctionLiteral(_, parameters, body) = expression {
        // Verify parameters
        assert_eq!(parameters.len(), 2);
        assert_literal_expression(
            &Expression::Identifier(parameters[0].token.clone(), parameters[0].value.clone()),
            &Value::String("x".to_string()),
        );
        assert_literal_expression(
            &Expression::Identifier(parameters[1].token.clone(), parameters[1].value.clone()),
            &Value::String("y".to_string()),
        );

        // Verify body
        assert_eq!(body.statements.len(), 1);
        let stmt = body.statements[0].clone();
        if let Statement::ExpressionStatement(_, expression) = stmt {
            assert_infix_expression(
                &expression,
                Value::String("x".to_string()),
                "+",
                Value::String("y".to_string()),
            );
        }
    } else {
        panic!("Expression not FunctionLiteral. Got={:?}", expression);
    }
}

#[test]
fn test_function_parameter_parsing() {
    let test_cases = vec![
        ("fn() {}", vec![]),
        ("fn(x) {}", vec!["x"]),
        ("fn(x, y, z) {}", vec!["x", "y", "z"]),
    ];

    for test_case in test_cases {
        let test_input = test_case.0;
        let expected_parameters = test_case.1;

        let expression = parse_single_expression_program(test_input);
        if let Expression::FunctionLiteral(_, parameters, _) = expression {
            assert_eq!(parameters.len(), expected_parameters.len());
            for i in 1..expected_parameters.len() {
                assert_eq!(parameters[i].value, expected_parameters[i]);
            }
        }
    }
}

#[test]
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";
    let expression = parse_single_expression_program(input);

    if let Expression::CallExpression(_, function, arguments) = expression {
        assert_identifier(function.deref(), "add");
        assert_eq!(arguments.len(), 3);
        assert_literal_expression(&arguments[0], &Value::Integer(1));
        assert_infix_expression(&arguments[1], Value::Integer(2), "*", Value::Integer(3));
        assert_infix_expression(&arguments[2], Value::Integer(4), "+", Value::Integer(5));
    }
}

#[test]
fn test_string_literal_expression() {
    let input = "\"hello world\"";
    let expression = parse_single_expression_program(input);

    if let Expression::StringLiteral(_, value) = expression {
        assert_eq!(value, "hello world");
    } else {
        panic!("Expression not StringLiteral. Got={:?}", expression);
    }
}

#[test]
fn test_parsing_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";
    let expression = parse_single_expression_program(input);

    if let Expression::ArrayLiteral(_, elements) = expression {
        assert_eq!(elements.len(), 3);
        assert_integer_literal(&elements[0], 1);
        assert_infix_expression(&elements[1], Value::Integer(2), "*", Value::Integer(2));
        assert_infix_expression(&elements[2], Value::Integer(3), "+", Value::Integer(3));
    } else {
        panic!("Expression not ArrayLiteral. Got={:?}", expression);
    }
}

#[test]
fn test_parsing_index_expressions() {
    let input = "myArray[1 + 1]";
    let expression = parse_single_expression_program(input);

    if let Expression::IndexExpression(_, left, index) = expression {
        assert_identifier(left.deref(), "myArray");
        assert_infix_expression(index.deref(), Value::Integer(1), "+", Value::Integer(1));
    } else {
        panic!("Expression not IndexExpression. Got={:?}", expression);
    }
}

#[test]
fn test_parsing_empty_hash_literal() {
    let input = "{}";
    let expression = parse_single_expression_program(input);

    if let Expression::HashLiteral(_, pairs) = expression {
        assert_eq!(pairs.len(), 0);
    } else {
        panic!("Expression not HashLiteral. Got={:?}", expression);
    }
}

#[test]
fn test_parsing_hash_literals_string_key() {
    let input = "{\"one\": 1, \"two\": 2, \"three\": 3}";
    let expected = HashMap::from([
        ("one", 1),
        ("two", 2),
        ("three", 3)]);

    let expression = parse_single_expression_program(input);

    if let Expression::HashLiteral(_, pairs) = expression {
        assert_eq!(pairs.len(), expected.len());

        for (key, value) in pairs {
            if let Expression::StringLiteral(_, string_key) = key {
                let expected_value = expected[string_key.as_str()];
                assert_integer_literal(&value, expected_value);
            } else {
                panic!("Key is not StringLiteral. Got={:?}", key);
            }
        }
    } else {
        panic!("Expression not HashLiteral. Got={:?}", expression);
    }
}

#[test]
fn test_parsing_hash_literals_boolean_keys() {
    let input = "{true: 1, false: 2}";
    let expected = HashMap::from([(true, 1), (false, 2)]);

    let expression = parse_single_expression_program(input);

    if let Expression::HashLiteral(_, pairs) = expression {
        assert_eq!(pairs.len(), expected.len());

        for (key, value) in pairs {
            if let Expression::Boolean(_, bool_key) = key {
                let expected_value = expected[&bool_key];
                assert_integer_literal(&value, expected_value);
            } else {
                panic!("Key is not Boolean. Got={:?}", key);
            }
        }
    } else {
        panic!("Expression not HashLiteral. Got={:?}", expression);
    }
}

#[test]
fn test_parsing_hash_literals_integer_keys() {
    let input = "{1: 1, 2: 2, 3: 3}";
    let expected = HashMap::from([(1, 1), (2, 2), (3, 3)]);

    let expression = parse_single_expression_program(input);

    if let Expression::HashLiteral(_, pairs) = expression {
        assert_eq!(pairs.len(), expected.len());

        for (key, value) in pairs {
            if let Expression::IntegerLiteral(_, int_key) = key {
                let expected_value = expected[&int_key];
                assert_integer_literal(&value, expected_value);
            } else {
                panic!("Key is not IntegerLiteral. Got={:?}", key);
            }
        }
    } else {
        panic!("Expression not HashLiteral. Got={:?}", expression);
    }
}

#[test]
fn test_parsing_hash_literals_with_expressions() {
    let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;

    let expression = parse_single_expression_program(input);

    if let Expression::HashLiteral(_, pairs) = expression {
        assert_eq!(pairs.len(), 3);

        let (actual_key_0, actual_value_0) = pairs.get(0).unwrap();
        if let Expression::StringLiteral(_, string_key) = actual_key_0 {
            assert_eq!(string_key, "one");
            assert_infix_expression(actual_value_0, Value::Integer(0), "+", Value::Integer(1));
        } else {
            panic!("Key is not StringLiteral. Got={:?}", actual_key_0);
        }

        let (actual_key_1, actual_value_1) = pairs.get(1).unwrap();
        if let Expression::StringLiteral(_, string_key) = actual_key_1 {
            assert_eq!(string_key, "two");
            assert_infix_expression(actual_value_1, Value::Integer(10), "-", Value::Integer(8));
        } else {
            panic!("Key is not StringLiteral. Got={:?}", actual_key_1);
        }

        let (actual_key_2, actual_value_2) = pairs.get(2).unwrap();
        if let Expression::StringLiteral(_, string_key) = actual_key_2 {
            assert_eq!(string_key, "three");
            assert_infix_expression(actual_value_2, Value::Integer(15), "/", Value::Integer(5));
        } else {
            panic!("Key is not StringLiteral. Got={:?}", actual_key_2);
        }
    } else {
        panic!("Expression not HashLiteral. Got={:?}", expression);
    }

}

fn assert_infix_expression(
    exp: &ast::Expression,
    expected_left: Value,
    expected_operator: &str,
    expected_right: Value,
) {
    if let Expression::InfixExpression(_token, actual_left, actual_op, actual_right) = exp {
        assert_literal_expression(&actual_left, &expected_left);
        assert_eq!(actual_op, expected_operator);
        assert_literal_expression(&actual_right, &expected_right);
    }
}

fn assert_integer_literal(exp: &ast::Expression, expected_value: i64) {
    if let Expression::IntegerLiteral(token, value) = exp {
        assert_eq!(*value, expected_value);
        assert_eq!(token.literal, expected_value.to_string())
    } else {
        panic!("Expression not IntegerLiteral. Got={:?}", exp);
    }
}

fn assert_boolean_literal(exp: &ast::Expression, expected_value: bool) {
    if let Expression::Boolean(token, value) = exp {
        assert_eq!(*value, expected_value);
        assert_eq!(token.literal, expected_value.to_string())
    } else {
        panic!("Expression not Boolean. Got={:?}", exp);
    }
}

fn assert_let_statement(stmt: &Statement, expected: &str) {
    if let Statement::LetStatement(_token, identifier, _expression) = stmt {
        assert_eq!(stmt.token_literal(), "let");
        assert_eq!(identifier.value, expected);
    } else {
        panic!("Statement not LetStatement. Got={:?}", stmt)
    }
}

fn assert_identifier(exp: &ast::Expression, expected: &str) {
    if let Expression::Identifier(token, value) = exp {
        assert_eq!(value, expected);
        assert_eq!(token.literal, expected)
    } else {
        panic!("Expression not Identifier. Got={:?}", exp);
    }
}

#[derive(Debug)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
}

fn assert_literal_expression(expression: &Expression, expected: &Value) {
    match expected {
        Value::Integer(val) => assert_integer_literal(expression, *val),
        Value::Boolean(val) => assert_boolean_literal(expression, *val),
        Value::String(val) => assert_identifier(expression, val),
    }
}

fn check_parser_errors(parser: &mut Parser) {
    let errors = parser.errors();
    if errors.is_empty() {
        return;
    }

    println!("parser has {} errors", errors.len());
    for error in errors {
        println!("parser error: {}", error);
    }
    panic!("Failed due to parser errors")
}

fn parse_program(code: &str) -> Program {
    let mut lexer = Lexer::new(String::from(code));
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();
    check_parser_errors(&mut parser);
    program
}

fn parse_single_statement_program(code: &str) -> Statement {
    let program = parse_program(code);
    assert_eq!(program.statements.len(), 1);
    program.statements[0].clone()
}

fn parse_single_expression_program(code: &str) -> Expression {
    let stmt = parse_single_statement_program(code);
    if let Statement::ExpressionStatement(_, expression) = stmt {
        return expression;
    } else {
        panic!("Statement not ExpressionStatement. Got={:?}", stmt);
    }
}

fn build_string_literal(value: &str) -> Expression {
    Expression::StringLiteral(Token::new(StringLiteral, value), value.to_string())
}
