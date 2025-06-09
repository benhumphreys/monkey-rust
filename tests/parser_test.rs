#![allow(dead_code)]

use std::ops::Deref;
use monkey::ast;
use monkey::ast::{Expression, Node, Program, Statement};
use monkey::ast::Statement::ExpressionStatement;
use monkey::lexer::Lexer;
use monkey::parser::Parser;

#[test]
fn test_let_statements() {
    let code = "let x = 5;\n\
            let y = 10;\n\
            let foobar = 838383;\n";

    let expected_identifiers = vec!["x", "y", "foobar"];
    let program = parse_program(code);

    for i in 1..expected_identifiers.len() {
        assert_let_statement(&program.statements[i], expected_identifiers[i])
    }
}

#[test]
fn test_return_statements() {
    let code = "return 5;\n\
            return 10;\n\
            return 993322;\n";

    let program = parse_program(code);

    assert_eq!(program.statements.len(), 3);

    for stmt in program.statements {
        if let Statement::ReturnStatement(token, _) = stmt {
            assert_eq!(token.literal, "return")
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
            ("!false", "!", Value::Boolean(false))
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
        ("true == true", Value::Boolean(true), "==", Value::Boolean(true)),
        ("true != false", Value::Boolean(true), "!=", Value::Boolean(false)),
        ("false == false", Value::Boolean(false), "==", Value::Boolean(false)),
    ];

    for test_case in test_cases {
        let test_code = test_case.0;
        let expected_left_value = test_case.1;
        let expected_operator = test_case.2;
        let expected_right_value = test_case.3;

        let expression = parse_single_expression_program(test_code);
        assert_infix_expression(&expression,
        expected_left_value,
        expected_operator,
        expected_right_value);
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
        ("!(true == true)", "(!(true == true))")
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
        assert_infix_expression(condition.deref(), Value::String("x".to_string()), "<", Value::String("y".to_string()));
       
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
        assert_infix_expression(condition.deref(),
                                Value::String("x".to_string()),
                                "<",
                                Value::String("y".to_string()));

        // Verify consequence
        assert_eq!(consequence.statements.len(), 1);
        let consequence_stmt = &consequence.statements[0];
        if let ExpressionStatement(_, consequence_expr) = consequence_stmt {
            assert_identifier(&consequence_expr, "x");
        } else {
            panic!("Expression not ExpressionStatement. Got={:?}", consequence_stmt);
        }

        // Verify alternative
        let alternative = maybe_alternative.expect("Alternative is empty");
        assert_eq!(alternative.statements.len(), 1);
        let alternative_stmt = &alternative.statements[0];
        if let ExpressionStatement(_, alternative_expr) = alternative_stmt {
            assert_identifier(&alternative_expr, "y");
        } else {
            panic!("Expression not ExpressionStatement. Got={:?}", alternative_stmt);
        }
    } else {
        panic!("Expression not IfExpression. Got={:?}", expression);
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

fn parse_single_expression_program(code: &str) -> Expression {
    let program = parse_program(code);
    assert_eq!(program.statements.len(), 1);
    let stmt = program.statements[0].clone();
    if let Statement::ExpressionStatement(_, expression) = stmt {
        return expression;
    } else {
        panic!("Statement not ExpressionStatement. Got={:?}", stmt);
    }
}