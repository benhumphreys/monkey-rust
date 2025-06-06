#![allow(dead_code)]

use monkey::ast;
use monkey::ast::{Expression, Node, Program, Statement};
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
    let stmt = parse_single_expression_program(code);
    
    if let Statement::ExpressionStatement(_, expression) = stmt {
        assert_identifier(&expression, "foobar");
    } else {
        panic!("Statement not ExpressionStatement. Got={:?}", stmt);
    }
}

#[test]
fn test_integer_literal_expression() {
    let code = "5;";
    let stmt = parse_single_expression_program(code);
    
    if let Statement::ExpressionStatement(_, expression) = stmt {
        assert_integer_literal(&expression, 5);
    } else {
        panic!("Statement not ExpressionStatement. Got={:?}", stmt);
    }
}

#[test]
fn test_parsing_prefix_expressions() {
    let test_cases = vec![("!5", "!", 5), ("-15", "-", 15)];

    for test_case in test_cases {
        let test_code = test_case.0;
        let expected_operator = test_case.1;
        let expected_int_value = test_case.2;

        let stmt = parse_single_expression_program(test_code);
        if let Statement::ExpressionStatement(_, expression) = stmt {
            if let Expression::PrefixExpression(_, operator, right) = expression {
                assert_eq!(operator, expected_operator);
                assert_literal_expression(&right, &Value::Integer(expected_int_value));
            } else {
                panic!("Expression not PrefixExpression. Got={:?}", expression);
            }
        } else {
            panic!("Statement not ExpressionStatement. Got={:?}", stmt);
        }
    }
}

#[test]
fn test_parsing_infix_expressions() {
    let test_cases = vec![
        ("5 + 5;", 5, "+", 5),
        ("5 - 5;", 5, "-", 5),
        ("5 * 5;", 5, "*", 5),
        ("5 / 5;", 5, "/", 5),
        ("5 > 5;", 5, ">", 5),
        ("5 < 5;", 5, "<", 5),
        ("5 == 5;", 5, "==", 5),
        ("5 != 5;", 5, "!=", 5),
    ];

    for test_case in test_cases {
        let test_code = test_case.0;
        let expected_left_value = Value::Integer(test_case.1);
        let expected_operator = test_case.2;
        let expected_right_value = Value::Integer(test_case.3);

        let stmt = parse_single_expression_program(test_code);
        if let Statement::ExpressionStatement(_, expression) = stmt {
            assert_infix_expression(&expression,
            expected_left_value,
            expected_operator,
            expected_right_value);
        } else {
            panic!("Statement not ExpressionStatement. Got={:?}", stmt);
        }
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
    ];

    for test_case in test_cases {
        let test_input = test_case.0;
        let expected = test_case.1;

        let program = parse_program(test_input);
        assert_eq!(program.to_string(), expected);
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
    String(String),
}

fn assert_literal_expression(expression: &Expression, expected: &Value) {
    match expected {
        Value::Integer(val) => assert_integer_literal(expression, *val),
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

fn parse_single_expression_program(code: &str) -> Statement {
    let program = parse_program(code);
    assert_eq!(program.statements.len(), 1);
    program.statements[0].clone()
}