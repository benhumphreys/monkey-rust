#![allow(dead_code)]

use monkey::ast::{Node, Statement};
use monkey::lexer::Lexer;
use monkey::parser::Parser;

#[test]
fn test_let_statements() {
    let code = "let x = 5;\n\
            let y = 10;\n\
            let foobar = 838383;\n";

    let expected_identifiers = vec!["x", "y", "foobar"];

    let mut lexer = Lexer::new(code.to_string());
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();

    check_parser_errors(&mut parser);

    for i in 1..expected_identifiers.len() {
        verify_let_statement(&program.statements[i], expected_identifiers[i].to_string())
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

fn verify_let_statement(actual: &Statement, name: String) {
    if let Statement::LetStatement(_token, identifier, _expression) = actual {
        assert_eq!(actual.token_literal(), "let");
        assert_eq!(identifier.value, name);
    } else {
        panic!("Expected LetStatement")
    }
}
