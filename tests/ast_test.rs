#![allow(dead_code)]

use monkey::ast::{Expression, Identifier, Program, Statement};
use monkey::token::{Token, TokenType};

#[test]
fn test_display() {
    let statement = Statement::LetStatement(
        Token::new(TokenType::Let, "let"),
        Identifier {
            token: Token::new(TokenType::Ident, "myVar"),
            value: String::from("myVar"),
        },
        Expression::Identifier(
            Token::new(TokenType::Ident, "anotherVar"),
            String::from("anotherVar"),
        ),
    );

    let mut program = Program::new();
    program.statements.push(statement);

    assert_eq!("let myVar = anotherVar;", program.to_string())
}
