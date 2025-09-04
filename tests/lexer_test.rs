#![allow(dead_code)]

use monkey::lexer::Lexer;
use monkey::token::{Token, TokenType};

#[test]
fn test_next_token() {
    let code = "let five = 5;\n\
        let ten = 10;\n\
        \n\
        let add = fn(x, y) {\n\
          x + y;\n\
        };\
        \n\
        let result = add(five, ten);\
        \n\
        !-*5;\n\
        5 < 10 > 5;\n\
        \n\
        if (5 < 10) {\n\
          return true;\n\
        } else {\n\
          return false;\
        }\n\
        \n\
        10 == 10;\n
        10 != 9;\n
        \"foobar\"\n
        \"foo bar\"\n\
        [1, 2];\n";

    let expected_tokens = vec![
        (TokenType::Let, "let"),
        (TokenType::Ident, "five"),
        (TokenType::Assign, "="),
        (TokenType::IntLiteral, "5"),
        (TokenType::SemiColon, ";"),
        (TokenType::Let, "let"),
        (TokenType::Ident, "ten"),
        (TokenType::Assign, "="),
        (TokenType::IntLiteral, "10"),
        (TokenType::SemiColon, ";"),
        (TokenType::Let, "let"),
        (TokenType::Ident, "add"),
        (TokenType::Assign, "="),
        (TokenType::Function, "fn"),
        (TokenType::LeftParen, "("),
        (TokenType::Ident, "x"),
        (TokenType::Comma, ","),
        (TokenType::Ident, "y"),
        (TokenType::RightParen, ")"),
        (TokenType::LeftBrace, "{"),
        (TokenType::Ident, "x"),
        (TokenType::Plus, "+"),
        (TokenType::Ident, "y"),
        (TokenType::SemiColon, ";"),
        (TokenType::RightBrace, "}"),
        (TokenType::SemiColon, ";"),
        (TokenType::Let, "let"),
        (TokenType::Ident, "result"),
        (TokenType::Assign, "="),
        (TokenType::Ident, "add"),
        (TokenType::LeftParen, "("),
        (TokenType::Ident, "five"),
        (TokenType::Comma, ","),
        (TokenType::Ident, "ten"),
        (TokenType::RightParen, ")"),
        (TokenType::SemiColon, ";"),
        (TokenType::Bang, "!"),
        (TokenType::Minus, "-"),
        (TokenType::Asterisk, "*"),
        (TokenType::IntLiteral, "5"),
        (TokenType::SemiColon, ";"),
        (TokenType::IntLiteral, "5"),
        (TokenType::LessThan, "<"),
        (TokenType::IntLiteral, "10"),
        (TokenType::GreaterThan, ">"),
        (TokenType::IntLiteral, "5"),
        (TokenType::SemiColon, ";"),
        (TokenType::If, "if"),
        (TokenType::LeftParen, "("),
        (TokenType::IntLiteral, "5"),
        (TokenType::LessThan, "<"),
        (TokenType::IntLiteral, "10"),
        (TokenType::RightParen, ")"),
        (TokenType::LeftBrace, "{"),
        (TokenType::Return, "return"),
        (TokenType::True, "true"),
        (TokenType::SemiColon, ";"),
        (TokenType::RightBrace, "}"),
        (TokenType::Else, "else"),
        (TokenType::LeftBrace, "{"),
        (TokenType::Return, "return"),
        (TokenType::False, "false"),
        (TokenType::SemiColon, ";"),
        (TokenType::RightBrace, "}"),
        (TokenType::IntLiteral, "10"),
        (TokenType::Eq, "=="),
        (TokenType::IntLiteral, "10"),
        (TokenType::SemiColon, ";"),
        (TokenType::IntLiteral, "10"),
        (TokenType::NotEq, "!="),
        (TokenType::IntLiteral, "9"),
        (TokenType::SemiColon, ";"),
        (TokenType::StringLiteral, "foobar"),
        (TokenType::StringLiteral, "foo bar"),
        (TokenType::LeftBracket, "["),
        (TokenType::IntLiteral, "1"),
        (TokenType::Comma, ","),
        (TokenType::IntLiteral, "2"),
        (TokenType::RightBracket, "]"),
        (TokenType::SemiColon, ";"),
        (TokenType::EOF, "\0"),
    ];

    let mut lexer = Lexer::new(code.to_string());
    for expected in expected_tokens {
        let token = lexer.next_token();
        assert_eq!(token, Token::new(expected.0, expected.1))
    }
}
