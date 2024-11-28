#![allow(dead_code)]

use donkey::lexer::Lexer;
use donkey::token;
use donkey::token::Token;

#[test]
fn test_new() {
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
        10 == 10;\
        10 != 9;";

    let expected_tokens = vec![
        (token::LET, "let"),
        (token::IDENT, "five"),
        (token::ASSIGN, "="),
        (token::INT, "5"),
        (token::SEMICOLON, ";"),
        (token::LET, "let"),
        (token::IDENT, "ten"),
        (token::ASSIGN, "="),
        (token::INT, "10"),
        (token::SEMICOLON, ";"),
        (token::LET, "let"),
        (token::IDENT, "add"),
        (token::ASSIGN, "="),
        (token::FUNCTION, "fn"),
        (token::LPAREN, "("),
        (token::IDENT, "x"),
        (token::COMMA, ","),
        (token::IDENT, "y"),
        (token::RPAREN, ")"),
        (token::LBRACE, "{"),
        (token::IDENT, "x"),
        (token::PLUS, "+"),
        (token::IDENT, "y"),
        (token::SEMICOLON, ";"),
        (token::RBRACE, "}"),
        (token::SEMICOLON, ";"),
        (token::LET, "let"),
        (token::IDENT, "result"),
        (token::ASSIGN, "="),
        (token::IDENT, "add"),
        (token::LPAREN, "("),
        (token::IDENT, "five"),
        (token::COMMA, ","),
        (token::IDENT, "ten"),
        (token::RPAREN, ")"),
        (token::SEMICOLON, ";"),
        (token::BANG, "!"),
        (token::MINUS, "-"),
        (token::ASTERISK, "*"),
        (token::INT, "5"),
        (token::SEMICOLON, ";"),
        (token::INT, "5"),
        (token::LT, "<"),
        (token::INT, "10"),
        (token::GT, ">"),
        (token::INT, "5"),
        (token::SEMICOLON, ";"),
        (token::IF, "if"),
        (token::LPAREN, "("),
        (token::INT, "5"),
        (token::LT, "<"),
        (token::INT, "10"),
        (token::RPAREN, ")"),
        (token::LBRACE, "{"),
        (token::RETURN, "return"),
        (token::TRUE, "true"),
        (token::SEMICOLON, ";"),
        (token::RBRACE, "}"),
        (token::ELSE, "else"),
        (token::LBRACE, "{"),
        (token::RETURN, "return"),
        (token::FALSE, "false"),
        (token::SEMICOLON, ";"),
        (token::RBRACE, "}"),
        (token::INT, "10"),
        (token::EQ, "=="),
        (token::INT, "10"),
        (token::SEMICOLON, ";"),
        (token::INT, "10"),
        (token::NOT_EQ, "!="),
        (token::INT, "9"),
        (token::SEMICOLON, ";"),
        (token::END_OF_FILE, "\0"),
    ];

    let mut lexer = Lexer::new(code.to_string());
    for expected in expected_tokens {
        let token = lexer.next_token();
        assert_eq!(token, Token::new(expected.0, expected.1))
    }
}
