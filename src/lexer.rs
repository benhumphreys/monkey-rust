#![allow(dead_code)]

use crate::token::{Token, TokenType};

#[derive(Clone)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(code: String) -> Lexer {
        if !code.is_ascii() {
            panic!("Only ASCII characters are supported")
        }

        let mut lexer = Lexer {
            input: code,
            position: 0,
            read_position: 0,
            ch: '\0',
        };

        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::Eq, "==")
                } else {
                    Token::new(TokenType::Assign, self.ch.to_string().as_str())
                }
            }
            '(' => Token::new(TokenType::LeftParen, self.ch.to_string().as_str()),
            ')' => Token::new(TokenType::RightParen, self.ch.to_string().as_str()),
            ',' => Token::new(TokenType::Comma, self.ch.to_string().as_str()),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=")
                } else {
                    Token::new(TokenType::Bang, self.ch.to_string().as_str())
                }
            }
            '+' => Token::new(TokenType::Plus, self.ch.to_string().as_str()),
            '-' => Token::new(TokenType::Minus, self.ch.to_string().as_str()),
            '*' => Token::new(TokenType::Asterisk, self.ch.to_string().as_str()),
            '/' => Token::new(TokenType::Slash, self.ch.to_string().as_str()),
            '<' => Token::new(TokenType::LessThan, self.ch.to_string().as_str()),
            '>' => Token::new(TokenType::GreaterThan, self.ch.to_string().as_str()),
            '{' => Token::new(TokenType::LeftBrace, self.ch.to_string().as_str()),
            '}' => Token::new(TokenType::RightBrace, self.ch.to_string().as_str()),
            '[' => Token::new(TokenType::LeftBracket, self.ch.to_string().as_str()),
            ']' => Token::new(TokenType::RightBracket, self.ch.to_string().as_str()),
            ';' => Token::new(TokenType::SemiColon, self.ch.to_string().as_str()),
            '\"' => Token::new(TokenType::StringLiteral, self.read_string().as_str()),
            '\0' => Token::new(TokenType::EOF, self.ch.to_string().as_str()),
            _ => {
                return if is_letter(self.ch) {
                    let ident = self.read_identifier();
                    Token::new(lookup_ident(ident.as_str()), ident.as_str())
                } else if is_digit(self.ch) {
                    Token::new(TokenType::IntLiteral, self.read_number().as_str())
                } else {
                    Token::new(TokenType::Illegal, self.ch.to_string().as_str())
                }
            }
        };

        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.as_bytes()[self.read_position] as char;
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            0 as char
        } else {
            self.input.as_bytes()[self.read_position] as char
        }
    }

    fn read_number(&mut self) -> String {
        let start: usize = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }
        String::from(&self.input[start..self.position])
    }

    fn read_identifier(&mut self) -> String {
        let start: usize = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        String::from(&self.input[start..self.position])
    }

    fn read_string(&mut self) -> String {
        let start: usize = self.position + 1;
        loop {
            self.read_char();
            if self.ch == '"' || self.ch == '\0' {
                break;
            }
        }

        String::from(&self.input[start..self.position])
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }
}

fn lookup_ident(str: &str) -> TokenType {
    match str {
        "fn" => TokenType::Function,
        "let" => TokenType::Let,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "return" => TokenType::Return,
        _ => TokenType::Ident,
    }
}

fn is_letter(c: char) -> bool {
    'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c == '_'
}

fn is_digit(c: char) -> bool {
    '0' <= c && c <= '9'
}
