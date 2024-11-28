#![allow(dead_code)]

use crate::token;
use crate::token::Token;

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
                    Token::new(token::EQ, "==")
                } else {
                    Token::new(token::ASSIGN, self.ch.to_string().as_str())
                }
            }
            '(' => Token::new(token::LPAREN, self.ch.to_string().as_str()),
            ')' => Token::new(token::RPAREN, self.ch.to_string().as_str()),
            ',' => Token::new(token::COMMA, self.ch.to_string().as_str()),
            '!' => {
               if self.peek_char() == '=' {
                   self.read_char();
                   Token::new(token::NOT_EQ, "!=")
               } else {
                   Token::new(token::BANG, self.ch.to_string().as_str())
               }
            }
            '+' => Token::new(token::PLUS, self.ch.to_string().as_str()),
            '-' => Token::new(token::MINUS, self.ch.to_string().as_str()),
            '*' => Token::new(token::ASTERISK, self.ch.to_string().as_str()),
            '/' => Token::new(token::SLASH, self.ch.to_string().as_str()),
            '<' => Token::new(token::LT, self.ch.to_string().as_str()),
            '>' => Token::new(token::GT, self.ch.to_string().as_str()),
            '{' => Token::new(token::LBRACE, self.ch.to_string().as_str()),
            '}' => Token::new(token::RBRACE, self.ch.to_string().as_str()),
            ';' => Token::new(token::SEMICOLON, self.ch.to_string().as_str()),
            '\0' => Token::new(token::END_OF_FILE, self.ch.to_string().as_str()),
            _ => {
                return if is_letter(self.ch) {
                    let ident = self.read_identifier();
                    Token::new(lookup_ident(ident.as_str()), ident.as_str())
                } else if is_digit(self.ch) {
                    Token::new(token::INT, self.read_number().as_str())
                } else {
                    Token::new(token::ILLEGAL, self.ch.to_string().as_str())
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

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }
}

fn lookup_ident(str: &str) -> &str {
    match str {
        "fn" => token::FUNCTION,
        "let" => token::LET,
        "true" => token::TRUE,
        "false" => token::FALSE,
        "if" => token::IF,
        "else" => token::ELSE,
        "return" => token::RETURN,
        _ => token::IDENT,
    }
}

fn is_letter(c: char) -> bool {
    'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c == '_'
}

fn is_digit(c: char) -> bool {
    '0' <= c && c <= '9'
}
