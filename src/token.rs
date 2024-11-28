#![allow(dead_code)]

pub type TokenType = String;

#[derive(Eq, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: &str, literal: &str) -> Token {
        Token {
            token_type: TokenType::from(token_type),
            literal: String::from(literal),
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.token_type == other.token_type && self.literal == other.literal
    }
}

//
// Token types
//
pub const ILLEGAL: &'static str = "ILLEGAL";
pub const END_OF_FILE: &'static str = "END_OF_FILE";

// Identifiers and literals
pub const IDENT: &'static str = "IDENT"; // add, foobar, x, y, ..
pub const INT: &'static str = "INT"; // 1, 2, 3, ..

// Operators
pub const ASSIGN: &'static str = "=";
pub const PLUS: &'static str = "+";
pub const MINUS: &'static str = "-";
pub const BANG: &'static str = "!";
pub const ASTERISK: &'static str = "*";
pub const SLASH: &'static str = "/";

pub const LT: &'static str = "<";
pub const GT: &'static str = ">";

pub const EQ: &'static str = "==";
pub const NOT_EQ: &'static str = "!=";

// Delimiters
pub const COMMA: &'static str = ",";
pub const SEMICOLON: &'static str = ";";

pub const LPAREN: &'static str = "(";
pub const RPAREN: &'static str = "(";
pub const LBRACE: &'static str = "{";
pub const RBRACE: &'static str = "}";

// Keywords
pub const FUNCTION: &'static str = "FUNCTION";
pub const LET: &'static str = "LET";
pub const TRUE: &'static str = "TRUE";
pub const FALSE: &'static str = "FALSE";
pub const IF: &'static str = "IF";
pub const ELSE: &'static str = "ELSE";
pub const RETURN: &'static str = "RETURN";
