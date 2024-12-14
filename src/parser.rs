use crate::ast;
use crate::ast::{Expression, Program};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use ast::Statement;

type ParseError = String;

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(l: &mut Lexer) -> Parser {
        let mut p = Parser {
            lexer: l.clone(),
            cur_token: Token::new(TokenType::EOF, ""),
            peek_token: Token::new(TokenType::EOF, ""),
            errors: Vec::new(),
        };

        // Populate cur_token and peek_token
        p.next_token();
        p.next_token();
        p
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.cur_token.token_type != TokenType::EOF {
            let stmt = self.parse_statement();
            match stmt {
                Some(x) => program.statements.push(x),
                None => (),
            }
            self.next_token()
        }
        program
    }

    pub fn errors(&mut self) -> Vec<ParseError> {
        self.errors.clone()
    }

    pub fn add_peak_error(&mut self, token_type: &TokenType) {
        let msg = format!(
            "expected next token to be {}, got {} instead",
            token_type, self.peek_token
        );
        self.errors.push(msg);
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.token_type {
            TokenType::Let => Some(self.parse_let_statement()?),
            _ => None, // TODO
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();

        if !self.expect_peek(&TokenType::Ident) {
            return None;
        }

        let name = ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        if !self.expect_peek(&TokenType::Assign) {
            return None;
        }

        // TODO: We're skipping expressions until we encounter a semicolon
        while !self.cur_token_is(&TokenType::SemiColon) {
            self.next_token();
        }

        Some(Statement::LetStatement(token, name, Expression::Dummy))
    }

    fn cur_token_is(&self, token_type: &TokenType) -> bool {
        self.cur_token.token_type == *token_type
    }

    fn peek_token_is(&self, token_type: &TokenType) -> bool {
        self.peek_token.token_type == *token_type
    }

    fn expect_peek(&mut self, token_type: &TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            true
        } else {
            self.add_peak_error(token_type);
            false
        }
    }
}
