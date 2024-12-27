use crate::ast;
use crate::ast::{Expression, Program};
use crate::lexer::Lexer;
use crate::token::TokenType::{Ident, Int, SemiColon};
use crate::token::{Token, TokenType};
use ast::Statement;
use std::collections::HashMap;
use std::num::ParseIntError;

type ParseError = String;
type PrefixParseFn = fn(&mut Parser) -> Expression;
type InfixParseFn = fn(&mut Parser, Expression) -> Expression;

#[repr(u8)]
enum Precedence {
    Lowest = 0,
    Equals = 1,      // ==
    LessGreater = 2, // > or <
    Sum = 3,         // +
    Product = 4,     // *
    Prefix = 5,      // -X or !X
    Call = 6,        // myFunction(X)
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<ParseError>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(l: &mut Lexer) -> Parser {
        let mut p = Parser {
            lexer: l.clone(),
            cur_token: Token::new(TokenType::EOF, ""),
            peek_token: Token::new(TokenType::EOF, ""),
            errors: Vec::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        // Register parsing functions
        p.register_prefix(Ident, Parser::parse_identifier);
        p.register_prefix(Int, Parser::parse_integer_literal);

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

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
    }

    fn add_peak_error(&mut self, token_type: &TokenType) {
        let msg = format!(
            "expected next token to be {}, got {} instead",
            token_type, self.peek_token
        );
        self.errors.push(msg);
    }

    fn add_error(&mut self, error: String) {
        self.errors.push(error)
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_identifier(&mut self) -> Expression {
        Expression::Identifier(self.cur_token.clone(), self.cur_token.literal.clone())
    }

    fn parse_integer_literal(&mut self) -> Expression {
        let token = self.cur_token.clone();
        let value = token.literal.parse::<i64>();

        match value {
            Ok(v) => Expression::IntegerLiteral(token, v),
            Err(_) => {
                self.add_error(format!("could not parse '{}' as integer", token.literal));
               Expression::Nil
            }
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.token_type {
            TokenType::Let => Some(self.parse_let_statement()?),
            TokenType::Return => Some(self.parse_return_statement()?),
            _ => Some(self.parse_expression_statement()?),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.token_type);
        if prefix.is_some() {
            Some(prefix.unwrap()(self))
        } else {
            None
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        let expression_statement = self.parse_expression(Precedence::Lowest);
        match expression_statement {
            Some(es) => {
                if self.peek_token_is(&SemiColon) {
                    self.next_token();
                }

                Some(Statement::ExpressionStatement(token, es))
            }
            None => None,
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

        Some(Statement::LetStatement(token, name, Expression::Nil))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        self.next_token();

        // TODO: We're skipping expressions until we encounter a semicolon
        while !self.cur_token_is(&TokenType::SemiColon) {
            self.next_token();
        }

        Some(Statement::ReturnStatement(token, Expression::Nil))
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
