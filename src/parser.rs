use crate::ast;
use crate::ast::{Expression, Program};
use crate::lexer::Lexer;
use crate::token::TokenType::{Ident, Int, SemiColon};
use crate::token::{Token, TokenType};
use ast::Statement;
use std::collections::HashMap;

type ParseError = String;
type PrefixParseFn = fn(&mut Parser) -> Result<Expression, ParseError>;
type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression, ParseError>;

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
                Ok(s) => program.statements.push(s),
                Err(e) => self.errors.push(e)
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

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParseError> {
        Ok(Expression::Identifier(self.cur_token.clone(), self.cur_token.literal.clone()))
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, ParseError> {
        let token = self.cur_token.clone();
        let value = token.literal.parse::<i64>();

        match value {
            Ok(v) => Ok(Expression::IntegerLiteral(token, v)),
            Err(_) => Err(format!("could not parse '{}' as integer", token.literal))
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.token_type);
        if prefix.is_some() {
            prefix.unwrap()(self)
        } else {
            Err(format!("no prefix parse function for {} found", self.cur_token.token_type))
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let token = self.cur_token.clone();
        let expression_statement = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&SemiColon) {
            self.next_token();
        }

        Ok(Statement::ExpressionStatement(token, expression_statement))
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        let token = self.cur_token.clone();

        if !self.expect_peek(&TokenType::Ident) {
            return Err("no identifier in let statement".to_string());
        }

        let name = ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        if !self.expect_peek(&TokenType::Assign) {
            return Err("no assignment in let statement".to_string());
        }

        // TODO: We're skipping expressions until we encounter a semicolon
        while !self.cur_token_is(&TokenType::SemiColon) {
            self.next_token();
        }

        Ok(Statement::LetStatement(token, name, Expression::Nil))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        let token = self.cur_token.clone();
        self.next_token();

        // TODO: We're skipping expressions until we encounter a semicolon
        while !self.cur_token_is(&TokenType::SemiColon) {
            self.next_token();
        }

        Ok(Statement::ReturnStatement(token, Expression::Nil))
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
