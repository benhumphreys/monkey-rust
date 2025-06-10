use crate::ast;
use crate::ast::Expression::FunctionLiteral;
use crate::ast::{BlockStatement, Expression, Identifier, Program};
use crate::lexer::Lexer;
use crate::parser::Precedence::Lowest;
use crate::token::TokenType::*;
use crate::token::{Token, TokenType};
use ast::Statement;
use std::collections::HashMap;

pub type ParseError = String;

type PrefixParseFn = fn(&mut Parser) -> Result<Expression, ParseError>;
type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression, ParseError>;

#[repr(u8)]
#[derive(Debug, Clone)]
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
    precedences: HashMap<TokenType, Precedence>,
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
            precedences: HashMap::from([
                (Eq, Precedence::Equals),
                (NotEq, Precedence::Equals),
                (LessThan, Precedence::LessGreater),
                (GreaterThan, Precedence::LessGreater),
                (Plus, Precedence::Sum),
                (Minus, Precedence::Sum),
                (Slash, Precedence::Product),
                (Asterisk, Precedence::Product),
            ]),
        };

        // Register parsing functions
        p.register_prefix(Ident, Parser::parse_identifier);
        p.register_prefix(Int, Parser::parse_integer_literal);
        p.register_prefix(Bang, Parser::parse_prefix_expression);
        p.register_prefix(Minus, Parser::parse_prefix_expression);
        p.register_prefix(True, Parser::parse_boolean_expression);
        p.register_prefix(False, Parser::parse_boolean_expression);
        p.register_prefix(LeftParen, Parser::parse_grouped_expression);
        p.register_prefix(If, Parser::parse_if_expression);
        p.register_prefix(Function, Parser::parse_function_literal);

        p.register_infix(Plus, Parser::parse_infix_expression);
        p.register_infix(Minus, Parser::parse_infix_expression);
        p.register_infix(Slash, Parser::parse_infix_expression);
        p.register_infix(Asterisk, Parser::parse_infix_expression);
        p.register_infix(Eq, Parser::parse_infix_expression);
        p.register_infix(NotEq, Parser::parse_infix_expression);
        p.register_infix(LessThan, Parser::parse_infix_expression);
        p.register_infix(GreaterThan, Parser::parse_infix_expression);

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
                Err(e) => self.errors.push(e),
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
        Ok(Expression::Identifier(
            self.cur_token.clone(),
            self.cur_token.literal.clone(),
        ))
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, ParseError> {
        let token = self.cur_token.clone();
        let value = token.literal.parse::<i64>();

        match value {
            Ok(v) => Ok(Expression::IntegerLiteral(token, v)),
            Err(_) => Err(format!("could not parse '{}' as integer", token.literal)),
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
        let prefix_fn = self.prefix_parse_fns.get(&self.cur_token.token_type);
        if prefix_fn.is_none() {
            return Err(format!(
                "no prefix parse function for {} found",
                self.cur_token.token_type
            ));
        }

        let mut left_exp = prefix_fn.unwrap()(self);

        while !self.peek_token_is(&SemiColon)
            && (precedence.clone() as u8) < (self.peek_precedence() as u8)
        {
            let infix_fn = self.infix_parse_fns.get(&self.peek_token.token_type);
            if infix_fn.is_none() {
                return left_exp;
            }

            let infix_fn_unwrapped = *infix_fn.unwrap();

            self.next_token();
            left_exp = infix_fn_unwrapped(self, left_exp?);
        }

        left_exp
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.cur_token.clone();
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;
        Ok(Expression::PrefixExpression(
            token.clone(),
            token.literal,
            Box::new(right),
        ))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        let token = self.cur_token.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        Ok(Expression::InfixExpression(
            token.clone(),
            Box::new(left),
            token.literal,
            Box::new(right),
        ))
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

    fn parse_boolean_expression(&mut self) -> Result<Expression, ParseError> {
        Ok(Expression::Boolean(
            self.cur_token.clone(),
            self.cur_token_is(&True),
        ))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParseError> {
        self.next_token();

        let expression = self.parse_expression(Lowest);

        if !self.expect_peek(&RightParen) {
            return Err(ParseError::from("Expected right paren"));
        }
        expression
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.cur_token.clone();

        if !self.expect_peek(&LeftParen) {
            return Ok(Expression::Nil);
        }

        self.next_token();
        let condition = self.parse_expression(Lowest)?;

        if !self.expect_peek(&RightParen) {
            return Ok(Expression::Nil);
        }

        if !self.expect_peek(&LeftBrace) {
            return Ok(Expression::Nil);
        }

        let consequence = self.parse_block_statement()?;

        let mut alternative: Option<BlockStatement> = None;
        if self.peek_token_is(&Else) {
            self.next_token();

            if !self.expect_peek(&LeftBrace) {
                return Ok(Expression::Nil);
            }
            alternative = Some(self.parse_block_statement()?);
        }

        Ok(Expression::IfExpression(
            token,
            Box::new(condition),
            consequence,
            alternative,
        ))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        let token = self.cur_token.clone();
        let mut statements = Vec::new();

        self.next_token();

        while !self.cur_token_is(&RightBrace) && !self.cur_token_is(&EOF) {
            let stmt = self.parse_statement();
            if stmt.is_ok() {
                statements.push(stmt?)
            }
            self.next_token()
        }

        Ok(BlockStatement::new(token, statements))
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ParseError> {
        let token = self.cur_token.clone();

        if !self.expect_peek(&LeftParen) {
            return Ok(Expression::Nil);
        }

        let parameters = self.parse_function_parameters();
        if !self.expect_peek(&LeftBrace) {
            return Ok(Expression::Nil);
        }

        let body = self.parse_block_statement();
        Ok(FunctionLiteral(token, parameters?, body?))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, ParseError> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(&RightParen) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        identifiers.push(Identifier::new(
            self.cur_token.clone(),
            self.cur_token.literal.clone(),
        ));

        while self.peek_token_is(&Comma) {
            self.next_token();
            self.next_token();

            identifiers.push(Identifier::new(
                self.cur_token.clone(),
                self.cur_token.literal.clone(),
            ));
        }

        if !self.expect_peek(&RightParen) {
            return Err("Failed to parse function parameters".to_string());
        }

        Ok(identifiers)
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

    fn cur_precedence(&mut self) -> Precedence {
        match self.precedences.get(&self.cur_token.token_type) {
            Some(p) => p.clone(),
            None => Precedence::Lowest,
        }
    }

    fn peek_precedence(&mut self) -> Precedence {
        match self.precedences.get(&self.peek_token.token_type) {
            Some(p) => p.clone(),
            None => Precedence::Lowest,
        }
    }
}
