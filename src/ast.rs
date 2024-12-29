use crate::token::Token;
use std::fmt::{Debug, Display, Formatter};

pub trait Node {
    fn token_literal(&self) -> String;
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Token, String),
    IntegerLiteral(Token, i64),
    PrefixExpression(Token, String, Box<Expression>),
    InfixExpression(Token, Box<Expression>, String, Box<Expression>),
    Nil, // TODO: Work out how to deal with this. It has been simply translated from the Go example
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        String::new()
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(_, value) => write!(f, "{}", value),
            Expression::IntegerLiteral(_, value) => write!(f, "{}", value),
            Expression::PrefixExpression(_, operator, right) => {
                write!(f, "({}{})", operator, right.to_string())
            }
            Expression::InfixExpression(_, left, operator, right) => {
                write!(f, "({} {} {})", left, operator, right)
            }
            Expression::Nil => write!(f, "Nil"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    token: Token,
    operator: String,
    right: Box<Expression>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    token: Token,
    left: Box<Expression>,
    operator: String,
    right: Box<Expression>
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left.to_string(), self.operator, self.right.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    token: Token,
    expression: Expression,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    token: Token,
    return_value: Expression,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    LetStatement(Token, Identifier, Expression),
    ReturnStatement(Token, Expression),
    ExpressionStatement(Token, Expression),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStatement(_, id, expr) => write!(f, "let {} = {};", id, expr),
            Statement::ReturnStatement(_, expr) => write!(f, "return {};", expr),
            Statement::ExpressionStatement(_, expr) => write!(f, "{}", expr),
        }
    }
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        use Statement::*;
        match self {
            LetStatement(token, _, _) => token.literal.clone(),
            ReturnStatement(token, _) => token.literal.clone(),
            ExpressionStatement(token, _) => token.literal.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }

    /*
    pub fn token_literal(&self) -> String {
        match self.statements.is_empty() {
            true => { String::new() },
            false => { self.statements[0].token_literal().clone() }
        }
    }
    */
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let formatted_statements = self
            .statements
            .iter()
            .map(|stmt| stmt.to_string())
            .collect::<Vec<String>>()
            .join("");

        write!(f, "{}", formatted_statements)
    }
}
