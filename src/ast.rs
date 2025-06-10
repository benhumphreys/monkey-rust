use crate::token::Token;
use std::fmt::{Debug, Display, Formatter};

pub trait Node {
    fn token_literal(&self) -> String;
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Token, String),
    IntegerLiteral(Token, i64),
    Boolean(Token, bool),
    PrefixExpression(Token, String, Box<Expression>),
    InfixExpression(Token, Box<Expression>, String, Box<Expression>),
    IfExpression(Token, Box<Expression>, BlockStatement, Option<BlockStatement>),
    FunctionLiteral(Token, Vec<Identifier>, BlockStatement),
    CallExpression(Token, Box<Expression>, Vec<Expression>),
    Nil, // TODO: Work out if this can be replaced with error handling
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(token, _) => { token.literal.clone() }
            Expression::IntegerLiteral(token, _) => { token.literal.clone() }
            Expression::Boolean(token, _) => {token.literal.clone() }
            Expression::PrefixExpression(token, _, _) => { token.literal.clone() }
            Expression::InfixExpression(token, _, _, _) => { token.literal.clone() }
            Expression::IfExpression(token, _, _, _) => { token.literal.clone() }
            Expression::FunctionLiteral(token, _, _) => { token.literal.clone() }
            Expression::CallExpression(token, _, _) => { token.literal.clone() }
            Expression::Nil => { "Nil".to_string() }
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(_, value) => write!(f, "{}", value),
            Expression::IntegerLiteral(_, value) => write!(f, "{}", value),
            Expression::Boolean(_, value) => write!(f, "{}", value),
            Expression::PrefixExpression(_, operator, right) => {
                write!(f, "({}{})", operator, right.to_string())
            }
            Expression::InfixExpression(_, left, operator, right) => {
                write!(f, "({} {} {})", left.to_string(), operator, right.to_string())
            },
            Expression::IfExpression(_, condition, consequence, alternative) => {
                write!(f, "if{} {}", condition, consequence)?;
                if let Some(alt) = alternative {
                    write!(f, "else {}", alt)?;
                }
                Ok(())
            },
            Expression::FunctionLiteral(token, parameters, body) => {
                write!(f, "{}({}) {}",
                       token.literal,
                       parameters
                           .iter()
                           .map(|id| id.to_string())
                           .collect::<Vec<String>>()
                           .join(", "),
                       body)
            },
            Expression::CallExpression(_, function, arguments) => {
                write!(f, "{}({})",
                       function.to_string(),
                       arguments
                           .iter()
                           .map(|id| id.to_string())
                           .collect::<Vec<String>>()
                           .join(", "))
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

impl Identifier {
    pub fn new(token: Token, value: String) -> Identifier {
        Identifier {
            token,
            value
        }
    }    
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

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct Boolean {
    token: Token,
    value: bool
}

impl Display for Boolean {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for Boolean {
    fn token_literal(&self) -> String { self.token.literal.clone() }
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
    BlockStatement(Token, Vec<Statement>)
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStatement(_, id, expr) => write!(f, "let {} = {};", id, expr),
            Statement::ReturnStatement(_, expr) => write!(f, "return {};", expr),
            Statement::ExpressionStatement(_, expr) => write!(f, "{}", expr),
            Statement::BlockStatement(_, stmts) => {
                let formatted_statements = stmts
                    .iter()
                    .map(|stmt| stmt.to_string())
                    .collect::<Vec<String>>()
                    .join("");

                write!(f, "{}", formatted_statements)
            }
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
            BlockStatement(token, _) => token.literal.clone()
        }
    }
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>
}

impl BlockStatement {
    pub fn new(token: Token, statements: Vec<Statement>) -> BlockStatement {
        BlockStatement {
            token: token,
            statements: statements
        }
    }    
}

impl Display for BlockStatement {
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

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Expression,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "if{} {}", self.condition, self.consequence)?;
        if let Some(alt) = &self.alternative {
            write!(f, "else {}", alt)?;
        }
        Ok(())
    }
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}


#[derive(Debug, Clone)]
pub struct FunctionLiteral {
    token: Token,
    parameters: Vec<Identifier>,
    body: BlockStatement
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    token: Token,
    function: Expression,
    arguments: Vec<Expression>
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}


impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})",
               self.function.to_string(),
               self.arguments
                   .iter()
                   .map(|id| id.to_string())
                   .collect::<Vec<String>>()
                   .join(","))
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

    pub fn token_literal(&self) -> String {
        match self.statements.is_empty() {
            true => { String::new() },
            false => { self.statements[0].token_literal().clone() }
        }
    }
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