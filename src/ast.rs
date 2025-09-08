use crate::token::Token;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

pub trait Node {
    fn token_literal(&self) -> String;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Token, String),
    IntegerLiteral(Token, i64),
    StringLiteral(Token, String),
    Boolean(Token, bool),
    PrefixExpression(Token, String, Box<Expression>),
    InfixExpression(Token, Box<Expression>, String, Box<Expression>),
    IfExpression(Token, Box<Expression>, BlockStatement, Option<BlockStatement>),
    FunctionLiteral(Token, Vec<Identifier>, BlockStatement),
    CallExpression(Token, Box<Expression>, Vec<Expression>),
    ArrayLiteral(Token, Vec<Expression>),
    IndexExpression(Token, Box<Expression>, Box<Expression>),
    HashLiteral(Token, Vec<(Expression, Expression)>),
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(token, _) => token.literal.clone(),
            Expression::IntegerLiteral(token, _) => token.literal.clone(),
            Expression::StringLiteral(token, _) => token.literal.clone(),
            Expression::Boolean(token, _) => token.literal.clone(),
            Expression::PrefixExpression(token, _, _) => token.literal.clone(),
            Expression::InfixExpression(token, _, _, _) => token.literal.clone(),
            Expression::IfExpression(token, _, _, _) => token.literal.clone(),
            Expression::FunctionLiteral(token, _, _) => token.literal.clone(),
            Expression::CallExpression(token, _, _) => token.literal.clone(),
            Expression::ArrayLiteral(token, _) => token.literal.clone(),
            Expression::IndexExpression(token, _, _) => token.literal.clone(),
            Expression::HashLiteral(token, _) => token.literal.clone(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(_, value) => write!(f, "{}", value),
            Expression::IntegerLiteral(_, value) => write!(f, "{}", value),
            Expression::StringLiteral(_, value) => write!(f, "{}", value),
            Expression::Boolean(_, value) => write!(f, "{}", value),
            Expression::PrefixExpression(_, operator, right) => {
                write!(f, "({}{})", operator, right)
            }
            Expression::InfixExpression(_, left, operator, right) => {
                write!(f, "({} {} {})", left, operator, right)
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
                       function,
                       arguments
                           .iter()
                           .map(|id| id.to_string())
                           .collect::<Vec<String>>()
                           .join(", "))
            }
            Expression::ArrayLiteral(_, elements) => {
                write!(f, "[{}]",
                       elements
                           .iter()
                           .map(|id| id.to_string())
                           .collect::<Vec<String>>()
                           .join(", "))
            }
            Expression::IndexExpression(_, left, index) => {
                write!(f, "({}[{}])", left.deref(), index.deref())           
            }
            Expression::HashLiteral(_, pairs) => {
                write!(f, "{{{}}}",
                       pairs
                           .iter()
                           .map(|pair| format!("{}: {}", pair.0, pair.1))
                           .collect::<Vec<String>>()
                           .join(", "))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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
            Statement::BlockStatement(_, stmts) =>  write!(f, "{}", format_statements(stmts))
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

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>
}

impl BlockStatement {
    pub fn new(token: Token, statements: Vec<Statement>) -> BlockStatement {
        BlockStatement {
            token,
            statements
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
            true => String::new(),
            false => self.statements[0].token_literal().clone()
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format_statements(&self.statements))
    }
}

fn format_statements(statements: &[Statement]) -> String {
    statements
        .iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join("")
}