use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> String;
}

#[derive(Debug, Clone)]
pub enum Expression {
    Dummy,
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        String::new()
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Clone)]
pub struct LetStatement {
    token: Token,
    name: Identifier,
    value: Expression,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    LetStatement(Token, Identifier, Expression),
    Dummy, // Remove this later
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        use Statement::*;
        match self {
            LetStatement(token, _, _) => token.literal.clone(),
            Dummy => "Remove later".to_string()
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

    pub fn token_literal(&self) -> String {
        if self.statements.is_empty() {
            String::new()
        } else {
            //self.statements[0].token_literal()
            String::new()
        }
    }
}
