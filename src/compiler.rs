#![allow(dead_code)]

use crate::ast::{Expression, Program, Statement};
use crate::code::{make, Instructions, Opcode};
use crate::code::Opcode::OpConstant;
use crate::object::Object;
use crate::object::Object::Integer;

pub type CompilerResult<T = ()> = Result<T, String>;

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Instructions::new(),
            constants: Vec::new()
        }
    }

    pub fn compile(&mut self, program: &Program) -> CompilerResult {
        for stmt in &program.statements {
            self.compile_statement(stmt)?;
        }
        Ok(())
    }

    fn compile_statement(&mut self, stmt: &Statement) -> CompilerResult {
        match stmt {
            Statement::LetStatement(_, _, _) => {todo!()}
            Statement::ReturnStatement(_, _) => {todo!()}
            Statement::ExpressionStatement(_, expression) => {
                self.compile_expression(expression)?;
                self.emit(Opcode::OpPop, vec![]);
                Ok(())
            },
            Statement::BlockStatement(_, _) => {todo!()}
        }
    }

    fn compile_expression(&mut self, expr: &Expression) -> CompilerResult {
        match expr {
            Expression::Identifier(_, _) => {todo!()}
            Expression::IntegerLiteral(_, value) => {
                let pos = self.add_constant(&Integer(*value));
                self.emit(OpConstant, vec![pos]);
                Ok(())
            }
            Expression::StringLiteral(_, _) => {todo!()}
            Expression::Boolean(_, _) => {todo!()}
            Expression::PrefixExpression(_, _, _) => {todo!()}
            Expression::InfixExpression(_, left, operator, right) => {
                self.compile_expression(left)?;
                self.compile_expression(right)?;

                match operator.as_str() {
                    "+" => {
                        self.emit(Opcode::OpAdd, vec![]);
                        Ok(())
                    },
                    &_ => Err(format!("unknown operator: {}", operator))
                }
            }
            Expression::IfExpression(_, _, _, _) => {todo!()}
            Expression::FunctionLiteral(_, _, _) => {todo!()}
            Expression::CallExpression(_, _, _) => {todo!()}
            Expression::ArrayLiteral(_, _) => {todo!()}
            Expression::IndexExpression(_, _, _) => {todo!()}
            Expression::HashLiteral(_, _) => {todo!()}
        }
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone()
        }
    }

    fn add_constant(&mut self, obj: &Object) -> i32 {
        self.constants.push(obj.clone());
        self.constants.len() as i32 - 1
    }

    fn emit(&mut self, op: Opcode, operands: Vec<i32>) -> i32 {
        let ins = make(op, operands);
        let pos = self.add_instruction(&ins);
        pos
    }

    fn add_instruction(&mut self, ins: &Vec<u8>) -> i32 {
        let pos = self.instructions.len();
        self.instructions.extend(ins);
        pos as i32
    }
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>
}
