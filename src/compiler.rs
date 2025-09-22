#![allow(dead_code)]

use std::ops::Deref;
use crate::ast::{BlockStatement, Expression, Program, Statement};
use crate::code::{make, Instructions, Opcode};
use crate::code::Opcode::OpConstant;
use crate::object::Object;
use crate::object::Object::Integer;

pub type CompilerResult<T = ()> = Result<T, String>;

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
    last_instruction: EmittedInstruction,
    previous_instruction: EmittedInstruction
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Instructions::new(),
            constants: Vec::new(),
            last_instruction: EmittedInstruction::new(),
            previous_instruction: EmittedInstruction::new()
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
            Statement::BlockStatement(_, statements) => {
                for stmt in statements {
                    self.compile_statement(stmt)?;
                }
                Ok(())
            }
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
            Expression::Boolean(_, value) => {
                if *value {
                    self.emit(Opcode::OpTrue, vec![]);
                    Ok(())
                } else {
                    self.emit(Opcode::OpFalse, vec![]);
                    Ok(())
                }
            }
            Expression::PrefixExpression(_, operator, right) => {
                self.compile_expression(right)?;

                match operator.as_str() {
                    "!" => {
                        self.emit(Opcode::OpBang, vec![]);
                        Ok(())
                    },
                    "-" => {
                        self.emit(Opcode::OpMinus, vec![]);
                        Ok(())
                    },
                    &_ => Err(format!("unknown operator: {}", operator))
                }
            }
            Expression::InfixExpression(_, left, operator, right) => {
                if operator == "<" {
                    self.compile_expression(right)?;
                    self.compile_expression(left)?;
                    self.emit(Opcode::OpGreaterThan, vec![]);
                    return Ok(())
                }

                self.compile_expression(left)?;
                self.compile_expression(right)?;

                match operator.as_str() {
                    "+" => {
                        self.emit(Opcode::OpAdd, vec![]);
                        Ok(())
                    },
                    "-" => {
                        self.emit(Opcode::OpSub, vec![]);
                        Ok(())
                    },
                    "*" => {
                        self.emit(Opcode::OpMul, vec![]);
                        Ok(())
                    },
                    "/" => {
                        self.emit(Opcode::OpDiv, vec![]);
                        Ok(())
                    },
                    ">" => {
                        self.emit(Opcode::OpGreaterThan, vec![]);
                        Ok(())
                    },
                    "==" => {
                        self.emit(Opcode::OpEqual, vec![]);
                        Ok(())
                    },
                    "!=" => {
                        self.emit(Opcode::OpNotEqual, vec![]);
                        Ok(())
                    }
                    &_ => Err(format!("unknown operator: {}", operator))
                }
            }
            Expression::IfExpression(_, condition, consequence, alternative) => {
                self.compile_expression(condition.deref())?;

                // Emit an OpJumpNotTruthy with a bogus jump offset, to be patched later
                let jump_not_truthy_pos = self.emit(Opcode::OpJumpNotTruthy, vec![9999]) as usize;

                self.compile_block_statement(consequence)?;

                // If expressions are indeed expressions, therefore need to leave the last value on the stack
                if self.last_instruction_is_pop() {
                    self.remove_last_pop();
                }

                if let Some(alternative) = alternative {
                    // Emit jump with a bogus jump offset, to be patched later
                    let jump_pos = self.emit(Opcode::OpJump, vec![9999]);
                    let after_consequence_pos = self.instructions.len() as i32;
                    self.change_operand(jump_not_truthy_pos, after_consequence_pos);

                    self.compile_block_statement(alternative)?;
                    if self.last_instruction_is_pop() {
                        self.remove_last_pop();
                    }

                    let after_alternative_pos = self.instructions.len() as i32;
                    self.change_operand(jump_pos, after_alternative_pos);
                } else {
                    let after_consequence_pos = self.instructions.len() as i32;
                    self.change_operand(jump_not_truthy_pos, after_consequence_pos);
                }

                Ok(())
            }
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

    fn emit(&mut self, op: Opcode, operands: Vec<i32>) -> usize {
        let ins = make(op, operands);
        let pos = self.add_instruction(&ins);

        self.set_last_instruction(op, pos);
        pos
    }

    fn add_instruction(&mut self, ins: &Vec<u8>) -> usize {
        let pos = self.instructions.len();
        self.instructions.extend(ins);
        pos
    }

    fn compile_block_statement(&mut self, block: &BlockStatement) -> CompilerResult {
        for stmt in &block.statements {
            self.compile_statement(stmt)?;
        }
        Ok(())
    }

    fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        let previous = self.last_instruction.clone();
        let last = EmittedInstruction{opcode: op, pos: pos};

        self.previous_instruction = previous;
        self.last_instruction = last;
    }

    fn last_instruction_is_pop(&self) -> bool {
        self.last_instruction.opcode == Opcode::OpPop
    }

    fn remove_last_pop(&mut self) {
        self.instructions.pop();
        self.last_instruction = self.previous_instruction.clone();
    }

    fn replace_instruction(&mut self, pos: usize, new_instruction: &[u8]) {
        for i in 0..new_instruction.len() {
            self.instructions[pos + i] = new_instruction[i];
        }
    }

    fn change_operand(&mut self, op_pos: usize, operand: i32) {
        let op = Opcode::from_ordinal(self.instructions[op_pos]).unwrap();
        let new_instruction = make(op, vec![operand]);
        self.replace_instruction(op_pos, &new_instruction);
    }
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>
}

#[derive(Clone)]
struct EmittedInstruction {
    opcode: Opcode,
    pos: usize,
}

impl EmittedInstruction {
    fn new() -> Self {
        EmittedInstruction {
            opcode: OpConstant,
            pos: 0,
        }
    }
}