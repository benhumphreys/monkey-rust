#![allow(dead_code)]

use std::cell::RefCell;
use crate::ast::{BlockStatement, Expression, Program, Statement};
use crate::code::Opcode::{OpConstant, OpGetGlobal, OpGetLocal, OpPop, OpReturn, OpReturnValue, OpSetGlobal, OpSetLocal};
use crate::code::{make, Instructions, Opcode};
use crate::object::Object;
use crate::object::Object::{CompiledFunction, Integer, StringObject};
use crate::symbol_table::{SymbolScope, SymbolTable};
use std::ops::Deref;
use std::rc::Rc;
use Opcode::{OpAdd, OpBang, OpDiv, OpEqual, OpFalse, OpGreaterThan, OpJump, OpJumpNotTruthy, OpMinus, OpMul, OpNotEqual, OpNull, OpSub, OpTrue};

pub type CompilerResult<T = ()> = Result<T, String>;

pub struct CompilationScope {
    pub instructions: Instructions,
    pub last_instruction: EmittedInstruction,
    pub previous_instruction: EmittedInstruction,
}

impl CompilationScope {
    pub fn new() -> Self {
        CompilationScope {
            instructions: Instructions::new(),
            last_instruction: EmittedInstruction::new(),
            previous_instruction: EmittedInstruction::new(),
        }
    }
}

pub struct Compiler {
    pub constants: Rc<RefCell<Vec<Object>>>,
    pub symbol_table: Rc<RefCell<SymbolTable>>,
    pub scopes: Vec<CompilationScope>,
    pub scope_index: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self::new_with_state(Rc::new(RefCell::new(SymbolTable::new())), Rc::new(RefCell::new(Vec::new())))
    }

    pub fn new_with_state(s: Rc<RefCell<SymbolTable>>, constants: Rc<RefCell<Vec<Object>>>) -> Self {
        let main_scope = CompilationScope::new();

        Compiler {
            constants: constants,
            symbol_table: s,
            scopes: vec![main_scope],
            scope_index: 0,
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
            Statement::LetStatement(_, id, expression) => {
                self.compile_expression(expression)?;

                let symbol = self.symbol_table.borrow_mut().define(id.value.as_str());
                match symbol.scope {
                    SymbolScope::GlobalScope => self.emit(OpSetGlobal, vec![symbol.index]),
                    SymbolScope::LocalScope => self.emit(OpSetLocal, vec![symbol.index])
                };
                Ok(())
            }
            Statement::ReturnStatement(_, expr) => {
                self.compile_expression(expr)?;
                self.emit(Opcode::OpReturnValue, vec![]);
                Ok(())
            }
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
            Expression::Identifier(_, value) => {
                let maybe_symbol = self.symbol_table.borrow_mut().resolve(value);
                match maybe_symbol {
                    Some(symbol) => {
                        match symbol.scope {
                            SymbolScope::GlobalScope => self.emit(OpGetGlobal, vec![symbol.index]),
                            SymbolScope::LocalScope => self.emit(OpGetLocal, vec![symbol.index]),
                        };
                        Ok(())
                    }
                    None => Err(format!("undefined variable: {}", value))
                }
            }
            Expression::IntegerLiteral(_, value) => {
                let pos = self.add_constant(&Integer(*value));
                self.emit(OpConstant, vec![pos]);
                Ok(())
            }
            Expression::StringLiteral(_, value) => {
                let pos = self.add_constant(&StringObject(value.to_string()));
                self.emit(OpConstant, vec![pos]);
                Ok(())
            }
            Expression::Boolean(_, value) => {
                if *value {
                    self.emit(OpTrue, vec![]);
                    Ok(())
                } else {
                    self.emit(OpFalse, vec![]);
                    Ok(())
                }
            }
            Expression::PrefixExpression(_, operator, right) => {
                self.compile_expression(right)?;

                match operator.as_str() {
                    "!" => {
                        self.emit(OpBang, vec![]);
                        Ok(())
                    },
                    "-" => {
                        self.emit(OpMinus, vec![]);
                        Ok(())
                    },
                    &_ => Err(format!("unknown operator: {}", operator))
                }
            }
            Expression::InfixExpression(_, left, operator, right) => {
                if operator == "<" {
                    self.compile_expression(right)?;
                    self.compile_expression(left)?;
                    self.emit(OpGreaterThan, vec![]);
                    return Ok(())
                }

                self.compile_expression(left)?;
                self.compile_expression(right)?;

                match operator.as_str() {
                    "+" => {
                        self.emit(OpAdd, vec![]);
                        Ok(())
                    },
                    "-" => {
                        self.emit(OpSub, vec![]);
                        Ok(())
                    },
                    "*" => {
                        self.emit(OpMul, vec![]);
                        Ok(())
                    },
                    "/" => {
                        self.emit(OpDiv, vec![]);
                        Ok(())
                    },
                    ">" => {
                        self.emit(OpGreaterThan, vec![]);
                        Ok(())
                    },
                    "==" => {
                        self.emit(OpEqual, vec![]);
                        Ok(())
                    },
                    "!=" => {
                        self.emit(OpNotEqual, vec![]);
                        Ok(())
                    }
                    &_ => Err(format!("unknown operator: {}", operator))
                }
            }
            Expression::IfExpression(_, condition, consequence, alternative) => {
                self.compile_expression(condition.deref())?;

                // Emit an OpJumpNotTruthy with a bogus jump offset, to be patched later
                let jump_not_truthy_pos = self.emit(OpJumpNotTruthy, vec![9999]);

                self.compile_block_statement(consequence)?;

                // If expressions are indeed expressions, therefore need to leave the last value on the stack
                if self.last_instruction_is(&OpPop) {
                    self.remove_last_pop();
                }

                // Emit an OpJump with a bogus jump offset, to be patched later
                let jump_pos = self.emit(OpJump, vec![9999]);

                let after_consequence_pos = self.current_instructions().len() as i32;
                self.change_operand(jump_not_truthy_pos, after_consequence_pos);

                if let Some(alternative) = alternative {
                    self.compile_block_statement(alternative)?;
                    if self.last_instruction_is(&OpPop) {
                        self.remove_last_pop();
                    }
                } else {
                    self.emit(OpNull, vec![]) ;
                }

                let after_alternative_pos = self.current_instructions().len() as i32;
                self.change_operand(jump_pos, after_alternative_pos);

                Ok(())
            }
            Expression::FunctionLiteral(_, _, body) => {
                self.enter_scope();

                self.compile_block_statement(body)?;
                if self.last_instruction_is(&OpPop) {
                    self.replace_last_pop_with_return();
                }
                if !self.last_instruction_is(&OpReturnValue) {
                    self.emit(OpReturn, vec![]);
                }

                let instructions = self.leave_scope();

                let compiled_fn = CompiledFunction(instructions);
                let constant_idx = self.add_constant(&compiled_fn);
                self.emit(OpConstant, vec![constant_idx]);
                Ok(())
            }
            Expression::CallExpression(_, function, arguments) => {
                self.compile_expression(function)?;
                self.emit(Opcode::OpCall, vec![]);
                Ok(())
            }
            Expression::ArrayLiteral(_, elements) => {
                for element in elements {
                    self.compile_expression(element)?;
                }
                self.emit(Opcode::OpArray, vec![elements.len() as i32]);
                Ok(())
            }
            Expression::IndexExpression(_, left, index) => {
                self.compile_expression(left)?;
                self.compile_expression(index)?;
                self.emit(Opcode::OpIndex, vec![]);
                Ok(())
            }
            Expression::HashLiteral(_, pairs) => {
                for (key, value) in pairs {
                    self.compile_expression(key)?;
                    self.compile_expression(value)?;
                }
                self.emit(Opcode::OpHash, vec![pairs.len() as i32 * 2]);

                Ok(())
            }
        }
    }

    pub fn bytecode(&mut self) -> Bytecode {
        Bytecode {
            instructions: self.current_instructions().clone(),
            constants: self.constants.borrow().clone()
        }
    }

    pub fn add_constant(&mut self, obj: &Object) -> i32 {
        self.constants.borrow_mut().push(obj.clone());
        self.constants.borrow().len() as i32 - 1
    }

    pub fn emit(&mut self, op: Opcode, operands: Vec<i32>) -> usize {
        let ins = make(op, operands);
        let pos = self.add_instruction(&ins);

        self.set_last_instruction(op, pos);
        pos
    }

    pub fn add_instruction(&mut self, ins: &Vec<u8>) -> usize {
        let pos = self.current_instructions().len();
        self.current_instructions().extend(ins);
        self.scopes[self.scope_index].instructions = self.current_instructions().clone();
        pos
    }

    pub fn compile_block_statement(&mut self, block: &BlockStatement) -> CompilerResult {
        for stmt in &block.statements {
            self.compile_statement(stmt)?;
        }
        Ok(())
    }

    pub fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        let previous = self.scopes[self.scope_index].last_instruction.clone();
        let last = EmittedInstruction{opcode: op, pos: pos};

        self.scopes[self.scope_index].previous_instruction = previous;
        self.scopes[self.scope_index].last_instruction = last;
    }

    pub fn last_instruction_is(&mut self, op: &Opcode) -> bool {
        if self.current_instructions().len() == 0 {
            return false;
        }
        self.scopes[self.scope_index].last_instruction.opcode == *op
    }

    pub fn remove_last_pop(&mut self) {
        let last = self.scopes[self.scope_index].last_instruction.clone();
        let previous = self.scopes[self.scope_index].previous_instruction.clone();

        let old = self.current_instructions();
        let new = old[..last.pos].to_vec();

        self.scopes[self.scope_index].instructions = new;
        self.scopes[self.scope_index].last_instruction = previous;
    }

    pub fn replace_instruction(&mut self, pos: usize, new_instruction: &[u8]) {
        let ins = self.current_instructions();

        for i in 0..new_instruction.len() {
            ins[pos + i] = new_instruction[i];
        }
    }

    pub fn change_operand(&mut self, op_pos: usize, operand: i32) {
        let op = Opcode::from_ordinal(self.current_instructions()[op_pos]).unwrap();
        let new_instruction = make(op, vec![operand]);
        self.replace_instruction(op_pos, &new_instruction);
    }

    pub fn enter_scope(&mut self) {
        let scope = CompilationScope::new();
        self.scopes.push(scope);
        self.scope_index += 1;
        self.symbol_table = Rc::new(RefCell::new(SymbolTable::new_enclosed_symbol_table(&self.symbol_table)));
    }

    pub fn leave_scope(&mut self) -> Instructions {
        let instructions = self.current_instructions().clone();
        self.scopes.pop();
        self.scope_index -= 1;
        let outer = self.symbol_table.borrow().outer.clone().unwrap();
        self.symbol_table = outer;
        instructions
    }

    pub fn current_instructions(&mut self) -> &mut Instructions {
        &mut self.scopes[self.scope_index].instructions
    }

    fn replace_last_pop_with_return(&mut self) {
        let last_pos = self.scopes[self.scope_index].last_instruction.pos;
        self.replace_instruction(last_pos, &make(Opcode::OpReturnValue, vec![]));
        self.scopes[self.scope_index].last_instruction.opcode = Opcode::OpReturnValue;
    }
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>
}

#[derive(Clone, Copy)]
pub struct EmittedInstruction {
    pub opcode: Opcode,
    pub pos: usize,
}

impl EmittedInstruction {
    fn new() -> Self {
        EmittedInstruction {
            opcode: OpConstant,
            pos: 0,
        }
    }
}