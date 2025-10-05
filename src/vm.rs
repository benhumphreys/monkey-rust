use crate::code::{convert_u16_to_i32_be, Instructions, Opcode};
use crate::compiler::Bytecode;
use crate::object::{native_bool_to_bool_object, IsHashable, Object, ObjectType, OBJECT_BOOLEAN_FALSE, OBJECT_BOOLEAN_TRUE, OBJECT_NULL};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub const GLOBAL_SIZE: usize = 65536;
const STACK_SIZE: usize = 2048;

pub type VmResult<T = ()> = Result<T, String>;

pub struct Vm {
    constants: Vec<Object>,
    instructions: Instructions,
    globals: Rc<RefCell<Vec<Object>>>,

    stack: [Object; STACK_SIZE],
    sp: i32
}

impl Vm {
    pub fn new(bytecode: &Bytecode) -> Self {
        Self {
            constants: bytecode.constants.clone(),
            instructions: bytecode.instructions.clone(),
            globals: Rc::new(RefCell::new(vec![Object::Null; GLOBAL_SIZE])),
            stack: [OBJECT_NULL; STACK_SIZE],
            sp: 0, // Top of the stack is stack[sp - 1], the next free slot is stack[sp]
        }
    }

    pub fn new_with_globals_store(bytecode: Bytecode, globals: Rc<RefCell<Vec<Object>>>) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            globals: globals,
            stack: [OBJECT_NULL; STACK_SIZE],
            sp: 0, // Top of the stack is stack[sp - 1], the next free slot is stack[sp]
        }

    }

    pub fn stack_top(&mut self) -> Object {
        if self.sp == 0 {
            return OBJECT_NULL
        }
        self.stack[self.sp as usize - 1].clone()
    }

    pub fn last_popped_stack_elem(&mut self) -> Object {
        self.stack[self.sp as usize].clone()
    }

    pub fn run(&mut self) -> VmResult {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op = Opcode::from_ordinal(self.instructions[ip])?;

            match op {
                Opcode::OpConstant => {
                    let const_index = convert_u16_to_i32_be(&self.instructions[ip + 1..]) as usize;
                    self.push(&self.constants[const_index].clone())?;
                    ip += 3; // One byte op, plus two u8 operands
                }
                Opcode::OpAdd => {
                    ip += 1; // One byte op, no operands
                    self.execute_binary_operation(&op)?
                }
                Opcode::OpSub => {
                    ip += 1; // One byte op, no operands
                    self.execute_binary_operation(&op)?
                }
                Opcode::OpMul => {
                    ip += 1; // One byte op, no operands
                    self.execute_binary_operation(&op)?
                }
                Opcode::OpDiv => {
                    ip += 1; // One byte op, no operands
                    self.execute_binary_operation(&op)?
                }
                Opcode::OpPop => {
                    ip += 1; // One byte op, no operands
                    self.pop();
                }
                Opcode::OpTrue => {
                    ip += 1; // One byte op, no operands
                    self.push(&OBJECT_BOOLEAN_TRUE)?;
                }
                Opcode::OpFalse => {
                    ip += 1; // One byte op, no operands
                    self.push(&OBJECT_BOOLEAN_FALSE)?;
                }
                Opcode::OpEqual => {
                    ip += 1;
                    self.execute_comparison(&op)?
                }
                Opcode::OpNotEqual => {
                    ip += 1;
                    self.execute_comparison(&op)?
                }
                Opcode::OpGreaterThan => {
                    ip += 1;
                    self.execute_comparison(&op)?
                }
                Opcode::OpMinus => {
                    ip += 1;
                    self.execute_minus_operator()?
                }
                Opcode::OpBang => {
                    ip += 1;
                    self.execute_bang_operator()?
                }
                Opcode::OpJumpNotTruthy => {
                    let condition = self.pop();
                    if is_truthy(&condition) {
                        ip += 3; // One byte op, plus two u8 operands
                    } else {
                        let pos = convert_u16_to_i32_be(&self.instructions[ip + 1..]) as usize;
                        ip = pos;
                    }
                }
                Opcode::OpJump => {
                    let pos = convert_u16_to_i32_be(&self.instructions[ip + 1..]) as usize;
                    ip = pos;
                }
                Opcode::OpNull => {
                    self.push(&OBJECT_NULL)?;
                    ip += 1;
                }
                Opcode::OpGetGlobal => {
                    let global_index = convert_u16_to_i32_be(&self.instructions[ip + 1..]) as usize;
                    ip += 3; // One byte op, plus two u8 operands
                    let value = {
                        &self.globals.borrow()[global_index].clone()
                    };
                    self.push(value)?;
                }
                Opcode::OpSetGlobal => {
                    let global_index = convert_u16_to_i32_be(&self.instructions[ip + 1..]) as usize;
                    ip += 3; // One byte op, plus two u8 operands
                    let value = self.pop();
                    self.globals.borrow_mut()[global_index] = value;
                }
                Opcode::OpArray => {
                    let num_elements = convert_u16_to_i32_be(&self.instructions[ip + 1..]) as usize;
                    ip += 3; // One byte op, plus two u8 operands
                    let array = self.build_array(self.sp as usize - num_elements, self.sp as usize);
                    self.sp -= num_elements as i32;
                    self.push(&array)?;
                }
                Opcode::OpHash => {
                    let num_elements = convert_u16_to_i32_be(&self.instructions[ip + 1..]) as usize;
                    ip += 3; // One byte op, plus two u8 operands
                    let hash = self.build_hash(self.sp as usize - num_elements, self.sp as usize);
                    self.sp -= num_elements as i32;
                    self.push(&hash)?;
                }
            }
        }

        Ok(())
    }

    fn build_array(&self, start_index: usize, end_index: usize) -> Object {
        let elements = self.stack[start_index..end_index].to_vec();
        Object::Array(elements)
    }

    fn build_hash(&mut self, start_index: usize, end_index: usize) -> Object {
        let mut hashed_pairs = HashMap::with_capacity((end_index - start_index) / 2);

        for i in (start_index..end_index).step_by(2) {
            let key = self.stack[i].clone();
            if !key.is_hashable() {
                return Object::Error(format!("unusable as hash key: {}", key.object_type()));
            }

            let value = self.stack[i + 1].clone();
            hashed_pairs.insert(key, value);
        }

        Object::HashObject(hashed_pairs)
    }

    fn push(&mut self, obj: &Object) -> VmResult {
        if self.sp >= STACK_SIZE as i32 {
            return Err("stack overflow".to_string())
        }

        self.stack[self.sp as usize] = obj.clone();
        self.sp += 1;
        Ok(())
    }

    fn pop(&mut self) -> Object {
        // TODO: Handle underflow
        let obj = self.stack[self.sp as usize - 1].clone();
        self.sp -= 1;
        obj
    }

    fn execute_binary_operation(&mut self, op: &Opcode) -> VmResult {
        let right = self.pop();
        let left = self.pop();

        match (&left, &right) {
            (Object::Integer(left_val), Object::Integer(right_val)) => {
                self.execute_binary_integer_operation(op, *left_val, *right_val)
            }
            (Object::StringObject(left_val), Object::StringObject(right_val)) => {
                self.execute_binary_string_operation(op, left_val, right_val)
            }
            (_, _) => {
                Err(format!("unsupported types for binary operation: {} {}", left.object_type(), right.object_type()))
            }
        }
    }

    fn execute_binary_integer_operation(&mut self, op: &Opcode, left: i64, right: i64) -> VmResult {
        let result = match op {
            Opcode::OpAdd => Ok(left + right),
            Opcode::OpSub => Ok(left - right),
            Opcode::OpMul => Ok(left * right),
            Opcode::OpDiv => Ok(left / right),
            _ => Err(format!("unknown integer operator: {}", op))
        };

        self.push(&Object::Integer(result?))
    }

    fn execute_binary_string_operation(&mut self, op: &Opcode, left: &String, right: &String) -> VmResult {
        let result = match op {
            Opcode::OpAdd => Ok(format!("{}{}", left, right)),
            _ => Err(format!("unknown string operator: {}", op))
        };

        self.push(&Object::StringObject(result?))
    }

    fn execute_comparison(&mut self, op: &Opcode) -> VmResult {
        let right = self.pop();
        let left = self.pop();

        match (&left, &right) {
            (Object::Integer(left_val), Object::Integer(right_val)) => {
                self.execute_integer_comparison(op, *left_val, *right_val)
            },
            (_, _) => {
                match op {
                    Opcode::OpEqual => self.push(&native_bool_to_bool_object(right == left)),
                    Opcode::OpNotEqual => self.push(&native_bool_to_bool_object(right != left)),
                    _ => Err(format!("unknown operator: {} ({} {})",
                                     op, left.object_type(), right.object_type()))
                }
            }
        }
    }

    fn execute_integer_comparison(&mut self, op: &Opcode, left: i64, right: i64) -> VmResult {
        match op {
            Opcode::OpEqual => self.push(&native_bool_to_bool_object(left == right)),
            Opcode::OpNotEqual => self.push(&native_bool_to_bool_object(left != right)),
            Opcode::OpGreaterThan => self.push(&native_bool_to_bool_object(left > right)),
            _ => Err(format!("unknown operator: {}", op))
        }
    }

    fn execute_bang_operator(&mut self) -> VmResult {
        let operand = self.pop();

        match operand {
            Object::Boolean(value) => {
                match value {
                    true => self.push(&OBJECT_BOOLEAN_FALSE),
                    false => self.push(&OBJECT_BOOLEAN_TRUE)
                }
            }
            Object::Null => {
                self.push(&OBJECT_BOOLEAN_TRUE)
            }
            _ => self.push(&OBJECT_BOOLEAN_FALSE)
        }
    }

    fn execute_minus_operator(&mut self) -> VmResult {
        let operand = self.pop();

        if let Object::Integer(value) = operand {
            self.push(&Object::Integer(-value))
        } else {
            Err(format!("unsupported type for negation: {}", operand.object_type()))
        }
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Boolean(value) => *value,
        Object::Null => false,
        _ => true
    }
}
