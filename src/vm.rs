use crate::code::{convert_u16_to_i32_be, Instructions, Opcode};
use crate::compiler::Bytecode;
use crate::object::{Object, ObjectType, OBJECT_NULL};

const STACK_SIZE: usize = 2048;

pub type VmResult<T = ()> = Result<T, String>;

pub struct Vm {
    constants: Vec<Object>,
    instructions: Instructions,

    stack: [Object; STACK_SIZE],
    sp: i32
}

impl Vm {
    pub fn new(bytecode: &Bytecode) -> Self {
        Self {
            constants: bytecode.constants.clone(),
            instructions: bytecode.instructions.clone(),
            stack: [OBJECT_NULL; STACK_SIZE],
            sp: 0 // Top of the stack is stack[sp - 1], the next free slot is stack[sp]
        }
    }

    pub fn stack_top(&mut self) -> Object {
        if self.sp == 0 {
            return OBJECT_NULL
        }
        self.stack[self.sp as usize - 1].clone()
    }

    pub fn run(&mut self) -> VmResult {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op = Opcode::from_ordinal(self.instructions[ip])?;

            match op {
                Opcode::OpConstant => {
                    let const_index = convert_u16_to_i32_be(&self.instructions[ip + 1..]) as usize;
                    ip += 3; // One byte op, plus two u8 operands

                    self.push(&self.constants[const_index].clone())?;
                }
                Opcode::OpAdd => {
                    ip += 1; // One byte op, plus no operands
                    let right = self.pop();
                    let left = self.pop();

                    if let Object::Integer(right_value) = right {
                        if let Object::Integer(left_value) = left {
                           self.push(&Object::Integer(left_value + right_value))?;
                        } else {
                            return Err(format!("expected integer object. Got: {}", left.object_type()));
                        }
                    } else {
                        return Err(format!("expected integer object. Got: {}", right.object_type()));
                    }
                }
            }
        }

        Ok(())
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
}