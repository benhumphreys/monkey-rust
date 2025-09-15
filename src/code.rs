use crate::code::Opcode::{OpAdd, OpConstant};
use std::fmt;
use std::fmt::Write;

pub type Instructions = Vec<u8>;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum Opcode {
    OpConstant = 0,
    OpAdd = 1,
}

impl Opcode {
    pub fn from_ordinal(ordinal: u8) -> Result<Opcode, String> {
        match ordinal {
            0 => Ok(OpConstant),
            1 => Ok(OpAdd),
            _ => Err(format!("ERROR: no definition for opcode: {}", ordinal)),
        }
    }

    pub fn operand_widths(&self) -> Vec<u32> {
        match self {
            OpConstant => vec![2],
            OpAdd => vec![],
        }
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OpConstant => write!(f, "OpConstant"),
            OpAdd => write!(f, "OpAdd"),
        }
    }
}

pub fn make(op: Opcode, operands: Vec<i32>) -> Vec<u8> {
    let operand_widths = op.operand_widths();

    let mut instruction_len = 1;
    for operand_width in operand_widths.iter() {
        instruction_len += operand_width;
    }

    let mut instruction: Vec<u8> = vec![0; instruction_len as usize];
    instruction[0] = op as u8;

    // TODO: Should we get rid of offset and just use push() ?
    let mut offset = 1;
    for i in 0..operand_widths.len() {
        let width: usize = operand_widths[i] as usize;
        match width {
            2 => {
                instruction[offset] = (operands[i] >> 8) as u8;
                instruction[offset + 1] = (operands[i] & 0xFF) as u8;
            }
            _ => panic!("Unhandled operand width: {}", width),
        }
        offset += width;
    }

    instruction
}

pub fn disassemble(ins: &Instructions) -> String {
    let mut out = String::new();

    let mut i: usize = 0;
    while i < ins.len() {
        let maybe_opcode = Opcode::from_ordinal(ins[i]);
        if maybe_opcode.is_err() {
            writeln!(out, "ERROR: no definition for opcode: {}", ins[i]).unwrap();
            i += 1;
            continue;
        }

        let opcode = maybe_opcode.unwrap();
        let (operands, read) = read_operands(&opcode, &ins[i + 1..].to_vec());
        writeln!(out, "{:04} {}", i, format_instruction(&opcode, &operands)).unwrap();
        i += 1 + read;
    }
    out
}

fn format_instruction(op: &Opcode, operands: &[i32]) -> String {
    let operand_count = op.operand_widths().len();

    if operands.len() != operand_count {
        return format!("ERROR: operand len {} does not match defined {}", operands.len(), operand_count);
    }

    match operand_count {
        0 => format!("{}", op),
        1 => format!("{} {}", op, operands[0]),
        _ => panic!("Unhandled operand count for {}", op),
    }
}

pub fn read_operands(op: &Opcode, ins: &Instructions) -> (Vec<i32>, usize) {
    let operand_widths = op.operand_widths();
    let mut operands: Vec<i32> = vec![0; operand_widths.len()];
    let mut offset = 0;

    for i in 0..operand_widths.len() {
        let width: usize = operand_widths[i] as usize;
        match width {
            2 => operands[i] = convert_u16_to_i32_be(&ins[offset..offset + 2]),
            _ => panic!("Unhandled operand width: {}", width),
        }
        offset += width;
    }

    (operands, offset)
}

pub fn convert_u16_to_i32_be(ins: &[u8]) -> i32 {
    (ins[0] as i32) << 8 | (ins[1] as i32)
}
