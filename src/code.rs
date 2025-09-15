use crate::code::Opcode::OpConstant;
use std::collections::HashMap;
use std::fmt;
use std::sync::LazyLock;
use std::fmt::Write;

pub type Instructions = Vec<u8>;

const DEFINITIONS: LazyLock<HashMap<Opcode, Definition>> = LazyLock::new(definitions);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum Opcode {
    OpConstant = 0,
}

impl Opcode {
    fn from_ordinal(ordinal: u8) -> Opcode {
        match ordinal {
            0 => OpConstant,
            _ => panic!("Invalid opcode ordinal: {}", ordinal),
        }
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OpConstant => write!(f, "CONSTANT")
        }
    }
}

#[derive(Debug, Clone)]
pub struct Definition {
    // TODO: Can we get rid of the name and just use the enum name?
    pub name: String,
    pub operand_widths: Vec<u32>,
}

impl Definition {
    pub fn new(name: String, operand_widths: Vec<u32>) -> Definition {
        Definition {
            name,
            operand_widths,
        }
    }

    pub fn lookup(op: &Opcode) -> Option<Definition> {
        DEFINITIONS.get(op).cloned()
    }
}

fn definitions() -> HashMap<Opcode, Definition> {
    HashMap::from([(OpConstant, Definition::new(OpConstant.to_string(), vec![2]))])
}

pub fn make(op: Opcode, operands: Vec<i32>) -> Vec<u8> {
    let maybe_def = Definition::lookup(&op);
    if maybe_def.is_none() {
        return vec![];
    }

    let def = maybe_def.unwrap();

    let mut instruction_len = 1;
    for operand_width in def.operand_widths.iter() {
        instruction_len += operand_width;
    }

    //let mut instruction: Vec<u8> = Vec::with_capacity(instruction_len as usize);
    let mut instruction: Vec<u8> = vec![0; instruction_len as usize];
    instruction[0] = op as u8;

    // TODO: Should we get rid of offset and just use push() ?
    let mut offset = 1;
    for i in 0..def.operand_widths.len() {
        let width: usize = def.operand_widths[i] as usize;
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
        let maybe_def = Definition::lookup(&Opcode::from_ordinal(ins[i]));
        if maybe_def.is_none() {
            writeln!(out, "ERROR: no definition for opcode: {}", ins[i]).unwrap();
            i += 1;
            continue;
        }

        let def = maybe_def.unwrap();
        let (operands, read) = read_operands(&def, &ins[i + 1..].to_vec());
        writeln!(out, "{:04} {}", i, format_instruction(&def, &operands)).unwrap();
        i += 1 + read;
    }
    out
}

fn format_instruction(def: &Definition, operands: &[i32]) -> String {
    let operand_count = def.operand_widths.len();

    if operands.len() != operand_count {
        return format!("ERROR: operand len {} does not match defined {}", operands.len(), operand_count);
    }

    match operand_count {
        1 => format!("{} {}", def.name, operands[0]),
        _ => panic!("Unhandled operand count for {}", def.name),
    }
}

pub fn read_operands(def: &Definition, ins: &Instructions) -> (Vec<i32>, usize) {
    let mut operands: Vec<i32> = vec![0; def.operand_widths.len()];
    let mut offset = 0;

    for i in 0..def.operand_widths.len() {
        let width: usize = def.operand_widths[i] as usize;
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
