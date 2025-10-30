#![allow(dead_code)]

use monkey::code::{disassemble, make, read_operands, Instructions, Opcode};
use monkey::code::Opcode::{OpAdd, OpConstant, OpGetLocal};

#[test]
fn test_make() {
    let test_cases = [
        (OpConstant, vec![65534i32], vec![OpConstant as u8, 255, 254]),
        (OpAdd, vec![], vec![OpAdd as u8]),
        (OpGetLocal, vec![255], vec![OpGetLocal as u8, 255]),
    ];

    for test_case in test_cases {
        let (op, operands, expected) = test_case;
        let instruction = make(op, operands.to_vec());

        assert_eq!(instruction.len(), expected.len());

        for i in 0..expected.len() {
            assert_eq!(instruction[i], expected[i]);
        }
    }
}

#[test]
fn test_instructions_string() {
    let instructions = [
        make(OpAdd, vec![]),
        make(OpGetLocal, vec![1]),
        make(OpConstant, vec![2]),
        make(OpConstant, vec![65535]),
    ];

    let expected = "0000 OpAdd\n\
    0001 OpGetLocal 1\n\
    0003 OpConstant 2\n\
    0006 OpConstant 65535\n";

    let mut concatenated: Instructions = vec![];
    for instruction in instructions {
        concatenated.extend(instruction);
    }

    assert_eq!(disassemble(&concatenated), expected);
}

#[test]
fn test_read_operands() {
    let test_cases = [
       ReadOperandsTestCase {
           op: OpConstant,
           operands: vec![65535],
           bytes_read: 2
       },
        ReadOperandsTestCase {
           op: OpGetLocal,
           operands: vec![255],
           bytes_read: 1
        }
    ];

    for test_case in test_cases {
        let instruction = make(test_case.op, test_case.operands.clone());

        let encoded_operands = &instruction[1..].to_vec();
        let (operands_read, n) = read_operands(&test_case.op, encoded_operands);
        assert_eq!(n, test_case.bytes_read);

        for i in 0..test_case.operands.len() {
            assert_eq!(operands_read[i], test_case.operands[i]);
        }
    }
}

struct ReadOperandsTestCase {
    op: Opcode,
    operands: Vec<i32>,
    bytes_read: usize
}