#![allow(dead_code)]

use monkey::code::{disassemble, make, read_operands, Definition, Instructions, Opcode};
use monkey::code::Opcode::OpConstant;

#[test]
fn test_make() {
    let test_cases = [
        (OpConstant, [65534i32], [OpConstant as u8, 255, 254]),
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
        make(OpConstant, vec![1]),
        make(OpConstant, vec![2]),
        make(OpConstant, vec![65535]),
    ];

    let expected = "0000 CONSTANT 1\n0003 CONSTANT 2\n0006 CONSTANT 65535\n";

    let mut concatted: Instructions = vec![];
    for instruction in instructions {
        concatted.extend(instruction);
    }

    assert_eq!(disassemble(&concatted), expected);
}

#[test]
fn test_read_operands() {
    let test_cases = [
       ReadOperandsTestCase {
           op: Opcode::OpConstant,
           operands: vec![65535],
           bytes_read: 2
       }
    ];

    for test_case in test_cases {
        let instruction = make(test_case.op, test_case.operands.clone());

        let maybe_def = Definition::lookup(&test_case.op);
        assert!(maybe_def.is_some());

        let encoded_operands = &instruction[1..].to_vec();
        let (operands_read, n) = read_operands(&maybe_def.unwrap(), encoded_operands);
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