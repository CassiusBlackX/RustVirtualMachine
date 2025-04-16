use std::{fmt, str::FromStr};

use crate::op_fields::*;
use crate::register::Register;

use custom_macros::VMInstruction;

#[derive(Debug)]
pub enum InstructionParseError {
    NoContent,
    Fail(String),
}

pub trait InstructionPart {
    fn as_mask(&self) -> u16;
    fn from_instruction(instr: u16) -> Self;
}

#[derive(Debug, VMInstruction, PartialEq, Eq, Clone)]
pub enum Instruction {
    #[opcode(0xff)]
    Imm(Register, Literal12Bit), // Imm has unique instruction format, doesn't use an opcode
    #[opcode(0x0)]
    Invalid,
    // binary Operators
    #[opcode(0x1)]
    Add(Register, Register, Register),
    #[opcode(0x2)]
    Sub(Register, Register, Register),
    #[opcode(0x3)]
    Mul(Register, Register, Register),
    #[opcode(0x4)]
    And(Register, Register, Register),
    #[opcode(0x5)]
    Or(Register, Register, Register),
    #[opcode(0x6)]
    Xor(Register, Register, Register),
    #[opcode(0x7)]
    Mod(Register, Register, Register),
    // Reg + Imm
    #[opcode(0x8)]
    AddImm(Register, Literal7Bit),
    #[opcode(0x9)]
    AddImmSigned(Register, Literal7Bit),
    // shift
    #[opcode(0xa)]
    ShiftLeft(Register, Register, Literal4Bit),
    #[opcode(0xb)]
    ShiftRightLogical(Register, Register, Literal4Bit),
    #[opcode(0xc)]
    ShiftRightArithmetic(Register, Register, Literal4Bit),
    // load and store
    #[opcode(0xd)]
    LoadWord(Register, Register, Register),
    #[opcode(0xe)]
    StoreWord(Register, Register, Register),
    #[opcode(0xf)]
    LoadByte(Register, Register, Register),
    #[opcode(0x10)]
    StoreByte(Register, Register, Register),
    // control flow
    #[opcode(0x11)]
    Test(Register, Register, RelationOp),
    #[opcode(0x12)]
    AddIf(Register, Register, Literal4Bit),
    // stack op
    #[opcode(0x13)]
    Stack(Register, Register, StackOp),
    #[opcode(0x14)]
    LoadStackoffset(Register, Register, Literal4Bit),
    // jump
    #[opcode(0x15)]
    Jump(Literal10Bit),
    #[opcode(0x16)]
    JumpRegister(Register, Register),
    // branch
    #[opcode(0x17)]
    BranchIf(Literal10Bit),
    #[opcode(0x18)]
    Branch(Literal10Bit),
    #[opcode(0x19)]
    BranchRegisterIf(Register, Literal7Bit),
    // syscall
    #[opcode(0x1a)]
    System(Register, Register, Literal4Bit),
}

#[cfg(test)]
mod test {
    use super::Instruction::*;
    use crate::{
        op::Instruction,
        op_fields::{Literal4Bit, Literal7Bit, Literal10Bit, Literal12Bit},
        register::Register::*,
    };

    #[test]
    fn test_encoding() -> Result<(), String> {
        let ops = [
            Imm(M, Literal12Bit::new(0x30)?),
            AddImm(C, Literal7Bit::new(0x20)?),
            AddImmSigned(A, Literal7Bit::new(0x7)?),
            Add(C, A, B),
            Sub(D, BP, SP),
            Mul(D, BP, SP),
            And(D, BP, SP),
            Or(D, BP, SP),
            Xor(D, BP, SP),
            Mod(D, BP, SP),
            ShiftLeft(M, BP, Literal4Bit::new(0xe)?),
            ShiftRightLogical(M, BP, Literal4Bit::new(0xe)?),
            ShiftRightArithmetic(M, BP, Literal4Bit::new(0xe)?),
            LoadWord(A, C, M),
            LoadByte(A, C, M),
            StoreWord(C, A, M),
            StoreByte(C, A, M),
            Test(BP, A, crate::op_fields::RelationOp::Gte),
            AddIf(D, A, Literal4Bit::new(0x0)?),
            Stack(B, SP, crate::op_fields::StackOp::Dup),
            LoadStackoffset(A, BP, Literal4Bit::new(0x3)?),
            BranchIf(Literal10Bit::new(0x56)?),
            Branch(Literal10Bit::new(0x42)?),
            BranchRegisterIf(A, Literal7Bit::new(0x3)?),
            System(A, B, Literal4Bit::new(0x3)?),
        ];
        let encoded: Vec<_> = ops.iter().map(|x| x.encode_u16()).collect();
        for (instr, reg) in ops.iter().zip(encoded.iter()) {
            assert_eq!(*instr, Instruction::try_from(*reg)?);
        }
        Ok(())
    }
}
