use std::collections::HashMap;

use strum_macros::Display;

use crate::{Instruction, Literal7Bit, Literal10Bit, Literal12Bit, Register};

#[derive(Debug, Clone, Display)]
pub enum ResolveError {
    LiteralOutOfBounds { value: u32, min: u32, max: u32 },
    UnalignedJump(String, u32),
    UnknownSynbol(String),
}

#[derive(Debug, Clone, Display)]
pub enum UnResolvedInstruction {
    Instruction(Instruction),
    Imm(Register, String),
    AddImm(Register, String),
    AddImmSigned(Register, String),
    Jump(String),
    Branch(String),
    Label(String),
}

impl UnResolvedInstruction {
    pub fn resolve(
        &self,
        compiled_offset: u32,
        symbols: &HashMap<String, u32>,
    ) -> Result<Option<Instruction>, ResolveError> {
        match self {
            Self::Instruction(instr) => Ok(Some(instr.clone())),
            Self::Imm(reg, symbol) => symbols
                .get(symbol)
                .ok_or(ResolveError::UnknownSynbol(symbol.to_string()))
                .and_then(|val| {
                    Literal12Bit::new(*val as u16)
                        .map_err(|_| ResolveError::LiteralOutOfBounds {
                            value: *val,
                            min: 0,
                            max: 0xfff,
                        })
                        .map(|x| Some(Instruction::Imm(*reg, x)))
                }),
            Self::AddImm(reg, symbol) => symbols
                .get(symbol)
                .ok_or(ResolveError::UnknownSynbol(symbol.to_string()))
                .and_then(|val| {
                    Literal7Bit::new(*val as u8)
                        .map_err(|_| ResolveError::LiteralOutOfBounds {
                            value: *val,
                            min: 0,
                            max: 0x7f,
                        })
                        .map(|x| Some(Instruction::AddImm(*reg, x)))
                }),
            Self::AddImmSigned(reg, symbol) => symbols
                .get(symbol)
                .ok_or(ResolveError::UnknownSynbol(symbol.to_string()))
                .and_then(|val| {
                    Literal7Bit::new(*val as u8)
                        .map_err(|_| ResolveError::LiteralOutOfBounds {
                            value: *val,
                            min: 0,
                            max: 0x7f,
                        })
                        .map(|x| Some(Instruction::AddImmSigned(*reg, x)))
                }),
            Self::Jump(symbol) => symbols
                .get(symbol)
                .ok_or(ResolveError::UnknownSynbol(symbol.to_string()))
                .and_then(|val| {
                    if *val % 16 != 0 {
                        Err(ResolveError::UnalignedJump(symbol.clone(), *val))
                    } else {
                        let shifted = *val >> 4;
                        Literal10Bit::new(shifted as u16)
                            .map_err(|_| ResolveError::LiteralOutOfBounds {
                                value: shifted,
                                min: 0,
                                max: 0x3ff,
                            })
                            .map(|x| Some(Instruction::Jump(x)))
                    }
                }),
            Self::Branch(symbol) => symbols
                .get(symbol)
                .ok_or(ResolveError::UnknownSynbol(symbol.to_string()))
                .and_then(|val| {
                    let offset = (*val as i32) - compiled_offset as i32;
                    Literal10Bit::from_signed(offset as i16)
                        .map_err(|_| ResolveError::LiteralOutOfBounds {
                            value: *val,
                            min: 0,
                            max: 0x3ff,
                        })
                        .map(|x| Some(Instruction::Branch(x)))
                }),
            Self::Label(_) => Ok(None),
        }
    }

    pub fn size(&self) -> u32 {
        match self {
            Self::Label(_) => 0,
            _ => 2,
        }
    }
}
