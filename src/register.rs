// use std::str::FromStr;
// use std::fmt;

use strum_macros::{Display, EnumString};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum Flag {
    Compare = 0b1,
    DidJump = 0b10,
    OverFlow = 0b100,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, EnumString, Display)]
#[repr(u8)]
pub enum Register {
    Zero,
    A,
    B,
    C,
    M,
    SP,
    D,
    BP,
}

impl Register {
    pub fn new(v: u8) -> Option<Self> {
        match v {
            x if x == Register::A as u8 => Some(Register::A),
            x if x == Register::B as u8 => Some(Register::B),
            x if x == Register::C as u8 => Some(Register::C),
            x if x == Register::M as u8 => Some(Register::M),
            x if x == Register::SP as u8 => Some(Register::SP),
            x if x == Register::D as u8 => Some(Register::D),
            x if x == Register::BP as u8 => Some(Register::BP),
            x if x == Register::Zero as u8 => Some(Register::Zero),
            _ => None,
        }
    }

    pub fn as_mask_first(&self) -> u16 {
        (*self as u16 & 0x7) << 12
    }

    pub fn from_instruction_first(instr: u16) -> Option<Self> {
        Self::new(((instr & 0x7000) >> 12) as u8)
    }

    pub fn as_mask_second(&self) -> u16 {
        ((*self as u16) & 0x7) << 9
    }

    pub fn from_instruction_second(instr: u16) -> Option<Self> {
        Self::new(((instr & 0xe00) >> 9) as u8)
    }

    pub fn as_mask_third(&self) -> u16 {
        (*self as u16) & 0x7
    }

    pub fn from_instruction_third(instr: u16) -> Option<Self> {
        Self::new((instr & 0x7) as u8)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_encoding() {
        assert_eq!(Register::Zero as u16, 0);
        assert_eq!(Register::A as u16, 1);
        assert_eq!(Register::B as u16, 2);
        assert_eq!(Register::C as u16, 3);
        assert_eq!(Register::M as u16, 4);
        assert_eq!(Register::SP as u16, 5);
        assert_eq!(Register::D as u16, 6);
        assert_eq!(Register::BP as u16, 7);
    }

    #[test]
    fn test_display() {
        let reg_a_str = "A";
        let reg_a: Register = reg_a_str.parse().unwrap();
        assert_eq!(reg_a,Register::A);
        assert_eq!(reg_a.to_string(), "A");
    }
}
