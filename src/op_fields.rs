use std::fmt;

use strum_macros::{Display, EnumString};

enum CombinedResult<A, B> {
    Unsigned(A),
    Signed(B),
}

impl<A, B> CombinedResult<A, B> {
    fn from<E>(left: Result<A, E>, right: Result<B, E>) -> Result<CombinedResult<A, B>, E> {
        match left {
            Ok(a) => Ok(CombinedResult::Unsigned(a)),
            Err(_) => match right {
                Ok(b) => Ok(CombinedResult::Signed(b)),
                Err(e) => Err(e),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Literal4Bit(pub u8);

impl Literal4Bit {
    pub fn new(value: u8) -> Result<Self, String> {
        if value > 0xf {
            Err(format!("out of range [0x0, 0xf]: {:X}", value))
        } else {
            Ok(Self(value))
        }
    }

    pub fn from_str_radix(s: &str, radix: u32) -> Result<Self, String> {
        let result =
            CombinedResult::from(u8::from_str_radix(s, radix), i8::from_str_radix(s, radix))
                .map_err(|_| format!("invalid number {}", s))?;
        match result {
            CombinedResult::Unsigned(a) => Self::new(a),
            CombinedResult::Signed(b) => Self::from_signed(b),
        }
    }

    pub fn from_signed(value: i8) -> Result<Self, String> {
        if value >= 0 {
            Self::new(value as u8)
        } else {
            let v = (-value) as u8;
            let inv = !(v & 0xf);
            Self::new((inv + 1) & 0xf)
        }
    }

    pub fn as_signed(&self) -> i8 {
        let sign = (self.0 & 0x8) >> 3;
        if sign == 0 {
            (self.0 & 0xf) as i8
        } else {
            (self.0 | 0xf0) as i8
        }
    }
}

impl fmt::Display for Literal4Bit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Literal7Bit(pub u8);

impl Literal7Bit {
    pub fn new(value: u8) -> Result<Self, String> {
        if value > 0x7f {
            Err(format!("out of range [0x0, 0x7f]: {:X}", value))
        } else {
            Ok(Self(value))
        }
    }

    pub fn from_str_radix(s: &str, radix: u32) -> Result<Self, String> {
        let result =
            CombinedResult::from(u8::from_str_radix(s, radix), i8::from_str_radix(s, radix))
                .map_err(|_| format!("invalid number {}", s))?;
        match result {
            CombinedResult::Unsigned(a) => Self::new(a),
            CombinedResult::Signed(b) => Self::from_signed(b),
        }
    }

    pub fn from_signed(value: i8) -> Result<Self, String> {
        if value >= 0 {
            Self::new(value as u8)
        } else {
            let v = (-value) as u8;
            let inv = !(v & 0x7f);
            Self::new((inv + 1) & 0x7f)
        }
    }

    pub fn as_signed(&self) -> i8 {
        let sign = (self.0 & 0x40) >> 6;
        if sign == 0 {
            (self.0 & 0x7f) as i8
        } else {
            (self.0 | 0x80) as i8
        }
    }
}

impl fmt::Display for Literal7Bit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Literal10Bit(pub u16);

impl Literal10Bit {
    pub fn new(value: u16) -> Result<Self, String> {
        if value > 0x3ff {
            Err(format!("out of range [0x0, 0x3ff]: {:X}", value))
        } else {
            Ok(Self(value))
        }
    }

    pub fn from_str_radix(s: &str, radix: u32) -> Result<Self, String> {
        let result =
            CombinedResult::from(u16::from_str_radix(s, radix), i16::from_str_radix(s, radix))
                .map_err(|_| format!("invalid number {}", s))?;
        match result {
            CombinedResult::Unsigned(a) => Self::new(a),
            CombinedResult::Signed(b) => Self::from_signed(b),
        }
    }

    pub fn from_signed(value: i16) -> Result<Self, String> {
        if value >= 0 {
            Self::new(value as u16)
        } else {
            let v = (-value) as u16;
            let inv = !(v & 0x3ff);
            Self::new((inv + 1) & 0x3ff)
        }
    }

    pub fn as_signed(&self) -> i16 {
        let sign = (self.0 & 0x200) >> 9;
        if sign == 0 {
            (self.0 & 0x3ff) as i16
        } else {
            (self.0 | 0xf000) as i16
        }
    }
}

impl fmt::Display for Literal10Bit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Literal12Bit(pub u16);

impl Literal12Bit {
    pub fn new(value: u16) -> Result<Self, String> {
        if value > 0xfff {
            Err(format!("out of range [0x0, 0xfff]: {:X}", value))
        } else {
            Ok(Self(value))
        }
    }

    pub fn from_str_radix(s: &str, radix: u32) -> Result<Self, String> {
        let result =
            CombinedResult::from(u16::from_str_radix(s, radix), i16::from_str_radix(s, radix))
                .map_err(|_| format!("invalid number {}", s))?;
        match result {
            CombinedResult::Unsigned(a) => Self::new(a),
            CombinedResult::Signed(b) => Self::from_signed(b),
        }
    }

    pub fn from_signed(value: i16) -> Result<Self, String> {
        if value >= 0 {
            Self::new(value as u16)
        } else {
            let v = (-value) as u16;
            let inv = !(v & 0xfff);
            Self::new((inv + 1) & 0xfff)
        }
    }

    pub fn as_signed(&self) -> i16 {
        let sign = (self.0 & 0x800) >> 11;
        if sign == 0 {
            (self.0 & 0xfff) as i16
        } else {
            (self.0 | 0xf000) as i16
        }
    }
}

impl fmt::Display for Literal12Bit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumString, Display)]
pub enum RelationOp {
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    BothZero,
    EitherNonZero,
    BothNonZero,
}

impl TryFrom<u16> for RelationOp {
    type Error = String;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            x if x == RelationOp::Eq as u16 => Ok(RelationOp::Eq),
            x if x == RelationOp::Neq as u16 => Ok(RelationOp::Neq),
            x if x == RelationOp::Lt as u16 => Ok(RelationOp::Lt),
            x if x == RelationOp::Lte as u16 => Ok(RelationOp::Lte),
            x if x == RelationOp::Gt as u16 => Ok(RelationOp::Gt),
            x if x == RelationOp::Gte as u16 => Ok(RelationOp::Gte),
            x if x == RelationOp::BothZero as u16 => Ok(RelationOp::BothZero),
            x if x == RelationOp::EitherNonZero as u16 => Ok(RelationOp::EitherNonZero),
            x if x == RelationOp::BothNonZero as u16 => Ok(RelationOp::BothNonZero),
            _ => Err(format!("unknown relational operator value {}", value)),
        }
    }
}

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumString, Display)]
pub enum StackOp {
    Pop,
    Push,
    Peek,
    Swap,
    Dup,
    Rotate,
    Add,
    Sub,
    PushPC,
}

impl TryFrom<u16> for StackOp {
    type Error = String;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            x if x == StackOp::Pop as u16 => Ok(StackOp::Pop),
            x if x == StackOp::Push as u16 => Ok(StackOp::Push),
            x if x == StackOp::Peek as u16 => Ok(StackOp::Peek),
            x if x == StackOp::Swap as u16 => Ok(StackOp::Swap),
            x if x == StackOp::Dup as u16 => Ok(StackOp::Dup),
            x if x == StackOp::Rotate as u16 => Ok(StackOp::Rotate),
            x if x == StackOp::Add as u16 => Ok(StackOp::Add),
            x if x == StackOp::Sub as u16 => Ok(StackOp::Sub),
            x if x == StackOp::PushPC as u16 => Ok(StackOp::PushPC),
            _ => Err(format!("unknown stack operator value {}", value)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_literal7bit() {
        assert_eq!(Literal7Bit::new(0x6f).unwrap().0, 0x6f);
        assert_eq!(Literal7Bit::from_signed(10).unwrap().0, 10);
        assert_eq!(Literal7Bit::from_signed(-5).unwrap().0, 0b111_1011);
        assert_eq!(Literal7Bit::from_str_radix("-5", 10).unwrap().0, 0b111_1011);
        assert_eq!(Literal7Bit::from_signed(-2).unwrap().as_signed(), -2);
    }

    #[test]
    fn test_relational_op() {
        let relation_str = "Gt";
        let mut relation_op = relation_str.parse().unwrap();
        assert_eq!(relation_op, RelationOp::Gt);
        relation_op = RelationOp::try_from(5).unwrap();
        assert_eq!(relation_op, RelationOp::Gte);
    }
}
