use std::collections::HashMap;

use crate::memory::{Addressable, MemoryMapper};
use crate::op::Instruction;
use crate::op_fields::{RelationOp, StackOp};
use crate::register::{Flag, Register};

pub trait SignalHandler {
    fn handle(&self, m: &mut VM, arg: u16) -> Result<(), String>;
}

impl<T> SignalHandler for T
where
    T: Fn(&mut VM, u16) -> Result<(), String>,
{
    fn handle(&self, m: &mut VM, arg: u16) -> Result<(), String> {
        self(m, arg)
    }
}

#[derive(Default)]
pub struct VM {
    registers: [u16; 8],
    flags: u16,
    pc: u32,
    pub halt: bool,
    pub memeory: MemoryMapper,
}

impl VM {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn map(
        &mut self,
        start: usize,
        size: usize,
        a: Box<dyn Addressable>,
    ) -> Result<(), String> {
        self.memeory.map(start, size, a)
    }

    pub fn reset(&mut self) {
        let _ = self.memeory.zero_all();
        self.registers = [0; 8];
        self.flags = 0;
        self.halt = false;
        self.pc = 0;
    }

    pub fn get_register(&self, r: Register) -> u16 {
        if r == Register::Zero {
            0
        } else {
            self.registers[r as usize]
        }
    }

    pub fn set_register(&mut self, r: Register, value: u16) {
        if r == Register::Zero {
            return;
        }
        self.registers[r as usize] = value;
    }

    pub fn set_flag(&mut self, flag: Flag, state: bool) {
        if state {
            self.flags |= flag as u16;
        } else {
            self.flags &= !(flag as u16);
        }
    }

    pub fn test_flag(&self, flag: Flag) -> bool {
        self.flags & (flag as u16) != 0
    }

    pub fn get_pc(&self) -> u32 {
        self.pc
    }

    pub fn set_pc(&mut self, addr: u32) {
        self.set_flag(Flag::DidJump, true); // when setting pc, it must be a jump instr
        self.pc = addr;
    }

    pub fn pop(&mut self, reg_sp: Register) -> Result<u16, String> {
        let sp = self.get_register(reg_sp) - 2;
        let value = self.memeory.read2(sp as u32).map_err(|x| x.to_string())?;
        self.set_register(reg_sp, sp);
        Ok(value)
    }

    pub fn peek(&self, reg_sp: Register) -> Result<u16, String> {
        let sp = self.get_register(reg_sp) - 2;
        self.memeory.read2(sp as u32).map_err(|x| x.to_string())
    }

    pub fn push(&mut self, reg_sp: Register, value: u16) -> Result<(), String> {
        let sp = self.get_register(reg_sp);
        self.set_register(reg_sp, sp + 2);
        self.memeory
            .write2(sp as u32, value)
            .map_err(|x| x.to_string())
    }

    pub fn state(&self) -> String {
        format!(
            "A: {} | B: {} | C: {} | D: {} | SP: {} | M: {} | BP: {} | PC @ {}, Flags: {:016b}",
            self.get_register(Register::A),
            self.get_register(Register::B),
            self.get_register(Register::C),
            self.get_register(Register::D),
            self.get_register(Register::SP),
            self.get_register(Register::M),
            self.get_register(Register::BP),
            self.get_pc(),
            self.flags,
        )
    }

    pub fn step(
        &mut self,
        signal_handlers: &HashMap<u8, Box<dyn SignalHandler>>,
    ) -> Result<(), String> {
        let pc = self.get_pc();
        let instruction = self.memeory.read2(pc).map_err(|x| x.to_string())?;
        self.set_flag(Flag::DidJump, false);
        let op = Instruction::try_from(instruction)?;
        log::info!("running {}", op);
        println!("running {}", op);

        match op {
            Instruction::Invalid => Err("0 instruction".to_string()),
            Instruction::Imm(reg, value) => {
                self.set_register(reg, value.0);
                Ok(())
            }
            // binary ops
            Instruction::Add(dst, r0, r1) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                let (result, overflow) = a.overflowing_add(b);
                self.set_register(dst, result);
                self.set_flag(Flag::OverFlow, overflow);
                Ok(())
            }
            Instruction::Sub(dst, r0, r1) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                let (result, overflow) = a.overflowing_sub(b);
                self.set_register(dst, result);
                self.set_flag(Flag::OverFlow, overflow);
                Ok(())
            }
            Instruction::Mul(dst, r0, r1) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                let (result, overflow) = a.overflowing_mul(b);
                self.set_register(dst, result);
                self.set_flag(Flag::OverFlow, overflow);
                Ok(())
            }
            Instruction::And(dst, r0, r1) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                self.set_register(dst, a & b);
                Ok(())
            }
            Instruction::Or(dst, r0, r1) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                self.set_register(dst, a | b);
                Ok(())
            }
            Instruction::Xor(dst, r0, r1) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                self.set_register(dst, a ^ b);
                Ok(())
            }
            Instruction::Mod(dst, r0, r1) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                self.set_register(dst, a % b);
                Ok(())
            }
            // immediates
            Instruction::AddImm(r, i) => {
                let a = self.get_register(r);
                let (result, _overflow) = a.overflowing_add(i.0 as u16);
                self.set_register(r, result);
                Ok(())
            }
            Instruction::AddImmSigned(r, i) => {
                let raw_register_value = self.get_register(r);
                let imm_signed = i.as_signed();
                let register_signed = raw_register_value as i16;
                let (result, _overflow) = register_signed.overflowing_add(imm_signed as i16);
                self.set_register(r, result as u16);
                Ok(())
            }
            // shift
            Instruction::ShiftLeft(dst, r0, offset) => {
                let base = self.get_register(r0);
                self.set_register(dst, base << (offset.0 as u16));
                Ok(())
            }
            Instruction::ShiftRightLogical(dst, r0, offset) => {
                let base = self.get_register(r0);
                self.set_register(dst, base >> (offset.0 as u16));
                Ok(())
            }
            Instruction::ShiftRightArithmetic(dst, r0, offset) => {
                let base = self.get_register(r0);
                let signed_base = base as i16;
                let shifted = signed_base >> (offset.0 as u16);
                self.set_register(dst, shifted as u16);
                Ok(())
            }
            // load and store
            Instruction::LoadWord(dst, r1, r2) => {
                let base = self.get_register(r1);
                let page = self.get_register(r2);
                let addr = (base as u32) + ((page as u32) << 16);
                let value = self.memeory.read2(addr).map_err(|x| x.to_string())?;
                self.set_register(dst, value);
                Ok(())
            }
            Instruction::LoadByte(dst, r1, r2) => {
                let base = self.get_register(r1);
                let page = self.get_register(r2);
                let addr = (base as u32) + ((page as u32) << 16);
                let value = self.memeory.read(addr).map_err(|x| x.to_string())?;
                self.set_register(dst, value as u16);
                Ok(())
            }
            Instruction::StoreWord(src, r1, r2) => {
                let base = self.get_register(r1);
                let page = self.get_register(r2);
                let addr = (base as u32) + ((page as u32) << 16);
                self.memeory
                    .write2(addr, self.get_register(src))
                    .map_err(|x| x.to_string())
            }
            Instruction::StoreByte(src, r1, r2) => {
                let base = self.get_register(r1);
                let page = self.get_register(r2);
                let addr = (base as u32) + ((page as u32) << 16);
                self.memeory
                    .write(addr, (self.get_register(src) & 0xff) as u8)
                    .map_err(|x| x.to_string())
            }
            // Relation
            Instruction::Test(r0, r1, op) => {
                let a = self.get_register(r0);
                let b = self.get_register(r1);
                let result = match op {
                    RelationOp::Eq => a == b,
                    RelationOp::Neq => a != b,
                    RelationOp::Lt => a < b,
                    RelationOp::Lte => a <= b,
                    RelationOp::Gt => a > b,
                    RelationOp::Gte => a >= b,
                    RelationOp::BothZero => a == 0 && b == 0,
                    RelationOp::EitherNonZero => a != 0 || b != 0,
                    RelationOp::BothNonZero => a != 0 && b != 0,
                };
                self.set_flag(Flag::Compare, result);
                Ok(())
            }
            Instruction::AddIf(r0, r1, offset) => {
                if self.test_flag(Flag::Compare) {
                    self.set_register(r0, self.get_register(r1) + 2 * (offset.0 as u16));
                    self.set_flag(Flag::Compare, false);
                }
                Ok(())
            }
            Instruction::Stack(r0, sp, op) => {
                match op {
                    StackOp::Push => {
                        let value = self.get_register(r0);
                        self.push(sp, value)?;
                    }
                    StackOp::Pop => {
                        let value = self.pop(sp)?;
                        self.set_register(r0, value);
                    }
                    StackOp::Peek => {
                        let value = self.peek(sp)?;
                        self.set_register(r0, value);
                    }
                    StackOp::Dup => {
                        let head = self.peek(sp)?;
                        self.push(sp, head)?;
                    }
                    StackOp::Swap => {
                        let a = self.pop(sp)?;
                        let b = self.pop(sp)?;
                        self.push(sp, a)?;
                        self.push(sp, b)?;
                    }
                    StackOp::Rotate => {
                        let a = self.pop(sp)?;
                        let b = self.pop(sp)?;
                        let c = self.pop(sp)?;
                        self.push(sp, a)?;
                        self.push(sp, c)?;
                        self.push(sp, b)?;
                    }
                    StackOp::Add => {
                        let a = self.pop(sp)?;
                        let b = self.pop(sp)?;
                        self.push(sp, a.wrapping_add(b))?;
                    }
                    StackOp::Sub => {
                        let a = self.pop(sp)?;
                        let b = self.pop(sp)?;
                        self.push(sp, a.wrapping_sub(b))?;
                    }
                    StackOp::PushPC => {
                        // BUG what if bigger then u16?
                        self.push(sp, self.pc as u16)?;
                    }
                };
                Ok(())
            }
            Instruction::LoadStackoffset(dst, sp, word_offset) => {
                let base = self.get_register(sp);
                let addr = base - ((word_offset.0 as u16) * 2);
                let stack_value = self.memeory.read2(addr as u32).map_err(|x| x.to_string())?;
                self.set_register(dst, stack_value);
                Ok(())
            }
            Instruction::Jump(addr) => {
                self.pc = (addr.0 as u32) << 4;
                self.set_flag(Flag::DidJump, true);
                Ok(())
            }
            Instruction::JumpRegister(reg_page, reg_dst) => {
                let page = self.get_register(reg_page);
                let addr = self.get_register(reg_dst);
                let full_addr = ((page as u32) << 16) + addr as u32;
                self.pc = full_addr;
                self.set_flag(Flag::DidJump, true);
                Ok(())
            }
            Instruction::BranchIf(offset) => {
                if self.test_flag(Flag::Compare) {
                    self.pc = self.pc.wrapping_add_signed(offset.as_signed() as i32);
                    self.set_flag(Flag::DidJump, true);
                    self.set_flag(Flag::Compare, false);
                }
                Ok(())
            }
            Instruction::Branch(offset) => {
                self.pc = self.pc.wrapping_add_signed(offset.as_signed() as i32);
                self.set_flag(Flag::DidJump, true);
                Ok(())
            }
            Instruction::BranchRegisterIf(reg_offset, lit_offset) => {
                if self.test_flag(Flag::Compare) {
                    let offset =
                        (self.get_register(reg_offset) as i32) + (lit_offset.as_signed() as i32);
                    self.pc = self.pc.wrapping_add_signed(offset);
                    self.set_flag(Flag::DidJump, true);
                    self.set_flag(Flag::Compare, false);
                }
                Ok(())
            }
            Instruction::System(Register::Zero, reg_arg, signal) => {
                let handler = signal_handlers
                    .get(&signal.0)
                    .ok_or(format!("unknown signal: 0x:{:X}", signal.0))?;
                let arg = self.get_register(reg_arg);
                handler.handle(self, arg)
            }
            Instruction::System(sig, _, arg) => {
                let sig_value = self.get_register(sig);
                if sig_value > 0xff {
                    Err(format!(
                        "unknown signal :0x{:X}, must be <= 0xff",
                        sig_value
                    ))
                } else {
                    let handler = signal_handlers
                        .get(&(sig_value as u8))
                        .ok_or(format!("unknown signal: 0x{:X}", sig_value))?;
                    handler.handle(self, arg.0 as u16)
                }
            }
        }?;

        if !self.test_flag(Flag::DidJump) {
            self.pc += 2;
            self.set_flag(Flag::DidJump, false);
        }
        Ok(())
    }
}

#[derive(Default)]
pub struct Machine {
    signal_handlers: HashMap<u8, Box<dyn SignalHandler>>,
    pub vm: VM,
}

impl Machine {
    pub fn is_halt(&self) -> bool {
        self.vm.halt
    }

    pub fn define_handler(&mut self, index: u8, f: impl SignalHandler + 'static) {
        self.signal_handlers.insert(index, Box::new(f));
    }

    pub fn step(&mut self) -> Result<(), String> {
        self.vm.step(&self.signal_handlers)
    }

    pub fn map(
        &mut self,
        start: usize,
        size: usize,
        a: Box<dyn Addressable>,
    ) -> Result<(), String> {
        self.vm.map(start, size, a)
    }

    pub fn reset(&mut self) {
        self.vm.reset();
    }

    pub fn state(&self) -> String {
        self.vm.state()
    }

    pub fn get_register(&self, r: Register) -> u16 {
        self.vm.get_register(r)
    }

    pub fn set_register(&mut self, r: Register, value: u16) {
        self.vm.set_register(r, value);
    }

    pub fn get_pc(&self) -> u32 {
        self.vm.get_pc()
    }

    pub fn set_pc(&mut self, addr: u32) {
        self.vm.set_pc(addr);
    }

    pub fn set_flag(&mut self, flag: Flag, state: bool) {
        self.vm.set_flag(flag, state);
    }

    pub fn test_flag(&self, flag: Flag) -> bool {
        self.vm.test_flag(flag)
    }
}
