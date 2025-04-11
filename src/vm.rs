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
            // immediates
            Instruction::AddImm(r, i) => {
                let a = self.get_register(r);
                let (result, _overflow) = a.overflowing_add(i.0 as u16);
                self.set_register(r, result);
                Ok(())
            }

            _ => panic!(),
        }
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
