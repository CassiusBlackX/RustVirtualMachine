use std::{cell::RefCell, fmt};

#[derive(Debug)]
pub enum MemoryError {
    OutOfBounds(u32),
    AddressTranslation(u32, Box<MemoryError>),
    NoMap(u32),
    InvalidMap(u32, usize),
    InternalMapperError(u32),
    InternalMapperWithMessage(u32, String),
    ReadOnly,
}

impl fmt::Display for MemoryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OutOfBounds(x) => write!(f, "out of bounds: {:X}", x),
            Self::AddressTranslation(x, e) => write!(f, "translation @{:X}: {}", x, e),
            Self::NoMap(x) => write!(f, "no mapping: {:X}", x),
            Self::InvalidMap(x, i) => write!(f, "invalid mapping index: {:X}, {}", x, i),
            Self::InternalMapperError(x) => write!(f, "internal mapper error @{:X}", x),
            Self::InternalMapperWithMessage(x, s) => {
                write!(f, "internal mapper error @ {:X}: {}", x, s)
            }
            Self::ReadOnly => write!(f, "this memory is read only"),
        }
    }
}

impl From<MemoryError> for String {
    fn from(m: MemoryError) -> Self {
        format!("{}", m)
    }
}

pub trait Addressable {
    fn read(&self, addr: u32) -> Result<u8, MemoryError>;
    fn write(&mut self, addr: u32, value: u8) -> Result<(), MemoryError>;
    fn zero_all(&mut self) -> Result<(), MemoryError>;

    fn read2(&self, addr: u32) -> Result<u16, MemoryError> {
        let x0 = self.read(addr)?;
        let x1 = self.read(addr + 1)?;
        Ok((x0 as u16) | ((x1 as u16) << 8))
    }

    fn write2(&mut self, addr: u32, value: u16) -> Result<(), MemoryError> {
        let lower = value & 0xff;
        let upper = (value & 0xff00) >> 8;
        self.write(addr, lower as u8)?;
        self.write(addr + 1, upper as u8)
    }

    fn copy(&mut self, src: u32, dst: u32, n: usize) -> Result<(), MemoryError> {
        for i in 0..n {
            let data = self.read(src + i as u32)?;
            self.write(dst + i as u32, data)?;
        }
        Ok(())
    }

    fn load_from_vec(&mut self, arr: &[u8], addr: u32) -> Result<(), MemoryError> {
        for (i, byte) in arr.iter().enumerate() {
            self.write(addr + i as u32, *byte)?
        }
        Ok(())
    }

    fn zero(&mut self, src: u32, dst: u32) -> Result<(), MemoryError> {
        for i in src..dst {
            self.write(i, 0)?;
        }
        Ok(())
    }
}

impl<T> Addressable for RefCell<T>
where
    T: Addressable,
{
    fn read(&self, addr: u32) -> Result<u8, MemoryError> {
        self.borrow().read(addr)
    }

    fn write(&mut self, addr: u32, value: u8) -> Result<(), MemoryError> {
        self.borrow_mut().write(addr, value)
    }

    fn zero_all(&mut self) -> Result<(), MemoryError> {
        self.borrow_mut().zero_all()
    }
}

type MemoryRecord = (usize, usize, RefCell<Box<dyn Addressable>>);

#[derive(Default)]
pub struct MemoryMapper {
    pub mapped: Vec<MemoryRecord>,
}

impl MemoryMapper {
    pub fn map(
        &mut self,
        start: usize,
        size: usize,
        val: Box<dyn Addressable>,
    ) -> Result<(), String> {
        self.mapped.push((start, size, RefCell::new(val)));
        Ok(())
    }

    pub fn map_ref(
        &mut self,
        start: usize,
        size: usize,
        val: RefCell<Box<dyn Addressable>>,
    ) -> Result<(), String> {
        self.mapped.push((start, size, val));
        Ok(())
    }

    pub fn get_mapped(&self, addr: u32) -> Option<(usize, usize)> {
        let mut candidate: Option<(usize, usize)> = None;
        for (i, (start, _, _)) in self.mapped.iter().enumerate() {
            if *start <= (addr as usize) {
                if let Some((c, _)) = candidate {
                    if *start > c {
                        candidate = Some((*start, 1));
                    }
                } else {
                    candidate = Some((*start, i))
                }
            }
        }
        candidate
    }
}

impl Addressable for MemoryMapper {
    fn read(&self, addr: u32) -> Result<u8, MemoryError> {
        let (_, i) = self.get_mapped(addr).ok_or(MemoryError::NoMap(addr))?;
        match self.mapped.get(i) {
            Some((start, size, a)) => {
                let addr_local = addr - (*start as u32);
                if addr_local >= *size as u32 {
                    Err(MemoryError::AddressTranslation(
                        addr,
                        Box::new(MemoryError::OutOfBounds(addr_local)),
                    ))
                } else {
                    a.try_borrow()
                        .unwrap()
                        .read(addr_local)
                        .map_err(|e| MemoryError::AddressTranslation(addr, Box::new(e)))
                }
            }
            None => Err(MemoryError::InvalidMap(addr, i)),
        }
    }

    fn write(&mut self, addr: u32, value: u8) -> Result<(), MemoryError> {
        let (_, i) = self.get_mapped(addr).ok_or(MemoryError::NoMap(addr))?;
        match self.mapped.get(i) {
            Some((start, size, a)) => {
                let addr_local = addr - (*start as u32);
                if addr_local >= *size as u32 {
                    Err(MemoryError::AddressTranslation(
                        addr,
                        Box::new(MemoryError::OutOfBounds(addr_local)),
                    ))
                } else {
                    a.try_borrow_mut().unwrap().write(addr_local, value)
                }
            }
            None => Err(MemoryError::InvalidMap(addr, i)),
        }
    }

    fn zero_all(&mut self) -> Result<(), MemoryError> {
        Ok(())
    }
}

pub struct LinearMemory {
    bytes: Vec<u8>,
    size: usize,
}

impl LinearMemory {
    pub fn new(n: usize) -> Self {
        Self {
            bytes: vec![0; n],
            size: n,
        }
    }
}

impl Addressable for LinearMemory {
    fn read(&self, addr: u32) -> Result<u8, MemoryError> {
        if (addr as usize) < self.size {
            Ok(self.bytes[addr as usize])
        } else {
            Err(MemoryError::OutOfBounds(addr))
        }
    }

    fn write(&mut self, addr: u32, value: u8) -> Result<(), MemoryError> {
        if (addr as usize) < self.size {
            self.bytes[addr as usize] = value;
            Ok(())
        } else {
            Err(MemoryError::OutOfBounds(addr))
        }
    }

    fn zero_all(&mut self) -> Result<(), MemoryError> {
        self.zero(0, self.size as u32)
    }
}
