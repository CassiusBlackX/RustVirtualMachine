use core::fmt;
use std::{collections::HashMap, str::FromStr};

use strum_macros::Display;

use crate::{
    Instruction, Register,
    binfmt::{BinaryFile, Section, SectionMode},
    resolve::{ResolveError, UnResolvedInstruction},
};

#[derive(Debug)]
pub enum PreProcessErr {
    UnknownToken(String),
    MacroEval(String, String),
    ResolveLine(String, ResolveError),
    NoActiveSection,
    Other(String),
}

impl fmt::Display for PreProcessErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownToken(s) => write!(f, "unknown token {}", s),
            Self::MacroEval(name, error) => write!(f, "eval macro {}: {}", name, error),
            Self::ResolveLine(s, e) => write!(f, "resolve line \"{}\": {}", s, e),
            Self::NoActiveSection => write!(f, "no active section"),
            Self::Other(s) => write!(f, "{}", s),
        }
    }
}

type MacroFunc = fn(&mut PreProcessor, input: Vec<&str>) -> Result<Vec<String>, String>;

#[derive(Debug, Clone)]
enum Macro {
    Func(MacroFunc),
    Subst(Vec<String>),
}

#[derive(Debug, Clone, Display)]
enum Variable {
    Label(String),
    User(String),
}

#[derive(Debug, Clone)]
enum Chunk<T> {
    Raw(Vec<u8>),
    Lines(Vec<T>),
}

#[derive(Debug, Clone)]
struct Data<T> {
    offset: u32,
    mode: SectionMode,
    chunks: Vec<Chunk<T>>,
}

impl<T> Data<T> {
    fn new(offset: u32, mode: SectionMode) -> Self {
        Self {
            offset,
            mode,
            chunks: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
enum ProcessedLinePart {
    Body(String),
    Variable(String),
    Label(String),
}

#[derive(Debug, Clone)]
struct ProcessedLine {
    source_line_number: usize,
    line: Vec<ProcessedLinePart>,
}

impl ProcessedLine {
    fn from_str(s: &str, source_line_number: usize) -> Self {
        Self {
            source_line_number,
            line: vec![ProcessedLinePart::Body(s.to_string())],
        }
    }

    fn parse(parts: &[&str], source_line_number: usize) -> Self {
        let mut line = Vec::new();
        for part in parts {
            if part.is_empty() {
                continue;
            }
            if part.chars().nth(0) == Some('!') {
                line.push(ProcessedLinePart::Variable(part[1..].to_string()));
            } else {
                line.push(ProcessedLinePart::Body(part.to_string()));
            }
        }
        Self {
            source_line_number,
            line,
        }
    }

    fn label(s: &str, source_line_number: usize) -> Self {
        Self {
            source_line_number,
            line: vec![ProcessedLinePart::Label(s.to_string())],
        }
    }

    fn get_line_number(&self) -> usize {
        self.source_line_number
    }
}

#[derive(Debug, Default, Clone)]
pub struct PreProcessor {
    entrypoint: u16,
    sections: HashMap<String, Data<ProcessedLine>>,
    heaps: Vec<(u32, u32)>,
    pub variables: HashMap<String, Variable>,
    pub labels: HashMap<String, u32>,
    macros: HashMap<String, Macro>,
    active_section: Option<String>,
}

impl PreProcessor {
    pub fn set_entrypoint(&mut self, entrypoint: u16) {
        self.entrypoint = entrypoint;
    }

    pub fn set_active_section(&mut self, name: &str) {
        self.active_section = Some(name.to_string());
    }

    pub fn create_section(&mut self, name: &str, offset: u32, kind: SectionMode) {
        self.sections
            .insert(name.to_string(), Data::new(offset, kind));
    }

    pub fn create_heap(&mut self, offset: u32, size: u32) {
        self.heaps.push((offset, size));
    }

    pub fn write_section_raw(&mut self, data: &[u8]) {
        if let Some(section_name) = self.active_section.clone() {
            self.sections
                .get_mut(&section_name)
                .unwrap()
                .chunks
                .push(Chunk::Raw(data.to_vec()));
        } else {
            todo!("handle not in section!");
        }
    }

    fn define_label(&mut self, name: &str, value: u32) {
        self.labels.insert(name.to_string(), value);
    }

    fn get_variable(&self, name: &str) -> Option<Variable> {
        self.variables.get(name).cloned()
    }

    pub fn define_user_variable(&mut self, name: &str, value: &str) {
        self.variables
            .insert(name.to_string(), Variable::User(value.to_string()));
    }

    fn get_macro(&self, name: &str) -> Option<&Macro> {
        self.macros.get(name)
    }

    pub fn define_macro(&mut self, name: &str, value: MacroFunc) {
        self.macros.insert(name.to_string(), Macro::Func(value));
    }

    pub fn define_subst_macro(&mut self, name: &str, value: Vec<String>) {
        self.macros.insert(name.to_string(), Macro::Subst(value));
    }

    fn define_labels(
        &mut self,
        sections: &HashMap<String, Data<UnResolvedInstruction>>,
    ) -> Result<(), PreProcessErr> {
        // FIXME: we never generate an err in the function
        for section in sections.values() {
            let mut head = section.offset;
            for chunk in &section.chunks {
                match chunk {
                    Chunk::Raw(v) => head += v.len() as u32,
                    Chunk::Lines(unresolved_instrs) => {
                        for instr in unresolved_instrs {
                            if let UnResolvedInstruction::Label(name) = instr {
                                self.define_label(name, head);
                            }
                            head += instr.size();
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn get_unresolved_instructions(
        &self,
    ) -> Result<HashMap<String, Data<UnResolvedInstruction>>, PreProcessErr> {
        let mut out = HashMap::new();
        for (section_name, data) in &self.sections {
            let mut new_chunks = Vec::new();
            for chunk in &data.chunks {
                match chunk {
                    Chunk::Lines(lines) => {
                        let mut result = Vec::new();
                        for line in lines.iter() {
                            if let Some(ur_instr) = self.part_resolve_line(&line.line)? {
                                result.push(ur_instr);
                            }
                        }
                        new_chunks.push(Chunk::Lines(result))
                    }
                    Chunk::Raw(x) => new_chunks.push(Chunk::Raw(x.to_vec())),
                }
            }
            out.insert(
                section_name.to_string(),
                Data {
                    offset: data.offset,
                    mode: data.mode,
                    chunks: new_chunks,
                },
            );
        }
        Ok(out)
    }

    fn dealias_instruction(part: ProcessedLinePart) -> ProcessedLinePart {
        if let ProcessedLinePart::Body(instr) = part {
            ProcessedLinePart::Body(
                match instr.to_uppercase().as_str() {
                    "ADDIMMS" => "ADDIMMSIGNED",
                    "SHIFTL" => "SHIFTLEFT",
                    "SHIFTRG" => "SHIFTRIGHTLOGICAL",
                    "SHIFTRA" => "SHIFTRIGHTARITHMETIC",
                    "LOADSTACK" => "LOADSTACIOFFSET",
                    "JUMPR" => "JUMPREGISTER",
                    "BRANCHIFR" => "BRANCHREGISTERIF",
                    x => x,
                }
                .to_string(),
            )
        } else {
            part
        }
    }

    fn try_parse_unresolved_instruction(
        first: &ProcessedLinePart,
        parts: &[ProcessedLinePart],
    ) -> Option<UnResolvedInstruction> {
        if parts.len() != 2 {
            return None;
        }
        if let ProcessedLinePart::Body(head) = first {
            if let ProcessedLinePart::Body(reg_str) = parts.first().unwrap() {
                if let ProcessedLinePart::Variable(label) = parts.get(1).unwrap() {
                    let reg = Register::from_str(reg_str).ok()?;
                    return match head.as_str() {
                        "Imm" => Some(UnResolvedInstruction::Imm(reg, label.to_string())),
                        "AddImm" => Some(UnResolvedInstruction::AddImm(reg, label.to_string())),
                        "AddImmSigned" => {
                            Some(UnResolvedInstruction::AddImmSigned(reg, label.to_string()))
                        }
                        _ => None,
                    };
                }
            }
        }
        None
    }

    fn part_resolve_line(
        &self,
        parts: &[ProcessedLinePart],
    ) -> Result<Option<UnResolvedInstruction>, PreProcessErr> {
        let head = match parts.first() {
            Some(x) => x,
            None => return Ok(None),
        };

        match head {
            ProcessedLinePart::Body(s) => {
                if s.starts_with(';') {
                    // skip comments
                    return Ok(None);
                }

                if parts.len() > 1 {
                    let dealiased = Self::dealias_instruction(head.clone());
                    if let Some(ur_instr) =
                        Self::try_parse_unresolved_instruction(&dealiased, &parts[1..])
                    {
                        return Ok(Some(ur_instr));
                    } else {
                        let instr_str = parts
                            .iter()
                            .map(|s| match s {
                                ProcessedLinePart::Body(x) => Ok(x.to_string()),
                                ProcessedLinePart::Label(_) => Ok("".to_string()),
                                ProcessedLinePart::Variable(v) => self
                                    .get_variable(v)
                                    .map(|x| x.to_string())
                                    .ok_or(PreProcessErr::UnknownToken(v.to_string())),
                            })
                            .collect::<Result<Vec<_>, _>>()?
                            .join(" ");
                        let instr = Instruction::from_str(&instr_str).map_err(|e| {
                            PreProcessErr::Other(format!("invalid instruction: {:?}", e))
                        })?;
                        return Ok(Some(UnResolvedInstruction::Instruction(instr)));
                    }
                }
                Ok(None)
            }
            ProcessedLinePart::Label(l) => Ok(Some(UnResolvedInstruction::Label(l.to_string()))),
            ProcessedLinePart::Variable(_) => Err(PreProcessErr::Other(
                "invalid variable in first position".to_string(),
            )),
        }
    }

    fn resolve_line(
        &mut self,
        line: &str,
        line_number: usize,
    ) -> Result<Vec<ProcessedLine>, PreProcessErr> {
        let parts: Vec<_> = line.split_whitespace().collect();
        if parts.is_empty() {
            return Ok(Vec::new());
        }

        let head = parts.first().unwrap();
        match head.chars().nth(0) {
            Some(';') => Ok(vec![ProcessedLine::from_str(line, line_number)]),
            Some('.') => {
                let name = &head[1..];
                let func = self
                    .get_macro(name)
                    .ok_or(PreProcessErr::UnknownToken(head[1..].to_string()))?;
                let macro_result = match func {
                    Macro::Func(f) => f(self, parts[1..].to_vec())
                        .map_err(|x| PreProcessErr::MacroEval(name.to_string(), x))?,
                    Macro::Subst(lines) => lines
                        .iter()
                        .map(|line| {
                            let mp: Result<Vec<String>, String> = line
                                .split(' ')
                                .map(|p| match p.chars().nth(0) {
                                    Some('|') => match p[1..].parse::<u32>() {
                                        Ok(n) => parts
                                            .get((n + 1) as usize)
                                            .ok_or(format!("subst {}: out of bounds", p))
                                            .map(|x| x.to_string()),
                                        Err(_) => Ok(p.to_string()),
                                    },
                                    _ => Ok(p.to_string()),
                                })
                                .collect();
                            match mp {
                                Ok(s) => s.join(" "),
                                Err(e) => format!("err: {}", e),
                            }
                        })
                        .collect(),
                };
                let b: Result<Vec<Vec<ProcessedLine>>, PreProcessErr> = macro_result
                    .iter()
                    .map(|x| self.resolve_line(x, line_number))
                    .collect();
                let mut out = Vec::new();
                for proc in b? {
                    out.extend(proc);
                }
                Ok(out)
            }
            Some(':') => Ok(vec![ProcessedLine::label(&head[1..], line_number)]),
            _ => Ok(vec![ProcessedLine::parse(&parts, line_number)]),
        }
    }

    pub fn handle(&mut self, input: &str) -> Result<(), PreProcessErr> {
        for (i, line) in input.lines().enumerate() {
            let resolved = self.resolve_line(line, i)?;
            if !resolved.is_empty() {
                if let Some(section_name) = self.active_section.clone() {
                    let chunks = &mut self.sections.get_mut(&section_name).unwrap().chunks;
                    chunks.push(Chunk::Lines(resolved));
                } else {
                    return Err(PreProcessErr::NoActiveSection);
                }
            }
        }
        Ok(())
    }
}

impl TryFrom<PreProcessor> for BinaryFile {
    type Error = PreProcessErr;

    fn try_from(mut value: PreProcessor) -> Result<Self, Self::Error> {
        let sections = value.get_unresolved_instructions()?;
        value.define_labels(&sections)?;
        let mut bin = BinaryFile {
            entrypoint: 0,
            version: 99,
            ..BinaryFile::default()
        };

        for (_name, section) in sections {
            let mut section_data: Vec<u8> = Vec::new();
            for chunk in section.chunks {
                match chunk {
                    Chunk::Raw(v) => section_data.extend(v),
                    Chunk::Lines(lines) => {
                        for line in lines {
                            let instr_result = line
                                .resolve(section.offset + section_data.len() as u32, &value.labels)
                                .map_err(|e| PreProcessErr::ResolveLine(line.to_string(), e))?;
                            if let Some(instr) = instr_result {
                                section_data.extend_from_slice(&instr.encode_u16().to_le_bytes());
                            }
                        }
                    }
                }
            }
            bin.sections.push(Section {
                size: section_data.len() as u16,
                mode: section.mode,
                address: section.offset,
                file_offset: bin.data.len() as u32,
            });
            bin.data.extend(section_data);
        }
        let header_size = bin.get_header_size() as u32;
        for section in bin.sections.iter_mut() {
            section.file_offset += header_size;
        }

        Ok(bin)
    }
}
