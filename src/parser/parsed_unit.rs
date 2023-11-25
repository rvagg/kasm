use std::fmt;
use std::io;

use crate::parser::ast;

pub struct ParsedUnit {
    pub name: String,
    pub magic: u32,
    pub version: u32,
    pub function_types: Vec<FunctionType>,
    pub imports: Vec<Import>,
    pub functions: Vec<Function>,
    pub memory: Option<Memory>,
    pub exports: Vec<Export>,
    pub function_bodies: Vec<FunctionBody>,
    pub data: Vec<Data>,
    pub table: Vec<u64>,
    pub start: u64,
}

pub struct FunctionType {
    pub parameters: Vec<ValueType>,
    pub return_types: Vec<ValueType>,
}

pub struct Import {
    pub external_kind: ExternalKind,
    pub module: String,
    pub name: String,
    pub ftype_index: Option<u8>,
    pub memory_type: Option<MemoryType>,
}

pub struct Function {
    pub ftype_index: u8,
}

pub struct Memory {
    pub min: u64,
    pub max: u64,
    pub exported: bool,
}

pub enum ExportIndex {
    Function(u64),
    Table(u64),
    Memory(u64),
    Global(u64),
}
pub struct Export {
    pub index: ExportIndex,
    pub name: String,
}

pub struct FunctionBody {
    pub locals: Vec<ValueType>,
    pub body: Vec<u8>,
    pub instructions: Vec<ast::Instruction>,
}

pub struct Data {
    pub address: u64,
    pub data: Vec<u8>,
}

pub enum Form {
    Function,
}

pub enum ExternalKind {
    Function,
    Table,
    Memory,
    Global,
}

pub struct MemoryType {
    // limits : ResizableLimits
}

/*
pub struct ResizableLimits {
    flags   : u8
  , initial : u64
  , maximum : u64
}
*/

impl ParsedUnit {
    pub fn new(name: &str) -> ParsedUnit {
        ParsedUnit {
            name: name.to_string(),
            magic: 0,
            version: 0,
            function_types: vec![],
            imports: vec![],
            functions: vec![],
            memory: None,
            exports: vec![],
            function_bodies: vec![],
            data: vec![],
            table: vec![],
            start: 0,
        }
    }
}

impl fmt::Display for ParsedUnit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParsedUnit('{}') {{", self.name).unwrap();
        write!(f, "\n  magic number = {}", self.magic).unwrap();
        write!(f, "\n  version = {}", self.version).unwrap();
        write!(f, "\n  function_types").unwrap();
        let mut i = 0;
        for t in &self.function_types {
            write!(f, "\n    {} {}", i, t).unwrap();
            i += 1;
        }
        write!(f, "\n  imports").unwrap();
        i = 0;
        for imp in &self.imports {
            write!(f, "\n    {} {}", i, imp).unwrap();
            i += 1;
        }
        write!(f, "\n  functions").unwrap();
        i = 0;
        for func in &self.functions {
            write!(f, "\n    {} {}", i, func).unwrap();
            i += 1;
        }
        write!(f, "\n  memory: ").unwrap();
        match &self.memory.as_ref() {
            &Some(v) => {
                write!(f, "{}", v).unwrap();
            }
            &None => {
                write!(f, "None").unwrap();
            }
        }
        write!(f, "\n  exports").unwrap();
        i = 0;
        for imp in &self.exports {
            write!(f, "\n    {} {}", i, imp).unwrap();
            i += 1;
        }
        write!(f, "\n  function_bodies").unwrap();
        i = 0;
        for imp in &self.function_bodies {
            write!(f, "\n    {} {}", i, imp).unwrap();
            i += 1;
        }
        write!(f, "\n  data").unwrap();
        i = 0;
        for d in &self.data {
            write!(f, "\n    {} {}", i, d).unwrap();
            i += 1;
        }
        write!(f, "\n  table").unwrap();
        i = 0;
        for fi in &self.table {
            write!(f, "\n    {} function = {}", i, fi).unwrap();
            i += 1;
        }
        write!(f, "\n  start function = {}", &self.start).unwrap();
        write!(f, "\n}}")
    }
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({}) : ({})",
            self.parameters
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", "),
            self.return_types
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl fmt::Display for Import {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}::{} kind = {}",
            self.module, self.name, self.external_kind
        )
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Type({})", self.ftype_index)
    }
}

impl fmt::Display for Memory {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "min = {}, max = {}, exported = {}",
            self.min, self.max, self.exported
        )
    }
}

impl fmt::Display for Export {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let typ = match self.index {
            ExportIndex::Function(i) => format!("Function({})", i),
            ExportIndex::Table(i) => format!("Table({})", i),
            ExportIndex::Memory(i) => format!("Memory({})", i),
            ExportIndex::Global(i) => format!("Global({})", i),
        };
        write!(f, "{} {}", self.name, typ)
    }
}

impl fmt::Display for FunctionBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "locals = ").unwrap();
        f.debug_set().entries(self.locals.iter()).finish().unwrap();
        write!(f, " body = ").unwrap();
        let hex_string = self
            .body
            .iter()
            .map(|byte| format!("{:02x}", byte))
            .collect::<Vec<String>>()
            .join("");
        write!(f, "0x{}", hex_string).unwrap();
        write!(f, " instructions = ").unwrap();
        for instruction in &self.instructions {
            write!(f, "{} ", instruction)?;
        }
        Ok(())
    }
}

impl fmt::Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "address = {} data = ", self.address).unwrap();
        f.debug_set().entries(self.data.iter()).finish()
    }
}

impl ExportIndex {
    pub fn decode(byte: u8, idx: u32) -> Result<ExportIndex, io::Error> {
        match byte {
            0x00 => Ok(ExportIndex::Function(idx as u64)),
            0x01 => Ok(ExportIndex::Table(idx as u64)),
            0x02 => Ok(ExportIndex::Memory(idx as u64)),
            0x03 => Ok(ExportIndex::Global(idx as u64)),
            _ => {
                let msg: String = format!("Invalid export type: {}", byte);
                return Err(io::Error::new(io::ErrorKind::InvalidData, msg));
            }
        }
    }
}

#[derive(PartialEq, Clone, Copy)]
pub enum ValueType {
    // Number types
    I32,
    I64,
    F32,
    F64,
    // Vector types
    V128,
    // Reference types
    FuncRef,
    ExternRef,
}

impl ValueType {
    pub fn is_value_type_byte(byte: u8) -> bool {
        byte == 0x7f
            || byte == 0x7e
            || byte == 0x7d
            || byte == 0x7c
            || byte == 0x7b
            || byte == 0x70
            || byte == 0x6f
    }

    pub fn decode(byte: u8) -> Result<Self, String> {
        match byte {
            0x7f => Ok(ValueType::I32),
            0x7e => Ok(ValueType::I64),
            0x7d => Ok(ValueType::F32),
            0x7c => Ok(ValueType::F64),
            0x7b => Ok(ValueType::V128),
            0x70 => Ok(ValueType::FuncRef),
            0x6f => Ok(ValueType::ExternRef),
            _ => Err(format!("Invalid value type: {}", byte)),
        }
    }
}

impl ExternalKind {
    pub fn decode(byte: u8) -> Result<ExternalKind, io::Error> {
        match byte {
            0x01 => Ok(ExternalKind::Function),
            0x02 => Ok(ExternalKind::Table),
            0x03 => Ok(ExternalKind::Memory),
            0x04 => Ok(ExternalKind::Global),
            _ => {
                let msg: String = format!("Invalid external kind: {}", byte);
                return Err(io::Error::new(io::ErrorKind::InvalidData, msg));
            }
        }
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &ValueType::I32 => "I32",
                &ValueType::I64 => "I64",
                &ValueType::F64 => "F64",
                &ValueType::F32 => "F32",
                &ValueType::V128 => "V128",
                &ValueType::FuncRef => "FuncRef",
                &ValueType::ExternRef => "ExternRef",
            }
        )
    }
}

impl fmt::Debug for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

impl fmt::Display for ExternalKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &ExternalKind::Function => "Function",
                &ExternalKind::Table => "Table",
                &ExternalKind::Memory => "Memory",
                &ExternalKind::Global => "Global",
            }
        )
    }
}

impl fmt::Debug for ExternalKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}
