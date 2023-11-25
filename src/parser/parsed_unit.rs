use std::fmt;
use std::io;

use base64::write;

use crate::parser::ast;

#[derive(Debug)]
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
    pub table: Vec<TableType>,
    pub start: u64,
    pub elements: Vec<Element>,
}

#[derive(Debug)]
pub struct FunctionType {
    pub parameters: Vec<ValueType>,
    pub return_types: Vec<ValueType>,
}

#[derive(Debug)]
pub struct Import {
    pub external_kind: ExternalKind,
    pub module: String,
    pub name: String,
    pub ftype_index: Option<u8>,
    pub memory_type: Option<MemoryType>,
}

#[derive(Debug)]
pub struct Function {
    pub ftype_index: u8,
}

#[derive(Debug)]
pub struct Memory {
    pub min: u64,
    pub max: u64,
    pub exported: bool,
}

#[derive(Debug)]
pub enum ExportIndex {
    Function(u64),
    Table(u64),
    Memory(u64),
    Global(u64),
}

#[derive(Debug)]
pub struct Export {
    pub index: ExportIndex,
    pub name: String,
}

#[derive(Debug)]
pub struct FunctionBody {
    pub locals: Vec<ValueType>,
    pub instructions: Vec<ast::Instruction>,
}

#[derive(Debug)]
pub struct Data {
    pub address: u64,
    pub data: Vec<u8>,
}

#[derive(Debug)]
pub enum RefType {
    FuncRef,
    ExternRef,
}

impl From<RefType> for ValueType {
    fn from(rt: RefType) -> Self {
        match rt {
            RefType::FuncRef => ValueType::FuncRef,
            RefType::ExternRef => ValueType::ExternRef,
        }
    }
}

#[derive(Debug)]
pub struct Limits {
    pub min: u32,
    pub max: u32, // TODO: is u32::MAX ok for unlimited?
}

impl Limits {
    pub fn new(min: u32, max: u32) -> Limits {
        Limits { min, max }
    }
}

#[derive(Debug)]
pub struct TableType {
    pub ref_type: RefType,
    pub limits: Limits,
}

impl fmt::Display for TableType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "TableType({}) min = {}, max = {}",
            match self.ref_type {
                RefType::FuncRef => "FuncRef",
                RefType::ExternRef => "ExternRef",
            },
            self.limits.min,
            self.limits.max
        )
    }
}

#[derive(Debug)]
pub enum ElementMode {
    Passive,
    Declarative,
    Active {
        table_index: u32,
        offset: Vec<ast::Instruction>,
    },
}

impl fmt::Display for ElementMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ElementMode::Passive => write!(f, "Passive"),
            &ElementMode::Declarative => write!(f, "Declarative"),
            &ElementMode::Active {
                table_index,
                ref offset,
            } => {
                write!(f, "Active {{ table_index = {}, offset =", table_index).unwrap();
                for instruction in offset {
                    write!(f, "{} ", instruction)?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Debug)]
pub struct Element {
    pub ref_type: RefType,
    pub init: Vec<Vec<ast::Instruction>>,
    pub mode: ElementMode,
}

impl fmt::Display for Element {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Element({}) init = [",
            match self.ref_type {
                RefType::FuncRef => "FuncRef",
                RefType::ExternRef => "ExternRef",
            }
        )
        .unwrap();
        for iv in &self.init {
            write!(f, "[")?;
            for instruction in iv {
                write!(f, "{} ", instruction)?;
            }
            write!(f, "] ")?;
        }
        write!(f, "] mode = {}", self.mode)
    }
}

pub enum ExternalKind {
    Function,
    Table,
    Memory,
    Global,
}

#[derive(Debug)]
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
            elements: vec![],
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
        /* TODO: do we want this back? need to intercept the reader
        write!(f, " body = ").unwrap();
        let hex_string = self
            .body
            .iter()
            .map(|byte| format!("{:02x}", byte))
            .collect::<Vec<String>>()
            .join("");
        write!(f, "0x{}", hex_string).unwrap();
         */
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
                let msg: String = format!("invalid export type: {}", byte);
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
            _ => Err(format!("invalid value type: {}", byte)),
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
                let msg: String = format!("invalid external kind: {}", byte);
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
