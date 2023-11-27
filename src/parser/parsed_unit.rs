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
    pub table: Vec<TableType>,
    pub memory: Vec<Memory>,
    pub globals: Vec<Global>,
    pub exports: Vec<Export>,
    pub start: u64,
    pub elements: Vec<Element>,
    pub code: Vec<FunctionBody>,
    pub data: Vec<Data>,
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
    pub memory_type: Limits,
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

impl fmt::Display for Limits {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "min = {}, max = {}", self.min, self.max)
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

#[derive(Debug)]
pub enum DataMode {
    Passive,
    Active {
        memory_index: u32,
        offset: Vec<ast::Instruction>,
    },
}

impl fmt::Display for DataMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &DataMode::Passive => write!(f, "Passive"),
            &DataMode::Active {
                memory_index,
                ref offset,
            } => {
                write!(f, "Active {{ memory_index = {}, offset =", memory_index).unwrap();
                for instruction in offset {
                    write!(f, "{} ", instruction)?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Debug)]
pub struct Data {
    pub init: Vec<u8>,
    pub mode: DataMode,
}

impl fmt::Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Data init = [")?;
        for byte in &self.init {
            write!(f, "{:02x} ", byte)?;
        }
        write!(f, "] mode = {}", self.mode)
    }
}

#[derive(Debug)]
pub struct GlobalType {
    pub value_type: ValueType,
    pub mutable: bool, // const or var
}

impl fmt::Display for GlobalType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "GlobalType({}) {}",
            self.value_type,
            if self.mutable { "var" } else { "const" }
        )
    }
}

#[derive(Debug)]
pub struct Global {
    pub global_type: GlobalType,
    pub init: Vec<ast::Instruction>,
}

impl fmt::Display for Global {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Global global_type = {}", self.global_type)?;
        write!(f, " init = [")?;
        for instruction in &self.init {
            write!(f, "{} ", instruction)?;
        }
        write!(f, "]")
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
            table: vec![],
            memory: vec![],
            globals: vec![],
            exports: vec![],
            start: 0,
            elements: vec![],
            code: vec![],
            data: vec![],
        }
    }
}

impl fmt::Display for ParsedUnit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParsedUnit name = {}", self.name)?;
        write!(f, " magic = 0x{:08x}", self.magic)?;
        write!(f, " version = {}", self.version)?;
        write!(f, " function_types = ").unwrap();
        f.debug_set()
            .entries(self.function_types.iter())
            .finish()
            .unwrap();
        write!(f, " imports = ").unwrap();
        f.debug_set().entries(self.imports.iter()).finish().unwrap();
        write!(f, " functions = ").unwrap();
        f.debug_set()
            .entries(self.functions.iter())
            .finish()
            .unwrap();
        write!(f, " table = ").unwrap();
        f.debug_set().entries(self.table.iter()).finish().unwrap();
        write!(f, " memory = ").unwrap();
        f.debug_set().entries(self.memory.iter()).finish().unwrap();
        write!(f, " globals = ").unwrap();
        f.debug_set().entries(self.globals.iter()).finish().unwrap();
        write!(f, " exports = ").unwrap();
        f.debug_set().entries(self.exports.iter()).finish().unwrap();
        write!(f, " start = {}", self.start)?;
        write!(f, " elements = ").unwrap();
        f.debug_set()
            .entries(self.elements.iter())
            .finish()
            .unwrap();
        write!(f, " code = ").unwrap();
        f.debug_set().entries(self.code.iter()).finish().unwrap();
        write!(f, " data = ").unwrap();
        f.debug_set().entries(self.data.iter()).finish().unwrap();
        Ok(())
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
        write!(f, "Memory({})", self.memory_type)
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
