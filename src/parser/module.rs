use std::fmt;
use std::io;

use crate::parser::ast;

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub magic: u32,
    pub version: u32,

    pub types: TypeSection,
    pub imports: ImportSection,
    pub functions: FunctionSection,
    pub table: TableSection,
    pub memory: MemorySection,
    pub globals: GlobalSection,
    pub exports: ExportSection,
    pub start: StartSection,
    pub elements: ElementSection,
    pub code: CodeSection,
    pub data: DataSection,
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParsedUnit name = {}", self.name)?;
        write!(f, " magic = 0x{:08x}", self.magic)?;
        write!(f, " version = {}", self.version)?;
        write!(f, " types = ").unwrap();
        f.debug_set()
            .entries(self.types.types.iter())
            .finish()
            .unwrap();
        write!(f, " imports = ").unwrap();
        f.debug_set()
            .entries(self.imports.imports.iter())
            .finish()
            .unwrap();
        write!(f, " functions = ").unwrap();
        f.debug_set()
            .entries(self.functions.functions.iter())
            .finish()
            .unwrap();
        write!(f, " table = ").unwrap();
        f.debug_set()
            .entries(self.table.table.iter())
            .finish()
            .unwrap();
        write!(f, " memory = ").unwrap();
        f.debug_set()
            .entries(self.memory.memory.iter())
            .finish()
            .unwrap();
        write!(f, " globals = ").unwrap();
        f.debug_set()
            .entries(self.globals.globals.iter())
            .finish()
            .unwrap();
        write!(f, " exports = ").unwrap();
        f.debug_set()
            .entries(self.exports.exports.iter())
            .finish()
            .unwrap();
        write!(f, " start = {}", self.start.start)?;
        write!(f, " elements = ").unwrap();
        f.debug_set()
            .entries(self.elements.elements.iter())
            .finish()
            .unwrap();
        write!(f, " code = ").unwrap();
        f.debug_set()
            .entries(self.code.code.iter())
            .finish()
            .unwrap();
        write!(f, " data = ").unwrap();
        f.debug_set()
            .entries(self.data.data.iter())
            .finish()
            .unwrap();
        Ok(())
    }
}

#[derive(Debug)]
pub struct SectionPosition {
    pub start: u32,
    pub end: u32,
}

impl SectionPosition {
    pub fn new(start: u32, end: u32) -> SectionPosition {
        SectionPosition { start, end }
    }

    pub fn len(&self) -> u32 {
        self.end - self.start
    }
}

impl ToString for SectionPosition {
    fn to_string(&self) -> String {
        format!(
            "start=0x{:08x} end=0x{:08x} (size=0x{:08x})",
            self.start,
            self.end,
            self.len()
        )
    }
}

pub trait Positional {
    fn set_position(&mut self, start: u32, end: u32);
    fn has_position(&self) -> bool;
}

macro_rules! impl_positional {
    ($($t:ty),*) => {
        $(
            impl Positional for $t {
                fn set_position(&mut self, start: u32, end: u32) {
                    self.position.start = start;
                    self.position.end = end;
                }

                fn has_position(&self) -> bool {
                    self.position.start != 0 || self.position.end != 0
                }
            }
        )*
    }
}

impl_positional!(
    TypeSection,
    ImportSection,
    FunctionSection,
    TableSection,
    MemorySection,
    GlobalSection,
    ExportSection,
    StartSection,
    ElementSection,
    CodeSection,
    DataSection
);

pub trait SectionToString {
    fn to_header_string(&self) -> String;
    fn to_details_string(&self, unit: &Module) -> String;
}

#[derive(Debug)]
pub struct TypeSection {
    pub types: Vec<FunctionType>,
    pub position: SectionPosition,
}

impl TypeSection {
    pub fn new() -> TypeSection {
        TypeSection {
            types: Vec::new(),
            position: SectionPosition::new(0, 0),
        }
    }

    pub fn push(&mut self, function_type: FunctionType) {
        self.types.push(function_type);
    }

    pub fn len(&self) -> usize {
        self.types.len()
    }

    pub fn get(&self, index: u8) -> Option<&FunctionType> {
        self.types.get(index as usize)
    }
}

impl SectionToString for TypeSection {
    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position.to_string(), self.types.len())
    }

    fn to_details_string(&self, _: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!("Type[{}]:\n", self.types.len()));
        for (i, function_type) in self.types.iter().enumerate() {
            result.push_str(&format!(" - type[{}] {}\n", i, function_type));
        }
        result
    }
}

#[derive(Debug)]
pub struct ImportSection {
    pub imports: Vec<Import>,
    pub position: SectionPosition,
}

impl ImportSection {
    pub fn new() -> ImportSection {
        ImportSection {
            imports: Vec::new(),
            position: SectionPosition::new(0, 0),
        }
    }

    pub fn push(&mut self, import: Import) {
        self.imports.push(import);
    }
}

#[derive(Debug)]
pub struct FunctionSection {
    pub functions: Vec<Function>,
    pub position: SectionPosition,
}

impl FunctionSection {
    pub fn new() -> FunctionSection {
        FunctionSection {
            functions: Vec::new(),
            position: SectionPosition::new(0, 0),
        }
    }

    pub fn push(&mut self, function: Function) {
        self.functions.push(function);
    }

    pub fn get(&self, index: u8) -> Option<&Function> {
        self.functions.get(index as usize)
    }
}

impl SectionToString for FunctionSection {
    fn to_header_string(&self) -> String {
        format!(
            "{} count: {}",
            self.position.to_string(),
            self.functions.len()
        )
    }

    fn to_details_string(&self, unit: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!("Function[{}]:\n", self.functions.len()));
        for (i, function) in self.functions.iter().enumerate() {
            let mut exp = String::new();
            for export in &unit.exports.exports {
                match export.index {
                    ExportIndex::Function(idx) => {
                        if idx == i as u64 {
                            exp = format!(" <{}>", export.name);
                        }
                    }
                    _ => {}
                }
            }
            result.push_str(&format!(
                " - func[{}] sig={}{}\n",
                i, function.ftype_index, exp
            ));
        }
        result
    }
}

#[derive(Debug)]
pub struct TableSection {
    pub table: Vec<TableType>,
    pub position: SectionPosition,
}

impl TableSection {
    pub fn new() -> TableSection {
        TableSection {
            table: Vec::new(),
            position: SectionPosition::new(0, 0),
        }
    }
}

#[derive(Debug)]
pub struct MemorySection {
    pub memory: Vec<Memory>,
    pub position: SectionPosition,
}

impl MemorySection {
    pub fn new() -> MemorySection {
        MemorySection {
            memory: Vec::new(),
            position: SectionPosition::new(0, 0),
        }
    }

    pub fn push(&mut self, memory: Memory) {
        self.memory.push(memory);
    }
}

#[derive(Debug)]
pub struct GlobalSection {
    pub globals: Vec<Global>,
    pub position: SectionPosition,
}

impl GlobalSection {
    pub fn new() -> GlobalSection {
        GlobalSection {
            globals: Vec::new(),
            position: SectionPosition::new(0, 0),
        }
    }
}

#[derive(Debug)]
pub struct ExportSection {
    pub exports: Vec<Export>,
    pub position: SectionPosition,
}

impl ExportSection {
    pub fn new() -> ExportSection {
        ExportSection {
            exports: Vec::new(),
            position: SectionPosition::new(0, 0),
        }
    }

    pub fn push(&mut self, export: Export) {
        self.exports.push(export);
    }
}

impl SectionToString for ExportSection {
    fn to_header_string(&self) -> String {
        format!(
            "{} count: {}",
            self.position.to_string(),
            self.exports.len()
        )
    }

    fn to_details_string(&self, _: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!("Export[{}]:\n", self.exports.len()));
        for (i, export) in self.exports.iter().enumerate() {
            let typ = match export.index {
                ExportIndex::Function(_) => "func",
                ExportIndex::Table(_) => "table",
                ExportIndex::Memory(_) => "mem",
                ExportIndex::Global(_) => "global",
            };
            result.push_str(&format!(
                " - {}[{}] <{}> -> \"{}\"\n",
                typ, i, export.name, export.name
            ));
        }
        result
    }
}

#[derive(Debug)]
pub struct StartSection {
    pub start: u64,
    pub position: SectionPosition,
}

impl StartSection {
    pub fn new() -> StartSection {
        StartSection {
            start: 0,
            position: SectionPosition::new(0, 0),
        }
    }
}

#[derive(Debug)]
pub struct ElementSection {
    pub elements: Vec<Element>,
    pub position: SectionPosition,
}

impl ElementSection {
    pub fn new() -> ElementSection {
        ElementSection {
            elements: Vec::new(),
            position: SectionPosition::new(0, 0),
        }
    }

    pub fn push(&mut self, element: Element) {
        self.elements.push(element);
    }
}

#[derive(Debug)]
pub struct CodeSection {
    pub code: Vec<FunctionBody>,
    pub position: SectionPosition,
}

impl CodeSection {
    pub fn new() -> CodeSection {
        CodeSection {
            code: Vec::new(),
            position: SectionPosition::new(0, 0),
        }
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }
}

impl SectionToString for CodeSection {
    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position.to_string(), self.code.len())
    }

    fn to_details_string(&self, unit: &Module) -> String {
        let mut result: String = String::new();
        result.push_str(&format!("Code[{}]:\n", self.code.len()));
        for (i, function_body) in self.code.iter().enumerate() {
            let mut exp = String::new();
            for export in &unit.exports.exports {
                match export.index {
                    ExportIndex::Function(idx) => {
                        if idx == i as u64 {
                            exp = format!(" <{}>", export.name);
                        }
                    }
                    _ => {}
                }
            }
            result.push_str(&format!(
                " - func[{}] size={}{}\n",
                i,
                function_body.position.len(),
                exp
            ));
        }
        result
    }
}

impl CodeSection {
    fn to_disassemble_string(&self, unit: &Module) -> String {
        let mut result: String = String::new();
        for (i, function_body) in self.code.iter().enumerate() {
            let mut exp = String::new();
            for export in &unit.exports.exports {
                match export.index {
                    ExportIndex::Function(idx) => {
                        if idx == i as u64 {
                            exp = format!(" <{}>", export.name);
                        }
                    }
                    _ => {}
                }
            }
            let mut pos = function_body.position.start as usize;
            result.push_str(&format!("{:06x} func[{}]{}:\n", pos, i, exp));
            pos += 1; // TODO: do we need more bytes to represent a function start?
                      // for each instruction, ignoring the opcodes for now
                      //  00011f: 20 00                      | local.get 0
            function_body.instructions.iter().for_each(|instruction| {
                let coding = ast::get_codings_by_type()
                    .get(instruction.get_type())
                    .unwrap_or_else(|| {
                        panic!(
                            "failed to get codings for instruction type: {:?}",
                            instruction.get_type()
                        )
                    });

                // all of the complexity here is to get nice wrapping when we overflow 26 chars in
                // the instruction hex, so we end up with something like this:
                //  000017: 42 80 80 80 80 80 80 80 80 | i64.const -9223372036854775808
                //  000020: 80 7f                      |
                result.push_str(&format!(" {:06x}: ", pos));
                let mut byte_string = String::new();
                let coding_bytes = (coding.emit_bytes)(instruction.get_data());
                let coding_string = (coding.emit_str)(instruction.get_data());
                let mut coding_string_added = false;
                for byte in &coding_bytes {
                    let new_byte_string = format!("{:02x} ", byte);
                    if byte_string.len() + new_byte_string.len() > 27 {
                        result.push_str(&format!(
                            "{:27}| {}\n {:06x}: ",
                            byte_string,
                            if coding_string_added {
                                ""
                            } else {
                                coding_string.as_str()
                            },
                            pos
                        ));
                        byte_string.clear();
                        coding_string_added = true;
                    }
                    byte_string.push_str(&new_byte_string);
                    pos += 1;
                }
                result.push_str(&format!(
                    "{:27}| {}\n",
                    byte_string,
                    if coding_string_added {
                        ""
                    } else {
                        coding_string.as_str()
                    }
                ));
            });
        }
        result
    }
}

#[derive(Debug)]
pub struct DataSection {
    pub data: Vec<Data>,
    pub position: SectionPosition,
}

impl DataSection {
    pub fn new() -> DataSection {
        DataSection {
            data: Vec::new(),
            position: SectionPosition::new(0, 0),
        }
    }
}

#[derive(Debug)]
pub struct FunctionType {
    pub parameters: Vec<ValueType>,
    pub return_types: Vec<ValueType>,
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({}) -> {}", // assumes a single return type
            self.parameters
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", "),
            if self.return_types.is_empty() {
                "nil".to_string()
            } else {
                self.return_types
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            }
        )
    }
}

#[derive(Debug)]
pub struct Import {
    // TODO: this is no longer correct
    pub external_kind: ExternalKind,
    pub module: String,
    pub name: String,
    pub ftype_index: Option<u8>,
    pub memory_type: Option<MemoryType>,
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

#[derive(Debug)]
pub struct Function {
    pub ftype_index: u8,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Type({})", self.ftype_index)
    }
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
    pub position: SectionPosition, // sub-section position
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

impl fmt::Debug for ExternalKind {
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

#[derive(Debug)]
pub struct MemoryType {
    // TODO: fix this, no longer correct
    // limits : ResizableLimits
}

/*
pub struct ResizableLimits {
    flags   : u8
  , initial : u64
  , maximum : u64
}
*/

impl Module {
    pub fn new(name: &str) -> Module {
        Module {
            name: name.to_string(),
            magic: 0,
            version: 0,

            types: TypeSection::new(),
            imports: ImportSection::new(),
            functions: FunctionSection::new(),
            table: TableSection::new(),
            memory: MemorySection::new(),
            globals: GlobalSection::new(),
            exports: ExportSection::new(),
            start: StartSection::new(),
            elements: ElementSection::new(),
            code: CodeSection::new(),
            data: DataSection::new(),
        }
    }
}

#[allow(dead_code)] // used in tests so far
pub enum ParsedUnitFormat {
    Header,
    Details,
    Disassemble,
}

impl Module {
    #[allow(dead_code)] // used in tests
    pub fn to_string(&self, format: ParsedUnitFormat) -> String {
        match format {
            ParsedUnitFormat::Header => self.to_header_string(),
            ParsedUnitFormat::Details => self.to_details_string(),
            ParsedUnitFormat::Disassemble => self.to_disassemble_string(),
        }
    }

    fn to_header_string(&self) -> String {
        let mut result = String::new();

        if self.types.has_position() {
            result.push_str(&format!("     Type {}\n", self.types.to_header_string()));
        }
        if self.functions.has_position() {
            result.push_str(&format!(
                " Function {}\n",
                self.functions.to_header_string()
            ));
        }
        if self.exports.has_position() {
            result.push_str(&format!("   Export {}\n", self.exports.to_header_string()));
        }
        if self.code.has_position() {
            result.push_str(&format!("     Code {}\n", self.code.to_header_string()));
        }

        result
    }

    fn to_details_string(&self) -> String {
        let mut result = String::new();

        if self.types.has_position() {
            result.push_str(self.types.to_details_string(&self).as_str());
        }
        if self.functions.has_position() {
            result.push_str(self.functions.to_details_string(&self).as_str());
        }
        if self.exports.has_position() {
            result.push_str(self.exports.to_details_string(&self).as_str());
        }
        if self.code.has_position() {
            result.push_str(self.code.to_details_string(&self).as_str());
        }

        result
    }

    fn to_disassemble_string(&self) -> String {
        if self.code.has_position() {
            self.code.to_disassemble_string(&self)
        } else {
            String::new()
        }
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

#[derive(PartialEq, Clone, Copy, Debug)]
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

    pub fn emit_bytes(&self) -> Vec<u8> {
        match self {
            &ValueType::I32 => vec![0x7f],
            &ValueType::I64 => vec![0x7e],
            &ValueType::F32 => vec![0x7d],
            &ValueType::F64 => vec![0x7c],
            &ValueType::V128 => vec![0x7b],
            &ValueType::FuncRef => vec![0x70],
            &ValueType::ExternRef => vec![0x6f],
        }
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &ValueType::I32 => "i32",
                &ValueType::I64 => "i64",
                &ValueType::F64 => "f64",
                &ValueType::F32 => "f32",
                &ValueType::V128 => "v128",
                &ValueType::FuncRef => "FuncRef",
                &ValueType::ExternRef => "ExternRef",
            }
        )
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
