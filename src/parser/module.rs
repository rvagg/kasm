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
    pub data_count: DataCountSection,
    pub custom: CustomSection,
}

impl Module {
    pub fn get_function_name(&self, index: u32) -> Option<&String> {
        for export in &self.exports.exports {
            match export.index {
                ExportIndex::Function(idx) => {
                    if idx == index as u64 {
                        return Some(&export.name);
                    }
                }
                _ => {}
            }
        }
        None
    }
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
    DataSection,
    DataCountSection,
    CustomSection
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

impl SectionToString for ImportSection {
    fn to_header_string(&self) -> String {
        format!(
            "{} count: {}",
            self.position.to_string(),
            self.imports.len()
        )
    }

    fn to_details_string(&self, _: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!("Import[{}]:\n", self.imports.len()));
        for (index, import) in self.imports.iter().enumerate() {
            result.push_str(&format!(" - func[{}] {}\n", index, import));
        }
        result
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
            result.push_str(&format!(
                " - func[{}] sig={}{}\n",
                i,
                function.ftype_index,
                match unit.get_function_name(i as u32) {
                    Some(name) => format!(" <{}>", name),
                    None => "".to_string(),
                }
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

impl SectionToString for TableSection {
    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position.to_string(), self.table.len())
    }

    fn to_details_string(&self, _: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!("Table[{}]:\n", self.table.len()));
        for (i, table_type) in self.table.iter().enumerate() {
            result.push_str(&format!(" - table[{}] {}\n", i, table_type));
        }
        result
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

impl SectionToString for MemorySection {
    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position.to_string(), self.memory.len())
    }

    fn to_details_string(&self, _: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!("Memory[{}]:\n", self.memory.len()));
        for (i, memory) in self.memory.iter().enumerate() {
            result.push_str(&format!(
                " - memory[{}] pages: initial={}{}\n",
                i,
                memory.memory_type.min,
                if memory.memory_type.max < u32::MAX {
                    format!(" max={}", memory.memory_type.max)
                } else {
                    "".to_string()
                }
            ));
        }
        result
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

impl From<&GlobalSection> for Vec<GlobalType> {
    fn from(section: &GlobalSection) -> Self {
        section
            .globals
            .iter()
            .map(|g| g.global_type.clone())
            .collect()
    }
}

impl SectionToString for GlobalSection {
    fn to_header_string(&self) -> String {
        format!(
            "{} count: {}",
            self.position.to_string(),
            self.globals.len()
        )
    }

    fn to_details_string(&self, _: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!("Global[{}]:\n", self.globals.len()));
        for (i, global) in self.globals.iter().enumerate() {
            result.push_str(&format!(
                " - global[{}] {} mutable={} - init {}={}\n",
                i,
                global.global_type.value_type,
                if global.global_type.mutable { 1 } else { 0 },
                global.global_type.value_type,
                global
                    .init
                    .iter()
                    .fold(String::new(), |mut acc, instruction| {
                        // TODO: this doesn't scale .. need to figure out how to handle
                        match instruction.get_type() {
                            ast::InstructionType::I32Const => {
                                if let ast::InstructionData::I32Instruction { value } =
                                    *instruction.get_data()
                                {
                                    acc.push_str(&format!("{}", value as i32));
                                    acc
                                } else {
                                    acc
                                }
                            }
                            ast::InstructionType::I64Const => {
                                if let ast::InstructionData::I64Instruction { value } =
                                    *instruction.get_data()
                                {
                                    acc.push_str(&format!("{}", value as i32));
                                    acc
                                } else {
                                    acc
                                }
                            }
                            _ => acc,
                        }
                    }),
            ));
        }
        result
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
        for export in &self.exports {
            let (typ, idx) = match export.index {
                ExportIndex::Function(i) => ("func", i),
                ExportIndex::Table(i) => ("table", i),
                ExportIndex::Memory(i) => ("mem", i),
                ExportIndex::Global(i) => ("global", i),
            };
            result.push_str(&format!(
                " - {}[{}] <{}> -> \"{}\"\n",
                typ, idx, export.name, export.name
            ));
        }
        result
    }
}

#[derive(Debug)]
pub struct StartSection {
    pub start: u32,
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

impl SectionToString for StartSection {
    fn to_header_string(&self) -> String {
        format!("{} start: {}", self.position.to_string(), self.start)
    }

    fn to_details_string(&self, _: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!("Start:\n - start function: {}\n", self.start));
        result
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

fn init_expr_to_string(
    init: &Vec<ast::Instruction>,
    as_unsigned: bool,
    with_prefix: bool,
) -> String {
    let mut result: String = String::new();
    if with_prefix {
        result.push_str(" - init ");
    }
    match init.len() {
        0 => {
            result.push_str("<EMPTY>\n");
        }
        1 => result.push_str("<INVALID INIT EXPR>\n"),
        2 => match init[0].get_type() {
            // include instruction + end
            ast::InstructionType::I32Const => {
                if let ast::InstructionData::I32Instruction { value } = *init[0].get_data() {
                    if as_unsigned {
                        result.push_str(&format!("i32={}", value as u32));
                    } else {
                        result.push_str(&format!("i32={}", value as i32));
                    }
                }
            }
            ast::InstructionType::I64Const => {
                if let ast::InstructionData::I64Instruction { value } = *init[0].get_data() {
                    if as_unsigned {
                        result.push_str(&format!("i64={}", value as u64));
                    } else {
                        result.push_str(&format!("i64={}", value as i64));
                    }
                }
            }
            ast::InstructionType::F32Const => {
                if let ast::InstructionData::F32Instruction { value } = *init[0].get_data() {
                    result.push_str(&format!("f32={}", value));
                }
            }
            ast::InstructionType::F64Const => {
                if let ast::InstructionData::F64Instruction { value } = *init[0].get_data() {
                    result.push_str(&format!("f64={}", value));
                }
            }
            // TODO: v128
            ast::InstructionType::GlobalGet => {
                if let ast::InstructionData::GlobalInstruction { global_index } =
                    *init[0].get_data()
                {
                    result.push_str(&format!("global={}", global_index));
                }
            }
            ast::InstructionType::RefFunc => {
                if let ast::InstructionData::FunctionInstruction { function_index } =
                    *init[0].get_data()
                {
                    result.push_str(&format!("ref.func:{}", function_index));
                }
            }
            ast::InstructionType::RefNull => {
                if let ast::InstructionData::RefTypeInstruction { ref_type } = *init[0].get_data() {
                    result.push_str(&format!("ref.null {}", ref_type));
                }
            }
            _ => {
                result.push_str("<INVALID>");
            }
        },
        _ => {
            result.push_str("(");
            let mut first = true;
            if let Some(len) = init.len().checked_sub(1) {
                for instruction in init.iter().take(len) {
                    if !first {
                        result.push_str(", ");
                    }
                    first = false;
                    result.push_str(&format!("{}", instruction));
                    match instruction.get_type() {
                        ast::InstructionType::I32Const => {
                            if let ast::InstructionData::I32Instruction { value } =
                                *instruction.get_data()
                            {
                                result.push_str(&format!("{}", value as i32));
                            }
                        }
                        ast::InstructionType::I64Const => {
                            if let ast::InstructionData::I64Instruction { value } =
                                *instruction.get_data()
                            {
                                result.push_str(&format!("{}", value as i32));
                            }
                        }
                        ast::InstructionType::F32Const => {
                            if let ast::InstructionData::F32Instruction { value } =
                                *instruction.get_data()
                            {
                                result.push_str(&format!("{}", value));
                            }
                        }
                        ast::InstructionType::F64Const => {
                            if let ast::InstructionData::F64Instruction { value } =
                                *instruction.get_data()
                            {
                                result.push_str(&format!("{}", value));
                            }
                        }
                        ast::InstructionType::GlobalGet => {
                            if let ast::InstructionData::GlobalInstruction { global_index } =
                                *instruction.get_data()
                            {
                                result.push_str(&format!("{}", global_index));
                                // TODO: global name?
                            }
                        }
                        _ => {
                            result.push_str("<INVALID>");
                        }
                    };
                }
            } else {
                result.push_str("<UNEXPECTED ERR>");
            }
        }
    }
    result
}

impl SectionToString for ElementSection {
    fn to_header_string(&self) -> String {
        format!(
            "{} count: {}",
            self.position.to_string(),
            self.elements.len()
        )
    }

    fn to_details_string(&self, _: &Module) -> String {
        let mut result: String = String::new();
        result.push_str(&format!("Elem[{}]:\n", self.elements.len()));
        for (i, element) in self.elements.iter().enumerate() {
            result.push_str(&format!(
                " - segment[{}] flags={} table={} count={}{}\n",
                i,
                element.flags,
                match element.mode {
                    ElementMode::Passive => 0,
                    ElementMode::Declarative => 0,
                    ElementMode::Active { table_index, .. } => table_index,
                },
                element.init.len(),
                match element.mode {
                    ElementMode::Active { ref offset, .. } =>
                        init_expr_to_string(offset, true, true),
                    _ => "".to_string(),
                }
            ));
            element.init.iter().enumerate().for_each(|(i, _)| {
                // TODO: probably not .. this is simplistic for now, more work to do, i.e. not even printing constant expressions yet
                result.push_str(&format!(
                    "  - elem[{}] = {}\n",
                    i,
                    init_expr_to_string(&element.init[i], false, false),
                ));
            });
        }
        result
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
                i, // TODO: i is wrong when `(func $dummy)` is included - it should skip over these, need to figure out how it knows
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
            let ftype_index = match unit.functions.get(i as u8) {
                Some(f) => f.ftype_index,
                None => {
                    result.push_str(&format!("invalid function index: {}\n", i));
                    continue;
                }
            };
            let ftype = match unit.types.get(ftype_index as u8) {
                Some(t) => t,
                None => {
                    result.push_str(&format!("invalid type index: {}\n", ftype_index));
                    continue;
                }
            };

            let mut pos = function_body.position.start as usize;
            result.push_str(&format!("{:06x} func[{}]{}:\n", pos, i, exp));
            pos += 1; // TODO: do we need more bytes to represent a function start?
                      // for each instruction, ignoring the opcodes for now
                      //  00011f: 20 00                      | local.get 0

            let mut offset = ftype.parameters.len() as u32;
            function_body.locals.iter().for_each(|(count, value_type)| {
                let count_bytes = super::reader::emit_vu32(*count);
                let mut byts = count_bytes.iter().fold(String::new(), |mut acc, byte| {
                    acc.push_str(&format!("{:02x} ", byte));
                    acc
                });
                byts.push_str(&format!("{:02x}", value_type.emit_bytes()[0]));

                let range = match *count {
                    0 => "".to_string(),
                    1 => format!("{}", offset),
                    _ => format!("{}..{}", offset, offset + count - 1),
                };

                result.push_str(&format!(
                    " {:06x}: {:27}| local[{}] type={}\n",
                    pos, byts, range, value_type,
                ));
                pos += count_bytes.len() + 1;
                offset += count;
            });

            let mut indent: usize = 0;
            function_body.instructions.iter().for_each(|instruction| {
                let coding = ast::get_codings_by_type()
                    .get(instruction.get_type())
                    .unwrap_or_else(|| {
                        panic!(
                            "failed to get codings for instruction type: {:?}",
                            instruction.get_type()
                        )
                    });

                match instruction.get_type() {
                    ast::InstructionType::Else | ast::InstructionType::End => {
                        indent = indent.saturating_sub(1)
                    }
                    _ => {}
                }

                // all of the complexity here is to get nice wrapping when we overflow 26 chars in
                // the instruction hex, so we end up with something like this:
                //  000017: 42 80 80 80 80 80 80 80 80 | i64.const -9223372036854775808
                //  000020: 80 7f                      |

                let mut byte_string = String::new();
                let coding_bytes = (coding.emit_bytes)(instruction.get_data());
                let coding_string = (coding.emit_str)(instruction.get_data(), &unit);
                let mut coding_string_added = false;

                let push_format = |byte_string: &String, coding_string_added: bool| -> String {
                    let (indent_string, coding_str) = if coding_string_added {
                        ("".to_string(), "")
                    } else {
                        ("  ".repeat(indent), coding_string.as_str())
                    };
                    format!("{:27}| {}{}\n", byte_string, indent_string, coding_str,)
                };

                // crop_last_2, and related paraphernalia is a hack to handle what appears to be
                // a bug in wabt when it comes to printing subopcodes that are encoded with more
                // bytes than necessary, as per binary-leb128.81.wasm.
                // TODO: figure out why and maybe fix upstream so we can ditch this garbage
                let crop_last_2 = coding_bytes.len() > 2
                    && coding_bytes[0] >= 0xfc
                    && coding.subopcode != /* memory.init gets an out */ 8
                    && coding.subopcode != /* data.drop gets an out */ 9
                    && coding.subopcode != /* memory.copy gets an out */ 10
                    && coding.subopcode != /* memory.fill gets an out */ 11
                    && coding.subopcode != /* table.init gets an out */ 12
                    && coding.subopcode != /* elem.drop gets an out */ 13
                    && coding.subopcode != /* table.copy gets an out */ 14;
                let mut p = pos as u32;
                // if crop_last_2, correct p to account for the skipped bytes
                if crop_last_2 {
                    p += (coding_bytes.len() - 2) as u32;
                }
                result.push_str(&format!(" {:06x}: ", p));

                for (index, byte) in coding_bytes.iter().enumerate() {
                    if !crop_last_2 || index >= coding_bytes.len() - 2 {
                        let new_byte_string = format!("{:02x} ", byte);
                        if byte_string.len() + new_byte_string.len() > 27 {
                            // the case of a byte block that's too long, so we dump the
                            // current batch and loop again, possibly dumping more until
                            // we're within the 26 char limit
                            result.push_str(&push_format(&byte_string, coding_string_added));
                            let mut p = pos as u32;
                            if crop_last_2 {
                                p += (coding_bytes.len() - 2) as u32;
                            }
                            result.push_str(&format!(" {:06x}: ", p));
                            byte_string.clear();
                            coding_string_added = true;
                        }
                        byte_string.push_str(&new_byte_string);
                    }
                    pos += 1;
                }
                result.push_str(&push_format(&byte_string, coding_string_added));

                match instruction.get_type() {
                    ast::InstructionType::Block
                    | ast::InstructionType::Loop
                    | ast::InstructionType::If
                    | ast::InstructionType::Else => indent += 1,
                    _ => {}
                }
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
pub struct DataCountSection {
    pub count: u32,
    pub position: SectionPosition,
}

impl DataCountSection {
    pub fn new() -> DataCountSection {
        DataCountSection {
            count: 0,
            position: SectionPosition::new(0, 0),
        }
    }
}

impl SectionToString for DataCountSection {
    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position.to_string(), self.count)
    }

    fn to_details_string(&self, _: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!("DataCount:\n - data count: {}\n", self.count));
        result
    }
}

#[derive(Debug)]
pub struct CustomSection {
    pub name: String,
    pub data: Vec<u8>,
    pub position: SectionPosition,
}

impl CustomSection {
    pub fn new() -> CustomSection {
        CustomSection {
            name: String::new(),
            data: Vec::new(),
            position: SectionPosition::new(0, 0),
        }
    }
}

impl SectionToString for CustomSection {
    fn to_header_string(&self) -> String {
        format!(
            "{} \"{}\"",
            self.position.to_string(),
            self.name.to_string()
        )
    }

    fn to_details_string(&self, _: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!("Custom:\n - name: \"{}\"\n", self.name));
        result
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
            "({}) -> {}",
            self.parameters
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", "),
            if self.return_types.is_empty() {
                "nil".to_string()
            } else if self.return_types.len() > 1 {
                format!(
                    "({})",
                    self.return_types
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<String>>()
                        .join(", ")
                )
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
    pub external_kind: ExternalKind,
    pub module: String,
    pub name: String,
}

impl fmt::Display for Import {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.external_kind {
            ExternalKind::Function(type_index) => write!(
                f,
                "sig={} <{}.{}> <- {}.{}",
                type_index, self.module, self.name, self.module, self.name
            ),
            _ => panic!("TODO: implement"),
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub ftype_index: u32,
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
pub struct Locals {
    entries: Vec<(u32, ValueType)>,
}

impl Locals {
    // Initialize with a list of count+ValueType pairs
    pub fn new(entries: Vec<(u32, ValueType)>) -> Self {
        Self { entries }
    }

    pub fn empty() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn len(&self) -> u64 {
        self.entries.iter().map(|(count, _)| *count as u64).sum()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, (u32, ValueType)> {
        self.entries.iter()
    }

    pub fn get(&self, index: u32) -> Option<&ValueType> {
        let mut remaining = index;
        for (count, value_type) in &self.entries {
            if remaining < *count {
                return Some(value_type);
            }
            remaining -= count;
        }
        None
    }
}

#[derive(Debug)]
pub struct FunctionBody {
    pub locals: Locals,
    pub instructions: Vec<ast::Instruction>,
    pub position: SectionPosition, // sub-section position
}

#[derive(Debug)]
pub enum RefType {
    FuncRef,
    ExternRef,
}

impl From<&RefType> for ValueType {
    fn from(rt: &RefType) -> Self {
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
            "type={} initial={}{}",
            match self.ref_type {
                RefType::FuncRef => "funcref",
                RefType::ExternRef => "externref",
            },
            self.limits.min,
            if self.limits.max < u32::MAX {
                format!(" max={}", self.limits.max as i32)
            } else {
                "".to_string()
            }
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
    pub flags: u32, // we only keep this for now to match the dump output
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

impl SectionToString for DataSection {
    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position.to_string(), self.data.len())
    }

    fn to_details_string(&self, _: &Module) -> String {
        let mut result: String = String::new();
        result.push_str(&format!("Data[{}]:\n", self.data.len()));
        for (i, data) in self.data.iter().enumerate() {
            result.push_str(&format!(
                " - segment[{}] {} size={}{}\n",
                i,
                match data.mode {
                    DataMode::Passive => "passive".to_string(),
                    DataMode::Active { memory_index, .. } => format!("memory={}", memory_index),
                },
                data.init.len(),
                match data.mode {
                    DataMode::Passive => "".to_string(),
                    DataMode::Active { .. } => {
                        // TODO: need const expression support to do this properly
                        format!(" - init i32=0")
                    }
                }
            ));
            // for data.init byte array, print it in blocks of 8 bytes like so:
            //   - 0000000: 0100 0000 0000 0000 0100 0000 0000 0080  ................
            let mut pos = 0;
            while pos < data.init.len() {
                let mut byts = String::new();
                let mut chars = String::new();
                for i in (0..16).step_by(2) {
                    if pos + i + 1 < data.init.len() {
                        byts.push_str(&format!(
                            "{:02x}{:02x} ",
                            data.init[pos + i],
                            data.init[pos + i + 1]
                        ));
                    } else if pos + i < data.init.len() {
                        byts.push_str(&format!("{:02x}   ", data.init[pos + i]));
                    } else {
                        byts.push_str("     ");
                    }
                }
                for i in 0..16 {
                    if pos + i < data.init.len() {
                        let byte = data.init[pos + i];
                        let ch = if byte.is_ascii_graphic() || byte == /* space */ 32 {
                            byte as char
                        } else {
                            '.'
                        };
                        chars.push(ch);
                    }
                }

                result.push_str(&format!("  - {:07x}: {} {}\n", pos, byts, chars));
                pos += 16;
            }
        }
        result
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
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
    Function(u32), // typeidx
    Table(TableType),
    Memory(Limits),
    Global(GlobalType),
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
                &ExternalKind::Function(ref typeidx) => format!("Function({})", typeidx),
                &ExternalKind::Table(ref table_type) => format!("Table({})", table_type),
                &ExternalKind::Memory(ref limits) => format!("Memory({})", limits),
                &ExternalKind::Global(ref global_type) => format!("Global({})", global_type),
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
            data_count: DataCountSection::new(),
            custom: CustomSection::new(),
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
        if self.table.has_position() {
            result.push_str(&format!("    Table {}\n", self.table.to_header_string()));
        }
        if self.imports.has_position() {
            result.push_str(&format!("   Import {}\n", self.imports.to_header_string()));
        }
        if self.memory.has_position() {
            result.push_str(&format!("   Memory {}\n", self.memory.to_header_string()));
        }
        if self.globals.has_position() {
            result.push_str(&format!("   Global {}\n", self.globals.to_header_string()));
        }
        if self.exports.has_position() {
            result.push_str(&format!("   Export {}\n", self.exports.to_header_string()));
        }
        if self.start.has_position() {
            result.push_str(&format!("    Start {}\n", self.start.to_header_string()));
        }
        if self.elements.has_position() {
            result.push_str(&format!("     Elem {}\n", self.elements.to_header_string()));
        }
        if self.data_count.has_position() {
            result.push_str(&format!(
                "DataCount {}\n",
                self.data_count.to_header_string()
            ));
        }
        if self.code.has_position() {
            result.push_str(&format!("     Code {}\n", self.code.to_header_string()));
        }
        if self.data.has_position() {
            result.push_str(&format!("     Data {}\n", self.data.to_header_string()));
        }
        if self.custom.has_position() {
            result.push_str(&format!("   Custom {}\n", self.custom.to_header_string()));
        }

        result
    }

    fn to_details_string(&self) -> String {
        let mut result = String::new();

        if self.types.has_position() {
            result.push_str(self.types.to_details_string(&self).as_str());
        }
        if self.imports.has_position() {
            result.push_str(self.imports.to_details_string(&self).as_str());
        }
        if self.functions.has_position() {
            result.push_str(self.functions.to_details_string(&self).as_str());
        }
        if self.table.has_position() {
            result.push_str(self.table.to_details_string(&self).as_str());
        }
        if self.memory.has_position() {
            result.push_str(self.memory.to_details_string(&self).as_str());
        }
        if self.globals.has_position() {
            result.push_str(self.globals.to_details_string(&self).as_str());
        }
        if self.exports.has_position() {
            result.push_str(self.exports.to_details_string(&self).as_str());
        }
        if self.start.has_position() {
            result.push_str(self.start.to_details_string(&self).as_str());
        }
        if self.elements.has_position() {
            result.push_str(self.elements.to_details_string(&self).as_str());
        }
        if self.data_count.has_position() {
            result.push_str(self.data_count.to_details_string(&self).as_str());
        }
        if self.code.has_position() {
            result.push_str(self.code.to_details_string(&self).as_str());
        }
        if self.data.has_position() {
            result.push_str(self.data.to_details_string(&self).as_str());
        }
        if self.custom.has_position() {
            result.push_str(self.custom.to_details_string(&self).as_str());
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
        write!(f, "locals = {:?}", self.locals).unwrap();
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
                &ValueType::FuncRef => "funcref",
                &ValueType::ExternRef => "externref",
            }
        )
    }
}
