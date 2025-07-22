use std::fmt;
use std::io;

use fhex::ToHex;

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
    pub custom: Vec<CustomSection>,
}

impl Module {
    pub fn get_function_type(&self, index: u32) -> Option<&FunctionType> {
        self.functions
            .get(index as u8)
            .and_then(|f| self.types.get(f.ftype_index as u8))
    }

    pub fn get_table(&self, index: u32) -> Option<&TableType> {
        self.imports
            .get_table(index)
            .or_else(|| self.table.tables.get(index as usize))
    }

    pub fn get_function_name(&self, index: u32) -> Option<String> {
        // Check if it's an imported function
        let import_count = self.imports.function_count() as u32;
        if index < import_count {
            // It's an imported function
            let mut func_idx = 0;
            for import in &self.imports.imports {
                if let ExternalKind::Function(_) = import.external_kind {
                    if func_idx == index {
                        return Some(format!("{}.{}", import.module, import.name));
                    }
                    func_idx += 1;
                }
            }
        }

        // Check if it's exported
        if let Some(export) = self.exports.get_function(index) {
            return Some(export.name.clone());
        }

        None
    }

    pub fn get_global_name(&self, index: u32) -> Option<String> {
        // Check if it's an imported global
        let import_count = self.imports.global_count() as u32;
        if index < import_count {
            // It's an imported global
            let mut global_idx = 0;
            for import in &self.imports.imports {
                if let ExternalKind::Global(_) = import.external_kind {
                    if global_idx == index {
                        return Some(format!("{}.{}", import.module, import.name));
                    }
                    global_idx += 1;
                }
            }
        }

        // Check if it's exported
        if let Some(export) = self
            .exports
            .exports
            .iter()
            .find(|e| matches!(e.index, ExportIndex::Global(idx) if idx == index))
        {
            return Some(export.name.clone());
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
        f.debug_set().entries(self.types.types.iter()).finish().unwrap();
        write!(f, " imports = ").unwrap();
        f.debug_set().entries(self.imports.imports.iter()).finish().unwrap();
        write!(f, " functions = ").unwrap();
        f.debug_set().entries(self.functions.functions.iter()).finish().unwrap();
        write!(f, " table = ").unwrap();
        f.debug_set().entries(self.table.tables.iter()).finish().unwrap();
        write!(f, " memory = ").unwrap();
        f.debug_set().entries(self.memory.memory.iter()).finish().unwrap();
        write!(f, " globals = ").unwrap();
        f.debug_set().entries(self.globals.globals.iter()).finish().unwrap();
        write!(f, " exports = ").unwrap();
        f.debug_set().entries(self.exports.exports.iter()).finish().unwrap();
        write!(f, " start = {}", self.start.start)?;
        write!(f, " elements = ").unwrap();
        f.debug_set().entries(self.elements.elements.iter()).finish().unwrap();
        write!(f, " code = ").unwrap();
        f.debug_set().entries(self.code.code.iter()).finish().unwrap();
        write!(f, " data = ").unwrap();
        f.debug_set().entries(self.data.data.iter()).finish().unwrap();
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

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl fmt::Display for SectionPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
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
    fn get_position(&self) -> Option<SectionPosition>;
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
                fn get_position(&self) -> Option<SectionPosition> {
                    if self.has_position() {
                        Some(SectionPosition::new(self.position.start, self.position.end))
                    } else {
                        None
                    }
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
    fn header_prefix(&self) -> &'static str;
    fn to_header_string(&self) -> String;
    fn to_details_string(&self, unit: &Module) -> String;
}

#[derive(Debug)]
pub struct TypeSection {
    pub types: Vec<FunctionType>,
    pub position: SectionPosition,
}

impl Default for TypeSection {
    fn default() -> Self {
        Self::new()
    }
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

    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }

    pub fn get(&self, index: u8) -> Option<&FunctionType> {
        self.types.get(index as usize)
    }

    pub fn find(&self, function_type: &FunctionType) -> Option<u32> {
        self.types.iter().position(|t| t == function_type).map(|i| i as u32)
    }
}

impl SectionToString for TypeSection {
    fn header_prefix(&self) -> &'static str {
        "Type"
    }

    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position, self.types.len())
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

impl Default for ImportSection {
    fn default() -> Self {
        Self::new()
    }
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

    pub fn function_count(&self) -> usize {
        self.imports
            .iter()
            .filter(|i| matches!(i.external_kind, ExternalKind::Function(_)))
            .count()
    }

    pub fn get_function_type_index(&self, index: u32) -> Option<u32> {
        self.imports
            .iter()
            .filter_map(|i| match i.external_kind {
                ExternalKind::Function(ref f) => Some(*f),
                _ => None,
            })
            .nth(index as usize)
    }

    pub fn table_count(&self) -> usize {
        self.imports
            .iter()
            .filter(|i| matches!(i.external_kind, ExternalKind::Table(_)))
            .count()
    }

    pub fn get_table_import(&self, index: u32) -> Option<&Import> {
        // return the index'th table, if we have that many
        self.imports
            .iter()
            .filter(|i| matches!(i.external_kind, ExternalKind::Table(_)))
            .nth(index as usize)
    }

    pub fn get_table(&self, index: u32) -> Option<&TableType> {
        // return the index'th table, if we have that many
        self.get_table_import(index).and_then(|i| match i.external_kind {
            ExternalKind::Table(ref t) => Some(t),
            _ => None,
        })
    }

    pub fn memory_count(&self) -> usize {
        self.imports
            .iter()
            .filter(|i| matches!(i.external_kind, ExternalKind::Memory(_)))
            .count()
    }

    pub fn global_count(&self) -> usize {
        self.imports
            .iter()
            .filter(|i| matches!(i.external_kind, ExternalKind::Global(_)))
            .count()
    }

    pub fn get_global_import(&self, index: u32) -> Option<&Import> {
        // return the index'th global, if we have that many
        self.imports
            .iter()
            .filter(|i| matches!(i.external_kind, ExternalKind::Global(_)))
            .nth(index as usize)
            .and_then(|i| match i.external_kind {
                ExternalKind::Global(_) => Some(i),
                _ => None,
            })
    }
}

impl SectionToString for ImportSection {
    fn header_prefix(&self) -> &'static str {
        "Import"
    }

    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position, self.imports.len())
    }

    fn to_details_string(&self, unit: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!("Import[{}]:\n", self.imports.len()));
        let mut function_count = 0;
        let mut table_count = 0;
        let mut memory_count = 0;
        let mut global_count = 0;

        for import in &self.imports {
            match import.external_kind {
                ExternalKind::Function(ref f) => {
                    // Check if this function is re-exported
                    // Use the last export if there are multiple (for test compatibility)
                    let display_name = if let Some(export) = unit
                        .exports
                        .exports
                        .iter()
                        .rev()
                        .find(|e| matches!(e.index, ExportIndex::Function(idx) if idx == function_count as u32))
                    {
                        export.name.clone()
                    } else {
                        format!("{}.{}", import.module, import.name)
                    };

                    result.push_str(&format!(
                        " - func[{}] sig={} <{}> <- {}.{}\n",
                        function_count, f, display_name, import.module, import.name
                    ));
                    function_count += 1;
                }
                ExternalKind::Table(ref t) => {
                    result.push_str(&format!(
                        " - table[{}] type={} initial={}{} <- {}.{}\n",
                        table_count,
                        match t.ref_type {
                            RefType::FuncRef => "funcref",
                            RefType::ExternRef => "externref",
                        },
                        t.limits.min,
                        if t.limits.max < u32::MAX {
                            format!(" max={}", t.limits.max as i32)
                        } else {
                            "".to_string()
                        },
                        import.module,
                        import.name,
                    ));
                    table_count += 1;
                }
                ExternalKind::Memory(ref m) => {
                    result.push_str(&format!(
                        " - memory[{}] pages: initial={}{} <- {}.{}\n",
                        memory_count,
                        m.min,
                        if m.max < u32::MAX {
                            format!(" max={}", m.max)
                        } else {
                            "".to_string()
                        },
                        import.module,
                        import.name
                    ));
                    memory_count += 1;
                }
                ExternalKind::Global(ref g) => {
                    result.push_str(&format!(
                        " - global[{}] {} mutable={} <- {}.{}\n",
                        global_count,
                        g.value_type,
                        if g.mutable { 1 } else { 0 },
                        import.module,
                        import.name
                    ));
                    global_count += 1;
                }
            }
        }
        result
    }
}

#[derive(Debug)]
pub struct FunctionSection {
    pub functions: Vec<Function>,
    pub position: SectionPosition,
}

impl Default for FunctionSection {
    fn default() -> Self {
        Self::new()
    }
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

    pub fn len(&self) -> usize {
        self.functions.len()
    }

    pub fn is_empty(&self) -> bool {
        self.functions.is_empty()
    }

    pub fn get(&self, index: u8) -> Option<&Function> {
        self.functions.get(index as usize)
    }
}

impl SectionToString for FunctionSection {
    fn header_prefix(&self) -> &'static str {
        "Function"
    }

    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position, self.functions.len())
    }

    fn to_details_string(&self, unit: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!("Function[{}]:\n", self.functions.len()));
        for (i, function) in self.functions.iter().enumerate() {
            let fi = unit.imports.function_count() + i;
            result.push_str(&format!(
                " - func[{}] sig={}{}\n",
                fi,
                function.ftype_index,
                match unit.exports.get_function(fi as u32) {
                    Some(export) => format!(" <{}>", export.name),
                    None => "".to_string(),
                }
            ));
        }
        result
    }
}

#[derive(Debug)]
pub struct TableSection {
    pub tables: Vec<TableType>,
    pub position: SectionPosition,
}

impl Default for TableSection {
    fn default() -> Self {
        Self::new()
    }
}

impl TableSection {
    pub fn new() -> TableSection {
        TableSection {
            tables: Vec::new(),
            position: SectionPosition::new(0, 0),
        }
    }

    pub fn push(&mut self, table: TableType) {
        self.tables.push(table);
    }

    pub fn len(&self) -> usize {
        self.tables.len()
    }

    pub fn is_empty(&self) -> bool {
        self.tables.is_empty()
    }
}

impl SectionToString for TableSection {
    fn header_prefix(&self) -> &'static str {
        "Table"
    }

    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position, self.tables.len())
    }

    fn to_details_string(&self, unit: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!("Table[{}]:\n", self.tables.len()));
        let import_table_count = unit.imports.table_count();
        for (i, table_type) in self.tables.iter().enumerate() {
            result.push_str(&format!(" - table[{}] {}\n", import_table_count + i, table_type));
        }
        result
    }
}

#[derive(Debug)]
pub struct MemorySection {
    pub memory: Vec<Memory>,
    pub position: SectionPosition,
}

impl Default for MemorySection {
    fn default() -> Self {
        Self::new()
    }
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

    pub fn len(&self) -> usize {
        self.memory.len()
    }

    pub fn is_empty(&self) -> bool {
        self.memory.is_empty()
    }
}

impl SectionToString for MemorySection {
    fn header_prefix(&self) -> &'static str {
        "Memory"
    }

    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position, self.memory.len())
    }

    fn to_details_string(&self, _: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!("Memory[{}]:\n", self.memory.len()));
        for (i, memory) in self.memory.iter().enumerate() {
            result.push_str(&format!(
                " - memory[{}] pages: initial={}{}\n",
                i,
                memory.limits.min,
                if memory.limits.max < u32::MAX {
                    format!(" max={}", memory.limits.max)
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

impl Default for GlobalSection {
    fn default() -> Self {
        Self::new()
    }
}

impl GlobalSection {
    pub fn new() -> GlobalSection {
        GlobalSection {
            globals: Vec::new(),
            position: SectionPosition::new(0, 0),
        }
    }

    pub fn push(&mut self, global: Global) {
        self.globals.push(global);
    }

    pub fn len(&self) -> usize {
        self.globals.len()
    }

    pub fn is_empty(&self) -> bool {
        self.globals.is_empty()
    }

    pub fn find(&self, global: &Global) -> Option<u32> {
        self.globals.iter().position(|g| g == global).map(|i| i as u32)
    }

    pub fn get(&self, index: u32) -> Option<&Global> {
        self.globals.get(index as usize)
    }
}

impl From<&GlobalSection> for Vec<GlobalType> {
    fn from(section: &GlobalSection) -> Self {
        section.globals.iter().map(|g| g.global_type).collect()
    }
}

impl SectionToString for GlobalSection {
    fn header_prefix(&self) -> &'static str {
        "Global"
    }

    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position, self.globals.len())
    }

    fn to_details_string(&self, unit: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!("Global[{}]:\n", self.globals.len()));
        for (i, global) in self.globals.iter().enumerate() {
            result.push_str(&format!(
                " - global[{}] {} mutable={}{}{}\n",
                i,
                global.global_type.value_type,
                if global.global_type.mutable { 1 } else { 0 },
                match unit.exports.get_global(i as u32) {
                    Some(e) => format!(" <{}>", e.name),
                    None => "".to_string(),
                },
                init_expr_to_string(unit, &global.init, false, true),
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

impl Default for ExportSection {
    fn default() -> Self {
        Self::new()
    }
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

    pub fn get_global(&self, global_index: u32) -> Option<&Export> {
        self.exports
            .iter()
            .rev()
            .find(|e| matches!(e.index, ExportIndex::Global(idx) if idx == global_index))
    }

    pub fn get_function(&self, function_index: u32) -> Option<&Export> {
        self.exports
            .iter()
            .rev()
            .find(|e| matches!(e.index, ExportIndex::Function(idx) if idx == function_index))
    }
}

impl SectionToString for ExportSection {
    fn header_prefix(&self) -> &'static str {
        "Export"
    }

    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position, self.exports.len())
    }

    fn to_details_string(&self, unit: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!("Export[{}]:\n", self.exports.len()));
        for export in &self.exports {
            let (typ, idx) = match export.index {
                ExportIndex::Function(i) => ("func", i),
                ExportIndex::Table(i) => ("table", i),
                ExportIndex::Memory(i) => ("memory", i),
                ExportIndex::Global(i) => ("global", i),
            };
            result.push_str(&format!(
                " - {}[{}]{} -> \"{}\"\n",
                typ,
                idx,
                match export.index {
                    ExportIndex::Function(func_idx) => {
                        match unit.exports.get_function(func_idx) {
                            Some(export) => format!(" <{}>", export.name),
                            None => "".to_string(),
                        }
                    }
                    _ => "".to_string(),
                },
                export.name
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

impl Default for StartSection {
    fn default() -> Self {
        Self::new()
    }
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
    fn header_prefix(&self) -> &'static str {
        "Start"
    }

    fn to_header_string(&self) -> String {
        format!("{} start: {}", self.position, self.start)
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

impl Default for ElementSection {
    fn default() -> Self {
        Self::new()
    }
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

fn init_expr_to_offset(init: &[ast::Instruction]) -> Option<u32> {
    if init.len() != 2 {
        // const + end
        return None;
    }
    match init[0].get_type() {
        // include instruction + end
        ast::InstructionType::I32Const => {
            if let ast::InstructionData::I32 { value } = *init[0].get_data() {
                Some(value)
            } else {
                None
            }
        }
        ast::InstructionType::GlobalGet => Some(0),
        _ => None,
    }
}

fn init_expr_to_string(unit: &Module, init: &[ast::Instruction], as_unsigned: bool, with_prefix: bool) -> String {
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
                if let ast::InstructionData::I32 { value } = *init[0].get_data() {
                    if as_unsigned {
                        result.push_str(&format!("i32={}", { value }));
                    } else {
                        result.push_str(&format!("i32={}", value as i32));
                    }
                }
            }
            ast::InstructionType::I64Const => {
                if let ast::InstructionData::I64 { value } = *init[0].get_data() {
                    if as_unsigned {
                        result.push_str(&format!("i64={}", { value }));
                    } else {
                        result.push_str(&format!("i64={}", value as i64));
                    }
                }
            }
            ast::InstructionType::F32Const => {
                if let ast::InstructionData::F32 { value } = *init[0].get_data() {
                    result.push_str(&format!("f32={}", value.to_hex()));
                }
            }
            ast::InstructionType::F64Const => {
                if let ast::InstructionData::F64 { value } = *init[0].get_data() {
                    result.push_str(&format!("f64={}", value.to_hex()));
                }
            }
            // TODO: v128
            ast::InstructionType::GlobalGet => {
                if let ast::InstructionData::Global { global_index } = *init[0].get_data() {
                    match unit.imports.get_global_import(global_index) {
                        Some(import) => {
                            result.push_str(&format!("global={} <{}.{}>", global_index, import.module, import.name));
                        }
                        None => {
                            result.push_str(&format!("global={} <INVALID>", global_index));
                        }
                    }
                }
            }
            ast::InstructionType::RefFunc => {
                if let ast::InstructionData::Function { function_index } = *init[0].get_data() {
                    result.push_str(&format!(
                        "ref.func:{}{}",
                        function_index,
                        match unit.get_function_name(function_index) {
                            Some(name) => format!(" <{}>", name),
                            None => "".to_string(),
                        }
                    ));
                }
            }
            ast::InstructionType::RefNull => {
                if let ast::InstructionData::RefType { ref_type } = *init[0].get_data() {
                    result.push_str(&format!("ref.null {}", ref_type));
                }
            }
            _ => {
                result.push_str("<INVALID>");
            }
        },
        _ => {
            result.push('(');
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
                            if let ast::InstructionData::I32 { value } = *instruction.get_data() {
                                result.push_str(&format!("{}", value as i32));
                            }
                        }
                        ast::InstructionType::I64Const => {
                            if let ast::InstructionData::I64 { value } = *instruction.get_data() {
                                result.push_str(&format!("{}", value as i32));
                            }
                        }
                        ast::InstructionType::F32Const => {
                            if let ast::InstructionData::F32 { value } = *instruction.get_data() {
                                result.push_str(&format!("{}", value));
                            }
                        }
                        ast::InstructionType::F64Const => {
                            if let ast::InstructionData::F64 { value } = *instruction.get_data() {
                                result.push_str(&format!("{}", value));
                            }
                        }
                        ast::InstructionType::GlobalGet => {
                            if let ast::InstructionData::Global { global_index } = *instruction.get_data() {
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
    fn header_prefix(&self) -> &'static str {
        "Elem"
    }

    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position, self.elements.len())
    }

    fn to_details_string(&self, unit: &Module) -> String {
        let mut result: String = String::new();
        result.push_str(&format!("Elem[{}]:\n", self.elements.len()));
        for (i, element) in self.elements.iter().enumerate() {
            let (expr_str, elem_offset) = match element.mode {
                ElementMode::Active { ref offset, .. } => (
                    init_expr_to_string(unit, offset, true, true),
                    match init_expr_to_offset(offset) {
                        Some(o) => o as i32,
                        None => -1,
                    },
                ),
                _ => (String::new(), 0),
            };
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
                expr_str,
            ));
            element.init.iter().enumerate().for_each(|(i, _)| {
                // TODO: probably not .. this is simplistic for now, more work to do, i.e. not even printing constant expressions yet
                result.push_str(&format!(
                    "  - elem[{}] = {}\n",
                    match elem_offset {
                        -1 => "<INVALID_OFFSET>".to_string(),
                        _ => format!("{}", elem_offset + i as i32),
                    },
                    init_expr_to_string(unit, &element.init[i], false, false),
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

impl Default for CodeSection {
    fn default() -> Self {
        Self::new()
    }
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

    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }
}

impl SectionToString for CodeSection {
    fn header_prefix(&self) -> &'static str {
        "Code"
    }

    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position, self.code.len())
    }

    fn to_details_string(&self, unit: &Module) -> String {
        let mut result: String = String::new();
        result.push_str(&format!("Code[{}]:\n", self.code.len()));
        for (i, function_body) in self.code.iter().enumerate() {
            let mut exp = String::new();
            let fi = unit.imports.function_count() + i;
            for export in &unit.exports.exports {
                if let ExportIndex::Function(idx) = export.index {
                    if idx == fi as u32 {
                        exp = format!(" <{}>", export.name);
                    }
                }
            }
            result.push_str(&format!(
                " - func[{}] size={}{}\n",
                fi, // TODO: i is wrong when `(func $dummy)` is included - it should skip over these, need to figure out how it knows
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
            let fi = unit.imports.function_count() + i;
            for export in &unit.exports.exports {
                if let ExportIndex::Function(idx) = export.index {
                    if idx == fi as u32 {
                        exp = format!(" <{}>", export.name);
                    }
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
            result.push_str(&format!("{:06x} func[{}]{}:\n", pos, fi, exp));
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
                    ast::InstructionType::Else | ast::InstructionType::End => indent = indent.saturating_sub(1),
                    _ => {}
                }

                // all of the complexity here is to get nice wrapping when we overflow 26 chars in
                // the instruction hex, so we end up with something like this:
                //  000017: 42 80 80 80 80 80 80 80 80 | i64.const -9223372036854775808
                //  000020: 80 7f                      |

                let mut byte_string = String::new();
                let coding_bytes = (coding.emit_bytes)(instruction.get_data());
                let coding_string = (coding.emit_str)(instruction.get_data(), unit);
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

impl Default for DataSection {
    fn default() -> Self {
        Self::new()
    }
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

impl Default for DataCountSection {
    fn default() -> Self {
        Self::new()
    }
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
    fn header_prefix(&self) -> &'static str {
        "DataCount"
    }

    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position, self.count)
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

impl Default for CustomSection {
    fn default() -> Self {
        Self::new()
    }
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
    fn header_prefix(&self) -> &'static str {
        "Custom"
    }

    fn to_header_string(&self) -> String {
        format!(
            "{} \"{}\"",
            self.position,
            self.name
                .split('\0') // TODO: this is a hack to replicate C++ printing behaviour to match wabt output
                .next()
                .unwrap_or(&self.name)
        )
    }

    fn to_details_string(&self, _: &Module) -> String {
        let mut result = String::new();
        result.push_str(&format!(
            "Custom:\n - name: \"{}\"\n",
            self.name
                .split('\0') // TODO: this is a hack to replicate C++ printing behaviour to match wabt output
                .next()
                .unwrap_or(&self.name)
        ));
        result
    }
}

#[derive(Debug, PartialEq, Eq)]

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
            ExternalKind::Table(ref table_type) => write!(
                f,
                "type={} initial={}{} <- {}.{}",
                match table_type.ref_type {
                    RefType::FuncRef => "funcref",
                    RefType::ExternRef => "externref",
                },
                table_type.limits.min,
                if table_type.limits.max < u32::MAX {
                    format!(" max={}", table_type.limits.max as i32)
                } else {
                    "".to_string()
                },
                self.module,
                self.name
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
    pub limits: Limits,
}

#[derive(Debug)]
pub enum ExportIndex {
    Function(u32),
    Table(u32),
    Memory(u32),
    Global(u32),
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
        Self { entries: Vec::new() }
    }

    pub fn len(&self) -> u64 {
        self.entries.iter().map(|(count, _)| *count as u64).sum()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
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

#[derive(PartialEq, Clone, Copy, Debug)]
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Limits {
    pub min: u32,
    pub max: u32, // TODO: is u32::MAX ok for unlimited?
}

impl Limits {
    /// Check if these limits are compatible for import
    /// According to WASM spec: import can be more restrictive than export
    /// Import min must be >= export min, import max must be <= export max
    pub fn is_compatible_with(&self, exported: &Limits) -> bool {
        // Import's min must be >= exported min
        if self.min < exported.min {
            return false;
        }
        // If export has a max, import must have a max <= exported max
        if exported.max != u32::MAX && (self.max == u32::MAX || self.max > exported.max) {
            return false;
        }
        // If export has no max, import can have any max (or no max)
        true
    }
}

impl fmt::Display for Limits {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "min = {}, max = {}", self.min, self.max)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
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
            ElementMode::Passive => write!(f, "Passive"),
            ElementMode::Declarative => write!(f, "Declarative"),
            ElementMode::Active {
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
            DataMode::Passive => write!(f, "Passive"),
            DataMode::Active {
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
    fn header_prefix(&self) -> &'static str {
        "Data"
    }

    fn to_header_string(&self) -> String {
        format!("{} count: {}", self.position, self.data.len())
    }

    fn to_details_string(&self, unit: &Module) -> String {
        let mut result: String = String::new();
        result.push_str(&format!("Data[{}]:\n", self.data.len()));
        let mut data_offset = 0;
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
                    DataMode::Active { ref offset, .. } => {
                        if let Some(o) = init_expr_to_offset(offset) {
                            data_offset = o as i32
                        } else {
                            data_offset = -1
                        }
                        init_expr_to_string(unit, offset, false, true)
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
                        byts.push_str(&format!("{:02x}{:02x} ", data.init[pos + i], data.init[pos + i + 1]));
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

                let offset_str = match data_offset {
                    -1 => "<INVALID OFFSET>".to_string(),
                    _ => format!("{:07x}", pos as i32 + data_offset),
                };
                result.push_str(&format!("  - {}: {} {}\n", offset_str, byts, chars));
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

#[derive(Debug, PartialEq, Clone)]
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
                ExternalKind::Function(typeidx) => format!("Function({})", typeidx),
                ExternalKind::Table(table_type) => format!("Table({})", table_type),
                ExternalKind::Memory(limits) => format!("Memory({})", limits),
                ExternalKind::Global(global_type) => format!("Global({})", global_type),
            }
        )
    }
}

/*
#[derive(Debug)]
pub struct MemoryType {
    // TODO: fix this, no longer correct
    // limits : ResizableLimits
}
 */

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
            custom: Vec::new(),
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
        let mut components: Vec<(&dyn Positional, &dyn SectionToString)> = vec![
            (&self.types, &self.types),
            (&self.imports, &self.imports),
            (&self.functions, &self.functions),
            (&self.table, &self.table),
            (&self.memory, &self.memory),
            (&self.globals, &self.globals),
            (&self.exports, &self.exports),
            (&self.start, &self.start),
            (&self.elements, &self.elements),
            (&self.data_count, &self.data_count),
            (&self.code, &self.code),
            (&self.data, &self.data),
        ];
        components.extend(
            self.custom
                .iter()
                .map(|custom| (custom as &dyn Positional, custom as &dyn SectionToString)),
        );

        components.sort_by_key(|(p, _)| p.get_position().map(|pos| pos.start));

        let mut result = String::new();
        for (p, s) in components {
            if p.has_position() {
                result.push_str(&format!("{:>9} {}\n", s.header_prefix(), s.to_header_string()));
            }
        }

        result
    }

    fn to_details_string(&self) -> String {
        let mut result = String::new();

        let mut components: Vec<(&dyn Positional, &dyn SectionToString)> = vec![
            (&self.types, &self.types),
            (&self.imports, &self.imports),
            (&self.functions, &self.functions),
            (&self.table, &self.table),
            (&self.memory, &self.memory),
            (&self.globals, &self.globals),
            (&self.exports, &self.exports),
            (&self.start, &self.start),
            (&self.elements, &self.elements),
            (&self.data_count, &self.data_count),
            (&self.code, &self.code),
            (&self.data, &self.data),
        ];
        components.extend(
            self.custom
                .iter()
                .map(|custom| (custom as &dyn Positional, custom as &dyn SectionToString)),
        );

        components.sort_by_key(|(p, _)| p.get_position().map(|pos| pos.start));

        for (p, s) in components {
            if p.has_position() {
                result.push_str(s.to_details_string(self).as_str());
            }
        }

        result
    }

    fn to_disassemble_string(&self) -> String {
        if self.code.has_position() {
            self.code.to_disassemble_string(self)
        } else {
            String::new()
        }
    }
}

impl fmt::Display for Memory {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Memory({})", self.limits)
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
            0x00 => Ok(ExportIndex::Function(idx)),
            0x01 => Ok(ExportIndex::Table(idx)),
            0x02 => Ok(ExportIndex::Memory(idx)),
            0x03 => Ok(ExportIndex::Global(idx)),
            _ => {
                let msg: String = format!("invalid export type: {}", byte);
                Err(io::Error::new(io::ErrorKind::InvalidData, msg))
            }
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
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
        byte == 0x7f || byte == 0x7e || byte == 0x7d || byte == 0x7c || byte == 0x7b || byte == 0x70 || byte == 0x6f
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
            ValueType::I32 => vec![0x7f],
            ValueType::I64 => vec![0x7e],
            ValueType::F32 => vec![0x7d],
            ValueType::F64 => vec![0x7c],
            ValueType::V128 => vec![0x7b],
            ValueType::FuncRef => vec![0x70],
            ValueType::ExternRef => vec![0x6f],
        }
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ValueType::I32 => "i32",
                ValueType::I64 => "i64",
                ValueType::F64 => "f64",
                ValueType::F32 => "f32",
                ValueType::V128 => "v128",
                ValueType::FuncRef => "funcref",
                ValueType::ExternRef => "externref",
            }
        )
    }
}
