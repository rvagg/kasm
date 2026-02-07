//! WAT Parser: S-Expression Tree -> WebAssembly Module.
//!
//! Transforms the S-expression tree from [`sexpr::read`] into a WebAssembly [`Module`].
//!
//! # Design Principles
//!
//! 1. **Grammar Correspondence**: Each parsing function corresponds to a grammar
//!    production. Comments show the grammar rule being implemented.
//!
//! 2. **No Lookahead**: Because we work on a tree, not a token stream, we can
//!    always see all children of a node. No speculative parsing needed.
//!
//! 3. **Consistent Errors**: All errors use the same pattern, with precise spans.
//!
//! 4. **Scope-Based State**: Label scopes use RAII guards, not manual push/pop.
//!
//! # Grammar Reference
//!
//! Based on the WebAssembly Text Format specification:
//! <https://webassembly.github.io/spec/core/text/index.html>

use super::sexpr::{ReadError, SExpr, SExprList};
use super::token::{Span, TokenKind};
use crate::parser::instruction::{BlockType, ByteRange, Instruction, InstructionKind, MemArg};
use crate::parser::module::{
    CodeSection, Data, DataCountSection, DataMode, DataSection, Element, ElementMode, ElementSection, Export,
    ExportIndex, ExportSection, ExternalKind, Function, FunctionBody, FunctionSection, FunctionType, Global,
    GlobalSection, GlobalType, Import, ImportSection, Limits, Locals, Memory, MemorySection, Module, RefType,
    SectionPosition, StartSection, TableSection, TableType, TypeSection, ValueType,
};
use crate::parser::structure_builder::StructureBuilder;
use std::collections::HashMap;
use std::fmt;

// ============================================================================
// Constants
// ============================================================================

// Memory alignment constants.
//
// WebAssembly encodes alignment as log2(bytes), not raw bytes. These constants
// are the default ("natural") alignments for each operand size, used when
// the WAT source omits an explicit `align=N`.
//
// The alignment hint tells the runtime how the address will be aligned in
// memory, enabling optimised load/store paths on architectures that care.
const ALIGN_8: u32 = 0; // log2(1) - for i32.load8_s, i64.load8_u, etc.
const ALIGN_16: u32 = 1; // log2(2) - for i32.load16_s, i64.load16_u, etc.
const ALIGN_32: u32 = 2; // log2(4) - for i32.load, f32.load, i32.store, etc.
const ALIGN_64: u32 = 3; // log2(8) - for i64.load, f64.load, i64.store, etc.

// ============================================================================
// Error Type
// ============================================================================

/// An error encountered during WAT parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl ParseError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }

    /// Creates an "expected X, found Y" error.
    pub fn expected(expected: &str, found: &str, span: Span) -> Self {
        Self::new(format!("expected {}, found {}", expected, found), span)
    }

    /// Creates an "undefined X" error.
    pub fn undefined(kind: &str, name: &str, span: Span) -> Self {
        Self::new(format!("undefined {}: ${}", kind, name), span)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at line {}, column {}",
            self.message, self.span.line, self.span.column
        )
    }
}

impl std::error::Error for ParseError {}

impl From<ReadError> for ParseError {
    fn from(e: ReadError) -> Self {
        Self {
            message: e.message,
            span: e.span,
        }
    }
}

// ============================================================================
// Namespaces
// ============================================================================

/// Index namespaces in WebAssembly.
///
/// WebAssembly has separate index spaces for different kinds of definitions.
/// Named references ($name) are resolved within their respective namespace.
///
/// Labels are not included here, they use stack-based scoping via
/// `push_label()`/`pop_label()`/`resolve_label()` instead of a flat mapping.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Namespace {
    Type,
    Func,
    Table,
    Memory,
    Global,
    Local,
}

impl Namespace {
    fn name(self) -> &'static str {
        match self {
            Namespace::Type => "type",
            Namespace::Func => "function",
            Namespace::Table => "table",
            Namespace::Memory => "memory",
            Namespace::Global => "global",
            Namespace::Local => "local",
        }
    }
}

// ============================================================================
// Parse Context
// ============================================================================

/// Accumulates state during parsing.
///
/// Tracks name->index mappings for each namespace, collects definitions,
/// and manages the label stack for branch target resolution.
struct ParseContext {
    // Name tables
    type_names: HashMap<String, u32>,
    func_names: HashMap<String, u32>,
    table_names: HashMap<String, u32>,
    memory_names: HashMap<String, u32>,
    global_names: HashMap<String, u32>,
    local_names: HashMap<String, u32>,

    // Counters (includes imports)
    type_count: u32,
    func_count: u32,
    table_count: u32,
    memory_count: u32,
    global_count: u32,
    local_count: u32,
    param_count: u32,

    // Label stack for branch resolution
    label_stack: Vec<Option<String>>,

    // Module sections
    types: Vec<FunctionType>,
    imports: Vec<Import>,
    functions: Vec<Function>,
    tables: Vec<TableType>,
    memories: Vec<Memory>,
    globals: Vec<Global>,
    exports: Vec<Export>,
    start: Option<u32>,
    elements: Vec<Element>,
    code: Vec<FunctionBody>,
    data: Vec<Data>,
}

impl ParseContext {
    fn new() -> Self {
        Self {
            type_names: HashMap::new(),
            func_names: HashMap::new(),
            table_names: HashMap::new(),
            memory_names: HashMap::new(),
            global_names: HashMap::new(),
            local_names: HashMap::new(),

            type_count: 0,
            func_count: 0,
            table_count: 0,
            memory_count: 0,
            global_count: 0,
            local_count: 0,
            param_count: 0,

            label_stack: Vec::new(),

            types: Vec::new(),
            imports: Vec::new(),
            functions: Vec::new(),
            tables: Vec::new(),
            memories: Vec::new(),
            globals: Vec::new(),
            exports: Vec::new(),
            start: None,
            elements: Vec::new(),
            code: Vec::new(),
            data: Vec::new(),
        }
    }

    /// Resets per-function state (locals, labels) for a new function.
    fn reset_function(&mut self) {
        self.local_names.clear();
        self.local_count = 0;
        self.param_count = 0;
        self.label_stack.clear();
    }

    /// Registers a named or anonymous definition, returning its index.
    fn register(&mut self, ns: Namespace, name: Option<&str>) -> u32 {
        let (names, count) = match ns {
            Namespace::Type => (&mut self.type_names, &mut self.type_count),
            Namespace::Func => (&mut self.func_names, &mut self.func_count),
            Namespace::Table => (&mut self.table_names, &mut self.table_count),
            Namespace::Memory => (&mut self.memory_names, &mut self.memory_count),
            Namespace::Global => (&mut self.global_names, &mut self.global_count),
            Namespace::Local => (&mut self.local_names, &mut self.local_count),
        };

        let idx = *count;
        *count += 1;
        if let Some(n) = name {
            names.insert(n.to_string(), idx);
        }
        idx
    }

    /// Resolves a name to an index in the given namespace.
    fn resolve(&self, ns: Namespace, name: &str, span: Span) -> Result<u32, ParseError> {
        let names = match ns {
            Namespace::Type => &self.type_names,
            Namespace::Func => &self.func_names,
            Namespace::Table => &self.table_names,
            Namespace::Memory => &self.memory_names,
            Namespace::Global => &self.global_names,
            Namespace::Local => &self.local_names,
        };

        names
            .get(name)
            .copied()
            .ok_or_else(|| ParseError::undefined(ns.name(), name, span))
    }

    /// Resolves a label name to a branch depth.
    fn resolve_label(&self, name: &str, span: Span) -> Result<u32, ParseError> {
        for (depth, label) in self.label_stack.iter().rev().enumerate() {
            if label.as_deref() == Some(name) {
                return Ok(depth as u32);
            }
        }
        Err(ParseError::undefined("label", name, span))
    }

    /// Pushes a label onto the stack (for block/loop/if).
    fn push_label(&mut self, name: Option<String>) {
        self.label_stack.push(name);
    }

    /// Pops a label from the stack.
    fn pop_label(&mut self) {
        self.label_stack.pop();
    }

    /// Registers a type, deduplicating identical signatures.
    ///
    /// When a duplicate signature is found, the name (if any) is bound to the
    /// existing index. Duplicate names silently overwrite - the WAT spec forbids
    /// them but we don't enforce that.
    fn register_type(&mut self, name: Option<&str>, func_type: FunctionType) -> u32 {
        // Check for existing identical type
        for (idx, existing) in self.types.iter().enumerate() {
            if existing.parameters == func_type.parameters && existing.return_types == func_type.return_types {
                if let Some(n) = name {
                    self.type_names.insert(n.to_string(), idx as u32);
                }
                return idx as u32;
            }
        }

        // Register new type
        let idx = self.type_count;
        self.type_count += 1;
        if let Some(n) = name {
            self.type_names.insert(n.to_string(), idx);
        }
        self.types.push(func_type);
        idx
    }

    /// Builds the final Module from accumulated state.
    fn into_module(self) -> Module {
        let start_section = if let Some(func_idx) = self.start {
            StartSection {
                start: func_idx,
                position: SectionPosition::new(1, 1), // Non-zero indicates presence
            }
        } else {
            StartSection {
                start: 0,
                position: SectionPosition::new(0, 0), // Zero indicates absence
            }
        };

        let data_count = self.data.len() as u32;

        Module {
            name: String::new(),
            magic: 0x6d736100,
            version: 1,
            types: TypeSection {
                types: self.types,
                position: SectionPosition::new(0, 0),
            },
            imports: ImportSection {
                imports: self.imports,
                position: SectionPosition::new(0, 0),
            },
            functions: FunctionSection {
                functions: self.functions,
                position: SectionPosition::new(0, 0),
            },
            table: TableSection {
                tables: self.tables,
                position: SectionPosition::new(0, 0),
            },
            memory: MemorySection {
                memory: self.memories,
                position: SectionPosition::new(0, 0),
            },
            globals: GlobalSection {
                globals: self.globals,
                position: SectionPosition::new(0, 0),
            },
            exports: ExportSection {
                exports: self.exports,
                position: SectionPosition::new(0, 0),
            },
            start: start_section,
            elements: ElementSection {
                elements: self.elements,
                position: SectionPosition::new(0, 0),
            },
            code: CodeSection {
                code: self.code,
                position: SectionPosition::new(0, 0),
            },
            data: DataSection {
                data: self.data,
                position: SectionPosition::new(0, 0),
            },
            data_count: DataCountSection {
                count: data_count,
                position: SectionPosition::new(0, 0),
            },
            custom: Vec::new(),
            validation_context: std::cell::RefCell::new(None),
        }
    }
}

// ============================================================================
// Parser
// ============================================================================

/// Parses a WAT source string into a WebAssembly Module.
///
/// # Grammar
///
/// ```text
/// module ::= '(' 'module' id? field* ')'
/// ```
///
/// # Example
///
/// ```
/// use kasm::wat::parse;
///
/// let module = parse("(module (func (result i32) (i32.const 42)))").unwrap();
/// assert_eq!(module.functions.len(), 1);
/// ```
#[must_use = "parsing result should be checked"]
pub fn parse(source: &str) -> Result<Module, ParseError> {
    let sexpr = super::sexpr::read(source)?;
    let mut ctx = ParseContext::new();
    parse_module(&sexpr, &mut ctx)?;
    Ok(ctx.into_module())
}

/// Parses a module from its S-expression.
///
/// Grammar: `module ::= '(' 'module' id? field* ')'`
fn parse_module(sexpr: &SExpr, ctx: &mut ParseContext) -> Result<(), ParseError> {
    let list = sexpr.expect_list()?;
    list.expect_head("module")?;

    // Optional module name (ignored but validated)
    let start_idx = if list.get(1).and_then(|s| s.as_id()).is_some() {
        2
    } else {
        1
    };

    // Parse fields
    for item in list.iter_from(start_idx) {
        parse_field(item, ctx)?;
    }

    Ok(())
}

/// Parses a module field.
///
/// Grammar: `field ::= type | import | func | table | memory | global | export | start | elem | data`
fn parse_field(sexpr: &SExpr, ctx: &mut ParseContext) -> Result<(), ParseError> {
    let list = sexpr.expect_list()?;
    let keyword = list
        .head_keyword()
        .ok_or_else(|| ParseError::new("expected field keyword", sexpr.span()))?;

    match keyword {
        "type" => parse_type_def(list, ctx),
        "import" => parse_import(list, ctx),
        "func" => parse_func(list, ctx),
        "table" => parse_table(list, ctx),
        "memory" => parse_memory(list, ctx),
        "global" => parse_global(list, ctx),
        "export" => parse_export(list, ctx),
        "start" => parse_start(list, ctx),
        "elem" => parse_elem(list, ctx),
        "data" => parse_data(list, ctx),
        _ => Err(ParseError::new(format!("unknown field type: {}", keyword), list.span)),
    }
}

// ============================================================================
// Type Definitions
// ============================================================================

/// Parses a type definition.
///
/// Grammar: `type ::= '(' 'type' id? '(' 'func' functype ')' ')'`
fn parse_type_def(list: SExprList<'_>, ctx: &mut ParseContext) -> Result<(), ParseError> {
    let mut idx = 1;

    // Optional name
    let name = list.get(idx).and_then(|s| s.as_id());
    if name.is_some() {
        idx += 1;
    }

    // Expect (func ...)
    let func_sexpr = list
        .get(idx)
        .ok_or_else(|| ParseError::new("expected function type", list.span))?;
    let func_list = func_sexpr.expect_list()?;
    func_list.expect_head("func")?;

    let func_type = parse_func_type(func_list)?;
    ctx.register_type(name, func_type);

    Ok(())
}

/// Parses a function type signature.
///
/// Grammar: `functype ::= '(' 'func' param* result* ')'`
fn parse_func_type(list: SExprList<'_>) -> Result<FunctionType, ParseError> {
    let mut parameters = Vec::new();
    let mut return_types = Vec::new();

    for item in list.iter_from(1) {
        let inner = item.expect_list()?;
        match inner.head_keyword() {
            Some("param") => {
                for param_item in inner.iter_from(1) {
                    // Skip optional parameter names
                    if param_item.as_id().is_some() {
                        continue;
                    }
                    parameters.push(parse_valtype(param_item)?);
                }
            }
            Some("result") => {
                for result_item in inner.iter_from(1) {
                    return_types.push(parse_valtype(result_item)?);
                }
            }
            Some(kw) => {
                return Err(ParseError::new(
                    format!("expected 'param' or 'result', found '{}'", kw),
                    inner.span,
                ));
            }
            None => return Err(ParseError::new("expected keyword in type", inner.span)),
        }
    }

    Ok(FunctionType {
        parameters,
        return_types,
    })
}

/// Parses a value type.
///
/// Grammar: `valtype ::= numtype | vectype | reftype`
fn parse_valtype(sexpr: &SExpr) -> Result<ValueType, ParseError> {
    match sexpr.as_keyword() {
        Some("i32") => Ok(ValueType::I32),
        Some("i64") => Ok(ValueType::I64),
        Some("f32") => Ok(ValueType::F32),
        Some("f64") => Ok(ValueType::F64),
        Some("funcref") => Ok(ValueType::FuncRef),
        Some("externref") => Ok(ValueType::ExternRef),
        Some(kw) => Err(ParseError::expected("value type", &format!("'{}'", kw), sexpr.span())),
        None => Err(ParseError::expected("value type", "list", sexpr.span())),
    }
}

// ============================================================================
// Imports
// ============================================================================

/// Parses an import.
///
/// Grammar: `import ::= '(' 'import' name name importdesc ')'`
fn parse_import(list: SExprList<'_>, ctx: &mut ParseContext) -> Result<(), ParseError> {
    let module_name = parse_string(
        list.get(1)
            .ok_or_else(|| ParseError::new("expected module name string", list.span))?,
    )?;

    let field_name = parse_string(
        list.get(2)
            .ok_or_else(|| ParseError::new("expected field name string", list.span))?,
    )?;

    let desc_sexpr = list
        .get(3)
        .ok_or_else(|| ParseError::new("expected import descriptor", list.span))?;
    let desc_list = desc_sexpr.expect_list()?;

    let external_kind = match desc_list.head_keyword() {
        Some("func") => {
            let name = desc_list.get(1).and_then(|s| s.as_id());
            ctx.register(Namespace::Func, name);
            let (type_idx, _) = parse_type_use(desc_list, 2, false, ctx)?;
            ExternalKind::Function(type_idx)
        }
        Some("memory") => {
            let name = desc_list.get(1).and_then(|s| s.as_id());
            ctx.register(Namespace::Memory, name);
            let start = if name.is_some() { 2 } else { 1 };
            let limits = parse_limits(desc_list, start)?;
            ExternalKind::Memory(limits)
        }
        Some("global") => {
            let name = desc_list.get(1).and_then(|s| s.as_id());
            ctx.register(Namespace::Global, name);
            let start = if name.is_some() { 2 } else { 1 };
            let global_type = parse_global_type(desc_list, start)?;
            ExternalKind::Global(global_type)
        }
        Some("table") => {
            let name = desc_list.get(1).and_then(|s| s.as_id());
            ctx.register(Namespace::Table, name);
            let start = if name.is_some() { 2 } else { 1 };
            let limits = parse_limits(desc_list, start)?;
            let ref_type = parse_reftype(desc_list.get(start + limits_len(&limits)))?;
            ExternalKind::Table(TableType { ref_type, limits })
        }
        Some(kw) => return Err(ParseError::new(format!("unknown import kind: {}", kw), desc_list.span)),
        None => return Err(ParseError::new("expected import kind", desc_list.span)),
    };

    ctx.imports.push(Import {
        module: module_name,
        name: field_name,
        external_kind,
    });

    Ok(())
}

/// Parses a type use (type reference or inline params/results).
///
/// Grammar: `typeuse ::= '(' 'type' idx ')' | param* result*`
///
/// When `register_locals` is true, parameter names are registered in the
/// local namespace (for function definitions). Returns the type index and
/// the position after the last type-related item.
fn parse_type_use(
    list: SExprList<'_>,
    start: usize,
    register_locals: bool,
    ctx: &mut ParseContext,
) -> Result<(u32, usize), ParseError> {
    let mut params = Vec::new();
    let mut results = Vec::new();
    let mut explicit_idx = None;
    let mut idx = start;

    while let Some(item) = list.get(idx) {
        let Some(inner) = item.as_list() else {
            break;
        };

        match inner.head_keyword() {
            Some("type") => {
                explicit_idx = Some(parse_index(inner.get(1), Namespace::Type, ctx)?);
                idx += 1;
            }
            Some("param") => {
                // Grammar: param ::= '(' 'param' id valtype ')' | '(' 'param' valtype* ')'
                let first = inner.get(1);
                let name = first.and_then(|s| s.as_id());
                if let Some(name) = name {
                    // Named form: exactly one valtype
                    let ty_item = inner
                        .get(2)
                        .ok_or_else(|| ParseError::new("expected valtype after param name", inner.span))?;
                    let ty = parse_valtype(ty_item)?;
                    if register_locals {
                        ctx.register(Namespace::Local, Some(name));
                        ctx.param_count += 1;
                    }
                    params.push(ty);
                } else {
                    // Anonymous form: zero or more valtypes
                    for p in inner.iter_from(1) {
                        let ty = parse_valtype(p)?;
                        if register_locals {
                            ctx.register(Namespace::Local, None);
                            ctx.param_count += 1;
                        }
                        params.push(ty);
                    }
                }
                idx += 1;
            }
            Some("result") => {
                for r in inner.iter_from(1) {
                    results.push(parse_valtype(r)?);
                }
                idx += 1;
            }
            _ => break,
        }
    }

    let type_idx = explicit_idx.unwrap_or_else(|| {
        ctx.register_type(
            None,
            FunctionType {
                parameters: params,
                return_types: results,
            },
        )
    });

    Ok((type_idx, idx))
}

// ============================================================================
// Functions
// ============================================================================

/// Parses a function definition.
///
/// Grammar: `func ::= '(' 'func' id? typeuse local* instr* ')'`
fn parse_func(list: SExprList<'_>, ctx: &mut ParseContext) -> Result<(), ParseError> {
    ctx.reset_function();

    let mut idx = 1;

    // Optional function name
    let name = list.get(idx).and_then(|s| s.as_id());
    if name.is_some() {
        idx += 1;
    }
    ctx.register(Namespace::Func, name);

    idx = collect_inline_exports(list, idx, ExportIndex::Function(ctx.func_count - 1), ctx)?;

    // Parse type use (type reference and/or inline params/results)
    let (type_idx, body_start) = parse_type_use(list, idx, true, ctx)?;

    // Parse locals and body
    let mut locals = Vec::new();
    let mut body = Vec::new();
    let mut body_idx = body_start;

    while let Some(item) = list.get(body_idx) {
        if item.is_list_headed_by("local") {
            parse_local(item.as_list().unwrap(), &mut locals, ctx)?;
            body_idx += 1;
        } else {
            body_idx = parse_body_item(list, body_idx, &mut body, ctx)?;
        }
    }

    // Build function
    let func_type = ctx
        .types
        .get(type_idx as usize)
        .cloned()
        .unwrap_or_else(|| FunctionType {
            parameters: Vec::new(),
            return_types: Vec::new(),
        });

    let structured = StructureBuilder::build_function(&body, ctx.local_count as usize, func_type.return_types.clone())
        .map_err(|e| ParseError::new(e.to_string(), list.span))?;

    ctx.functions.push(Function { ftype_index: type_idx });
    ctx.code.push(FunctionBody {
        locals: Locals::new(compress_locals(&locals)),
        body: structured,
        position: SectionPosition::new(0, 0),
    });

    Ok(())
}

/// Parses a local declaration.
///
/// Grammar: `local ::= '(' 'local' id valtype ')' | '(' 'local' valtype* ')'`
fn parse_local(list: SExprList<'_>, locals: &mut Vec<ValueType>, ctx: &mut ParseContext) -> Result<(), ParseError> {
    let mut idx = 1;

    let name = list.get(idx).and_then(|s| s.as_id());
    if name.is_some() {
        idx += 1;
        // Named form: exactly one valtype
        let item = list
            .get(idx)
            .ok_or_else(|| ParseError::new("expected valtype after local name", list.span))?;
        let ty = parse_valtype(item)?;
        ctx.register(Namespace::Local, name);
        locals.push(ty);
        if list.get(idx + 1).is_some() {
            return Err(ParseError::new("named local must have exactly one type", list.span));
        }
    } else {
        // Anonymous form: zero or more valtypes
        while let Some(item) = list.get(idx) {
            let ty = parse_valtype(item)?;
            ctx.register(Namespace::Local, None);
            locals.push(ty);
            idx += 1;
        }
    }

    Ok(())
}

// ============================================================================
// Tables, Memories, Globals
// ============================================================================

/// Parses a table definition.
///
/// Grammar: `table ::= '(' 'table' id? limits reftype ')'`
fn parse_table(list: SExprList<'_>, ctx: &mut ParseContext) -> Result<(), ParseError> {
    let mut idx = 1;
    let name = list.get(idx).and_then(|s| s.as_id());
    if name.is_some() {
        idx += 1;
    }
    ctx.register(Namespace::Table, name);
    idx = collect_inline_exports(list, idx, ExportIndex::Table(ctx.table_count - 1), ctx)?;

    let limits = parse_limits(list, idx)?;
    let ref_idx = idx + limits_len(&limits);
    let ref_type = parse_reftype(list.get(ref_idx))?;

    ctx.tables.push(TableType { limits, ref_type });
    Ok(())
}

/// Parses a memory definition.
///
/// Grammar: `memory ::= '(' 'memory' id? limits ')'`
fn parse_memory(list: SExprList<'_>, ctx: &mut ParseContext) -> Result<(), ParseError> {
    let mut idx = 1;
    let name = list.get(idx).and_then(|s| s.as_id());
    if name.is_some() {
        idx += 1;
    }
    ctx.register(Namespace::Memory, name);
    idx = collect_inline_exports(list, idx, ExportIndex::Memory(ctx.memory_count - 1), ctx)?;

    let limits = parse_limits(list, idx)?;
    ctx.memories.push(Memory { limits });
    Ok(())
}

/// Parses a global definition.
///
/// Grammar: `global ::= '(' 'global' id? globaltype expr ')'`
fn parse_global(list: SExprList<'_>, ctx: &mut ParseContext) -> Result<(), ParseError> {
    let mut idx = 1;
    let name = list.get(idx).and_then(|s| s.as_id());
    if name.is_some() {
        idx += 1;
    }
    ctx.register(Namespace::Global, name);
    idx = collect_inline_exports(list, idx, ExportIndex::Global(ctx.global_count - 1), ctx)?;

    let global_type = parse_global_type(list, idx)?;
    idx += 1; // Consume the type or (mut type)

    let init_sexpr = list
        .get(idx)
        .ok_or_else(|| ParseError::new("expected init expression", list.span))?;
    let init = parse_const_expr(init_sexpr, ctx)?;

    ctx.globals.push(Global { global_type, init });
    Ok(())
}

/// Parses a global type.
///
/// Grammar: `globaltype ::= valtype | '(' 'mut' valtype ')'`
fn parse_global_type(list: SExprList<'_>, idx: usize) -> Result<GlobalType, ParseError> {
    let item = list
        .get(idx)
        .ok_or_else(|| ParseError::new("expected global type", list.span))?;

    if let Some(inner) = item.as_list() {
        inner.expect_head("mut")?;
        let value_type = parse_valtype(
            inner
                .get(1)
                .ok_or_else(|| ParseError::new("expected value type in mut", inner.span))?,
        )?;
        Ok(GlobalType {
            value_type,
            mutable: true,
        })
    } else {
        let value_type = parse_valtype(item)?;
        Ok(GlobalType {
            value_type,
            mutable: false,
        })
    }
}

// ============================================================================
// Exports, Start, Data
// ============================================================================

/// Consumes inline `(export "name")` forms from a definition, registering
/// each as an export of the given kind. Returns the adjusted index past any
/// inline exports. Used by func, table, memory, and global parsers.
fn collect_inline_exports(
    list: SExprList<'_>,
    mut idx: usize,
    index: ExportIndex,
    ctx: &mut ParseContext,
) -> Result<usize, ParseError> {
    while let Some(item) = list.get(idx) {
        if item.is_list_headed_by("export") {
            let export_list = item.as_list().unwrap();
            let name = parse_string(
                export_list
                    .get(1)
                    .ok_or_else(|| ParseError::new("expected export name", export_list.span))?,
            )?;
            ctx.exports.push(Export { name, index });
            idx += 1;
        } else {
            break;
        }
    }
    Ok(idx)
}

/// Parses an export.
///
/// Grammar: `export ::= '(' 'export' name '(' exportdesc ')' ')'`
fn parse_export(list: SExprList<'_>, ctx: &mut ParseContext) -> Result<(), ParseError> {
    let name = parse_string(
        list.get(1)
            .ok_or_else(|| ParseError::new("expected export name", list.span))?,
    )?;

    let desc = list
        .get(2)
        .ok_or_else(|| ParseError::new("expected export descriptor", list.span))?
        .expect_list()?;

    let index = match desc.head_keyword() {
        Some("func") => ExportIndex::Function(parse_index(desc.get(1), Namespace::Func, ctx)?),
        Some("table") => ExportIndex::Table(parse_index(desc.get(1), Namespace::Table, ctx)?),
        Some("memory") => ExportIndex::Memory(parse_index(desc.get(1), Namespace::Memory, ctx)?),
        Some("global") => ExportIndex::Global(parse_index(desc.get(1), Namespace::Global, ctx)?),
        Some(kw) => return Err(ParseError::new(format!("unknown export kind: {}", kw), desc.span)),
        None => return Err(ParseError::new("expected export kind", desc.span)),
    };

    ctx.exports.push(Export { name, index });
    Ok(())
}

/// Parses a start section.
///
/// Grammar: `start ::= '(' 'start' funcidx ')'`
fn parse_start(list: SExprList<'_>, ctx: &mut ParseContext) -> Result<(), ParseError> {
    let idx = parse_index(list.get(1), Namespace::Func, ctx)?;
    ctx.start = Some(idx);
    Ok(())
}

/// Parses an element segment.
///
/// Grammar:
/// ```text
/// elem ::= '(' 'elem' id? elemlist ')'
///        | '(' 'elem' id? tableuse '(' 'offset' expr ')' elemlist ')'
///        | '(' 'elem' id? 'declare' elemlist ')'
///        | '(' 'elem' id? '(' 'offset' expr ')' funcidx* ')'  ;; abbreviated
///        | '(' 'elem' id? tableuse '(' 'offset' expr ')' funcidx* ')'  ;; abbreviated
/// elemlist ::= reftype elemexpr* | 'func' funcidx*
/// tableuse ::= '(' 'table' tableidx ')'
/// ```
fn parse_elem(list: SExprList<'_>, ctx: &mut ParseContext) -> Result<(), ParseError> {
    let mut idx = 1;

    // Optional name
    if list.get(idx).and_then(|s| s.as_id()).is_some() {
        idx += 1;
    }

    // Check for 'declare' keyword (declarative mode)
    if list.get(idx).and_then(|s| s.as_keyword()) == Some("declare") {
        idx += 1;
        let (ref_type, init) = parse_elem_list(list, idx, ctx)?;
        ctx.elements.push(Element {
            flags: 3, // declarative
            ref_type,
            init,
            mode: ElementMode::Declarative,
        });
        return Ok(());
    }

    // Check for (table ...) for explicit table index
    let mut table_index = 0u32;
    if let Some(item) = list.get(idx)
        && item.is_list_headed_by("table")
    {
        let table_list = item.as_list().unwrap();
        table_index = parse_index(table_list.get(1), Namespace::Table, ctx)?;
        idx += 1;
    }

    // Check for (offset ...), indicates active mode
    if let Some(item) = list.get(idx) {
        if item.is_list_headed_by("offset") {
            let offset_list = item.as_list().unwrap();
            let offset = parse_const_expr_multi(offset_list.iter_from(1), ctx)?;
            idx += 1;

            // Active mode, parse element list
            let (ref_type, init) = parse_elem_list(list, idx, ctx)?;
            ctx.elements.push(Element {
                flags: if table_index == 0 { 0 } else { 2 },
                ref_type,
                init,
                mode: ElementMode::Active { table_index, offset },
            });
            return Ok(());
        }

        // Check if first item is an expression (active mode with inline offset)
        if item.as_list().is_some() && !is_elem_keyword(item) {
            let offset = parse_const_expr(item, ctx)?;
            idx += 1;

            // Active mode with expression offset, parse element list (handles 'func' keyword)
            let (ref_type, init) = parse_elem_list(list, idx, ctx)?;
            ctx.elements.push(Element {
                flags: if table_index == 0 { 0 } else { 2 },
                ref_type,
                init,
                mode: ElementMode::Active { table_index, offset },
            });
            return Ok(());
        }
    }

    // Passive mode, just element list
    let (ref_type, init) = parse_elem_list(list, idx, ctx)?;
    ctx.elements.push(Element {
        flags: 1, // passive
        ref_type,
        init,
        mode: ElementMode::Passive,
    });

    Ok(())
}

/// Checks if an S-expression is an element-related keyword (not an init expression).
fn is_elem_keyword(sexpr: &SExpr) -> bool {
    sexpr.is_list_headed_by("item") || sexpr.is_list_headed_by("ref.func") || sexpr.is_list_headed_by("ref.null")
}

/// Parses an element list.
///
/// Grammar: `elemlist ::= reftype elemexpr* | 'func' funcidx*`
fn parse_elem_list(
    list: SExprList<'_>,
    start: usize,
    ctx: &mut ParseContext,
) -> Result<(RefType, Vec<Vec<Instruction>>), ParseError> {
    let mut idx = start;
    let mut ref_type = RefType::FuncRef;

    // Check for explicit reftype or 'func' keyword
    if let Some(item) = list.get(idx) {
        match item.as_keyword() {
            Some("funcref") => {
                ref_type = RefType::FuncRef;
                idx += 1;
            }
            Some("externref") => {
                ref_type = RefType::ExternRef;
                idx += 1;
            }
            Some("func") => {
                // Shorthand: 'func $f1 $f2' is equivalent to 'funcref (ref.func $f1) (ref.func $f2)'
                idx += 1;
                let init = parse_elem_func_indices(list, idx, ctx)?;
                return Ok((RefType::FuncRef, init));
            }
            _ => {}
        }
    }

    // Parse element expressions. Each init expression includes a trailing End
    // to match the binary format representation.
    let mut init = Vec::new();
    while let Some(item) = list.get(idx) {
        if let Some(inner) = item.as_list() {
            if inner.head_keyword() == Some("item") {
                let expr = parse_const_expr_multi(inner.iter_from(1), ctx)?;
                init.push(expr);
            } else if inner.head_keyword() == Some("ref.func") {
                let func_idx = parse_index(inner.get(1), Namespace::Func, ctx)?;
                init.push(vec![make_instr(InstructionKind::RefFunc { func_idx }), make_end()]);
            } else if inner.head_keyword() == Some("ref.null") {
                let heap_type = inner.get(1).and_then(|s| s.as_keyword()).unwrap_or("func");
                let ref_type = if heap_type == "extern" {
                    ValueType::ExternRef
                } else {
                    ValueType::FuncRef
                };
                init.push(vec![make_instr(InstructionKind::RefNull { ref_type }), make_end()]);
            } else {
                init.push(parse_const_expr(item, ctx)?);
            }
        } else if let Some(id_or_idx) = item.as_id().or_else(|| {
            item.as_atom()
                .filter(|t| matches!(t.kind, TokenKind::Integer(_)))
                .map(|_| "")
        }) {
            let func_idx = if id_or_idx.is_empty() {
                parse_index(Some(item), Namespace::Func, ctx)?
            } else {
                ctx.resolve(Namespace::Func, id_or_idx, item.span())?
            };
            init.push(vec![make_instr(InstructionKind::RefFunc { func_idx }), make_end()]);
        } else {
            break;
        }
        idx += 1;
    }

    Ok((ref_type, init))
}

/// Parses bare function indices as element init expressions.
///
/// Each index is wrapped in a `ref.func` instruction.
fn parse_elem_func_indices(
    list: SExprList<'_>,
    start: usize,
    ctx: &mut ParseContext,
) -> Result<Vec<Vec<Instruction>>, ParseError> {
    let mut init = Vec::new();
    let mut idx = start;

    while let Some(item) = list.get(idx) {
        if let Ok(func_idx) = parse_index(Some(item), Namespace::Func, ctx) {
            init.push(vec![make_instr(InstructionKind::RefFunc { func_idx }), make_end()]);
            idx += 1;
        } else {
            break;
        }
    }

    Ok(init)
}

/// Parses a data segment.
///
/// Grammar:
/// ```text
/// data ::= '(' 'data' id? string* ')'                               ;; passive
///        | '(' 'data' id? memuse? '(' 'offset' expr ')' string* ')' ;; active
/// memuse ::= '(' 'memory' memidx ')'
/// ```
fn parse_data(list: SExprList<'_>, ctx: &mut ParseContext) -> Result<(), ParseError> {
    let mut idx = 1;

    // Optional name
    if list.get(idx).and_then(|s| s.as_id()).is_some() {
        idx += 1;
    }

    // Check for (memory ...) for explicit memory index
    let mut memory_index = 0u32;
    if let Some(item) = list.get(idx)
        && item.is_list_headed_by("memory")
    {
        let mem_list = item.as_list().unwrap();
        memory_index = parse_index(mem_list.get(1), Namespace::Memory, ctx)?;
        idx += 1;
    }

    // Check for offset expression, determines active vs passive
    let mode = if let Some(item) = list.get(idx) {
        if item.is_list_headed_by("offset") {
            // Explicit (offset expr) form
            let offset_list = item.as_list().unwrap();
            let offset = parse_const_expr_multi(offset_list.iter_from(1), ctx)?;
            idx += 1;
            DataMode::Active { memory_index, offset }
        } else if item.as_list().is_some() && !is_string_atom(item) {
            // Implicit offset expression (abbreviated form)
            let offset = parse_const_expr(item, ctx)?;
            idx += 1;
            DataMode::Active { memory_index, offset }
        } else {
            // No offset, passive mode
            DataMode::Passive
        }
    } else {
        DataMode::Passive
    };

    // Parse data strings
    let mut init = Vec::new();
    while let Some(item) = list.get(idx) {
        if let Some(token) = item.as_atom()
            && let TokenKind::String(bytes) = &token.kind
        {
            init.extend(bytes);
        }
        idx += 1;
    }

    ctx.data.push(Data { mode, init });

    Ok(())
}

/// Checks if an S-expression is a string atom.
fn is_string_atom(sexpr: &SExpr) -> bool {
    sexpr
        .as_atom()
        .map(|t| matches!(t.kind, TokenKind::String(_)))
        .unwrap_or(false)
}

// ============================================================================
// Instructions
// ============================================================================

/// Argument source for instruction parsing.
///
/// Abstracts the difference between flat and folded instruction syntax:
/// - Folded: `(i32.add (local.get 0) (local.get 1))` - args are children
/// - Flat: `local.get 0` - args are siblings in parent list
enum ArgSource<'a> {
    /// Arguments are children of the instruction S-expression.
    Folded(&'a [SExpr]),
    /// Arguments are siblings starting at given index in parent list.
    Flat { list: SExprList<'a>, start: usize },
}

impl<'a> ArgSource<'a> {
    /// Gets argument at index.
    fn get(&self, idx: usize) -> Option<&'a SExpr> {
        match self {
            ArgSource::Folded(args) => args.get(idx),
            ArgSource::Flat { list, start } => list.get(*start + idx),
        }
    }

    /// Counts consecutive arguments matching a predicate (for br_table).
    fn count_matching(&self, mut pred: impl FnMut(&SExpr) -> bool) -> usize {
        let mut count = 0;
        while let Some(s) = self.get(count) {
            if pred(s) {
                count += 1;
            } else {
                break;
            }
        }
        count
    }
}

/// Parses a body item (instruction with possible trailing arguments).
/// Returns the new index after consuming the instruction and its arguments.
fn parse_body_item(
    list: SExprList<'_>,
    idx: usize,
    out: &mut Vec<Instruction>,
    ctx: &mut ParseContext,
) -> Result<usize, ParseError> {
    let item = list
        .get(idx)
        .ok_or_else(|| ParseError::new("expected instruction", list.span))?;

    match item {
        SExpr::Atom(token) => {
            if let TokenKind::Keyword(kw) = &token.kind {
                // Flat instruction, may need to consume subsequent arguments
                let (kind, consumed) = parse_flat_instr(kw, list, idx + 1, token.span, ctx)?;
                out.push(make_instr(kind));
                Ok(idx + 1 + consumed)
            } else {
                // Not a keyword, skip it
                Ok(idx + 1)
            }
        }
        SExpr::List { .. } => {
            // Folded instruction, self-contained
            parse_instruction(item, out, ctx)?;
            Ok(idx + 1)
        }
    }
}

/// Parses a flat instruction, consuming arguments from subsequent siblings.
/// Returns the instruction kind and number of sibling items consumed.
fn parse_flat_instr(
    kw: &str,
    list: SExprList<'_>,
    arg_start: usize,
    span: Span,
    ctx: &mut ParseContext,
) -> Result<(InstructionKind, usize), ParseError> {
    let args = ArgSource::Flat { list, start: arg_start };
    parse_instr_impl(kw, args, span, ctx)
}

/// Parses an instruction (flat or folded).
fn parse_instruction(sexpr: &SExpr, out: &mut Vec<Instruction>, ctx: &mut ParseContext) -> Result<(), ParseError> {
    match sexpr {
        SExpr::Atom(token) => {
            if let TokenKind::Keyword(kw) = &token.kind {
                let kind = parse_folded_instr(kw, &[], token.span, ctx)?;
                out.push(make_instr(kind));
            }
        }
        SExpr::List { items, span } => {
            if items.is_empty() {
                return Ok(());
            }

            let head = &items[0];
            let kw = head
                .as_keyword()
                .ok_or_else(|| ParseError::expected("instruction keyword", "list", head.span()))?;

            // Handle control flow specially
            match kw {
                "block" | "loop" => parse_block_instr(kw, &items[1..], *span, out, ctx)?,
                "if" => parse_if_instr(&items[1..], *span, out, ctx)?,
                _ => {
                    // Parse nested arguments first (folded form)
                    for arg in &items[1..] {
                        if arg.as_list().is_some() && !is_immediate_list(arg) {
                            parse_instruction(arg, out, ctx)?;
                        }
                    }
                    // Then the instruction itself
                    let kind = parse_folded_instr(kw, &items[1..], *span, ctx)?;
                    out.push(make_instr(kind));
                }
            }
        }
    }
    Ok(())
}

/// Parses a block or loop instruction.
///
/// Grammar: `block ::= 'block' label? blocktype instr* 'end'`
///          `loop  ::= 'loop'  label? blocktype instr* 'end'`
fn parse_block_instr(
    kw: &str,
    args: &[SExpr],
    _span: Span,
    out: &mut Vec<Instruction>,
    ctx: &mut ParseContext,
) -> Result<(), ParseError> {
    let mut idx = 0;

    // Optional label
    let label = args.get(idx).and_then(|s| s.as_id()).map(String::from);
    if label.is_some() {
        idx += 1;
    }

    // Optional block type
    let block_type = parse_block_type(args.get(idx))?;
    if args.get(idx).map(|s| s.is_list_headed_by("result")).unwrap_or(false) {
        idx += 1;
    }

    ctx.push_label(label);

    // Emit block/loop
    let block_kind = if kw == "block" {
        InstructionKind::Block { block_type }
    } else {
        InstructionKind::Loop { block_type }
    };
    out.push(make_instr(block_kind));

    // Parse body
    for item in &args[idx..] {
        parse_instruction(item, out, ctx)?;
    }

    // Emit end
    ctx.pop_label();
    out.push(make_end());

    Ok(())
}

/// Parses an if instruction.
///
/// Grammar: `if ::= 'if' label? blocktype instr* '(' 'then' instr* ')' '(' 'else' instr* ')?'`
fn parse_if_instr(
    args: &[SExpr],
    _span: Span,
    out: &mut Vec<Instruction>,
    ctx: &mut ParseContext,
) -> Result<(), ParseError> {
    let mut idx = 0;

    // Optional label
    let label = args.get(idx).and_then(|s| s.as_id()).map(String::from);
    if label.is_some() {
        idx += 1;
    }

    // Optional block type
    let block_type = parse_block_type(args.get(idx))?;
    if args.get(idx).map(|s| s.is_list_headed_by("result")).unwrap_or(false) {
        idx += 1;
    }

    // Parse condition (everything before 'then')
    while let Some(item) = args.get(idx) {
        if item.is_list_headed_by("then") {
            break;
        }
        parse_instruction(item, out, ctx)?;
        idx += 1;
    }

    ctx.push_label(label);
    out.push(make_instr(InstructionKind::If { block_type }));

    // Parse then branch
    if let Some(then_item) = args.get(idx)
        && then_item.is_list_headed_by("then")
    {
        let then_list = then_item.as_list().unwrap();
        for item in then_list.iter_from(1) {
            parse_instruction(item, out, ctx)?;
        }
        idx += 1;
    }

    // Parse else branch
    if let Some(else_item) = args.get(idx)
        && else_item.is_list_headed_by("else")
    {
        out.push(make_instr(InstructionKind::Else));
        let else_list = else_item.as_list().unwrap();
        for item in else_list.iter_from(1) {
            parse_instruction(item, out, ctx)?;
        }
    }

    ctx.pop_label();
    out.push(make_end());

    Ok(())
}

/// Parses a block type.
///
/// Grammar: `blocktype ::= '(' 'result' valtype ')' | ε`
fn parse_block_type(sexpr: Option<&SExpr>) -> Result<BlockType, ParseError> {
    let Some(s) = sexpr else {
        return Ok(BlockType::Empty);
    };

    // Plain value type
    if let Some(kw) = s.as_keyword() {
        return match kw {
            "i32" => Ok(BlockType::Value(ValueType::I32)),
            "i64" => Ok(BlockType::Value(ValueType::I64)),
            "f32" => Ok(BlockType::Value(ValueType::F32)),
            "f64" => Ok(BlockType::Value(ValueType::F64)),
            _ => Ok(BlockType::Empty),
        };
    }

    // (result type)
    if s.is_list_headed_by("result") {
        let list = s.as_list().unwrap();
        if let Some(ty_sexpr) = list.get(1) {
            let ty = parse_valtype(ty_sexpr)?;
            return Ok(BlockType::Value(ty));
        }
    }

    Ok(BlockType::Empty)
}

/// Parses a folded instruction (arguments are children).
/// Discards the consumed count since folded form always consumes all children.
fn parse_folded_instr(
    kw: &str,
    args: &[SExpr],
    span: Span,
    ctx: &mut ParseContext,
) -> Result<InstructionKind, ParseError> {
    let args = ArgSource::Folded(args);
    let (kind, _consumed) = parse_instr_impl(kw, args, span, ctx)?;
    Ok(kind)
}

/// Unified instruction parser - single source of truth for all instructions.
///
/// Handles both flat and folded syntax through the ArgSource abstraction.
/// Returns (InstructionKind, args_consumed) where args_consumed is relevant
/// for flat syntax to know how many siblings were used.
fn parse_instr_impl(
    kw: &str,
    args: ArgSource<'_>,
    span: Span,
    ctx: &ParseContext,
) -> Result<(InstructionKind, usize), ParseError> {
    match kw {
        // Control - no arguments
        "unreachable" => Ok((InstructionKind::Unreachable, 0)),
        "nop" => Ok((InstructionKind::Nop, 0)),
        "return" => Ok((InstructionKind::Return, 0)),

        // Branch - label argument
        "br" => Ok((
            InstructionKind::Br {
                label_idx: parse_label_idx(args.get(0), ctx)?,
            },
            1,
        )),
        "br_if" => Ok((
            InstructionKind::BrIf {
                label_idx: parse_label_idx(args.get(0), ctx)?,
            },
            1,
        )),
        "br_table" => {
            let count = args.count_matching(|s| parse_label_idx(Some(s), ctx).is_ok());
            let mut labels = Vec::with_capacity(count);
            for i in 0..count {
                labels.push(parse_label_idx(args.get(i), ctx)?);
            }
            let default = labels
                .pop()
                .ok_or_else(|| ParseError::new("br_table requires at least one label", span))?;
            Ok((InstructionKind::BrTable { labels, default }, count))
        }

        // Call - function/type index
        "call" => Ok((
            InstructionKind::Call {
                func_idx: parse_index(args.get(0), Namespace::Func, ctx)?,
            },
            1,
        )),
        "call_indirect" => {
            // Syntax: call_indirect $table? (type $t)? | call_indirect $table? $type_idx?
            let mut consumed = 0;
            let mut table_idx = 0u32;
            let mut type_idx = 0u32;

            if let Some(a) = args.get(0) {
                // Check if first arg is (type ...), if so, table is 0
                if a.is_list_headed_by("type") {
                    let list = a.as_list().unwrap();
                    type_idx = parse_index(list.get(1), Namespace::Type, ctx)?;
                    consumed = 1;
                } else if let Ok(idx) = parse_index(Some(a), Namespace::Table, ctx) {
                    // First arg is table index
                    table_idx = idx;
                    consumed = 1;
                    // Check for type in second arg
                    if let Some(b) = args.get(1) {
                        if b.is_list_headed_by("type") {
                            let list = b.as_list().unwrap();
                            type_idx = parse_index(list.get(1), Namespace::Type, ctx)?;
                            consumed = 2;
                        } else if let Ok(tidx) = parse_index(Some(b), Namespace::Type, ctx) {
                            type_idx = tidx;
                            consumed = 2;
                        }
                    }
                } else if let Ok(tidx) = parse_index(Some(a), Namespace::Type, ctx) {
                    // First arg is type index (table implicit 0)
                    type_idx = tidx;
                    consumed = 1;
                }
            }

            Ok((InstructionKind::CallIndirect { type_idx, table_idx }, consumed))
        }

        // Parametric - no arguments
        "drop" => Ok((InstructionKind::Drop, 0)),
        "select" => Ok((InstructionKind::Select, 0)),

        // Variable - index argument
        "local.get" => Ok((
            InstructionKind::LocalGet {
                local_idx: parse_index(args.get(0), Namespace::Local, ctx)?,
            },
            1,
        )),
        "local.set" => Ok((
            InstructionKind::LocalSet {
                local_idx: parse_index(args.get(0), Namespace::Local, ctx)?,
            },
            1,
        )),
        "local.tee" => Ok((
            InstructionKind::LocalTee {
                local_idx: parse_index(args.get(0), Namespace::Local, ctx)?,
            },
            1,
        )),
        "global.get" => Ok((
            InstructionKind::GlobalGet {
                global_idx: parse_index(args.get(0), Namespace::Global, ctx)?,
            },
            1,
        )),
        "global.set" => Ok((
            InstructionKind::GlobalSet {
                global_idx: parse_index(args.get(0), Namespace::Global, ctx)?,
            },
            1,
        )),

        // Constants - value argument
        "i32.const" => Ok((
            InstructionKind::I32Const {
                value: parse_i32(args.get(0))?,
            },
            1,
        )),
        "i64.const" => Ok((
            InstructionKind::I64Const {
                value: parse_i64(args.get(0))?,
            },
            1,
        )),
        "f32.const" => Ok((
            InstructionKind::F32Const {
                value: parse_f32(args.get(0))?,
            },
            1,
        )),
        "f64.const" => Ok((
            InstructionKind::F64Const {
                value: parse_f64(args.get(0))?,
            },
            1,
        )),

        // Memory size/grow - no arguments
        "memory.size" => Ok((InstructionKind::MemorySize, 0)),
        "memory.grow" => Ok((InstructionKind::MemoryGrow, 0)),

        // Memory load/store - memarg
        "i32.load" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_32)?;
            Ok((InstructionKind::I32Load { memarg: m }, c))
        }
        "i64.load" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_64)?;
            Ok((InstructionKind::I64Load { memarg: m }, c))
        }
        "f32.load" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_32)?;
            Ok((InstructionKind::F32Load { memarg: m }, c))
        }
        "f64.load" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_64)?;
            Ok((InstructionKind::F64Load { memarg: m }, c))
        }
        "i32.load8_s" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_8)?;
            Ok((InstructionKind::I32Load8S { memarg: m }, c))
        }
        "i32.load8_u" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_8)?;
            Ok((InstructionKind::I32Load8U { memarg: m }, c))
        }
        "i32.load16_s" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_16)?;
            Ok((InstructionKind::I32Load16S { memarg: m }, c))
        }
        "i32.load16_u" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_16)?;
            Ok((InstructionKind::I32Load16U { memarg: m }, c))
        }
        "i64.load8_s" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_8)?;
            Ok((InstructionKind::I64Load8S { memarg: m }, c))
        }
        "i64.load8_u" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_8)?;
            Ok((InstructionKind::I64Load8U { memarg: m }, c))
        }
        "i64.load16_s" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_16)?;
            Ok((InstructionKind::I64Load16S { memarg: m }, c))
        }
        "i64.load16_u" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_16)?;
            Ok((InstructionKind::I64Load16U { memarg: m }, c))
        }
        "i64.load32_s" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_32)?;
            Ok((InstructionKind::I64Load32S { memarg: m }, c))
        }
        "i64.load32_u" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_32)?;
            Ok((InstructionKind::I64Load32U { memarg: m }, c))
        }
        "i32.store" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_32)?;
            Ok((InstructionKind::I32Store { memarg: m }, c))
        }
        "i64.store" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_64)?;
            Ok((InstructionKind::I64Store { memarg: m }, c))
        }
        "f32.store" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_32)?;
            Ok((InstructionKind::F32Store { memarg: m }, c))
        }
        "f64.store" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_64)?;
            Ok((InstructionKind::F64Store { memarg: m }, c))
        }
        "i32.store8" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_8)?;
            Ok((InstructionKind::I32Store8 { memarg: m }, c))
        }
        "i32.store16" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_16)?;
            Ok((InstructionKind::I32Store16 { memarg: m }, c))
        }
        "i64.store8" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_8)?;
            Ok((InstructionKind::I64Store8 { memarg: m }, c))
        }
        "i64.store16" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_16)?;
            Ok((InstructionKind::I64Store16 { memarg: m }, c))
        }
        "i64.store32" => {
            let (m, c) = parse_memarg_from_args(&args, ALIGN_32)?;
            Ok((InstructionKind::I64Store32 { memarg: m }, c))
        }

        // i32 operations - no arguments
        "i32.add" => Ok((InstructionKind::I32Add, 0)),
        "i32.sub" => Ok((InstructionKind::I32Sub, 0)),
        "i32.mul" => Ok((InstructionKind::I32Mul, 0)),
        "i32.div_s" => Ok((InstructionKind::I32DivS, 0)),
        "i32.div_u" => Ok((InstructionKind::I32DivU, 0)),
        "i32.rem_s" => Ok((InstructionKind::I32RemS, 0)),
        "i32.rem_u" => Ok((InstructionKind::I32RemU, 0)),
        "i32.and" => Ok((InstructionKind::I32And, 0)),
        "i32.or" => Ok((InstructionKind::I32Or, 0)),
        "i32.xor" => Ok((InstructionKind::I32Xor, 0)),
        "i32.shl" => Ok((InstructionKind::I32Shl, 0)),
        "i32.shr_s" => Ok((InstructionKind::I32ShrS, 0)),
        "i32.shr_u" => Ok((InstructionKind::I32ShrU, 0)),
        "i32.rotl" => Ok((InstructionKind::I32Rotl, 0)),
        "i32.rotr" => Ok((InstructionKind::I32Rotr, 0)),
        "i32.clz" => Ok((InstructionKind::I32Clz, 0)),
        "i32.ctz" => Ok((InstructionKind::I32Ctz, 0)),
        "i32.popcnt" => Ok((InstructionKind::I32Popcnt, 0)),
        "i32.eqz" => Ok((InstructionKind::I32Eqz, 0)),
        "i32.eq" => Ok((InstructionKind::I32Eq, 0)),
        "i32.ne" => Ok((InstructionKind::I32Ne, 0)),
        "i32.lt_s" => Ok((InstructionKind::I32LtS, 0)),
        "i32.lt_u" => Ok((InstructionKind::I32LtU, 0)),
        "i32.gt_s" => Ok((InstructionKind::I32GtS, 0)),
        "i32.gt_u" => Ok((InstructionKind::I32GtU, 0)),
        "i32.le_s" => Ok((InstructionKind::I32LeS, 0)),
        "i32.le_u" => Ok((InstructionKind::I32LeU, 0)),
        "i32.ge_s" => Ok((InstructionKind::I32GeS, 0)),
        "i32.ge_u" => Ok((InstructionKind::I32GeU, 0)),
        "i32.wrap_i64" => Ok((InstructionKind::I32WrapI64, 0)),
        "i32.extend8_s" => Ok((InstructionKind::I32Extend8S, 0)),
        "i32.extend16_s" => Ok((InstructionKind::I32Extend16S, 0)),
        "i32.trunc_f32_s" => Ok((InstructionKind::I32TruncF32S, 0)),
        "i32.trunc_f32_u" => Ok((InstructionKind::I32TruncF32U, 0)),
        "i32.trunc_f64_s" => Ok((InstructionKind::I32TruncF64S, 0)),
        "i32.trunc_f64_u" => Ok((InstructionKind::I32TruncF64U, 0)),
        "i32.reinterpret_f32" => Ok((InstructionKind::I32ReinterpretF32, 0)),

        // i64 operations
        "i64.add" => Ok((InstructionKind::I64Add, 0)),
        "i64.sub" => Ok((InstructionKind::I64Sub, 0)),
        "i64.mul" => Ok((InstructionKind::I64Mul, 0)),
        "i64.div_s" => Ok((InstructionKind::I64DivS, 0)),
        "i64.div_u" => Ok((InstructionKind::I64DivU, 0)),
        "i64.rem_s" => Ok((InstructionKind::I64RemS, 0)),
        "i64.rem_u" => Ok((InstructionKind::I64RemU, 0)),
        "i64.and" => Ok((InstructionKind::I64And, 0)),
        "i64.or" => Ok((InstructionKind::I64Or, 0)),
        "i64.xor" => Ok((InstructionKind::I64Xor, 0)),
        "i64.shl" => Ok((InstructionKind::I64Shl, 0)),
        "i64.shr_s" => Ok((InstructionKind::I64ShrS, 0)),
        "i64.shr_u" => Ok((InstructionKind::I64ShrU, 0)),
        "i64.rotl" => Ok((InstructionKind::I64Rotl, 0)),
        "i64.rotr" => Ok((InstructionKind::I64Rotr, 0)),
        "i64.clz" => Ok((InstructionKind::I64Clz, 0)),
        "i64.ctz" => Ok((InstructionKind::I64Ctz, 0)),
        "i64.popcnt" => Ok((InstructionKind::I64Popcnt, 0)),
        "i64.eqz" => Ok((InstructionKind::I64Eqz, 0)),
        "i64.eq" => Ok((InstructionKind::I64Eq, 0)),
        "i64.ne" => Ok((InstructionKind::I64Ne, 0)),
        "i64.lt_s" => Ok((InstructionKind::I64LtS, 0)),
        "i64.lt_u" => Ok((InstructionKind::I64LtU, 0)),
        "i64.gt_s" => Ok((InstructionKind::I64GtS, 0)),
        "i64.gt_u" => Ok((InstructionKind::I64GtU, 0)),
        "i64.le_s" => Ok((InstructionKind::I64LeS, 0)),
        "i64.le_u" => Ok((InstructionKind::I64LeU, 0)),
        "i64.ge_s" => Ok((InstructionKind::I64GeS, 0)),
        "i64.ge_u" => Ok((InstructionKind::I64GeU, 0)),
        "i64.extend_i32_s" => Ok((InstructionKind::I64ExtendI32S, 0)),
        "i64.extend_i32_u" => Ok((InstructionKind::I64ExtendI32U, 0)),
        "i64.extend8_s" => Ok((InstructionKind::I64Extend8S, 0)),
        "i64.extend16_s" => Ok((InstructionKind::I64Extend16S, 0)),
        "i64.extend32_s" => Ok((InstructionKind::I64Extend32S, 0)),
        "i64.trunc_f32_s" => Ok((InstructionKind::I64TruncF32S, 0)),
        "i64.trunc_f32_u" => Ok((InstructionKind::I64TruncF32U, 0)),
        "i64.trunc_f64_s" => Ok((InstructionKind::I64TruncF64S, 0)),
        "i64.trunc_f64_u" => Ok((InstructionKind::I64TruncF64U, 0)),
        "i64.reinterpret_f64" => Ok((InstructionKind::I64ReinterpretF64, 0)),

        // f32 operations
        "f32.add" => Ok((InstructionKind::F32Add, 0)),
        "f32.sub" => Ok((InstructionKind::F32Sub, 0)),
        "f32.mul" => Ok((InstructionKind::F32Mul, 0)),
        "f32.div" => Ok((InstructionKind::F32Div, 0)),
        "f32.min" => Ok((InstructionKind::F32Min, 0)),
        "f32.max" => Ok((InstructionKind::F32Max, 0)),
        "f32.abs" => Ok((InstructionKind::F32Abs, 0)),
        "f32.neg" => Ok((InstructionKind::F32Neg, 0)),
        "f32.ceil" => Ok((InstructionKind::F32Ceil, 0)),
        "f32.floor" => Ok((InstructionKind::F32Floor, 0)),
        "f32.trunc" => Ok((InstructionKind::F32Trunc, 0)),
        "f32.nearest" => Ok((InstructionKind::F32Nearest, 0)),
        "f32.sqrt" => Ok((InstructionKind::F32Sqrt, 0)),
        "f32.copysign" => Ok((InstructionKind::F32Copysign, 0)),
        "f32.eq" => Ok((InstructionKind::F32Eq, 0)),
        "f32.ne" => Ok((InstructionKind::F32Ne, 0)),
        "f32.lt" => Ok((InstructionKind::F32Lt, 0)),
        "f32.gt" => Ok((InstructionKind::F32Gt, 0)),
        "f32.le" => Ok((InstructionKind::F32Le, 0)),
        "f32.ge" => Ok((InstructionKind::F32Ge, 0)),
        "f32.convert_i32_s" => Ok((InstructionKind::F32ConvertI32S, 0)),
        "f32.convert_i32_u" => Ok((InstructionKind::F32ConvertI32U, 0)),
        "f32.convert_i64_s" => Ok((InstructionKind::F32ConvertI64S, 0)),
        "f32.convert_i64_u" => Ok((InstructionKind::F32ConvertI64U, 0)),
        "f32.demote_f64" => Ok((InstructionKind::F32DemoteF64, 0)),
        "f32.reinterpret_i32" => Ok((InstructionKind::F32ReinterpretI32, 0)),

        // f64 operations
        "f64.add" => Ok((InstructionKind::F64Add, 0)),
        "f64.sub" => Ok((InstructionKind::F64Sub, 0)),
        "f64.mul" => Ok((InstructionKind::F64Mul, 0)),
        "f64.div" => Ok((InstructionKind::F64Div, 0)),
        "f64.min" => Ok((InstructionKind::F64Min, 0)),
        "f64.max" => Ok((InstructionKind::F64Max, 0)),
        "f64.abs" => Ok((InstructionKind::F64Abs, 0)),
        "f64.neg" => Ok((InstructionKind::F64Neg, 0)),
        "f64.ceil" => Ok((InstructionKind::F64Ceil, 0)),
        "f64.floor" => Ok((InstructionKind::F64Floor, 0)),
        "f64.trunc" => Ok((InstructionKind::F64Trunc, 0)),
        "f64.nearest" => Ok((InstructionKind::F64Nearest, 0)),
        "f64.sqrt" => Ok((InstructionKind::F64Sqrt, 0)),
        "f64.copysign" => Ok((InstructionKind::F64Copysign, 0)),
        "f64.eq" => Ok((InstructionKind::F64Eq, 0)),
        "f64.ne" => Ok((InstructionKind::F64Ne, 0)),
        "f64.lt" => Ok((InstructionKind::F64Lt, 0)),
        "f64.gt" => Ok((InstructionKind::F64Gt, 0)),
        "f64.le" => Ok((InstructionKind::F64Le, 0)),
        "f64.ge" => Ok((InstructionKind::F64Ge, 0)),
        "f64.convert_i32_s" => Ok((InstructionKind::F64ConvertI32S, 0)),
        "f64.convert_i32_u" => Ok((InstructionKind::F64ConvertI32U, 0)),
        "f64.convert_i64_s" => Ok((InstructionKind::F64ConvertI64S, 0)),
        "f64.convert_i64_u" => Ok((InstructionKind::F64ConvertI64U, 0)),
        "f64.promote_f32" => Ok((InstructionKind::F64PromoteF32, 0)),
        "f64.reinterpret_i64" => Ok((InstructionKind::F64ReinterpretI64, 0)),

        _ => Err(ParseError::new(format!("unknown instruction: {}", kw), span)),
    }
}

/// Parses optional `offset=N` and `align=N` for memory instructions.
///
/// WAT uses bytes for alignment (`align=4`), but the binary format uses log2
/// (`align=2`). This function converts: `trailing_zeros(4) = 2`.
///
/// If omitted, `offset` defaults to 0 and `align` defaults to `natural_align`
/// (the operand size of the instruction, e.g. ALIGN_32 for i32.load).
fn parse_memarg_from_args(args: &ArgSource<'_>, natural_align: u32) -> Result<(MemArg, usize), ParseError> {
    let mut offset = 0u32;
    let mut align = natural_align;
    let mut consumed = 0;

    while let Some(s) = args.get(consumed) {
        if let Some(kw) = s.as_keyword() {
            if let Some(val) = kw.strip_prefix("offset=") {
                offset = val.parse().map_err(|_| ParseError::new("invalid offset", s.span()))?;
                consumed += 1;
            } else if let Some(val) = kw.strip_prefix("align=") {
                // WAT: align=4 (bytes) -> binary: align=2 (log2)
                let bytes: u32 = val.parse().map_err(|_| ParseError::new("invalid align", s.span()))?;
                align = bytes.trailing_zeros();
                consumed += 1;
            } else {
                break;
            }
        } else {
            break;
        }
    }

    Ok((MemArg { align, offset }, consumed))
}

// ============================================================================
// Helpers
// ============================================================================

/// Creates an Instruction with empty position.
/// Parses a constant expression and appends a trailing `End` instruction.
///
/// The binary format terminates constant expressions with 0x0B (End). The
/// binary parser includes this in the instruction vector; the WAT parser must
/// do the same so the runtime evaluator sees a consistent representation.
fn parse_const_expr(sexpr: &SExpr, ctx: &mut ParseContext) -> Result<Vec<Instruction>, ParseError> {
    let mut instrs = Vec::new();
    parse_instruction(sexpr, &mut instrs, ctx)?;
    instrs.push(make_end());
    Ok(instrs)
}

/// Parses multiple S-expressions as a constant expression with trailing `End`.
fn parse_const_expr_multi<'a>(
    items: impl Iterator<Item = &'a SExpr>,
    ctx: &mut ParseContext,
) -> Result<Vec<Instruction>, ParseError> {
    let mut instrs = Vec::new();
    for item in items {
        parse_instruction(item, &mut instrs, ctx)?;
    }
    instrs.push(make_end());
    Ok(instrs)
}

fn make_instr(kind: InstructionKind) -> Instruction {
    Instruction {
        kind,
        position: ByteRange { offset: 0, length: 0 },
        original_bytes: Vec::new(),
    }
}

fn make_end() -> Instruction {
    make_instr(InstructionKind::End)
}

/// Checks if an S-expression is an immediate (not a nested instruction).
fn is_immediate_list(sexpr: &SExpr) -> bool {
    sexpr.is_list_headed_by("type") || sexpr.is_list_headed_by("result") || sexpr.is_list_headed_by("param")
}

/// Parses an index (numeric or named).
///
/// Grammar: `idx ::= u32 | id`
fn parse_index(sexpr: Option<&SExpr>, ns: Namespace, ctx: &ParseContext) -> Result<u32, ParseError> {
    let s = sexpr.ok_or_else(|| {
        ParseError::new(
            format!("expected {} index", ns.name()),
            Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            },
        )
    })?;

    if let Some(name) = s.as_id() {
        return ctx.resolve(ns, name, s.span());
    }

    if let Some(token) = s.as_atom()
        && let TokenKind::Integer(sv) = &token.kind
    {
        return sv
            .to_u64()
            .map(|n| n as u32)
            .ok_or_else(|| ParseError::new("index out of range", token.span));
    }

    Err(ParseError::expected("index", "other", s.span()))
}

/// Parses a label index (numeric depth or named).
///
/// Grammar: `labelidx ::= u32 | id`
fn parse_label_idx(sexpr: Option<&SExpr>, ctx: &ParseContext) -> Result<u32, ParseError> {
    let s = sexpr.ok_or_else(|| {
        ParseError::new(
            "expected label",
            Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            },
        )
    })?;

    if let Some(name) = s.as_id() {
        return ctx.resolve_label(name, s.span());
    }

    if let Some(token) = s.as_atom()
        && let TokenKind::Integer(sv) = &token.kind
    {
        return sv
            .to_u64()
            .map(|n| n as u32)
            .ok_or_else(|| ParseError::new("label out of range", token.span));
    }

    Err(ParseError::expected("label", "other", s.span()))
}

/// Parses a string literal.
///
/// Grammar: `string ::= '"' stringchar* '"'`
fn parse_string(sexpr: &SExpr) -> Result<String, ParseError> {
    let token = sexpr.expect_atom()?;
    if let TokenKind::String(bytes) = &token.kind {
        String::from_utf8(bytes.clone()).map_err(|_| ParseError::new("invalid UTF-8 string", token.span))
    } else {
        Err(ParseError::expected("string", &format!("{:?}", token.kind), token.span))
    }
}

/// Parses limits (min, optional max).
///
/// Grammar: `limits ::= u32 | u32 u32`
fn parse_limits(list: SExprList<'_>, start: usize) -> Result<Limits, ParseError> {
    let min_sexpr = list
        .get(start)
        .ok_or_else(|| ParseError::new("expected minimum limit", list.span))?;
    let min = parse_u32(min_sexpr)?;

    let max = list.get(start + 1).and_then(|s| {
        if s.as_atom()
            .map(|t| matches!(t.kind, TokenKind::Integer(_)))
            .unwrap_or(false)
        {
            parse_u32(s).ok()
        } else {
            None
        }
    });

    Ok(Limits { min, max })
}

/// Returns how many items the limits consumed.
fn limits_len(limits: &Limits) -> usize {
    if limits.max.is_some() { 2 } else { 1 }
}

/// Parses a reference type.
///
/// Grammar: `reftype ::= 'funcref' | 'externref'`
fn parse_reftype(sexpr: Option<&SExpr>) -> Result<RefType, ParseError> {
    let s = sexpr.ok_or_else(|| {
        ParseError::new(
            "expected reference type",
            Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            },
        )
    })?;

    match s.as_keyword() {
        Some("funcref") => Ok(RefType::FuncRef),
        Some("externref") => Ok(RefType::ExternRef),
        Some(kw) => Err(ParseError::expected("reference type", kw, s.span())),
        None => Err(ParseError::expected("reference type", "list", s.span())),
    }
}

/// Parses an i32 value.
fn parse_i32(sexpr: Option<&SExpr>) -> Result<i32, ParseError> {
    let s = sexpr.ok_or_else(|| {
        ParseError::new(
            "expected i32",
            Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            },
        )
    })?;
    let token = s.expect_atom()?;

    if let TokenKind::Integer(sv) = &token.kind {
        sv.to_i64()
            .and_then(|n| i32::try_from(n).ok())
            .or_else(|| sv.to_u64().and_then(|n| u32::try_from(n).ok().map(|u| u as i32)))
            .ok_or_else(|| ParseError::new("i32 out of range", token.span))
    } else {
        Err(ParseError::expected("i32", &format!("{:?}", token.kind), token.span))
    }
}

/// Parses an i64 value.
fn parse_i64(sexpr: Option<&SExpr>) -> Result<i64, ParseError> {
    let s = sexpr.ok_or_else(|| {
        ParseError::new(
            "expected i64",
            Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            },
        )
    })?;
    let token = s.expect_atom()?;

    if let TokenKind::Integer(sv) = &token.kind {
        sv.to_i64()
            .or_else(|| sv.to_u64().map(|u| u as i64))
            .ok_or_else(|| ParseError::new("i64 out of range", token.span))
    } else {
        Err(ParseError::expected("i64", &format!("{:?}", token.kind), token.span))
    }
}

/// Parses an f32 value.
fn parse_f32(sexpr: Option<&SExpr>) -> Result<f32, ParseError> {
    let s = sexpr.ok_or_else(|| {
        ParseError::new(
            "expected f32",
            Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            },
        )
    })?;
    let token = s.expect_atom()?;

    match &token.kind {
        TokenKind::Float(fl) => Ok(fl.to_f64() as f32),
        TokenKind::Integer(sv) => {
            let val = if sv.negative {
                -(sv.value as f64)
            } else {
                sv.value as f64
            };
            Ok(val as f32)
        }
        _ => Err(ParseError::expected("f32", &format!("{:?}", token.kind), token.span)),
    }
}

/// Parses an f64 value.
fn parse_f64(sexpr: Option<&SExpr>) -> Result<f64, ParseError> {
    let s = sexpr.ok_or_else(|| {
        ParseError::new(
            "expected f64",
            Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            },
        )
    })?;
    let token = s.expect_atom()?;

    match &token.kind {
        TokenKind::Float(fl) => Ok(fl.to_f64()),
        TokenKind::Integer(sv) => Ok(if sv.negative {
            -(sv.value as f64)
        } else {
            sv.value as f64
        }),
        _ => Err(ParseError::expected("f64", &format!("{:?}", token.kind), token.span)),
    }
}

/// Parses a u32 value.
fn parse_u32(sexpr: &SExpr) -> Result<u32, ParseError> {
    let token = sexpr.expect_atom()?;
    if let TokenKind::Integer(sv) = &token.kind {
        sv.to_u64()
            .and_then(|n| u32::try_from(n).ok())
            .ok_or_else(|| ParseError::new("u32 out of range", token.span))
    } else {
        Err(ParseError::expected("u32", &format!("{:?}", token.kind), token.span))
    }
}

/// Compresses consecutive identical types.
fn compress_locals(locals: &[ValueType]) -> Vec<(u32, ValueType)> {
    let mut result = Vec::new();
    let mut iter = locals.iter().peekable();

    while let Some(&ty) = iter.next() {
        let mut count = 1u32;
        while iter.peek() == Some(&&ty) {
            iter.next();
            count += 1;
        }
        result.push((count, ty));
    }

    result
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_module() {
        let module = parse("(module)").unwrap();
        assert_eq!(module.functions.len(), 0);
    }

    #[test]
    fn simple_function() {
        let wat = "(module (func (result i32) (i32.const 42)))";
        let module = parse(wat).unwrap();
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.code.code.len(), 1);
    }

    #[test]
    fn function_with_params() {
        let wat = "(module (func (param i32 i32) (result i32) (i32.add (local.get 0) (local.get 1))))";
        let module = parse(wat).unwrap();
        let func_type = &module.types.types[module.functions.functions[0].ftype_index as usize];
        assert_eq!(func_type.parameters.len(), 2);
        assert_eq!(func_type.return_types.len(), 1);
    }

    #[test]
    fn named_function() {
        let wat = "(module (func $add (param $a i32) (param $b i32) (result i32) \
                   (i32.add (local.get $a) (local.get $b))))";
        let module = parse(wat).unwrap();
        assert_eq!(module.functions.len(), 1);
    }

    #[test]
    fn nested_blocks() {
        let wat = "(module (func (block $outer (block $inner (br $outer)))))";
        let module = parse(wat).unwrap();
        assert_eq!(module.functions.len(), 1);
    }

    #[test]
    fn if_then_else() {
        let wat = "(module (func (param i32) (result i32) \
                   (if (result i32) (local.get 0) (then (i32.const 1)) (else (i32.const 0)))))";
        let module = parse(wat).unwrap();
        assert_eq!(module.functions.len(), 1);
    }

    #[test]
    fn import_function() {
        let wat = r#"(module (import "env" "log" (func (param i32))))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.imports.imports.len(), 1);
    }

    #[test]
    fn export_function() {
        let wat = r#"(module (func $main (result i32) (i32.const 0)) (export "main" (func $main)))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.exports.exports.len(), 1);
    }

    #[test]
    fn memory() {
        let wat = "(module (memory 1 4))";
        let module = parse(wat).unwrap();
        assert_eq!(module.memory.memory.len(), 1);
        assert_eq!(module.memory.memory[0].limits.min, 1);
        assert_eq!(module.memory.memory[0].limits.max, Some(4));
    }

    #[test]
    fn global() {
        let wat = "(module (global $g (mut i32) (i32.const 42)))";
        let module = parse(wat).unwrap();
        assert_eq!(module.globals.globals.len(), 1);
        assert!(module.globals.globals[0].global_type.mutable);
    }

    #[test]
    fn start_section() {
        use crate::parser::module::Positional;
        let wat = "(module (func $main) (start $main))";
        let module = parse(wat).unwrap();
        assert!(module.start.has_position());
    }

    #[test]
    fn type_definition() {
        let wat = "(module (type $sig (func (param i32) (result i32))))";
        let module = parse(wat).unwrap();
        assert_eq!(module.types.types.len(), 1);
    }

    #[test]
    fn error_unknown_instruction() {
        let result = parse("(module (func (not.an.instruction)))");
        assert!(result.is_err());
    }

    #[test]
    fn br_table_multiple_labels() {
        let wat = r#"(module
            (func (param i32) (result i32)
                (block $b0
                    (block $b1
                        (block $b2
                            (br_table $b0 $b1 $b2 (local.get 0)))
                        (return (i32.const 2)))
                    (return (i32.const 1)))
                (i32.const 0)))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.functions.len(), 1);
    }

    #[test]
    fn flat_instruction_syntax() {
        let wat = r#"(module
            (func (param i32 i32) (result i32)
                local.get 0
                local.get 1
                i32.add))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.functions.len(), 1);
    }

    #[test]
    fn memory_with_offset_align() {
        let wat = r#"(module
            (memory 1)
            (func (param i32) (result i32)
                (i32.load offset=4 align=4 (local.get 0))))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.functions.len(), 1);
    }

    #[test]
    fn flat_memory_with_offset_align() {
        let wat = r#"(module
            (memory 1)
            (func (param i32) (result i32)
                local.get 0
                i32.load offset=8 align=2))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.functions.len(), 1);
    }

    #[test]
    fn multiple_functions_cross_reference() {
        let wat = r#"(module
            (func $helper (param i32) (result i32)
                (i32.add (local.get 0) (i32.const 1)))
            (func $main (result i32)
                (call $helper (i32.const 41))))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.functions.len(), 2);
    }

    #[test]
    fn mixed_named_numeric_indices() {
        let wat = r#"(module
            (func $first (result i32) (i32.const 1))
            (func $second (result i32) (i32.const 2))
            (func (result i32)
                (i32.add (call $first) (call 1))))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.functions.len(), 3);
    }

    #[test]
    fn import_ordering() {
        let wat = r#"(module
            (import "env" "mem" (memory 1))
            (import "env" "log" (func (param i32)))
            (func $main (call 0 (i32.const 42))))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.imports.imports.len(), 2);
        assert_eq!(module.functions.len(), 1);
    }

    #[test]
    fn reference_types_in_params() {
        let wat = "(module (func (param funcref externref)))";
        let module = parse(wat).unwrap();
        let func_type = &module.types.types[0];
        assert_eq!(func_type.parameters.len(), 2);
        assert_eq!(func_type.parameters[0], ValueType::FuncRef);
        assert_eq!(func_type.parameters[1], ValueType::ExternRef);
    }

    // Element segment tests

    #[test]
    fn elem_active_abbreviated() {
        let wat = r#"(module
            (table 2 funcref)
            (func $f1)
            (func $f2)
            (elem (i32.const 0) $f1 $f2))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.elements.elements.len(), 1);
        assert!(matches!(
            module.elements.elements[0].mode,
            ElementMode::Active { table_index: 0, .. }
        ));
        assert_eq!(module.elements.elements[0].init.len(), 2);
    }

    #[test]
    fn elem_active_with_table() {
        let wat = r#"(module
            (table $t 2 funcref)
            (func $f1)
            (elem (table $t) (offset (i32.const 0)) funcref (ref.func $f1)))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.elements.elements.len(), 1);
        assert!(matches!(
            module.elements.elements[0].mode,
            ElementMode::Active { table_index: 0, .. }
        ));
    }

    #[test]
    fn elem_passive() {
        let wat = r#"(module
            (func $f1)
            (func $f2)
            (elem funcref (ref.func $f1) (ref.func $f2)))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.elements.elements.len(), 1);
        assert!(matches!(module.elements.elements[0].mode, ElementMode::Passive));
        assert_eq!(module.elements.elements[0].init.len(), 2);
    }

    #[test]
    fn elem_declarative() {
        let wat = r#"(module
            (func $f1)
            (elem declare funcref (ref.func $f1)))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.elements.elements.len(), 1);
        assert!(matches!(module.elements.elements[0].mode, ElementMode::Declarative));
    }

    #[test]
    fn elem_with_func_keyword() {
        let wat = r#"(module
            (table 2 funcref)
            (func $f1)
            (func $f2)
            (elem (i32.const 0) func $f1 $f2))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.elements.elements.len(), 1);
        assert_eq!(module.elements.elements[0].init.len(), 2);
    }

    // Data segment tests

    #[test]
    fn data_active_abbreviated() {
        let wat = r#"(module
            (memory 1)
            (data (i32.const 0) "hello"))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.data.data.len(), 1);
        assert!(matches!(
            module.data.data[0].mode,
            DataMode::Active { memory_index: 0, .. }
        ));
        assert_eq!(module.data.data[0].init, b"hello");
    }

    #[test]
    fn data_active_with_memory() {
        let wat = r#"(module
            (memory $m 1)
            (data (memory $m) (offset (i32.const 0)) "hello"))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.data.data.len(), 1);
        assert!(matches!(
            module.data.data[0].mode,
            DataMode::Active { memory_index: 0, .. }
        ));
    }

    #[test]
    fn data_passive() {
        let wat = r#"(module (data "hello world"))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.data.data.len(), 1);
        assert!(matches!(module.data.data[0].mode, DataMode::Passive));
        assert_eq!(module.data.data[0].init, b"hello world");
    }

    #[test]
    fn data_multiple_strings() {
        let wat = r#"(module
            (memory 1)
            (data (i32.const 0) "hello" " " "world"))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.data.data[0].init, b"hello world");
    }

    // ====================================================================
    // Inline exports
    // ====================================================================

    #[test]
    fn inline_export_func() {
        let wat = r#"(module (func (export "f") (result i32) (i32.const 1)))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.exports.exports.len(), 1);
        assert_eq!(module.exports.exports[0].name, "f");
        assert!(matches!(module.exports.exports[0].index, ExportIndex::Function(0)));
    }

    #[test]
    fn inline_export_memory() {
        let wat = r#"(module (memory (export "mem") 1))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.exports.exports.len(), 1);
        assert_eq!(module.exports.exports[0].name, "mem");
        assert!(matches!(module.exports.exports[0].index, ExportIndex::Memory(0)));
    }

    #[test]
    fn inline_export_table() {
        let wat = r#"(module (table (export "t") 1 funcref))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.exports.exports.len(), 1);
        assert_eq!(module.exports.exports[0].name, "t");
        assert!(matches!(module.exports.exports[0].index, ExportIndex::Table(0)));
    }

    #[test]
    fn inline_export_global() {
        let wat = r#"(module (global (export "g") i32 (i32.const 42)))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.exports.exports.len(), 1);
        assert_eq!(module.exports.exports[0].name, "g");
        assert!(matches!(module.exports.exports[0].index, ExportIndex::Global(0)));
    }

    #[test]
    fn inline_export_multiple_on_one_item() {
        let wat = r#"(module (memory (export "m1") (export "m2") 1))"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.exports.exports.len(), 2);
        assert_eq!(module.exports.exports[0].name, "m1");
        assert_eq!(module.exports.exports[1].name, "m2");
    }

    #[test]
    fn inline_export_named_item() {
        let wat = r#"(module
            (memory $mem (export "memory") 1)
            (func $f (export "_start") (nop))
        )"#;
        let module = parse(wat).unwrap();
        assert_eq!(module.exports.exports.len(), 2);
        assert_eq!(module.exports.exports[0].name, "memory");
        assert!(matches!(module.exports.exports[0].index, ExportIndex::Memory(0)));
        assert_eq!(module.exports.exports[1].name, "_start");
        assert!(matches!(module.exports.exports[1].index, ExportIndex::Function(0)));
    }

    // ====================================================================
    // Structured control flow via StructureBuilder
    // ====================================================================

    #[test]
    fn block_produces_structured_instruction() {
        use crate::parser::structured::StructuredInstruction;
        let wat = "(module (func (block (nop))))";
        let module = parse(wat).unwrap();
        let body = &module.code.code[0].body.body;
        assert!(
            matches!(body[0], StructuredInstruction::Block { .. }),
            "expected Block, got {:?}",
            body[0]
        );
    }

    #[test]
    fn nested_blocks_structured() {
        use crate::parser::structured::StructuredInstruction;
        let wat = "(module (func (result i32)
            (block $outer (result i32)
                (block $inner (result i32)
                    (i32.const 42)
                    (br $outer)
                )
            )
        ))";
        let module = parse(wat).unwrap();
        let body = &module.code.code[0].body.body;
        assert!(matches!(body[0], StructuredInstruction::Block { .. }));
        if let StructuredInstruction::Block { body: inner, .. } = &body[0] {
            assert!(matches!(inner[0], StructuredInstruction::Block { .. }));
        }
    }

    #[test]
    fn if_else_structured() {
        use crate::parser::structured::StructuredInstruction;
        let wat = "(module (func (param i32) (result i32)
            (if (result i32) (local.get 0)
                (then (i32.const 1))
                (else (i32.const 0))
            )
        ))";
        let module = parse(wat).unwrap();
        let body = &module.code.code[0].body.body;
        // body[0] is the condition (local.get 0), body[1] is the If
        assert!(matches!(body[0], StructuredInstruction::Plain(_)));
        assert!(matches!(
            body[1],
            StructuredInstruction::If {
                else_branch: Some(_),
                ..
            }
        ));
    }

    #[test]
    fn loop_structured() {
        use crate::parser::structured::StructuredInstruction;
        let wat = "(module (func (loop $l (br $l))))";
        let module = parse(wat).unwrap();
        let body = &module.code.code[0].body.body;
        assert!(matches!(body[0], StructuredInstruction::Loop { .. }));
    }
}
