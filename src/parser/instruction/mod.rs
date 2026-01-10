//! WebAssembly instruction representation and decoding
//!
//! This module defines the instruction types, decoding logic, and validation
//! for WebAssembly bytecode instructions.

pub mod decode;
pub mod encode;
pub mod streaming_decode;

pub use decode::{DecodeError, InstructionIterator, ParseType};
pub use streaming_decode::{VecCollector, decode_function, decode_with_processor};

use super::module::ValueType;
use fhex::ToHex;
use std::fmt;

/// Decode a constant expression (used in data segments, globals, etc)
pub fn decode_constant_expression(
    reader: &mut super::reader::Reader,
    imports: &super::module::ImportSection,
    return_type: ValueType,
) -> Result<Vec<Instruction>, DecodeError> {
    let mut validator = super::validate::ConstantExpressionValidator::new(imports, return_type);
    let mut collector = VecCollector::new();
    decode_with_processor(&mut validator, &mut collector, ParseType::ReadTillEnd, reader)?;
    Ok(collector.into_instructions())
}

/// Decode a constant expression that may contain ref.func instructions
pub fn decode_constant_expression_with_ref_func(
    reader: &mut super::reader::Reader,
    imports: &super::module::ImportSection,
    return_type: ValueType,
    total_functions: u32,
) -> Result<Vec<Instruction>, DecodeError> {
    let mut validator =
        super::validate::ConstantExpressionValidator::new(imports, return_type).with_function_count(total_functions);
    let mut collector = VecCollector::new();
    decode_with_processor(&mut validator, &mut collector, ParseType::ReadTillEnd, reader)?;
    Ok(collector.into_instructions())
}

/// Memory argument for memory access instructions
#[derive(Debug, Clone, PartialEq)]
pub struct MemArg {
    /// Memory alignment (as power of 2)
    pub align: u32,
    /// Memory offset
    pub offset: u32,
}

/// Block type for structured control instructions
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BlockType {
    /// Empty block type (no parameters or results)
    Empty,
    /// Single value type result
    Value(ValueType),
    /// Function type index for multi-value blocks
    FuncType(u32),
}

/// Position information for an instruction in the binary
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ByteRange {
    pub offset: usize,
    pub length: usize,
}

/// Main instruction representation
#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub position: ByteRange,
    /// Original bytes for instructions with non-canonical encodings (e.g., 0xFC prefix)
    /// This preserves the exact byte representation for byte-perfect output
    pub original_bytes: Vec<u8>,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

/// Algebraic data type representing all WebAssembly instructions
#[derive(Debug, Clone, PartialEq)]
pub enum InstructionKind {
    // Control instructions
    Unreachable,
    Nop,
    Block { block_type: BlockType },
    Loop { block_type: BlockType },
    If { block_type: BlockType },
    Else,
    End,
    Br { label_idx: u32 },
    BrIf { label_idx: u32 },
    BrTable { labels: Vec<u32>, default: u32 },
    Return,
    Call { func_idx: u32 },
    CallIndirect { type_idx: u32, table_idx: u32 },

    // Reference instructions
    RefNull { ref_type: ValueType },
    RefIsNull,
    RefFunc { func_idx: u32 },

    // Parametric instructions
    Drop,
    Select,
    SelectTyped { val_types: Vec<ValueType> },

    // Variable instructions
    LocalGet { local_idx: u32 },
    LocalSet { local_idx: u32 },
    LocalTee { local_idx: u32 },
    GlobalGet { global_idx: u32 },
    GlobalSet { global_idx: u32 },

    // Table instructions
    TableGet { table_idx: u32 },
    TableSet { table_idx: u32 },
    TableInit { elem_idx: u32, table_idx: u32 },
    ElemDrop { elem_idx: u32 },
    TableCopy { dst_table: u32, src_table: u32 },
    TableGrow { table_idx: u32 },
    TableSize { table_idx: u32 },
    TableFill { table_idx: u32 },

    // Memory instructions
    I32Load { memarg: MemArg },
    I64Load { memarg: MemArg },
    F32Load { memarg: MemArg },
    F64Load { memarg: MemArg },
    I32Load8S { memarg: MemArg },
    I32Load8U { memarg: MemArg },
    I32Load16S { memarg: MemArg },
    I32Load16U { memarg: MemArg },
    I64Load8S { memarg: MemArg },
    I64Load8U { memarg: MemArg },
    I64Load16S { memarg: MemArg },
    I64Load16U { memarg: MemArg },
    I64Load32S { memarg: MemArg },
    I64Load32U { memarg: MemArg },
    I32Store { memarg: MemArg },
    I64Store { memarg: MemArg },
    F32Store { memarg: MemArg },
    F64Store { memarg: MemArg },
    I32Store8 { memarg: MemArg },
    I32Store16 { memarg: MemArg },
    I64Store8 { memarg: MemArg },
    I64Store16 { memarg: MemArg },
    I64Store32 { memarg: MemArg },
    MemorySize,
    MemoryGrow,
    MemoryInit { data_idx: u32 },
    DataDrop { data_idx: u32 },
    MemoryCopy,
    MemoryFill,

    // Numeric instructions - Constants
    I32Const { value: i32 },
    I64Const { value: i64 },
    F32Const { value: f32 },
    F64Const { value: f64 },

    // Numeric instructions - i32 operations
    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,
    I32Clz,
    I32Ctz,
    I32Popcnt,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32Rotl,
    I32Rotr,

    // Numeric instructions - i64 operations
    I64Eqz,
    I64Eq,
    I64Ne,
    I64LtS,
    I64LtU,
    I64GtS,
    I64GtU,
    I64LeS,
    I64LeU,
    I64GeS,
    I64GeU,
    I64Clz,
    I64Ctz,
    I64Popcnt,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64DivU,
    I64RemS,
    I64RemU,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    I64ShrU,
    I64Rotl,
    I64Rotr,

    // Numeric instructions - f32 operations
    F32Eq,
    F32Ne,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,
    F32Abs,
    F32Neg,
    F32Ceil,
    F32Floor,
    F32Trunc,
    F32Nearest,
    F32Sqrt,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Min,
    F32Max,
    F32Copysign,

    // Numeric instructions - f64 operations
    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,
    F64Abs,
    F64Neg,
    F64Ceil,
    F64Floor,
    F64Trunc,
    F64Nearest,
    F64Sqrt,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Min,
    F64Max,
    F64Copysign,

    // Numeric instructions - Conversions
    I32WrapI64,
    I32TruncF32S,
    I32TruncF32U,
    I32TruncF64S,
    I32TruncF64U,
    I64ExtendI32S,
    I64ExtendI32U,
    I64TruncF32S,
    I64TruncF32U,
    I64TruncF64S,
    I64TruncF64U,
    F32ConvertI32S,
    F32ConvertI32U,
    F32ConvertI64S,
    F32ConvertI64U,
    F32DemoteF64,
    F64ConvertI32S,
    F64ConvertI32U,
    F64ConvertI64S,
    F64ConvertI64U,
    F64PromoteF32,
    I32ReinterpretF32,
    I64ReinterpretF64,
    F32ReinterpretI32,
    F64ReinterpretI64,

    // Sign extension operations
    I32Extend8S,
    I32Extend16S,
    I64Extend8S,
    I64Extend16S,
    I64Extend32S,

    // Saturating truncation operations (0xFC prefix)
    I32TruncSatF32S,
    I32TruncSatF32U,
    I32TruncSatF64S,
    I32TruncSatF64U,
    I64TruncSatF32S,
    I64TruncSatF32U,
    I64TruncSatF64S,
    I64TruncSatF64U,

    // SIMD instructions placeholder (0xFD prefix)
    V128Load { memarg: MemArg },
    V128Store { memarg: MemArg },
    V128Const { value: [u8; 16] },
    // Additional SIMD instructions would go here
}

impl InstructionKind {
    /// Format the instruction with module context
    pub fn format_with_context(&self, module: &crate::parser::module::Module) -> String {
        use InstructionKind::*;

        let base = self.to_string();

        // Add function/table names for call instructions
        match self {
            Call { func_idx } => {
                if let Some(name) = module.get_function_name(*func_idx) {
                    format!("{base} <{name}>")
                } else {
                    base
                }
            }
            CallIndirect { type_idx, table_idx } => {
                let table_part = if let Some(name) = module.get_table_name(*table_idx) {
                    format!(" <{name}>")
                } else {
                    String::new()
                };
                format!("call_indirect {table_idx}{table_part} (type {type_idx})")
            }
            GlobalGet { global_idx } => {
                if let Some(name) = module.get_global_name(*global_idx) {
                    format!("{base} <{name}>")
                } else {
                    base
                }
            }
            GlobalSet { global_idx } => {
                if let Some(name) = module.get_global_name(*global_idx) {
                    format!("{base} <{name}>")
                } else {
                    base
                }
            }
            _ => base,
        }
    }

    /// Get the mnemonic for this instruction
    pub fn mnemonic(&self) -> &'static str {
        use InstructionKind::*;
        match self {
            // Control
            Unreachable => "unreachable",
            Nop => "nop",
            Block { .. } => "block",
            Loop { .. } => "loop",
            If { .. } => "if",
            Else => "else",
            End => "end",
            Br { .. } => "br",
            BrIf { .. } => "br_if",
            BrTable { .. } => "br_table",
            Return => "return",
            Call { .. } => "call",
            CallIndirect { .. } => "call_indirect",

            // Reference
            RefNull { .. } => "ref.null",
            RefIsNull => "ref.is_null",
            RefFunc { .. } => "ref.func",

            // Parametric
            Drop => "drop",
            Select => "select",
            SelectTyped { .. } => "select",

            // Variable
            LocalGet { .. } => "local.get",
            LocalSet { .. } => "local.set",
            LocalTee { .. } => "local.tee",
            GlobalGet { .. } => "global.get",
            GlobalSet { .. } => "global.set",

            // Table
            TableGet { .. } => "table.get",
            TableSet { .. } => "table.set",
            TableInit { .. } => "table.init",
            ElemDrop { .. } => "elem.drop",
            TableCopy { .. } => "table.copy",
            TableGrow { .. } => "table.grow",
            TableSize { .. } => "table.size",
            TableFill { .. } => "table.fill",

            // Memory
            I32Load { .. } => "i32.load",
            I64Load { .. } => "i64.load",
            F32Load { .. } => "f32.load",
            F64Load { .. } => "f64.load",
            I32Load8S { .. } => "i32.load8_s",
            I32Load8U { .. } => "i32.load8_u",
            I32Load16S { .. } => "i32.load16_s",
            I32Load16U { .. } => "i32.load16_u",
            I64Load8S { .. } => "i64.load8_s",
            I64Load8U { .. } => "i64.load8_u",
            I64Load16S { .. } => "i64.load16_s",
            I64Load16U { .. } => "i64.load16_u",
            I64Load32S { .. } => "i64.load32_s",
            I64Load32U { .. } => "i64.load32_u",
            I32Store { .. } => "i32.store",
            I64Store { .. } => "i64.store",
            F32Store { .. } => "f32.store",
            F64Store { .. } => "f64.store",
            I32Store8 { .. } => "i32.store8",
            I32Store16 { .. } => "i32.store16",
            I64Store8 { .. } => "i64.store8",
            I64Store16 { .. } => "i64.store16",
            I64Store32 { .. } => "i64.store32",
            MemorySize => "memory.size",
            MemoryGrow => "memory.grow",
            MemoryInit { .. } => "memory.init",
            DataDrop { .. } => "data.drop",
            MemoryCopy => "memory.copy",
            MemoryFill => "memory.fill",

            // Numeric
            I32Const { .. } => "i32.const",
            I64Const { .. } => "i64.const",
            F32Const { .. } => "f32.const",
            F64Const { .. } => "f64.const",
            I32Eqz => "i32.eqz",
            I32Eq => "i32.eq",
            I32Ne => "i32.ne",
            I32LtS => "i32.lt_s",
            I32LtU => "i32.lt_u",
            I32GtS => "i32.gt_s",
            I32GtU => "i32.gt_u",
            I32LeS => "i32.le_s",
            I32LeU => "i32.le_u",
            I32GeS => "i32.ge_s",
            I32GeU => "i32.ge_u",
            I64Eqz => "i64.eqz",
            I64Eq => "i64.eq",
            I64Ne => "i64.ne",
            I64LtS => "i64.lt_s",
            I64LtU => "i64.lt_u",
            I64GtS => "i64.gt_s",
            I64GtU => "i64.gt_u",
            I64LeS => "i64.le_s",
            I64LeU => "i64.le_u",
            I64GeS => "i64.ge_s",
            I64GeU => "i64.ge_u",
            F32Eq => "f32.eq",
            F32Ne => "f32.ne",
            F32Lt => "f32.lt",
            F32Gt => "f32.gt",
            F32Le => "f32.le",
            F32Ge => "f32.ge",
            F64Eq => "f64.eq",
            F64Ne => "f64.ne",
            F64Lt => "f64.lt",
            F64Gt => "f64.gt",
            F64Le => "f64.le",
            F64Ge => "f64.ge",
            I32Clz => "i32.clz",
            I32Ctz => "i32.ctz",
            I32Popcnt => "i32.popcnt",
            I32Add => "i32.add",
            I32Sub => "i32.sub",
            I32Mul => "i32.mul",
            I32DivS => "i32.div_s",
            I32DivU => "i32.div_u",
            I32RemS => "i32.rem_s",
            I32RemU => "i32.rem_u",
            I32And => "i32.and",
            I32Or => "i32.or",
            I32Xor => "i32.xor",
            I32Shl => "i32.shl",
            I32ShrS => "i32.shr_s",
            I32ShrU => "i32.shr_u",
            I32Rotl => "i32.rotl",
            I32Rotr => "i32.rotr",
            I64Clz => "i64.clz",
            I64Ctz => "i64.ctz",
            I64Popcnt => "i64.popcnt",
            I64Add => "i64.add",
            I64Sub => "i64.sub",
            I64Mul => "i64.mul",
            I64DivS => "i64.div_s",
            I64DivU => "i64.div_u",
            I64RemS => "i64.rem_s",
            I64RemU => "i64.rem_u",
            I64And => "i64.and",
            I64Or => "i64.or",
            I64Xor => "i64.xor",
            I64Shl => "i64.shl",
            I64ShrS => "i64.shr_s",
            I64ShrU => "i64.shr_u",
            I64Rotl => "i64.rotl",
            I64Rotr => "i64.rotr",
            F32Abs => "f32.abs",
            F32Neg => "f32.neg",
            F32Ceil => "f32.ceil",
            F32Floor => "f32.floor",
            F32Trunc => "f32.trunc",
            F32Nearest => "f32.nearest",
            F32Sqrt => "f32.sqrt",
            F32Add => "f32.add",
            F32Sub => "f32.sub",
            F32Mul => "f32.mul",
            F32Div => "f32.div",
            F32Min => "f32.min",
            F32Max => "f32.max",
            F32Copysign => "f32.copysign",
            F64Abs => "f64.abs",
            F64Neg => "f64.neg",
            F64Ceil => "f64.ceil",
            F64Floor => "f64.floor",
            F64Trunc => "f64.trunc",
            F64Nearest => "f64.nearest",
            F64Sqrt => "f64.sqrt",
            F64Add => "f64.add",
            F64Sub => "f64.sub",
            F64Mul => "f64.mul",
            F64Div => "f64.div",
            F64Min => "f64.min",
            F64Max => "f64.max",
            F64Copysign => "f64.copysign",
            I32WrapI64 => "i32.wrap_i64",
            I32TruncF32S => "i32.trunc_f32_s",
            I32TruncF32U => "i32.trunc_f32_u",
            I32TruncF64S => "i32.trunc_f64_s",
            I32TruncF64U => "i32.trunc_f64_u",
            I64ExtendI32S => "i64.extend_i32_s",
            I64ExtendI32U => "i64.extend_i32_u",
            I64TruncF32S => "i64.trunc_f32_s",
            I64TruncF32U => "i64.trunc_f32_u",
            I64TruncF64S => "i64.trunc_f64_s",
            I64TruncF64U => "i64.trunc_f64_u",
            F32ConvertI32S => "f32.convert_i32_s",
            F32ConvertI32U => "f32.convert_i32_u",
            F32ConvertI64S => "f32.convert_i64_s",
            F32ConvertI64U => "f32.convert_i64_u",
            F32DemoteF64 => "f32.demote_f64",
            F64ConvertI32S => "f64.convert_i32_s",
            F64ConvertI32U => "f64.convert_i32_u",
            F64ConvertI64S => "f64.convert_i64_s",
            F64ConvertI64U => "f64.convert_i64_u",
            F64PromoteF32 => "f64.promote_f32",
            I32ReinterpretF32 => "i32.reinterpret_f32",
            I64ReinterpretF64 => "i64.reinterpret_f64",
            F32ReinterpretI32 => "f32.reinterpret_i32",
            F64ReinterpretI64 => "f64.reinterpret_i64",
            I32Extend8S => "i32.extend8_s",
            I32Extend16S => "i32.extend16_s",
            I64Extend8S => "i64.extend8_s",
            I64Extend16S => "i64.extend16_s",
            I64Extend32S => "i64.extend32_s",
            I32TruncSatF32S => "i32.trunc_sat_f32_s",
            I32TruncSatF32U => "i32.trunc_sat_f32_u",
            I32TruncSatF64S => "i32.trunc_sat_f64_s",
            I32TruncSatF64U => "i32.trunc_sat_f64_u",
            I64TruncSatF32S => "i64.trunc_sat_f32_s",
            I64TruncSatF32U => "i64.trunc_sat_f32_u",
            I64TruncSatF64S => "i64.trunc_sat_f64_s",
            I64TruncSatF64U => "i64.trunc_sat_f64_u",
            V128Load { .. } => "v128.load",
            V128Store { .. } => "v128.store",
            V128Const { .. } => "v128.const",
        }
    }
}

impl fmt::Display for InstructionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use InstructionKind::*;

        write!(f, "{}", self.mnemonic())?;

        match self {
            // Instructions with operands
            Block { block_type } | Loop { block_type } | If { block_type } => {
                write!(f, "{block_type}")
            }
            Br { label_idx } | BrIf { label_idx } => write!(f, " {label_idx}"),
            BrTable { labels, default } => {
                for label in labels {
                    write!(f, " {label}")?;
                }
                write!(f, " {default}")
            }
            Call { func_idx } | RefFunc { func_idx } => write!(f, " {func_idx}"),
            CallIndirect { type_idx, table_idx } => write!(f, " {table_idx} (type {type_idx})"),
            RefNull { ref_type } => {
                // Use shortened names for ref types to match WABT output
                let type_name = match ref_type {
                    ValueType::FuncRef => "func",
                    ValueType::ExternRef => "extern",
                    _ => return write!(f, " {ref_type}"),
                };
                write!(f, " {type_name}")
            }
            SelectTyped { val_types } => {
                // Display typed select with just the first type
                if let Some(vt) = val_types.first() {
                    write!(f, " {vt}")
                } else {
                    Ok(())
                }
            }
            LocalGet { local_idx } | LocalSet { local_idx } | LocalTee { local_idx } => {
                write!(f, " {local_idx}")
            }
            GlobalGet { global_idx } | GlobalSet { global_idx } => write!(f, " {global_idx}"),
            TableGet { table_idx }
            | TableSet { table_idx }
            | TableGrow { table_idx }
            | TableSize { table_idx }
            | TableFill { table_idx } => write!(f, " {table_idx}"),
            TableInit { elem_idx, table_idx } => write!(f, " {elem_idx} {table_idx}"),
            ElemDrop { elem_idx } => write!(f, " {elem_idx}"),
            TableCopy { dst_table, src_table } => write!(f, " {dst_table} {src_table}"),
            MemoryInit { data_idx } => write!(f, " {data_idx} 0"),
            DataDrop { data_idx } => write!(f, " {data_idx}"),
            MemoryCopy => write!(f, " 0 0"),
            MemoryFill => write!(f, " 0"),
            MemorySize | MemoryGrow => write!(f, " 0"),
            I32Const { value } => {
                // Always display as unsigned to match test expectations
                write!(f, " {}", *value as u32)
            }
            I64Const { value } => {
                // Always display as unsigned to match test expectations
                write!(f, " {}", *value as u64)
            }
            F32Const { value } => write!(f, " {}", value.to_hex()),
            F64Const { value } => write!(f, " {}", value.to_hex()),
            V128Const { value } => {
                for byte in value {
                    write!(f, " {byte:#02x}")?;
                }
                Ok(())
            }
            // Memory instructions with memarg
            I32Load { memarg }
            | I64Load { memarg }
            | F32Load { memarg }
            | F64Load { memarg }
            | I32Load8S { memarg }
            | I32Load8U { memarg }
            | I32Load16S { memarg }
            | I32Load16U { memarg }
            | I64Load8S { memarg }
            | I64Load8U { memarg }
            | I64Load16S { memarg }
            | I64Load16U { memarg }
            | I64Load32S { memarg }
            | I64Load32U { memarg }
            | I32Store { memarg }
            | I64Store { memarg }
            | F32Store { memarg }
            | F64Store { memarg }
            | I32Store8 { memarg }
            | I32Store16 { memarg }
            | I64Store8 { memarg }
            | I64Store16 { memarg }
            | I64Store32 { memarg }
            | V128Load { memarg }
            | V128Store { memarg } => {
                // Always display align and offset values (align as log2)
                write!(f, " {} {}", memarg.align, memarg.offset)
            }
            // Instructions with no operands
            _ => Ok(()),
        }
    }
}

impl fmt::Display for BlockType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BlockType::Empty => Ok(()),
            BlockType::Value(vt) => write!(f, " {vt}"),
            BlockType::FuncType(idx) => write!(f, " type[{idx}]"),
        }
    }
}
