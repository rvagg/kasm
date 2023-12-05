use fhex::ToHex;
use once_cell::sync::OnceCell;
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::sync::Arc;

use crate::parser::reader;

#[derive(PartialEq, Clone, Copy, Debug, Hash, Eq)]
pub enum InstructionType {
    // Control instructions¶ ---------------------------------------------------
    Unreachable,
    Nop,
    Block,
    Loop,
    If,
    Else,
    Br,
    BrIf,
    BrTable,
    Return,
    Call,
    CallIndirect,

    // Reference instructions¶ -------------------------------------------------
    RefNull,
    RefIsNull,
    RefFunc,

    // Parametric instructions¶ ------------------------------------------------
    Drop,
    Select,
    SelectT,

    // Variable instructions¶ --------------------------------------------------
    LocalGet,
    LocalSet,
    LocalTee,
    GlobalGet,
    GlobalSet,

    // Table instructions¶ -----------------------------------------------------
    TableGet,
    TableSet,
    TableInit,
    ElemDrop,
    TableCopy,
    TableGrow,
    TableSize,
    TableFill,

    // Memory instructions¶ ----------------------------------------------------
    I32Load,
    I64Load,
    F32Load,
    F64Load,
    I32Load8S,
    I32Load8U,
    I32Load16S,
    I32Load16U,
    I64Load8S,
    I64Load8U,
    I64Load16S,
    I64Load16U,
    I64Load32S,
    I64Load32U,
    I32Store,
    I64Store,
    F32Store,
    F64Store,
    I32Store8,
    I32Store16,
    I64Store8,
    I64Store16,
    I64Store32,
    MemorySize,
    MemoryGrow,
    MemoryInit,
    DataDrop,
    MemoryCopy,
    MemoryFill,

    // Numeric instructions¶ ----------------------------------------------------
    I32Const,
    I64Const,
    F64Const,
    F32Const,

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

    F32Eq,
    F32Ne,
    F32Lt,
    F32Le,
    F32Gt,
    F32Ge,

    F64Eq,
    F64Ne,
    F64Lt,
    F64Le,
    F64Gt,
    F64Ge,

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
    I32ShrU,
    I32ShrS,
    I32Rotr,
    I32Rotl,

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
    I64ShrU,
    I64ShrS,
    I64Rotr,
    I64Rotl,

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

    I32Extend8S,
    I32Extend16S,
    I64Extend8S,
    I64Extend16S,
    I64Extend32S,

    I32TruncSatF32S,
    I32TruncSatF32U,
    I32TruncSatF64S,
    I32TruncSatF64U,
    I64TruncSatF32S,
    I64TruncSatF32U,
    I64TruncSatF64S,
    I64TruncSatF64U,

    // Vector instructions¶ ----------------------------------------------------
    V128Load,
    V128Load8x8S,
    V128Load8x8U,
    V128Load16x4S,
    V128Load16x4U,
    V128Load32x2S,
    V128Load32x2U,
    V128Load8Splat,
    V128Load16Splat,
    V128Load32Splat,
    V128Load64Splat,
    V128Load32Zero,
    V128Load64Zero,
    V128Store,
    V128Load8Lane,
    V128Load16Lane,
    V128Load32Lane,
    V128Load64Lane,
    V128Store8Lane,
    V128Store16Lane,
    V128Store32Lane,
    V128Store64Lane,

    V128Const,

    I8x16Shuffle,

    I8x16ExtractLaneS,
    I8x16ExtractLaneU,
    I8x16ReplaceLane,
    I16x8ExtractLaneS,
    I16x8ExtractLaneU,
    I16x8ReplaceLane,
    I32x4ExtractLane,
    I32x4ReplaceLane,
    I64x2ExtractLane,
    I64x2ReplaceLane,
    F32x4ExtractLane,
    F32x4ReplaceLane,
    F64x2ExtractLane,
    F64x2ReplaceLane,

    I8x16Swizzle,
    I8x16Splat,
    I16x8Splat,
    I32x4Splat,
    I64x2Splat,
    F32x4Splat,
    F64x2Splat,

    I8x16Eq,
    I8x16Ne,
    I8x16LtS,
    I8x16LtU,
    I8x16GtS,
    I8x16GtU,
    I8x16LeS,
    I8x16LeU,
    I8x16GeS,
    I8x16GeU,

    I16x8Eq,
    I16x8Ne,
    I16x8LtS,
    I16x8LtU,
    I16x8GtS,
    I16x8GtU,
    I16x8LeS,
    I16x8LeU,
    I16x8GeS,
    I16x8GeU,

    I32x4Eq,
    I32x4Ne,
    I32x4LtS,
    I32x4LtU,
    I32x4GtS,
    I32x4GtU,
    I32x4LeS,
    I32x4LeU,
    I32x4GeS,
    I32x4GeU,

    I64x2Eq,
    I64x2Ne,
    I64x2LtS,
    I64x2GtS,
    I64x2LeS,
    I64x2GeS,

    F32x4Eq,
    F32x4Ne,
    F32x4Lt,
    F32x4Gt,
    F32x4Le,
    F32x4Ge,

    F64x2Eq,
    F64x2Ne,
    F64x2Lt,
    F64x2Gt,
    F64x2Le,
    F64x2Ge,

    V128Not,
    V128And,
    V128AndNot,
    V128Or,
    V128Xor,
    V128Bitselect,
    V128AnyTrue,

    I8x16Abs,
    I8x16Neg,
    I8x16Popcnt,
    I8x16AllTrue,
    I8x16Bitmask,
    I8x16NarrowI16x8S,
    I8x16NarrowI16x8U,
    I8x16Shl,
    I8x16ShrS,
    I8x16ShrU,
    I8x16Add,
    I8x16AddSaturateS,
    I8x16AddSaturateU,
    I8x16Sub,
    I8x16SubSaturateS,
    I8x16SubSaturateU,
    I8x16MinS,
    I8x16MinU,
    I8x16MaxS,
    I8x16MaxU,
    I8x16AvgrU,

    I16x8ExtAddPairwiseI8x16S,
    I16x8ExtAddPairwiseI8x16U,
    I16x8Abs,
    I16x8Neg,
    I16x8Q15MulrSatS,
    I16x8AllTrue,
    I16x8Bitmask,
    I16x8NarrowI32x4S,
    I16x8NarrowI32x4U,
    I16x8ExtendLowI8x16S,
    I16x8ExtendHighI8x16S,
    I16x8ExtendLowI8x16U,
    I16x8ExtendHighI8x16U,
    I16x8Shl,
    I16x8ShrS,
    I16x8ShrU,
    I16x8Add,
    I16x8AddSaturateS,
    I16x8AddSaturateU,
    I16x8Sub,
    I16x8SubSaturateS,
    I16x8SubSaturateU,
    I16x8Mul,
    I16x8MinS,
    I16x8MinU,
    I16x8MaxS,
    I16x8MaxU,
    I16x8AvgrU,
    I16x8ExtMulLowI8x16S,
    I16x8ExtMulHighI8x16S,
    I16x8ExtMulLowI8x16U,
    I16x8ExtMulHighI8x16U,

    I32x4ExtAddPairwiseI16x8S,
    I32x4ExtAddPairwiseI16x8U,
    I32x4Abs,
    I32x4Neg,
    I32x4AllTrue,
    I32x4Bitmask,
    I32x4ExtendLowI16x8S,
    I32x4ExtendHighI16x8S,
    I32x4ExtendLowI16x8U,
    I32x4ExtendHighI16x8U,
    I32x4Shl,
    I32x4ShrS,
    I32x4ShrU,
    I32x4Add,
    I32x4Sub,
    I32x4Mul,
    I32x4MinS,
    I32x4MinU,
    I32x4MaxS,
    I32x4MaxU,
    I32x4DotI16x8S,
    I32x4ExtMulLowI16x8S,
    I32x4ExtMulHighI16x8S,
    I32x4ExtMulLowI16x8U,
    I32x4ExtMulHighI16x8U,

    I64x2Abs,
    I64x2Neg,
    I64x2AllTrue,
    I64x2Bitmask,
    I64x2ExtendLowI32x4S,
    I64x2ExtendHighI32x4S,
    I64x2ExtendLowI32x4U,
    I64x2ExtendHighI32x4U,
    I64x2Shl,
    I64x2ShrS,
    I64x2ShrU,
    I64x2Add,
    I64x2Sub,
    I64x2Mul,
    I64x2ExtMulLowI32x4S,
    I64x2ExtMulHighI32x4S,
    I64x2ExtMulLowI32x4U,
    I64x2ExtMulHighI32x4U,

    F32x4Ceil,
    F32x4Floor,
    F32x4Trunc,
    F32x4Nearest,
    F32x4Abs,
    F32x4Neg,
    F32x4Sqrt,
    F32x4Add,
    F32x4Sub,
    F32x4Mul,
    F32x4Div,
    F32x4Min,
    F32x4Max,
    F32x4PMin,
    F32x4PMax,

    F64x2Ceil,
    F64x2Floor,
    F64x2Trunc,
    F64x2Nearest,
    F64x2Abs,
    F64x2Neg,
    F64x2Sqrt,
    F64x2Add,
    F64x2Sub,
    F64x2Mul,
    F64x2Div,
    F64x2Min,
    F64x2Max,
    F64x2PMin,
    F64x2PMax,

    I32x4TruncSatF32x4S,
    I32x4TruncSatF32x4U,
    F32x4ConvertI32x4S,
    F32x4ConvertI32x4U,
    I32x4TruncSatF64x2SZero,
    I32x4TruncSatF64x2UZero,
    F64x2ConvertLowI32x4S,
    F64x2ConvertLowI32x4U,
    F32x4DemoteF64x2Zero,
    F64x2PromoteLowF32x4,

    End,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Instruction {
    typ: InstructionType,
    data: InstructionData,
}

impl Instruction {
    pub fn new(typ: InstructionType, data: InstructionData) -> Self {
        Self { typ, data }
    }

    pub fn get_type(&self) -> &InstructionType {
        &self.typ
    }

    pub fn get_data(&self) -> &InstructionData {
        &self.data
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let coding = get_codings_by_type().get(&self.typ).unwrap();
        write!(f, "{}", coding.name)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum InstructionData {
    SimpleInstruction,

    // Control instructions¶ ---------------------------------------------------
    BlockInstruction {
        blocktype: BlockType,
    },
    LabelledInstruction {
        label_index: u32,
    },
    TableLabelledInstruction {
        labels: Vec<u32>,
        label_index: u32,
    },
    FunctionInstruction {
        function_index: u32,
    },
    IndirectInstruction {
        type_index: u32,
        table_index: u32,
    },

    // Reference instructions¶ -------------------------------------------------
    RefTypeInstruction {
        ref_type: u8,
    },

    // Parametric instructions¶ ------------------------------------------------
    ValueTypeInstruction {
        value_types: Vec<u8>,
    },

    // Variable instructions¶ --------------------------------------------------
    LocalInstruction {
        local_index: u32,
    },
    GlobalInstruction {
        global_index: u32,
    },

    // Table instructions¶ -----------------------------------------------------
    TableInstruction {
        table_index: u32,
    },
    TableInitInstruction {
        elem_index: u32,
        table_index: u32,
    },
    ElemInstruction {
        elem_index: u32,
    },
    TableCopyInstruction {
        src_table_index: u32,
        dst_table_index: u32,
    },

    // Memory instructions¶ ----------------------------------------------------
    MemoryInstruction {
        memarg: MemArg,
    },
    DataInstruction {
        data_index: u32,
    },

    // Numeric instructions¶ ----------------------------------------------------
    I32Instruction {
        value: u32,
    },
    I64Instruction {
        value: u64,
    },
    F64Instruction {
        value: f64,
    },
    F32Instruction {
        value: f32,
    },

    // Vector instructions¶ ----------------------------------------------------
    V128MemoryLaneInstruction {
        memarg: MemArg,
        lane_index: u8,
    },
    V128Instruction {
        value: [u8; 16],
    },
    V128LanesInstruction {
        lane_indices: Vec<u8>,
    },
    V128LaneInstruction {
        lane_index: u8,
    },
}

type MemArg = (u32, u32); // (align, offset)

#[derive(Clone)]
pub struct InstructionCoding {
    pub typ: InstructionType,
    pub opcode: u8,
    pub subopcode: u32,
    pub name: &'static str,
    pub parse_bytes:
        Arc<dyn Fn(&mut super::reader::Reader) -> Result<InstructionData, io::Error> + Send + Sync>,
    pub emit_bytes: Arc<dyn Fn(&InstructionData) -> Vec<u8> + Send + Sync>,
    pub emit_str: Arc<dyn Fn(&InstructionData) -> String + Send + Sync>,
}

impl InstructionCoding {
    pub fn new_simple(
        typ: InstructionType,
        opcode: u8,
        subopcode: u32,
        name: &'static str,
    ) -> Self {
        Self {
            typ,
            opcode,
            subopcode,
            name,
            parse_bytes: Arc::new(move |_| Ok(InstructionData::SimpleInstruction.clone())),
            // default should emit the opcode byte; and if opcode is 0xfc or 0xfd, emit the subopcode byte too
            emit_bytes: Arc::new(move |_| {
                let mut bytes = vec![opcode];
                if opcode == 0xfc || opcode == 0xfd {
                    bytes.push(subopcode as u8);
                }
                bytes
            }),
            emit_str: Arc::new(move |_| name.to_string()),
        }
    }

    pub fn new_with_parse(
        typ: InstructionType,
        opcode: u8,
        subopcode: u32,
        name: &'static str,
        parse_bytes: Arc<
            dyn Fn(&mut super::reader::Reader) -> Result<InstructionData, io::Error> + Send + Sync,
        >,
    ) -> Self {
        Self {
            typ,
            opcode,
            subopcode,
            name,
            parse_bytes,
            emit_bytes: Arc::new(move |_: &InstructionData| {
                let mut bytes = vec![opcode];
                if opcode == 0xfc || opcode == 0xfd {
                    bytes.push(subopcode as u8);
                }
                bytes
            }),
            emit_str: Arc::new(move |_| name.to_string()),
        }
    }

    pub fn new_with_options(
        typ: InstructionType,
        opcode: u8,
        subopcode: u32,
        name: &'static str,
        parse_bytes: Arc<
            dyn Fn(&mut super::reader::Reader) -> Result<InstructionData, io::Error> + Send + Sync,
        >,
        emit_bytes: Arc<dyn Fn(&InstructionData) -> Vec<u8> + Send + Sync>,
        emit_str: Arc<dyn Fn(&InstructionData) -> String + Send + Sync>,
    ) -> Self {
        Self {
            typ,
            opcode,
            subopcode,
            name,
            parse_bytes,
            emit_bytes,
            emit_str,
        }
    }
}

pub fn get_codings() -> &'static Vec<InstructionCoding> {
    static CODINGS: OnceCell<Vec<InstructionCoding>> = OnceCell::new();
    CODINGS.get_or_init(|| {
        vec![
            // Control instructions¶ -------------------------------------------
            InstructionCoding::new_simple(InstructionType::Unreachable, 0x00, 0, "unreachable"),
            InstructionCoding::new_simple(InstructionType::Nop, 0x01, 0, "nop"),
            InstructionCoding::new_with_options(
                InstructionType::Block,
                0x02,
                0,
                "block",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::BlockInstruction {
                            blocktype: BlockType::parse_bytes(bytes)?,
                        })
                    },
                ),
                Arc::new(|data| {
                    let mut bytes = vec![0x02];
                    if let InstructionData::BlockInstruction { blocktype } = &data {
                        let mut block_bytes = blocktype.emit_bytes();
                        bytes.append(&mut block_bytes);
                    } else {
                        panic!("expected block instruction");
                    }
                    bytes
                }),
                Arc::new(|data| {
                    if let InstructionData::BlockInstruction { blocktype } = &data {
                        if blocktype == &BlockType::Empty {
                            "block".to_string()
                        } else {
                            format!("block {}", blocktype)
                        }
                    } else {
                        panic!("expected block instruction");
                    }
                }),
            ),
            InstructionCoding::new_with_options(
                InstructionType::Loop,
                0x03,
                0,
                "loop",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::BlockInstruction {
                            blocktype: BlockType::parse_bytes(bytes)?,
                        })
                    },
                ),
                Arc::new(|data| {
                    let mut bytes = vec![0x03];
                    if let InstructionData::BlockInstruction { blocktype } = &data {
                        let mut block_bytes = blocktype.emit_bytes();
                        bytes.append(&mut block_bytes);
                    } else {
                        panic!("expected block instruction");
                    }
                    bytes
                }),
                Arc::new(|data| {
                    if let InstructionData::BlockInstruction { blocktype } = &data {
                        format!("loop {}", blocktype)
                    } else {
                        panic!("expected block instruction");
                    }
                }),
            ),
            InstructionCoding::new_with_options(
                InstructionType::If,
                0x04,
                0,
                "if",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::BlockInstruction {
                            blocktype: BlockType::parse_bytes(bytes)?,
                        })
                    },
                ),
                Arc::new(|data| {
                    let mut bytes = vec![0x04];
                    if let InstructionData::BlockInstruction { blocktype } = &data {
                        let mut block_bytes = blocktype.emit_bytes();
                        bytes.append(&mut block_bytes);
                    } else {
                        panic!("expected block instruction");
                    }
                    bytes
                }),
                Arc::new(|data| {
                    if let InstructionData::BlockInstruction { blocktype } = &data {
                        format!("if {}", blocktype)
                    } else {
                        panic!("expected block instruction");
                    }
                }),
            ),
            InstructionCoding::new_simple(InstructionType::Else, 0x05, 0, "else"),
            InstructionCoding::new_with_options(
                InstructionType::Br,
                0x0c,
                0,
                "br",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::LabelledInstruction {
                            label_index: bytes.read_vu32()?,
                        })
                    },
                ),
                Arc::new(|data| {
                    let mut bytes = vec![0x0c];
                    if let InstructionData::LabelledInstruction { label_index } = &data {
                        let mut label_bytes = reader::emit_vu32(*label_index);
                        bytes.append(&mut label_bytes);
                    } else {
                        panic!("expected labelled instruction");
                    }
                    bytes
                }),
                Arc::new(|data| {
                    if let InstructionData::LabelledInstruction { label_index } = &data {
                        format!("br {}", label_index)
                    } else {
                        panic!("expected labelled instruction");
                    }
                }),
            ),
            InstructionCoding::new_with_options(
                InstructionType::BrIf,
                0x0d,
                0,
                "br_if",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::LabelledInstruction {
                            label_index: bytes.read_vu32()?,
                        })
                    },
                ),
                Arc::new(|data| {
                    let mut bytes = vec![0x0d];
                    if let InstructionData::LabelledInstruction { label_index } = &data {
                        let mut label_bytes = reader::emit_vu32(*label_index);
                        bytes.append(&mut label_bytes);
                    } else {
                        panic!("expected labelled instruction");
                    }
                    bytes
                }),
                Arc::new(|data| {
                    if let InstructionData::LabelledInstruction { label_index } = &data {
                        format!("br_if {}", label_index)
                    } else {
                        panic!("expected labelled instruction");
                    }
                }),
            ),
            InstructionCoding::new_with_options(
                InstructionType::BrTable,
                0x0e,
                0,
                "br_table",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::TableLabelledInstruction {
                            labels: consume_vu32vec(bytes)?,
                            label_index: bytes.read_vu32()?,
                        })
                    },
                ),
                Arc::new(|data| {
                    let mut bytes = vec![0x0e];
                    if let InstructionData::TableLabelledInstruction {
                        labels,
                        label_index,
                    } = &data
                    {
                        let mut labels_bytes = emit_vu32vec(labels);
                        bytes.append(&mut labels_bytes);
                        let mut label_bytes = reader::emit_vu32(*label_index);
                        bytes.append(&mut label_bytes);
                    } else {
                        panic!("expected table labelled instruction");
                    }
                    bytes
                }),
                Arc::new(|data| {
                    if let InstructionData::TableLabelledInstruction {
                        labels,
                        label_index,
                    } = &data
                    {
                        let mut labels_str = String::new();
                        for label in labels {
                            labels_str.push_str(&format!(" {}", label));
                        }
                        format!("br_table{} {}", labels_str, label_index)
                    } else {
                        panic!("expected table labelled instruction");
                    }
                }),
            ),
            InstructionCoding::new_simple(InstructionType::Return, 0x0f, 0, "return"),
            InstructionCoding::new_with_parse(
                InstructionType::Call,
                0x10,
                0,
                "call",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::FunctionInstruction {
                            function_index: bytes.read_vu32()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::CallIndirect,
                0x11,
                0,
                "call_indirect",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::IndirectInstruction {
                            type_index: bytes.read_vu32()?,
                            table_index: bytes.read_vu32()?,
                        })
                    },
                ),
            ),
            // Reference instructions¶ -----------------------------------------
            InstructionCoding::new_with_parse(
                InstructionType::RefNull,
                0xd0,
                0,
                "ref.null",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::RefTypeInstruction {
                            ref_type: bytes.read_byte()?, // TODO: implement reference types
                        })
                    },
                ),
            ),
            InstructionCoding::new_simple(InstructionType::RefIsNull, 0xd1, 0, "ref.is_null"),
            InstructionCoding::new_with_parse(
                InstructionType::RefFunc,
                0xd2,
                0,
                "ref.func",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::FunctionInstruction {
                            function_index: bytes.read_vu32()?,
                        })
                    },
                ),
            ),
            // Parametric instructions¶ ----------------------------------------
            InstructionCoding::new_simple(InstructionType::Drop, 0x1a, 0, "drop"),
            InstructionCoding::new_simple(InstructionType::Select, 0x1b, 0, "select"),
            InstructionCoding::new_with_parse(
                InstructionType::SelectT,
                0x1c,
                0,
                "selectt", // TODO: name is just "select?
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::ValueTypeInstruction {
                            value_types: bytes.read_u8vec()?, // TODO: type vector?
                        })
                    },
                ),
            ),
            // Variable instructions¶ ------------------------------------------
            InstructionCoding::new_with_options(
                InstructionType::LocalGet,
                0x20,
                0,
                "local.get",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::LocalInstruction {
                            local_index: bytes.read_vu32()?,
                        })
                    },
                ),
                Arc::new(|data| {
                    let mut bytes = vec![0x20];
                    if let InstructionData::LocalInstruction { local_index } = &data {
                        let mut local_bytes = reader::emit_vu32(*local_index);
                        bytes.append(&mut local_bytes);
                    } else {
                        panic!("expected local instruction");
                    }
                    bytes
                }),
                Arc::new(|data| {
                    if let InstructionData::LocalInstruction { local_index } = &data {
                        format!("local.get {}", local_index)
                    } else {
                        panic!("expected local instruction");
                    }
                }),
            ),
            InstructionCoding::new_with_options(
                InstructionType::LocalSet,
                0x21,
                0,
                "local.set",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::LocalInstruction {
                            local_index: bytes.read_vu32()?,
                        })
                    },
                ),
                Arc::new(|data| {
                    let mut bytes = vec![0x21];
                    if let InstructionData::LocalInstruction { local_index } = &data {
                        let mut local_bytes = reader::emit_vu32(*local_index);
                        bytes.append(&mut local_bytes);
                    } else {
                        panic!("expected local instruction");
                    }
                    bytes
                }),
                Arc::new(|data| {
                    if let InstructionData::LocalInstruction { local_index } = &data {
                        format!("local.set {}", local_index)
                    } else {
                        panic!("expected local instruction");
                    }
                }),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::LocalTee,
                0x22,
                0,
                "local.tee",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::LocalInstruction {
                            local_index: bytes.read_vu32()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::GlobalGet,
                0x23,
                0,
                "global.get",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::GlobalInstruction {
                            global_index: bytes.read_vu32()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::GlobalSet,
                0x24,
                0,
                "global.set",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::GlobalInstruction {
                            global_index: bytes.read_vu32()?,
                        })
                    },
                ),
            ),
            // Table instructions¶ ---------------------------------------------
            InstructionCoding::new_with_parse(
                InstructionType::TableGet,
                0x25,
                0,
                "table.get",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::TableInstruction {
                            table_index: bytes.read_vu32()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::TableSet,
                0x26,
                0,
                "table.set",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::TableInstruction {
                            table_index: bytes.read_vu32()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::TableInit,
                0xfc,
                12,
                "table.init",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::TableInitInstruction {
                            elem_index: bytes.read_vu32()?,
                            table_index: bytes.read_vu32()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::ElemDrop,
                0xfc,
                13,
                "elem.drop",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::ElemInstruction {
                            elem_index: bytes.read_vu32()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::TableCopy,
                0xfc,
                14,
                "table.copy",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::TableCopyInstruction {
                            src_table_index: bytes.read_vu32()?,
                            dst_table_index: bytes.read_vu32()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::TableGrow,
                0xfc,
                15,
                "table.grow",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::TableInstruction {
                            table_index: bytes.read_vu32()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::TableSize,
                0xfc,
                16,
                "table.size",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::TableInstruction {
                            table_index: bytes.read_vu32()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::TableFill,
                0xfc,
                17,
                "table.fill",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::TableInstruction {
                            table_index: bytes.read_vu32()?,
                        })
                    },
                ),
            ),
            // Memory instructions¶ --------------------------------------------
            InstructionCoding::new_with_parse(
                InstructionType::I32Load,
                0x28,
                0,
                "i32.load",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I64Load,
                0x29,
                0,
                "i64.load",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::F32Load,
                0x2a,
                0,
                "f32.load",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::F64Load,
                0x2b,
                0,
                "f64.load",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I32Load8S,
                0x2c,
                0,
                "i32.load8s",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I32Load8U,
                0x2d,
                0,
                "i32.load8u",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I32Load16S,
                0x2e,
                0,
                "i32.load16s",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I32Load16U,
                0x2f,
                0,
                "i32.load16u",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I64Load8S,
                0x30,
                0,
                "i64.load8s",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I64Load8U,
                0x31,
                0,
                "i64.load8u",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I64Load16S,
                0x32,
                0,
                "i64.load16s",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I64Load16U,
                0x33,
                0,
                "i64.load16u",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I64Load32S,
                0x34,
                0,
                "i64.load32s",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I64Load32U,
                0x35,
                0,
                "i64.load32u",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I32Store,
                0x36,
                0,
                "i32.store",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I64Store,
                0x37,
                0,
                "i64.store",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::F32Store,
                0x38,
                0,
                "f32.store",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::F64Store,
                0x39,
                0,
                "f64.store",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I32Store8,
                0x3a,
                0,
                "i32.store8",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I32Store16,
                0x3b,
                0,
                "i32.store16",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I64Store8,
                0x3c,
                0,
                "i64.store8",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I64Store16,
                0x3d,
                0,
                "i64.store16",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I64Store32,
                0x3e,
                0,
                "i64.store32",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_simple(InstructionType::MemorySize, 0x3f, 0, "memory.size"),
            InstructionCoding::new_simple(InstructionType::MemoryGrow, 0x40, 0, "memory.grow"),
            InstructionCoding::new_with_parse(
                InstructionType::MemoryInit,
                0xfc,
                8,
                "memory.init",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        if bytes.read_byte()? != 0x0 {
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                "expected 0x0 for memory init",
                            ));
                        }
                        Ok(InstructionData::DataInstruction {
                            data_index: bytes.read_vu32()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::DataDrop,
                0xfc,
                9,
                "data.drop",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::DataInstruction {
                            data_index: bytes.read_vu32()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::MemoryCopy,
                0xfc,
                10,
                "memory.copy",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        if bytes.read_byte()? != 0x0 || bytes.read_byte()? != 0x0 {
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                "expected 0x0 0x0 for memory copy",
                            ));
                        }
                        Ok(InstructionData::SimpleInstruction)
                    },
                ),
            ),
            InstructionCoding::new_simple(InstructionType::MemoryFill, 0xfc, 11, "memory.fill"),
            // Numeric instructions¶ -------------------------------------------
            InstructionCoding::new_with_options(
                InstructionType::I32Const,
                0x41,
                0,
                "i32.const",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::I32Instruction {
                            value: bytes.read_vs32()? as u32,
                        })
                    },
                ),
                Arc::new(|data| {
                    let mut bytes = vec![0x41];
                    if let InstructionData::I32Instruction { value } = &data {
                        let mut u32_bytes = reader::emit_vs32(*value as i32);
                        bytes.append(&mut u32_bytes);
                    } else {
                        panic!("expected i32 instruction");
                    }
                    bytes
                }),
                Arc::new(|data| {
                    if let InstructionData::I32Instruction { value } = &data {
                        format!("i32.const {}", value)
                    } else {
                        panic!("expected i32 instruction");
                    }
                }),
            ),
            InstructionCoding::new_with_options(
                InstructionType::I64Const,
                0x42,
                0,
                "i64.const",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::I64Instruction {
                            value: bytes.read_vs64()? as u64,
                        })
                    },
                ),
                Arc::new(|data| {
                    let mut bytes = vec![0x42];
                    if let InstructionData::I64Instruction { value } = &data {
                        let mut u64_bytes = reader::emit_vs64(*value as i64);
                        bytes.append(&mut u64_bytes);
                    } else {
                        panic!("expected i64 instruction");
                    }
                    bytes
                }),
                Arc::new(|data| {
                    if let InstructionData::I64Instruction { value } = &data {
                        format!("i64.const {}", value)
                    } else {
                        panic!("expected i64 instruction");
                    }
                }),
            ),
            InstructionCoding::new_with_options(
                InstructionType::F32Const,
                0x43,
                0,
                "f32.const",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::F32Instruction {
                            value: bytes.read_f32()?,
                        })
                    },
                ),
                Arc::new(|data| {
                    let mut bytes = vec![0x43];
                    if let InstructionData::F32Instruction { value } = &data {
                        let mut f32_bytes = reader::emit_f32(*value);
                        bytes.append(&mut f32_bytes);
                    } else {
                        panic!("expected f32 instruction");
                    }
                    bytes
                }),
                Arc::new(|data| {
                    if let InstructionData::F32Instruction { value } = &data {
                        format!("f32.const {}", value.to_hex())
                    } else {
                        panic!("expected f32 instruction");
                    }
                }),
            ),
            InstructionCoding::new_with_options(
                InstructionType::F64Const,
                0x44,
                0,
                "f64.const",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::F64Instruction {
                            value: bytes.read_f64()?,
                        })
                    },
                ),
                Arc::new(|data| {
                    let mut bytes = vec![0x44];
                    if let InstructionData::F64Instruction { value } = &data {
                        let mut f64_bytes = reader::emit_f64(*value);
                        bytes.append(&mut f64_bytes);
                    } else {
                        panic!("expected f64 instruction");
                    }
                    bytes
                }),
                Arc::new(|data| {
                    if let InstructionData::F64Instruction { value } = &data {
                        format!("f64.const {}", value.to_hex())
                    } else {
                        panic!("expected f64 instruction");
                    }
                }),
            ),
            InstructionCoding::new_simple(InstructionType::I32Eqz, 0x45, 0, "i32.eqz"),
            InstructionCoding::new_simple(InstructionType::I32Eq, 0x46, 0, "i32.eq"),
            InstructionCoding::new_simple(InstructionType::I32Ne, 0x47, 0, "i32.ne"),
            InstructionCoding::new_simple(InstructionType::I32LtS, 0x48, 0, "i32.lt_s"),
            InstructionCoding::new_simple(InstructionType::I32LtU, 0x49, 0, "i32.lt_u"),
            InstructionCoding::new_simple(InstructionType::I32GtS, 0x4a, 0, "i32.gt_s"),
            InstructionCoding::new_simple(InstructionType::I32GtU, 0x4b, 0, "i32.gt_u"),
            InstructionCoding::new_simple(InstructionType::I32LeS, 0x4c, 0, "i32.le_s"),
            InstructionCoding::new_simple(InstructionType::I32LeU, 0x4d, 0, "i32.le_u"),
            InstructionCoding::new_simple(InstructionType::I32GeS, 0x4e, 0, "i32.ge_s"),
            InstructionCoding::new_simple(InstructionType::I32GeU, 0x4f, 0, "i32.ge_u"),
            InstructionCoding::new_simple(InstructionType::I64Eqz, 0x50, 0, "i64.eqz"),
            InstructionCoding::new_simple(InstructionType::I64Eq, 0x51, 0, "i64.eq"),
            InstructionCoding::new_simple(InstructionType::I64Ne, 0x52, 0, "i64.ne"),
            InstructionCoding::new_simple(InstructionType::I64LtS, 0x53, 0, "i64.lt_s"),
            InstructionCoding::new_simple(InstructionType::I64LtU, 0x54, 0, "i64.lt_u"),
            InstructionCoding::new_simple(InstructionType::I64GtS, 0x55, 0, "i64.gt_s"),
            InstructionCoding::new_simple(InstructionType::I64GtU, 0x56, 0, "i64.gt_u"),
            InstructionCoding::new_simple(InstructionType::I64LeS, 0x57, 0, "i64.le_s"),
            InstructionCoding::new_simple(InstructionType::I64LeU, 0x58, 0, "i64.le_u"),
            InstructionCoding::new_simple(InstructionType::I64GeS, 0x59, 0, "i64.ge_s"),
            InstructionCoding::new_simple(InstructionType::I64GeU, 0x5a, 0, "i64.ge_u"),
            InstructionCoding::new_simple(InstructionType::F32Eq, 0x5b, 0, "f32.eq"),
            InstructionCoding::new_simple(InstructionType::F32Ne, 0x5c, 0, "f32.ne"),
            InstructionCoding::new_simple(InstructionType::F32Lt, 0x5d, 0, "f32.lt"),
            InstructionCoding::new_simple(InstructionType::F32Le, 0x5e, 0, "f32.le"),
            InstructionCoding::new_simple(InstructionType::F32Gt, 0x5f, 0, "f32.gt"),
            InstructionCoding::new_simple(InstructionType::F32Ge, 0x60, 0, "f32.ge"),
            InstructionCoding::new_simple(InstructionType::F64Eq, 0x61, 0, "f64.eq"),
            InstructionCoding::new_simple(InstructionType::F64Ne, 0x62, 0, "f64.ne"),
            InstructionCoding::new_simple(InstructionType::F64Lt, 0x63, 0, "f64.lt"),
            InstructionCoding::new_simple(InstructionType::F64Le, 0x64, 0, "f64.le"),
            InstructionCoding::new_simple(InstructionType::F64Gt, 0x65, 0, "f64.gt"),
            InstructionCoding::new_simple(InstructionType::F64Ge, 0x66, 0, "f64.ge"),
            InstructionCoding::new_simple(InstructionType::I32Clz, 0x67, 0, "i32.clz"),
            InstructionCoding::new_simple(InstructionType::I32Ctz, 0x68, 0, "i32.ctz"),
            InstructionCoding::new_simple(InstructionType::I32Popcnt, 0x69, 0, "i32.popcnt"),
            InstructionCoding::new_simple(InstructionType::I32Add, 0x6a, 0, "i32.add"),
            InstructionCoding::new_simple(InstructionType::I32Sub, 0x6b, 0, "i32.sub"),
            InstructionCoding::new_simple(InstructionType::I32Mul, 0x6c, 0, "i32.mul"),
            InstructionCoding::new_simple(InstructionType::I32DivS, 0x6d, 0, "i32.div_s"),
            InstructionCoding::new_simple(InstructionType::I32DivU, 0x6e, 0, "i32.div_u"),
            InstructionCoding::new_simple(InstructionType::I32RemS, 0x6f, 0, "i32.rem_s"),
            InstructionCoding::new_simple(InstructionType::I32RemU, 0x70, 0, "i32.rem_u"),
            InstructionCoding::new_simple(InstructionType::I32And, 0x71, 0, "i32.and"),
            InstructionCoding::new_simple(InstructionType::I32Or, 0x72, 0, "i32.or"),
            InstructionCoding::new_simple(InstructionType::I32Xor, 0x73, 0, "i32.xor"),
            InstructionCoding::new_simple(InstructionType::I32Shl, 0x74, 0, "i32.shl"),
            InstructionCoding::new_simple(InstructionType::I32ShrS, 0x75, 0, "i32.shr_s"),
            InstructionCoding::new_simple(InstructionType::I32ShrU, 0x76, 0, "i32.shr_u"),
            InstructionCoding::new_simple(InstructionType::I32Rotl, 0x77, 0, "i32.rotl"),
            InstructionCoding::new_simple(InstructionType::I32Rotr, 0x78, 0, "i32.rotr"),
            InstructionCoding::new_simple(InstructionType::I64Clz, 0x79, 0, "i64.clz"),
            InstructionCoding::new_simple(InstructionType::I64Ctz, 0x7a, 0, "i64.ctz"),
            InstructionCoding::new_simple(InstructionType::I64Popcnt, 0x7b, 0, "i64.popcnt"),
            InstructionCoding::new_simple(InstructionType::I64Add, 0x7c, 0, "i64.add"),
            InstructionCoding::new_simple(InstructionType::I64Sub, 0x7d, 0, "i64.sub"),
            InstructionCoding::new_simple(InstructionType::I64Mul, 0x7e, 0, "i64.mul"),
            InstructionCoding::new_simple(InstructionType::I64DivS, 0x7f, 0, "i64.div_s"),
            InstructionCoding::new_simple(InstructionType::I64DivU, 0x80, 0, "i64.div_u"),
            InstructionCoding::new_simple(InstructionType::I64RemS, 0x81, 0, "i64.rem_s"),
            InstructionCoding::new_simple(InstructionType::I64RemU, 0x82, 0, "i64.rem_u"),
            InstructionCoding::new_simple(InstructionType::I64And, 0x83, 0, "i64.and"),
            InstructionCoding::new_simple(InstructionType::I64Or, 0x84, 0, "i64.or"),
            InstructionCoding::new_simple(InstructionType::I64Xor, 0x85, 0, "i64.xor"),
            InstructionCoding::new_simple(InstructionType::I64Shl, 0x86, 0, "i64.shl"),
            InstructionCoding::new_simple(InstructionType::I64ShrS, 0x87, 0, "i64.shr_s"),
            InstructionCoding::new_simple(InstructionType::I64ShrU, 0x88, 0, "i64.shr_u"),
            InstructionCoding::new_simple(InstructionType::I64Rotl, 0x89, 0, "i64.rotl"),
            InstructionCoding::new_simple(InstructionType::I64Rotr, 0x8a, 0, "i64.rotr"),
            InstructionCoding::new_simple(InstructionType::F32Abs, 0x8b, 0, "f32.abs"),
            InstructionCoding::new_simple(InstructionType::F32Neg, 0x8c, 0, "f32.neg"),
            InstructionCoding::new_simple(InstructionType::F32Ceil, 0x8d, 0, "f32.ceil"),
            InstructionCoding::new_simple(InstructionType::F32Floor, 0x8e, 0, "f32.floor"),
            InstructionCoding::new_simple(InstructionType::F32Trunc, 0x8f, 0, "f32.trunc"),
            InstructionCoding::new_simple(InstructionType::F32Nearest, 0x90, 0, "f32.nearest"),
            InstructionCoding::new_simple(InstructionType::F32Sqrt, 0x91, 0, "f32.sqrt"),
            InstructionCoding::new_simple(InstructionType::F32Add, 0x92, 0, "f32.add"),
            InstructionCoding::new_simple(InstructionType::F32Sub, 0x93, 0, "f32.sub"),
            InstructionCoding::new_simple(InstructionType::F32Mul, 0x94, 0, "f32.mul"),
            InstructionCoding::new_simple(InstructionType::F32Div, 0x95, 0, "f32.div"),
            InstructionCoding::new_simple(InstructionType::F32Min, 0x96, 0, "f32.min"),
            InstructionCoding::new_simple(InstructionType::F32Max, 0x97, 0, "f32.max"),
            InstructionCoding::new_simple(InstructionType::F32Copysign, 0x98, 0, "f32.copysign"),
            InstructionCoding::new_simple(InstructionType::F64Abs, 0x99, 0, "f64.abs"),
            InstructionCoding::new_simple(InstructionType::F64Neg, 0x9a, 0, "f64.neg"),
            InstructionCoding::new_simple(InstructionType::F64Ceil, 0x9b, 0, "f64.ceil"),
            InstructionCoding::new_simple(InstructionType::F64Floor, 0x9c, 0, "f64.floor"),
            InstructionCoding::new_simple(InstructionType::F64Trunc, 0x9d, 0, "f64.trunc"),
            InstructionCoding::new_simple(InstructionType::F64Nearest, 0x9e, 0, "f64.nearest"),
            InstructionCoding::new_simple(InstructionType::F64Sqrt, 0x9f, 0, "f64.sqrt"),
            InstructionCoding::new_simple(InstructionType::F64Add, 0xa0, 0, "f64.add"),
            InstructionCoding::new_simple(InstructionType::F64Sub, 0xa1, 0, "f64.sub"),
            InstructionCoding::new_simple(InstructionType::F64Mul, 0xa2, 0, "f64.mul"),
            InstructionCoding::new_simple(InstructionType::F64Div, 0xa3, 0, "f64.div"),
            InstructionCoding::new_simple(InstructionType::F64Min, 0xa4, 0, "f64.min"),
            InstructionCoding::new_simple(InstructionType::F64Max, 0xa5, 0, "f64.max"),
            InstructionCoding::new_simple(InstructionType::F64Copysign, 0xa6, 0, "f64.copysign"),
            InstructionCoding::new_simple(InstructionType::I32WrapI64, 0xa7, 0, "i32.wrapi64"),
            InstructionCoding::new_simple(
                InstructionType::I32TruncF32S,
                0xa8,
                0,
                "i32.trunc_f32_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32TruncF32U,
                0xa9,
                0,
                "i32.trunc_f32_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32TruncF64S,
                0xaa,
                0,
                "i32.trunc_f64_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32TruncF64U,
                0xab,
                0,
                "i32.trunc_f64_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64ExtendI32S,
                0xac,
                0,
                "i64.extendi32__s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64ExtendI32U,
                0xad,
                0,
                "i64.extendi32__u",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64TruncF32S,
                0xae,
                0,
                "i64.trunc_f32_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64TruncF32U,
                0xaf,
                0,
                "i64.trunc_f32_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64TruncF64S,
                0xb0,
                0,
                "i64.trunc_f64_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64TruncF64U,
                0xb1,
                0,
                "i64.trunc_f64_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::F32ConvertI32S,
                0xb2,
                0,
                "f32.convert_i32_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::F32ConvertI32U,
                0xb3,
                0,
                "f32.convert_i32_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::F32ConvertI64S,
                0xb4,
                0,
                "f32.convert_i64_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::F32ConvertI64U,
                0xb5,
                0,
                "f32.convert_i64_u",
            ),
            InstructionCoding::new_simple(InstructionType::F32DemoteF64, 0xb6, 0, "f32.demote_f64"),
            InstructionCoding::new_simple(
                InstructionType::F64ConvertI32S,
                0xb7,
                0,
                "f64.convert_i32_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::F64ConvertI32U,
                0xb8,
                0,
                "f64.convert_i32_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::F64ConvertI64S,
                0xb9,
                0,
                "f64.convert_i64_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::F64ConvertI64U,
                0xba,
                0,
                "f64.convert_i64_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::F64PromoteF32,
                0xbb,
                0,
                "f64.promote_f32",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32ReinterpretF32,
                0xbc,
                0,
                "i32.reinterpret_f32",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64ReinterpretF64,
                0xbd,
                0,
                "i64.reinterpret_f64",
            ),
            InstructionCoding::new_simple(
                InstructionType::F32ReinterpretI32,
                0xbe,
                0,
                "f32.reinterpret_i32",
            ),
            InstructionCoding::new_simple(
                InstructionType::F64ReinterpretI64,
                0xbf,
                0,
                "f64.reinterpret_i64",
            ),
            InstructionCoding::new_simple(InstructionType::I32Extend8S, 0xc0, 0, "i32.extend8_s"),
            InstructionCoding::new_simple(InstructionType::I32Extend16S, 0xc1, 0, "i32.extend16_s"),
            InstructionCoding::new_simple(InstructionType::I64Extend8S, 0xc2, 0, "i64.extend8_s"),
            InstructionCoding::new_simple(InstructionType::I64Extend16S, 0xc3, 0, "i64.extend16_s"),
            InstructionCoding::new_simple(InstructionType::I64Extend32S, 0xc4, 0, "i64.extend32_s"),
            InstructionCoding::new_simple(
                InstructionType::I32TruncSatF32S,
                0xfc,
                0,
                "i32.trunc_sat_f32_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32TruncSatF32U,
                0xfc,
                1,
                "i32.trunc_sat_f32_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32TruncSatF64S,
                0xfc,
                2,
                "i32.trunc_sat_f64_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32TruncSatF64U,
                0xfc,
                3,
                "i32.trunc_sat_f64_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64TruncSatF32S,
                0xfc,
                4,
                "i64.trunc_sat_f32_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64TruncSatF32U,
                0xfc,
                5,
                "i64.trunc_sat_f32_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64TruncSatF64S,
                0xfc,
                6,
                "i64.trunc_sat_f64_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64TruncSatF64U,
                0xfc,
                7,
                "i64.trunc_sat_f64_u",
            ),
            // Vector instructions¶ --------------------------------------------
            InstructionCoding::new_with_parse(
                InstructionType::V128Load,
                0xfd,
                0,
                "v128.load",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Load8x8S,
                0xfd,
                1,
                "v128.load8x8_s",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Load8x8U,
                0xfd,
                2,
                "v128.load8x8_u",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Load16x4S,
                0xfd,
                3,
                "v128.load16x4_s",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Load16x4U,
                0xfd,
                4,
                "v128.load16x4_u",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Load32x2S,
                0xfd,
                5,
                "v128.load32x2_s",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Load32x2U,
                0xfd,
                6,
                "v128.load32x2_u",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Load8Splat,
                0xfd,
                7,
                "v128.load8_splat",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Load16Splat,
                0xfd,
                8,
                "v128.load16_splat",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Load32Splat,
                0xfd,
                9,
                "v128.load32_splat",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Load64Splat,
                0xfd,
                10,
                "v128.load64_splat",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Load32Zero,
                0xfd,
                92,
                "v128.load32zero",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Load64Zero,
                0xfd,
                93,
                "v128.load64zero",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Store,
                0xfd,
                11,
                "v128.store",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::MemoryInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Load8Lane,
                0xfd,
                84,
                "v128.load8_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128MemoryLaneInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Load16Lane,
                0xfd,
                85,
                "v128.load16_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128MemoryLaneInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Load32Lane,
                0xfd,
                86,
                "v128.load32_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128MemoryLaneInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Load64Lane,
                0xfd,
                87,
                "v128.load64_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128MemoryLaneInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Store8Lane,
                0xfd,
                88,
                "v128.store8_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128MemoryLaneInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Store16Lane,
                0xfd,
                89,
                "v128.store16_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128MemoryLaneInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Store32Lane,
                0xfd,
                90,
                "v128.store32_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128MemoryLaneInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Store64Lane,
                0xfd,
                91,
                "v128.store64_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128MemoryLaneInstruction {
                            memarg: (bytes.read_vu32()?, bytes.read_vu32()?),
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::V128Const,
                0xfd,
                12,
                "v128.const",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128Instruction {
                            value: bytes.read_v128()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I8x16Shuffle,
                0xfd,
                13,
                "i8x16shuffle",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128LanesInstruction {
                            lane_indices: bytes.read_u8vec()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I8x16ExtractLaneS,
                0xfd,
                21,
                "i8x16.extract_lane_s",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128LaneInstruction {
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I8x16ExtractLaneU,
                0xfd,
                22,
                "i8x16.extract_lane_u",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128LaneInstruction {
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I8x16ReplaceLane,
                0xfd,
                23,
                "i8x16.replace_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128LaneInstruction {
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I16x8ExtractLaneS,
                0xfd,
                24,
                "i16x8.extract_lane_s",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128LaneInstruction {
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I16x8ExtractLaneU,
                0xfd,
                25,
                "i16x8.extract_lane_u",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128LaneInstruction {
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I16x8ReplaceLane,
                0xfd,
                26,
                "i16x8.replace_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128LaneInstruction {
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I32x4ExtractLane,
                0xfd,
                27,
                "i32x4.extract_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128LaneInstruction {
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I32x4ReplaceLane,
                0xfd,
                28,
                "i32x4.replace_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128LaneInstruction {
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I64x2ExtractLane,
                0xfd,
                29,
                "i64x2.extract_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128LaneInstruction {
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::I64x2ReplaceLane,
                0xfd,
                30,
                "i64x2.replace_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128LaneInstruction {
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::F32x4ExtractLane,
                0xfd,
                31,
                "f32x4.extract_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128LaneInstruction {
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::F32x4ReplaceLane,
                0xfd,
                32,
                "f32x4.replace_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128LaneInstruction {
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::F64x2ExtractLane,
                0xfd,
                33,
                "f64x2.extract_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128LaneInstruction {
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_with_parse(
                InstructionType::F64x2ReplaceLane,
                0xfd,
                34,
                "f64x2.replace_lane",
                Arc::new(
                    |bytes: &mut super::reader::Reader| -> Result<InstructionData, io::Error> {
                        Ok(InstructionData::V128LaneInstruction {
                            lane_index: bytes.read_byte()?,
                        })
                    },
                ),
            ),
            InstructionCoding::new_simple(InstructionType::I8x16Swizzle, 0xfd, 14, "i8x16.swizzle"),
            InstructionCoding::new_simple(InstructionType::I8x16Splat, 0xfd, 15, "i8x16.splat"),
            InstructionCoding::new_simple(InstructionType::I16x8Splat, 0xfd, 16, "i16x8.splat"),
            InstructionCoding::new_simple(InstructionType::I32x4Splat, 0xfd, 17, "i32x4.splat"),
            InstructionCoding::new_simple(InstructionType::I64x2Splat, 0xfd, 18, "i64x2.splat"),
            InstructionCoding::new_simple(InstructionType::F32x4Splat, 0xfd, 19, "f32x4.splat"),
            InstructionCoding::new_simple(InstructionType::F64x2Splat, 0xfd, 20, "f64x2.splat"),
            InstructionCoding::new_simple(InstructionType::I8x16Eq, 0xfd, 35, "i8x16.eq"),
            InstructionCoding::new_simple(InstructionType::I8x16Ne, 0xfd, 36, "i8x16.ne"),
            InstructionCoding::new_simple(InstructionType::I8x16LtS, 0xfd, 37, "i8x16.lt_s"),
            InstructionCoding::new_simple(InstructionType::I8x16LtU, 0xfd, 38, "i8x16.lt_u"),
            InstructionCoding::new_simple(InstructionType::I8x16GtS, 0xfd, 39, "i8x16.gt_s"),
            InstructionCoding::new_simple(InstructionType::I8x16GtU, 0xfd, 40, "i8x16.gt_u"),
            InstructionCoding::new_simple(InstructionType::I8x16LeS, 0xfd, 41, "i8x16.le_s"),
            InstructionCoding::new_simple(InstructionType::I8x16LeU, 0xfd, 42, "i8x16.le_u"),
            InstructionCoding::new_simple(InstructionType::I8x16GeS, 0xfd, 43, "i8x16.ge_s"),
            InstructionCoding::new_simple(InstructionType::I8x16GeU, 0xfd, 44, "i8x16.ge_u"),
            InstructionCoding::new_simple(InstructionType::I16x8Eq, 0xfd, 45, "i16x8.eq"),
            InstructionCoding::new_simple(InstructionType::I16x8Ne, 0xfd, 46, "i16x8.ne"),
            InstructionCoding::new_simple(InstructionType::I16x8LtS, 0xfd, 47, "i16x8.lt_s"),
            InstructionCoding::new_simple(InstructionType::I16x8LtU, 0xfd, 48, "i16x8.lt_u"),
            InstructionCoding::new_simple(InstructionType::I16x8GtS, 0xfd, 49, "i16x8.gt_s"),
            InstructionCoding::new_simple(InstructionType::I16x8GtU, 0xfd, 50, "i16x8.gt_u"),
            InstructionCoding::new_simple(InstructionType::I16x8LeS, 0xfd, 51, "i16x8.le_s"),
            InstructionCoding::new_simple(InstructionType::I16x8LeU, 0xfd, 52, "i16x8.le_u"),
            InstructionCoding::new_simple(InstructionType::I16x8GeS, 0xfd, 53, "i16x8.ge_s"),
            InstructionCoding::new_simple(InstructionType::I16x8GeU, 0xfd, 54, "i16x8.ge_u"),
            InstructionCoding::new_simple(InstructionType::I32x4Eq, 0xfd, 55, "i32x4.eq"),
            InstructionCoding::new_simple(InstructionType::I32x4Ne, 0xfd, 56, "i32x4.ne"),
            InstructionCoding::new_simple(InstructionType::I32x4LtS, 0xfd, 57, "i32x4.lt_s"),
            InstructionCoding::new_simple(InstructionType::I32x4LtU, 0xfd, 58, "i32x4.lt_u"),
            InstructionCoding::new_simple(InstructionType::I32x4GtS, 0xfd, 59, "i32x4.gt_s"),
            InstructionCoding::new_simple(InstructionType::I32x4GtU, 0xfd, 60, "i32x4.gt_u"),
            InstructionCoding::new_simple(InstructionType::I32x4LeS, 0xfd, 61, "i32x4.le_s"),
            InstructionCoding::new_simple(InstructionType::I32x4LeU, 0xfd, 62, "i32x4.le_u"),
            InstructionCoding::new_simple(InstructionType::I32x4GeS, 0xfd, 63, "i32x4.ge_s"),
            InstructionCoding::new_simple(InstructionType::I32x4GeU, 0xfd, 64, "i32x4.ge_u"),
            InstructionCoding::new_simple(InstructionType::I64x2Eq, 0xfd, 214, "i64x2.eq"),
            InstructionCoding::new_simple(InstructionType::I64x2Ne, 0xfd, 215, "i64x2.ne"),
            InstructionCoding::new_simple(InstructionType::I64x2LtS, 0xfd, 216, "i64x2.lt_s"),
            InstructionCoding::new_simple(InstructionType::I64x2GtS, 0xfd, 217, "i64x2.gt_s"),
            InstructionCoding::new_simple(InstructionType::I64x2LeS, 0xfd, 218, "i64x2.le_s"),
            InstructionCoding::new_simple(InstructionType::I64x2GeS, 0xfd, 219, "i64x2.ge_s"),
            InstructionCoding::new_simple(InstructionType::F32x4Eq, 0xfd, 65, "f32x4.eq"),
            InstructionCoding::new_simple(InstructionType::F32x4Ne, 0xfd, 66, "f32x4.ne"),
            InstructionCoding::new_simple(InstructionType::F32x4Lt, 0xfd, 67, "f32x4.lt"),
            InstructionCoding::new_simple(InstructionType::F32x4Gt, 0xfd, 68, "f32x4.gt"),
            InstructionCoding::new_simple(InstructionType::F32x4Le, 0xfd, 69, "f32x4.le"),
            InstructionCoding::new_simple(InstructionType::F32x4Ge, 0xfd, 70, "f32x4.ge"),
            InstructionCoding::new_simple(InstructionType::F64x2Eq, 0xfd, 71, "f64x2.eq"),
            InstructionCoding::new_simple(InstructionType::F64x2Ne, 0xfd, 72, "f64x2.ne"),
            InstructionCoding::new_simple(InstructionType::F64x2Lt, 0xfd, 73, "f64x2.lt"),
            InstructionCoding::new_simple(InstructionType::F64x2Gt, 0xfd, 74, "f64x2.gt"),
            InstructionCoding::new_simple(InstructionType::F64x2Le, 0xfd, 75, "f64x2.le"),
            InstructionCoding::new_simple(InstructionType::F64x2Ge, 0xfd, 76, "f64x2.ge"),
            InstructionCoding::new_simple(InstructionType::V128Not, 0xfd, 77, "v128.not"),
            InstructionCoding::new_simple(InstructionType::V128And, 0xfd, 78, "v128.and"),
            InstructionCoding::new_simple(InstructionType::V128AndNot, 0xfd, 79, "v128.andnot"),
            InstructionCoding::new_simple(InstructionType::V128Or, 0xfd, 80, "v128.or"),
            InstructionCoding::new_simple(InstructionType::V128Xor, 0xfd, 81, "v128.xor"),
            InstructionCoding::new_simple(
                InstructionType::V128Bitselect,
                0xfd,
                82,
                "v128.bitselect",
            ),
            InstructionCoding::new_simple(InstructionType::V128AnyTrue, 0xfd, 83, "v128.anytrue"),
            InstructionCoding::new_simple(InstructionType::I8x16Abs, 0xfd, 96, "i8x16.abs"),
            InstructionCoding::new_simple(InstructionType::I8x16Neg, 0xfd, 97, "i8x16.neg"),
            InstructionCoding::new_simple(InstructionType::I8x16Popcnt, 0xfd, 98, "i8x16.popcnt"),
            InstructionCoding::new_simple(
                InstructionType::I8x16AllTrue,
                0xfd,
                99,
                "i8x16.all_true",
            ),
            InstructionCoding::new_simple(
                InstructionType::I8x16Bitmask,
                0xfd,
                100,
                "i8x16.bitmask",
            ),
            InstructionCoding::new_simple(
                InstructionType::I8x16NarrowI16x8S,
                0xfd,
                101,
                "i8x16.narrow_i16x8_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I8x16NarrowI16x8U,
                0xfd,
                102,
                "i8x16.narrow_i16x8_u",
            ),
            InstructionCoding::new_simple(InstructionType::I8x16Shl, 0xfd, 107, "i8x16.shl"),
            InstructionCoding::new_simple(InstructionType::I8x16ShrS, 0xfd, 108, "i8x16.shr_s"),
            InstructionCoding::new_simple(InstructionType::I8x16ShrU, 0xfd, 109, "i8x16.shr_u"),
            InstructionCoding::new_simple(InstructionType::I8x16Add, 0xfd, 110, "i8x16.add"),
            InstructionCoding::new_simple(
                InstructionType::I8x16AddSaturateS,
                0xfd,
                111,
                "i8x16.add_sat_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I8x16AddSaturateU,
                0xfd,
                112,
                "i8x16.add_sat_u",
            ),
            InstructionCoding::new_simple(InstructionType::I8x16Sub, 0xfd, 113, "i8x16.sub"),
            InstructionCoding::new_simple(
                InstructionType::I8x16SubSaturateS,
                0xfd,
                114,
                "i8x16.sub_sat_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I8x16SubSaturateU,
                0xfd,
                115,
                "i8x16.sub_sat_u",
            ),
            InstructionCoding::new_simple(InstructionType::I8x16MinS, 0xfd, 118, "i8x16.min_s"),
            InstructionCoding::new_simple(InstructionType::I8x16MinU, 0xfd, 119, "i8x16.min_u"),
            InstructionCoding::new_simple(InstructionType::I8x16MaxS, 0xfd, 120, "i8x16.max_s"),
            InstructionCoding::new_simple(InstructionType::I8x16MaxU, 0xfd, 121, "i8x16.max_u"),
            InstructionCoding::new_simple(InstructionType::I8x16AvgrU, 0xfd, 123, "i8x16.avgr_u"),
            InstructionCoding::new_simple(
                InstructionType::I16x8ExtAddPairwiseI8x16S,
                0xfd,
                124,
                "i16x8.extadd_pairwise_i8x16_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I16x8ExtAddPairwiseI8x16U,
                0xfd,
                125,
                "i16x8.extadd_pairwise_i8x16_u",
            ),
            InstructionCoding::new_simple(InstructionType::I16x8Abs, 0xfd, 128, "i16x8.abs"),
            InstructionCoding::new_simple(InstructionType::I16x8Neg, 0xfd, 129, "i16x8.neg"),
            InstructionCoding::new_simple(
                InstructionType::I16x8Q15MulrSatS,
                0xfd,
                130,
                "i16x8.q15mulr_sat_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I16x8AllTrue,
                0xfd,
                131,
                "i16x8.all_true",
            ),
            InstructionCoding::new_simple(
                InstructionType::I16x8Bitmask,
                0xfd,
                132,
                "i16x8.bitmask",
            ),
            InstructionCoding::new_simple(
                InstructionType::I16x8NarrowI32x4S,
                0xfd,
                133,
                "i16x8.narrow_i32x4_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I16x8NarrowI32x4U,
                0xfd,
                134,
                "i16x8.narrow_i32x4_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::I16x8ExtendLowI8x16S,
                0xfd,
                135,
                "i16x8.extend_low_i8x16_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I16x8ExtendHighI8x16S,
                0xfd,
                136,
                "i16x8.extend_high_i8x16_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I16x8ExtendLowI8x16U,
                0xfd,
                137,
                "i16x8.extend_low_i8x16_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::I16x8ExtendHighI8x16U,
                0xfd,
                138,
                "i16x8.extend_high_i8x16_u",
            ),
            InstructionCoding::new_simple(InstructionType::I16x8Shl, 0xfd, 139, "i16x8.shl"),
            InstructionCoding::new_simple(InstructionType::I16x8ShrS, 0xfd, 140, "i16x8.shr_s"),
            InstructionCoding::new_simple(InstructionType::I16x8ShrU, 0xfd, 141, "i16x8.shr_u"),
            InstructionCoding::new_simple(InstructionType::I16x8Add, 0xfd, 142, "i16x8.add"),
            InstructionCoding::new_simple(
                InstructionType::I16x8AddSaturateS,
                0xfd,
                143,
                "i16x8.add_sat_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I16x8AddSaturateU,
                0xfd,
                144,
                "i16x8.add_sat_u",
            ),
            InstructionCoding::new_simple(InstructionType::I16x8Sub, 0xfd, 145, "i16x8.sub"),
            InstructionCoding::new_simple(
                InstructionType::I16x8SubSaturateS,
                0xfd,
                146,
                "i16x8.sub_sat_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I16x8SubSaturateU,
                0xfd,
                147,
                "i16x8.sub_sat_u",
            ),
            InstructionCoding::new_simple(InstructionType::I16x8Mul, 0xfd, 149, "i16x8.mul"),
            InstructionCoding::new_simple(InstructionType::I16x8MinS, 0xfd, 150, "i16x8.min_s"),
            InstructionCoding::new_simple(InstructionType::I16x8MinU, 0xfd, 151, "i16x8.min_u"),
            InstructionCoding::new_simple(InstructionType::I16x8MaxS, 0xfd, 152, "i16x8.max_s"),
            InstructionCoding::new_simple(InstructionType::I16x8MaxU, 0xfd, 153, "i16x8.max_u"),
            InstructionCoding::new_simple(InstructionType::I16x8AvgrU, 0xfd, 155, "i16x8.avgr_u"),
            InstructionCoding::new_simple(
                InstructionType::I16x8ExtMulLowI8x16S,
                0xfd,
                156,
                "i16x8.extmul_low_i8x16_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I16x8ExtMulHighI8x16S,
                0xfd,
                157,
                "i16x8.extmul_high_i8x16_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I16x8ExtMulLowI8x16U,
                0xfd,
                158,
                "i16x8.extmul_low_i8x16_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::I16x8ExtMulHighI8x16U,
                0xfd,
                159,
                "i16x8.extmul_high_i8x16_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32x4ExtAddPairwiseI16x8S,
                0xfd,
                126,
                "i32x4.extadd_pairwise_i16x8_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32x4ExtAddPairwiseI16x8U,
                0xfd,
                127,
                "i32x4.extadd_pairwise_i16x8_u",
            ),
            InstructionCoding::new_simple(InstructionType::I32x4Abs, 0xfd, 160, "i32x4.abs"),
            InstructionCoding::new_simple(InstructionType::I32x4Neg, 0xfd, 161, "i32x4.neg"),
            InstructionCoding::new_simple(
                InstructionType::I32x4AllTrue,
                0xfd,
                163,
                "i32x4.all_true",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32x4Bitmask,
                0xfd,
                164,
                "i32x4.bitmask",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32x4ExtendLowI16x8S,
                0xfd,
                167,
                "i32x4.extend_low_i16x8_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32x4ExtendHighI16x8S,
                0xfd,
                168,
                "i32x4.extend_high_i16x8_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32x4ExtendLowI16x8U,
                0xfd,
                169,
                "i32x4.extend_low_i16x8_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32x4ExtendHighI16x8U,
                0xfd,
                170,
                "i32x4.extend_high_i16x8_u",
            ),
            InstructionCoding::new_simple(InstructionType::I32x4Shl, 0xfd, 171, "i32x4.shl"),
            InstructionCoding::new_simple(InstructionType::I32x4ShrS, 0xfd, 172, "i32x4.shr_s"),
            InstructionCoding::new_simple(InstructionType::I32x4ShrU, 0xfd, 173, "i32x4.shr_u"),
            InstructionCoding::new_simple(InstructionType::I32x4Add, 0xfd, 174, "i32x4.add"),
            InstructionCoding::new_simple(InstructionType::I32x4Sub, 0xfd, 177, "i32x4.sub"),
            InstructionCoding::new_simple(InstructionType::I32x4Mul, 0xfd, 181, "i32x4.mul"),
            InstructionCoding::new_simple(InstructionType::I32x4MinS, 0xfd, 182, "i32x4.min_s"),
            InstructionCoding::new_simple(InstructionType::I32x4MinU, 0xfd, 183, "i32x4.min_u"),
            InstructionCoding::new_simple(InstructionType::I32x4MaxS, 0xfd, 184, "i32x4.max_s"),
            InstructionCoding::new_simple(InstructionType::I32x4MaxU, 0xfd, 185, "i32x4.max_u"),
            InstructionCoding::new_simple(
                InstructionType::I32x4DotI16x8S,
                0xfd,
                186,
                "i32x4.dot_i16x8_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32x4ExtMulLowI16x8S,
                0xfd,
                188,
                "i32x4.extmul_low_i16x8_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32x4ExtMulHighI16x8S,
                0xfd,
                189,
                "i32x4.extmul_high_i16x8_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32x4ExtMulLowI16x8U,
                0xfd,
                190,
                "i32x4.extmul_low_i16x8_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32x4ExtMulHighI16x8U,
                0xfd,
                191,
                "i32x4.extmul_high_i16x8_u",
            ),
            InstructionCoding::new_simple(InstructionType::I64x2Abs, 0xfd, 192, "i64x2.abs"),
            InstructionCoding::new_simple(InstructionType::I64x2Neg, 0xfd, 193, "i64x2.neg"),
            InstructionCoding::new_simple(
                InstructionType::I64x2AllTrue,
                0xfd,
                195,
                "i64x2.all_true",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64x2Bitmask,
                0xfd,
                196,
                "i64x2.bitmask",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64x2ExtendLowI32x4S,
                0xfd,
                199,
                "i64x2.extend_low_i32x4_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64x2ExtendHighI32x4S,
                0xfd,
                200,
                "i64x2.extend_high_i32x4_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64x2ExtendLowI32x4U,
                0xfd,
                201,
                "i64x2.extend_low_i32x4_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64x2ExtendHighI32x4U,
                0xfd,
                202,
                "i64x2.extend_high_i32x4_u",
            ),
            InstructionCoding::new_simple(InstructionType::I64x2Shl, 0xfd, 203, "i64x2.shl"),
            InstructionCoding::new_simple(InstructionType::I64x2ShrS, 0xfd, 204, "i64x2.shr_s"),
            InstructionCoding::new_simple(InstructionType::I64x2ShrU, 0xfd, 205, "i64x2.shr_u"),
            InstructionCoding::new_simple(InstructionType::I64x2Add, 0xfd, 206, "i64x2.add"),
            InstructionCoding::new_simple(InstructionType::I64x2Sub, 0xfd, 209, "i64x2.sub"),
            InstructionCoding::new_simple(InstructionType::I64x2Mul, 0xfd, 213, "i64x2.mul"),
            InstructionCoding::new_simple(
                InstructionType::I64x2ExtMulLowI32x4S,
                0xfd,
                220,
                "i64x2.extmul_low_i32x4_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64x2ExtMulHighI32x4S,
                0xfd,
                221,
                "i64x2.extmul_high_i32x4_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64x2ExtMulLowI32x4U,
                0xfd,
                222,
                "i64x2.extmul_low_i32x4_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::I64x2ExtMulHighI32x4U,
                0xfd,
                223,
                "i64x2.extmul_high_i32x4_u",
            ),
            InstructionCoding::new_simple(InstructionType::F32x4Ceil, 0xfd, 103, "f32x4.ceil"),
            InstructionCoding::new_simple(InstructionType::F32x4Floor, 0xfd, 104, "f32x4.floor"),
            InstructionCoding::new_simple(InstructionType::F32x4Trunc, 0xfd, 105, "f32x4.trunc"),
            InstructionCoding::new_simple(
                InstructionType::F32x4Nearest,
                0xfd,
                106,
                "f32x4.nearest",
            ),
            InstructionCoding::new_simple(InstructionType::F32x4Abs, 0xfd, 224, "f32x4.abs"),
            InstructionCoding::new_simple(InstructionType::F32x4Neg, 0xfd, 225, "f32x4.neg"),
            InstructionCoding::new_simple(InstructionType::F32x4Sqrt, 0xfd, 227, "f32x4.sqrt"),
            InstructionCoding::new_simple(InstructionType::F32x4Add, 0xfd, 228, "f32x4.add"),
            InstructionCoding::new_simple(InstructionType::F32x4Sub, 0xfd, 229, "f32x4.sub"),
            InstructionCoding::new_simple(InstructionType::F32x4Mul, 0xfd, 230, "f32x4.mul"),
            InstructionCoding::new_simple(InstructionType::F32x4Div, 0xfd, 231, "f32x4.div"),
            InstructionCoding::new_simple(InstructionType::F32x4Min, 0xfd, 232, "f32x4.min"),
            InstructionCoding::new_simple(InstructionType::F32x4Max, 0xfd, 233, "f32x4.max"),
            InstructionCoding::new_simple(InstructionType::F32x4PMin, 0xfd, 234, "f32x4.pmin"),
            InstructionCoding::new_simple(InstructionType::F32x4PMax, 0xfd, 235, "f32x4.pmax"),
            InstructionCoding::new_simple(InstructionType::F64x2Ceil, 0xfd, 116, "f64x2.ceil"),
            InstructionCoding::new_simple(InstructionType::F64x2Floor, 0xfd, 117, "f64x2.floor"),
            InstructionCoding::new_simple(InstructionType::F64x2Trunc, 0xfd, 122, "f64x2.trunc"),
            InstructionCoding::new_simple(
                InstructionType::F64x2Nearest,
                0xfd,
                148,
                "f64x2.nearest",
            ),
            InstructionCoding::new_simple(InstructionType::F64x2Abs, 0xfd, 236, "f64x2.abs"),
            InstructionCoding::new_simple(InstructionType::F64x2Neg, 0xfd, 237, "f64x2.neg"),
            InstructionCoding::new_simple(InstructionType::F64x2Sqrt, 0xfd, 239, "f64x2.sqrt"),
            InstructionCoding::new_simple(InstructionType::F64x2Add, 0xfd, 240, "f64x2.add"),
            InstructionCoding::new_simple(InstructionType::F64x2Sub, 0xfd, 241, "f64x2.sub"),
            InstructionCoding::new_simple(InstructionType::F64x2Mul, 0xfd, 242, "f64x2.mul"),
            InstructionCoding::new_simple(InstructionType::F64x2Div, 0xfd, 243, "f64x2.div"),
            InstructionCoding::new_simple(InstructionType::F64x2Min, 0xfd, 244, "f64x2.min"),
            InstructionCoding::new_simple(InstructionType::F64x2Max, 0xfd, 245, "f64x2.max"),
            InstructionCoding::new_simple(InstructionType::F64x2PMin, 0xfd, 246, "f64x2.pmin"),
            InstructionCoding::new_simple(InstructionType::F64x2PMax, 0xfd, 247, "f64x2.pmax"),
            InstructionCoding::new_simple(
                InstructionType::I32x4TruncSatF32x4S,
                0xfd,
                248,
                "i32x4.trunc_sat_f32x4_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32x4TruncSatF32x4U,
                0xfd,
                249,
                "i32x4.trunc_sat_f32x4_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::F32x4ConvertI32x4S,
                0xfd,
                250,
                "f32x4.convert_i32x4_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::F32x4ConvertI32x4U,
                0xfd,
                251,
                "f32x4.convert_i32x4_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32x4TruncSatF64x2SZero,
                0xfd,
                252,
                "i32x4.trunc_sat_f64x2_s_zero",
            ),
            InstructionCoding::new_simple(
                InstructionType::I32x4TruncSatF64x2UZero,
                0xfd,
                253,
                "i32x4.trunc_sat_f64x2_u_zero",
            ),
            InstructionCoding::new_simple(
                InstructionType::F64x2ConvertLowI32x4S,
                0xfd,
                254,
                "f64x2.convert_low_i32x4_s",
            ),
            InstructionCoding::new_simple(
                InstructionType::F64x2ConvertLowI32x4U,
                0xfd,
                255,
                "f64x2.convert_low_i32x4_u",
            ),
            InstructionCoding::new_simple(
                InstructionType::F32x4DemoteF64x2Zero,
                0xfd,
                94,
                "f32x4.demote_f64x2_zero",
            ),
            InstructionCoding::new_simple(
                InstructionType::F64x2PromoteLowF32x4,
                0xfd,
                95,
                "f64x2.promote_low_f32x4",
            ),
            InstructionCoding::new_simple(InstructionType::End, 0x0b, 0, "end"),
        ]
    })
}

pub fn get_codings_by_opcode() -> &'static HashMap<u8, InstructionCoding> {
    static CODINGS_BY_OPCODE: OnceCell<HashMap<u8, InstructionCoding>> = OnceCell::new();
    CODINGS_BY_OPCODE.get_or_init(|| {
        let mut map: HashMap<u8, InstructionCoding> = HashMap::new();
        for coding in get_codings().iter() {
            map.insert(coding.opcode, coding.clone());
        }
        map
    })
}

pub fn get_codings_by_type() -> &'static HashMap<InstructionType, InstructionCoding> {
    static CODINGS_BY_NAME: OnceCell<HashMap<InstructionType, InstructionCoding>> = OnceCell::new();
    CODINGS_BY_NAME.get_or_init(|| {
        let mut map: HashMap<InstructionType, InstructionCoding> = HashMap::new();
        for coding in get_codings().iter() {
            map.insert(coding.typ, coding.clone());
        }
        map
    })
}

struct InstructionIterator<'a> {
    bytes: &'a mut super::reader::Reader,
    parse_type: ParseType,
    ended: bool,
}

enum ParseType {
    ReadAll,
    ReadTillEnd,
}

impl InstructionIterator<'_> {
    fn new<'a>(
        bytes: &'a mut super::reader::Reader,
        parse_type: ParseType,
    ) -> InstructionIterator<'a> {
        InstructionIterator {
            bytes,
            parse_type,
            ended: false,
        }
    }
}

impl<'a> Iterator for InstructionIterator<'a> {
    type Item = Result<Instruction, io::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.ended {
            return None;
        }

        let pos = self.bytes.pos();
        match self.bytes.read_byte() {
            Ok(opcode) => match get_codings_by_opcode().get(&opcode) {
                Some(block_coding) => {
                    let instruction = (block_coding.to_owned().parse_bytes)(&mut self.bytes);
                    match instruction {
                        Ok(data) => {
                            println!("opcode: {} '{}' @ {:x}", opcode, block_coding.name, pos);
                            self.ended = !self.bytes.has_at_least(1)
                                || match self.parse_type {
                                    ParseType::ReadAll => false,
                                    ParseType::ReadTillEnd => {
                                        block_coding.typ == InstructionType::End
                                    }
                                };
                            Some(Ok(Instruction {
                                typ: block_coding.typ,
                                data: data,
                            }))
                        }
                        Err(e) => {
                            println!("error: {}", e);
                            Some(Err(e))
                        }
                    }
                }
                None => Some(Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("unknown opcode: {}", opcode),
                ))),
            },
            Err(e) => Some(Err(e)),
        }
    }
}

impl Instruction {
    pub fn decode_expression(
        bytes: &mut super::reader::Reader,
    ) -> Result<Vec<Instruction>, io::Error> {
        let types = &super::module::TypeSection::new();
        let locals = &vec![];
        let ftype = &super::module::FunctionType {
            // TODO: confirm this is the right signature for these: [0,n]=>[x]
            parameters: vec![super::module::ValueType::I32, super::module::ValueType::I32],
            return_types: vec![super::module::ValueType::I32],
        };

        let instruction_iter = InstructionIterator::new(bytes, ParseType::ReadTillEnd);
        let mut instructions: Vec<Instruction> = vec![];
        let mut validator = super::validate::Validator::new(&types, &locals, ftype);
        for result in instruction_iter {
            let instruction = result?;
            validator
                .validate(&instruction)
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
            instructions.push(instruction);
        }
        Ok(instructions)
    }

    pub fn decode_function(
        types: &super::module::TypeSection,
        locals: &Vec<super::module::ValueType>,
        ftype: &super::module::FunctionType,
        bytes: &mut super::reader::Reader,
    ) -> Result<Vec<Instruction>, io::Error> {
        let instruction_iter = InstructionIterator::new(bytes, ParseType::ReadAll);
        let mut instructions: Vec<Instruction> = vec![];
        let mut validator = super::validate::Validator::new(&types, &locals, ftype);
        for result in instruction_iter {
            let instruction = result?;
            validator
                .validate(&instruction)
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
            instructions.push(instruction);
        }
        Ok(instructions)
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum BlockType {
    Empty,
    Type(super::module::ValueType),
    TypeIndex(i32),
}

impl BlockType {
    fn emit_bytes(&self) -> Vec<u8> {
        match self {
            BlockType::Empty => vec![0x40],
            BlockType::Type(value_type) => value_type.emit_bytes(),
            BlockType::TypeIndex(type_index) => super::reader::emit_vs32(*type_index),
        }
    }

    fn parse_bytes(bytes: &mut super::reader::Reader) -> Result<BlockType, io::Error> {
        // if first byte is 0x40, it's an empty type
        // if it's one of the value type bytes, it's a value type
        // otherwise, interpret the first byte and following bytes as a signed
        // LEB128 integer using read_vs32, and use that as the type index _if_ it's
        // positive, otherwise it's an error
        let b = bytes.read_byte()?;
        if b == 0x40 {
            Ok(BlockType::Empty)
        } else if super::module::ValueType::is_value_type_byte(b) {
            Ok(BlockType::Type(
                super::module::ValueType::decode(b)
                    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?,
            ))
        } else {
            let mut first_byte = Some(b);
            let type_index = super::reader::read_vs32(&mut || match first_byte.take() {
                Some(byte) => Ok(byte),
                None => bytes.read_byte(),
            })?;
            if type_index < 0 {
                Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "invalid block type index",
                ))
            } else {
                Ok(BlockType::TypeIndex(type_index))
            }
        }
    }
}

impl fmt::Display for BlockType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BlockType::Empty => write!(f, ""),
            BlockType::Type(value_type) => write!(f, "{}", value_type),
            BlockType::TypeIndex(type_index) => write!(f, "type {}", type_index),
        }
    }
}

fn consume_vu32vec(bytes: &mut super::reader::Reader) -> Result<Vec<u32>, io::Error> {
    let len: u64 = bytes.read_vu64()?;
    let mut vec: Vec<u32> = vec![];
    for _ in 0..len {
        vec.push(bytes.read_vu32()?);
    }
    Ok(vec)
}

fn emit_vu32vec(vec: &Vec<u32>) -> Vec<u8> {
    let mut bytes: Vec<u8> = super::reader::emit_vu64(vec.len() as u64);
    for value in vec {
        bytes.extend(super::reader::emit_vu32(*value));
    }
    bytes
}
