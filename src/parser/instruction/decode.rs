//! Instruction decoding from binary format

use super::{BlockType, ByteRange, Instruction, InstructionKind, MemArg};
use crate::parser::module::ValueType;
use crate::parser::reader::Reader;
use std::io;

// WebAssembly opcode constants
const OPCODE_EMPTY_BLOCK_TYPE: u8 = 0x40;

/// Error type for instruction decoding
#[derive(Debug)]
pub enum DecodeError {
    Io(io::Error),
    UnknownOpcode(u8),
    UnknownInstruction(u8, u32), // For multi-byte opcodes (0xFC prefix for saturating/memory ops, 0xFD for SIMD)
    InvalidValueType(String),
    InvalidBlockType,
    Validation(crate::parser::validate::ValidationError),
}

impl From<io::Error> for DecodeError {
    fn from(e: io::Error) -> Self {
        DecodeError::Io(e)
    }
}

impl From<crate::parser::validate::ValidationError> for DecodeError {
    fn from(e: crate::parser::validate::ValidationError) -> Self {
        DecodeError::Validation(e)
    }
}

impl std::fmt::Display for DecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DecodeError::Io(e) => write!(f, "IO error: {e}"),
            DecodeError::UnknownOpcode(_op) => write!(f, "illegal opcode"),
            DecodeError::UnknownInstruction(prefix, sub) => {
                write!(f, "Unknown instruction: {prefix:#04x} {sub:#04x}")
            }
            DecodeError::InvalidValueType(s) => write!(f, "Invalid value type: {s}"),
            DecodeError::InvalidBlockType => write!(f, "Invalid block type"),
            DecodeError::Validation(e) => write!(f, "{e}"),
        }
    }
}

impl std::error::Error for DecodeError {}

impl From<String> for DecodeError {
    fn from(s: String) -> Self {
        DecodeError::InvalidValueType(s)
    }
}

impl MemArg {
    /// Decode a memory argument from the reader
    pub fn decode(reader: &mut Reader) -> Result<Self, DecodeError> {
        let align = reader.read_vu32()?;
        let offset = reader.read_vu32()?;
        Ok(MemArg { align, offset })
    }
}

impl BlockType {
    /// Decode a block type from the reader
    pub fn decode(reader: &mut Reader) -> Result<Self, DecodeError> {
        // Peek at the next byte to determine block type
        let b = reader.read_byte()?;
        if b == OPCODE_EMPTY_BLOCK_TYPE {
            Ok(BlockType::Empty)
        } else if ValueType::is_value_type_byte(b) {
            // Parse as value type
            match ValueType::decode(b) {
                Ok(vt) => Ok(BlockType::Value(vt)),
                Err(e) => Err(DecodeError::InvalidValueType(e)),
            }
        } else {
            // Not a value type, read as signed LEB128 with first byte already consumed
            let mut first_byte = Some(b);
            let type_idx = crate::parser::reader::read_vs33(&mut || match first_byte.take() {
                Some(byte) => Ok(byte),
                None => reader.read_byte(),
            })? as u32;
            Ok(BlockType::FuncType(type_idx))
        }
    }
}

impl InstructionKind {
    /// Decode an instruction from the reader
    pub fn decode(reader: &mut Reader) -> Result<Self, DecodeError> {
        let opcode = reader.read_byte()?;

        use InstructionKind::*;
        match opcode {
            // Control instructions (0x00-0x11)
            0x00 => Ok(Unreachable),
            0x01 => Ok(Nop),
            0x02 => Ok(Block {
                block_type: BlockType::decode(reader)?,
            }),
            0x03 => Ok(Loop {
                block_type: BlockType::decode(reader)?,
            }),
            0x04 => Ok(If {
                block_type: BlockType::decode(reader)?,
            }),
            0x05 => Ok(Else),
            0x0B => Ok(End),
            0x0C => Ok(Br {
                label_idx: reader.read_vu32()?,
            }),
            0x0D => Ok(BrIf {
                label_idx: reader.read_vu32()?,
            }),
            0x0E => {
                // BrTable
                let count = reader.read_vu32()?;
                let mut labels = Vec::with_capacity(count as usize);
                for _ in 0..count {
                    labels.push(reader.read_vu32()?);
                }
                let default = reader.read_vu32()?;
                Ok(BrTable { labels, default })
            }
            0x0F => Ok(Return),
            0x10 => Ok(Call {
                func_idx: reader.read_vu32()?,
            }),
            0x11 => Ok(CallIndirect {
                type_idx: reader.read_vu32()?,
                table_idx: reader.read_vu32()?,
            }),

            // Reference instructions (0xD0-0xD2)
            0xD0 => {
                let ref_type = ValueType::decode(reader.read_byte()?)?;
                Ok(RefNull { ref_type })
            }
            0xD1 => Ok(RefIsNull),
            0xD2 => Ok(RefFunc {
                func_idx: reader.read_vu32()?,
            }),

            // Parametric instructions (0x1A-0x1C)
            0x1A => Ok(Drop),
            0x1B => Ok(Select),
            0x1C => {
                // SelectTyped
                let count = reader.read_vu32()?;
                let mut val_types = Vec::with_capacity(count as usize);
                for _ in 0..count {
                    val_types.push(ValueType::decode(reader.read_byte()?)?);
                }
                Ok(SelectTyped { val_types })
            }

            // Variable instructions (0x20-0x24)
            0x20 => Ok(LocalGet {
                local_idx: reader.read_vu32()?,
            }),
            0x21 => Ok(LocalSet {
                local_idx: reader.read_vu32()?,
            }),
            0x22 => Ok(LocalTee {
                local_idx: reader.read_vu32()?,
            }),
            0x23 => Ok(GlobalGet {
                global_idx: reader.read_vu32()?,
            }),
            0x24 => Ok(GlobalSet {
                global_idx: reader.read_vu32()?,
            }),

            // Table instructions (0x25-0x26)
            0x25 => Ok(TableGet {
                table_idx: reader.read_vu32()?,
            }),
            0x26 => Ok(TableSet {
                table_idx: reader.read_vu32()?,
            }),

            // Memory instructions (0x28-0x40)
            0x28 => Ok(I32Load {
                memarg: MemArg::decode(reader)?,
            }),
            0x29 => Ok(I64Load {
                memarg: MemArg::decode(reader)?,
            }),
            0x2A => Ok(F32Load {
                memarg: MemArg::decode(reader)?,
            }),
            0x2B => Ok(F64Load {
                memarg: MemArg::decode(reader)?,
            }),
            0x2C => Ok(I32Load8S {
                memarg: MemArg::decode(reader)?,
            }),
            0x2D => Ok(I32Load8U {
                memarg: MemArg::decode(reader)?,
            }),
            0x2E => Ok(I32Load16S {
                memarg: MemArg::decode(reader)?,
            }),
            0x2F => Ok(I32Load16U {
                memarg: MemArg::decode(reader)?,
            }),
            0x30 => Ok(I64Load8S {
                memarg: MemArg::decode(reader)?,
            }),
            0x31 => Ok(I64Load8U {
                memarg: MemArg::decode(reader)?,
            }),
            0x32 => Ok(I64Load16S {
                memarg: MemArg::decode(reader)?,
            }),
            0x33 => Ok(I64Load16U {
                memarg: MemArg::decode(reader)?,
            }),
            0x34 => Ok(I64Load32S {
                memarg: MemArg::decode(reader)?,
            }),
            0x35 => Ok(I64Load32U {
                memarg: MemArg::decode(reader)?,
            }),
            0x36 => Ok(I32Store {
                memarg: MemArg::decode(reader)?,
            }),
            0x37 => Ok(I64Store {
                memarg: MemArg::decode(reader)?,
            }),
            0x38 => Ok(F32Store {
                memarg: MemArg::decode(reader)?,
            }),
            0x39 => Ok(F64Store {
                memarg: MemArg::decode(reader)?,
            }),
            0x3A => Ok(I32Store8 {
                memarg: MemArg::decode(reader)?,
            }),
            0x3B => Ok(I32Store16 {
                memarg: MemArg::decode(reader)?,
            }),
            0x3C => Ok(I64Store8 {
                memarg: MemArg::decode(reader)?,
            }),
            0x3D => Ok(I64Store16 {
                memarg: MemArg::decode(reader)?,
            }),
            0x3E => Ok(I64Store32 {
                memarg: MemArg::decode(reader)?,
            }),
            // Memory size/grow instructions (0x3F-0x40) are handled specially in the instruction
            // iterator because they require validating a reserved byte that must be 0x00.
            // We shouldn't reach here in normal parsing, but include for completeness.
            0x3F => Err(DecodeError::InvalidValueType(
                "0x3F requires special decoding".to_string(),
            )),
            0x40 => Err(DecodeError::InvalidValueType(
                "0x40 requires special decoding".to_string(),
            )),

            // Numeric instructions (0x41-0xC4)
            0x41 => Ok(I32Const {
                value: reader.read_vs32()?,
            }),
            0x42 => Ok(I64Const {
                value: reader.read_vs64()?,
            }),
            0x43 => Ok(F32Const {
                value: reader.read_f32()?,
            }),
            0x44 => Ok(F64Const {
                value: reader.read_f64()?,
            }),

            // i32 operations
            0x45 => Ok(I32Eqz),
            0x46 => Ok(I32Eq),
            0x47 => Ok(I32Ne),
            0x48 => Ok(I32LtS),
            0x49 => Ok(I32LtU),
            0x4A => Ok(I32GtS),
            0x4B => Ok(I32GtU),
            0x4C => Ok(I32LeS),
            0x4D => Ok(I32LeU),
            0x4E => Ok(I32GeS),
            0x4F => Ok(I32GeU),

            // i64 operations
            0x50 => Ok(I64Eqz),
            0x51 => Ok(I64Eq),
            0x52 => Ok(I64Ne),
            0x53 => Ok(I64LtS),
            0x54 => Ok(I64LtU),
            0x55 => Ok(I64GtS),
            0x56 => Ok(I64GtU),
            0x57 => Ok(I64LeS),
            0x58 => Ok(I64LeU),
            0x59 => Ok(I64GeS),
            0x5A => Ok(I64GeU),

            // f32 operations
            0x5B => Ok(F32Eq),
            0x5C => Ok(F32Ne),
            0x5D => Ok(F32Lt),
            0x5E => Ok(F32Gt),
            0x5F => Ok(F32Le),
            0x60 => Ok(F32Ge),

            // f64 operations
            0x61 => Ok(F64Eq),
            0x62 => Ok(F64Ne),
            0x63 => Ok(F64Lt),
            0x64 => Ok(F64Gt),
            0x65 => Ok(F64Le),
            0x66 => Ok(F64Ge),

            // i32 arithmetic
            0x67 => Ok(I32Clz),
            0x68 => Ok(I32Ctz),
            0x69 => Ok(I32Popcnt),
            0x6A => Ok(I32Add),
            0x6B => Ok(I32Sub),
            0x6C => Ok(I32Mul),
            0x6D => Ok(I32DivS),
            0x6E => Ok(I32DivU),
            0x6F => Ok(I32RemS),
            0x70 => Ok(I32RemU),
            0x71 => Ok(I32And),
            0x72 => Ok(I32Or),
            0x73 => Ok(I32Xor),
            0x74 => Ok(I32Shl),
            0x75 => Ok(I32ShrS),
            0x76 => Ok(I32ShrU),
            0x77 => Ok(I32Rotl),
            0x78 => Ok(I32Rotr),

            // i64 arithmetic
            0x79 => Ok(I64Clz),
            0x7A => Ok(I64Ctz),
            0x7B => Ok(I64Popcnt),
            0x7C => Ok(I64Add),
            0x7D => Ok(I64Sub),
            0x7E => Ok(I64Mul),
            0x7F => Ok(I64DivS),
            0x80 => Ok(I64DivU),
            0x81 => Ok(I64RemS),
            0x82 => Ok(I64RemU),
            0x83 => Ok(I64And),
            0x84 => Ok(I64Or),
            0x85 => Ok(I64Xor),
            0x86 => Ok(I64Shl),
            0x87 => Ok(I64ShrS),
            0x88 => Ok(I64ShrU),
            0x89 => Ok(I64Rotl),
            0x8A => Ok(I64Rotr),

            // f32 arithmetic
            0x8B => Ok(F32Abs),
            0x8C => Ok(F32Neg),
            0x8D => Ok(F32Ceil),
            0x8E => Ok(F32Floor),
            0x8F => Ok(F32Trunc),
            0x90 => Ok(F32Nearest),
            0x91 => Ok(F32Sqrt),
            0x92 => Ok(F32Add),
            0x93 => Ok(F32Sub),
            0x94 => Ok(F32Mul),
            0x95 => Ok(F32Div),
            0x96 => Ok(F32Min),
            0x97 => Ok(F32Max),
            0x98 => Ok(F32Copysign),

            // f64 arithmetic
            0x99 => Ok(F64Abs),
            0x9A => Ok(F64Neg),
            0x9B => Ok(F64Ceil),
            0x9C => Ok(F64Floor),
            0x9D => Ok(F64Trunc),
            0x9E => Ok(F64Nearest),
            0x9F => Ok(F64Sqrt),
            0xA0 => Ok(F64Add),
            0xA1 => Ok(F64Sub),
            0xA2 => Ok(F64Mul),
            0xA3 => Ok(F64Div),
            0xA4 => Ok(F64Min),
            0xA5 => Ok(F64Max),
            0xA6 => Ok(F64Copysign),

            // Conversions
            0xA7 => Ok(I32WrapI64),
            0xA8 => Ok(I32TruncF32S),
            0xA9 => Ok(I32TruncF32U),
            0xAA => Ok(I32TruncF64S),
            0xAB => Ok(I32TruncF64U),
            0xAC => Ok(I64ExtendI32S),
            0xAD => Ok(I64ExtendI32U),
            0xAE => Ok(I64TruncF32S),
            0xAF => Ok(I64TruncF32U),
            0xB0 => Ok(I64TruncF64S),
            0xB1 => Ok(I64TruncF64U),
            0xB2 => Ok(F32ConvertI32S),
            0xB3 => Ok(F32ConvertI32U),
            0xB4 => Ok(F32ConvertI64S),
            0xB5 => Ok(F32ConvertI64U),
            0xB6 => Ok(F32DemoteF64),
            0xB7 => Ok(F64ConvertI32S),
            0xB8 => Ok(F64ConvertI32U),
            0xB9 => Ok(F64ConvertI64S),
            0xBA => Ok(F64ConvertI64U),
            0xBB => Ok(F64PromoteF32),
            0xBC => Ok(I32ReinterpretF32),
            0xBD => Ok(I64ReinterpretF64),
            0xBE => Ok(F32ReinterpretI32),
            0xBF => Ok(F64ReinterpretI64),

            // Sign extension
            0xC0 => Ok(I32Extend8S),
            0xC1 => Ok(I32Extend16S),
            0xC2 => Ok(I64Extend8S),
            0xC3 => Ok(I64Extend16S),
            0xC4 => Ok(I64Extend32S),

            // Multi-byte instructions with 0xFC/0xFD prefixes are handled specially in the
            // instruction iterator to properly capture all bytes including prefix and subopcode.
            // We shouldn't reach here in normal parsing, but include for completeness.
            0xFC => Err(DecodeError::InvalidValueType(
                "0xFC prefix requires special decoding".to_string(),
            )),
            0xFD => Err(DecodeError::InvalidValueType(
                "0xFD prefix requires special decoding".to_string(),
            )),

            _ => Err(DecodeError::UnknownOpcode(opcode)),
        }
    }
}

/// Decode 0xFC prefix instructions
fn decode_0xfc(subopcode: u32, reader: &mut Reader) -> Result<InstructionKind, DecodeError> {
    use InstructionKind::*;

    match subopcode {
        // Saturating truncation instructions
        0x00 => Ok(I32TruncSatF32S),
        0x01 => Ok(I32TruncSatF32U),
        0x02 => Ok(I32TruncSatF64S),
        0x03 => Ok(I32TruncSatF64U),
        0x04 => Ok(I64TruncSatF32S),
        0x05 => Ok(I64TruncSatF32U),
        0x06 => Ok(I64TruncSatF64S),
        0x07 => Ok(I64TruncSatF64U),

        // Memory instructions
        0x08 => {
            let data_idx = reader.read_vu32()?;
            let reserved = reader.read_byte()?;
            if reserved != 0x00 {
                return Err(DecodeError::InvalidValueType(format!(
                    "memory.init reserved byte must be 0x00, got 0x{reserved:02X}"
                )));
            }
            Ok(MemoryInit { data_idx })
        }
        0x09 => Ok(DataDrop {
            data_idx: reader.read_vu32()?,
        }),
        0x0A => {
            let reserved1 = reader.read_byte()?;
            let reserved2 = reader.read_byte()?;
            if reserved1 != 0x00 || reserved2 != 0x00 {
                return Err(DecodeError::InvalidValueType(format!(
                    "memory.copy reserved bytes must be 0x00, got 0x{reserved1:02X} 0x{reserved2:02X}"
                )));
            }
            Ok(MemoryCopy)
        }
        0x0B => {
            let reserved = reader.read_byte()?;
            if reserved != 0x00 {
                return Err(DecodeError::InvalidValueType(format!(
                    "memory.fill reserved byte must be 0x00, got 0x{reserved:02X}"
                )));
            }
            Ok(MemoryFill)
        }

        // Table instructions
        0x0C => Ok(TableInit {
            elem_idx: reader.read_vu32()?,
            table_idx: reader.read_vu32()?,
        }),
        0x0D => Ok(ElemDrop {
            elem_idx: reader.read_vu32()?,
        }),
        0x0E => Ok(TableCopy {
            dst_table: reader.read_vu32()?,
            src_table: reader.read_vu32()?,
        }),
        0x0F => Ok(TableGrow {
            table_idx: reader.read_vu32()?,
        }),
        0x10 => Ok(TableSize {
            table_idx: reader.read_vu32()?,
        }),
        0x11 => Ok(TableFill {
            table_idx: reader.read_vu32()?,
        }),

        _ => Err(DecodeError::UnknownInstruction(0xFC, subopcode)),
    }
}

/// Decode 0xFD prefix instructions (SIMD)
fn decode_0xfd(subopcode: u32, reader: &mut Reader) -> Result<InstructionKind, DecodeError> {
    use InstructionKind::*;

    match subopcode {
        0x00 => Ok(V128Load {
            memarg: MemArg::decode(reader)?,
        }),
        0x0B => Ok(V128Store {
            memarg: MemArg::decode(reader)?,
        }),
        0x0C => {
            // V128Const
            let mut value = [0u8; 16];
            for byte in &mut value {
                *byte = reader.read_byte()?;
            }
            Ok(V128Const { value })
        }
        // Additional SIMD instructions would go here
        _ => Err(DecodeError::UnknownInstruction(0xFD, subopcode)),
    }
}

/// Iterator for decoding instructions from a byte stream
pub struct InstructionIterator<'a> {
    reader: &'a mut Reader,
    parse_type: ParseType,
    ended: bool,
}

/// Parsing mode for instruction iterator
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ParseType {
    /// Read all instructions until end of stream
    ReadAll,
    /// Read instructions until an End instruction is encountered
    ReadTillEnd,
}

impl<'a> InstructionIterator<'a> {
    /// Create a new instruction iterator
    pub fn new(reader: &'a mut Reader, parse_type: ParseType) -> Self {
        InstructionIterator {
            reader,
            parse_type,
            ended: false,
        }
    }
}

impl<'a> Iterator for InstructionIterator<'a> {
    type Item = Result<Instruction, DecodeError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.ended {
            return None;
        }

        // Check if there are more bytes to read
        if !self.reader.has_at_least(1) {
            self.ended = true;
            return None;
        }

        let pos = self.reader.pos();

        // First read the opcode to check for prefix instructions
        let opcode = match self.reader.read_byte() {
            Ok(b) => b,
            Err(e) => {
                self.ended = true;
                return Some(Err(DecodeError::Io(e)));
            }
        };

        // Handle special prefix opcodes that need byte preservation
        let (kind, original_bytes) = match opcode {
            0xFC => {
                let decode_start_pos = pos; // Start from the 0xFC byte
                let subopcode = match self.reader.read_vu32() {
                    Ok(result) => result,
                    Err(e) => {
                        self.ended = true;
                        return Some(Err(DecodeError::Io(e)));
                    }
                };
                match decode_0xfc(subopcode, self.reader) {
                    Ok(kind) => {
                        let decode_end_pos = self.reader.pos();
                        let original = self.reader.bytes[decode_start_pos..decode_end_pos].to_vec();
                        (kind, original)
                    }
                    Err(e) => {
                        self.ended = true;
                        return Some(Err(e));
                    }
                }
            }
            0xFD => {
                let decode_start_pos = pos; // Start from the 0xFD byte
                let subopcode = match self.reader.read_vu32() {
                    Ok(result) => result,
                    Err(e) => {
                        self.ended = true;
                        return Some(Err(DecodeError::Io(e)));
                    }
                };
                match decode_0xfd(subopcode, self.reader) {
                    Ok(kind) => {
                        let decode_end_pos = self.reader.pos();
                        let original = self.reader.bytes[decode_start_pos..decode_end_pos].to_vec();
                        (kind, original)
                    }
                    Err(e) => {
                        self.ended = true;
                        return Some(Err(e));
                    }
                }
            }
            0x3F => {
                // memory.size with reserved byte
                let reserved = match self.reader.read_byte() {
                    Ok(b) => b,
                    Err(e) => {
                        self.ended = true;
                        return Some(Err(DecodeError::Io(e)));
                    }
                };
                if reserved != 0x00 {
                    self.ended = true;
                    return Some(Err(DecodeError::Io(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "zero byte expected",
                    ))));
                }
                let original = vec![0x3F, reserved];
                (InstructionKind::MemorySize, original)
            }
            0x40 => {
                // memory.grow with reserved byte
                let reserved = match self.reader.read_byte() {
                    Ok(b) => b,
                    Err(e) => {
                        self.ended = true;
                        return Some(Err(DecodeError::Io(e)));
                    }
                };
                if reserved != 0x00 {
                    self.ended = true;
                    return Some(Err(DecodeError::Io(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "zero byte expected",
                    ))));
                }
                let original = vec![0x40, reserved];
                (InstructionKind::MemoryGrow, original)
            }
            _ => {
                // Reset position and decode normally, capturing all bytes
                let reset_pos = pos;
                if self.reader.set_pos(reset_pos).is_err() {
                    self.ended = true;
                    return Some(Err(DecodeError::Io(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "failed to reset position",
                    ))));
                }
                let start_decode_pos = self.reader.pos();
                match InstructionKind::decode(self.reader) {
                    Ok(kind) => {
                        let end_decode_pos = self.reader.pos();
                        let original = self.reader.bytes[start_decode_pos..end_decode_pos].to_vec();
                        (kind, original)
                    }
                    Err(e) => {
                        self.ended = true;
                        return Some(Err(e));
                    }
                }
            }
        };

        let end_pos = self.reader.pos();
        let instruction = Instruction {
            kind: kind.clone(),
            position: ByteRange {
                offset: pos,
                length: end_pos - pos,
            },
            original_bytes,
        };

        // Check if we should stop parsing
        self.ended = match self.parse_type {
            ParseType::ReadAll => !self.reader.has_at_least(1),
            ParseType::ReadTillEnd => matches!(kind, InstructionKind::End),
        };

        Some(Ok(instruction))
    }
}
