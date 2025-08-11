//! Structured representation of WebAssembly control flow
//!
//! This module provides a tree-based representation of WebAssembly instructions
//! that makes control flow explicit, eliminating the need for runtime scanning
//! of branch targets.

use super::instruction::{BlockType, Instruction, InstructionKind};
use super::module::ValueType;
use std::fmt;

/// Structured representation of a WebAssembly function body
#[derive(Debug, Clone)]
pub struct StructuredFunction {
    /// The structured instruction tree
    pub body: Vec<StructuredInstruction>,
    /// Local variables (including parameters) - stored as reference for now
    pub local_count: usize,
    /// Expected return types
    pub return_types: Vec<ValueType>,
    /// The final end instruction that terminates the function (if present)
    pub end_instruction: Option<Instruction>,
}

impl StructuredFunction {
    /// Flatten the structured representation back into a linear sequence of instructions.
    /// This reconstructs the original instruction sequence from the tree structure,
    /// preserving all position and byte information.
    pub fn flatten(&self) -> Vec<Instruction> {
        let mut result = Vec::new();
        for inst in &self.body {
            flatten_instruction(inst, &mut result);
        }
        // Add the final end instruction if present
        if let Some(ref end_inst) = self.end_instruction {
            result.push(end_inst.clone());
        }
        result
    }
}

/// Helper function to recursively flatten a structured instruction
fn flatten_instruction(inst: &StructuredInstruction, output: &mut Vec<Instruction>) {
    match inst {
        StructuredInstruction::Plain(i) => {
            output.push(i.clone());
        }
        StructuredInstruction::Block {
            block_type,
            body,
            position,
            original_bytes,
            end_position,
            end_bytes,
        } => {
            // Reconstruct the block instruction
            output.push(Instruction {
                kind: InstructionKind::Block {
                    block_type: *block_type,
                },
                position: *position,
                original_bytes: original_bytes.clone(),
            });

            // Flatten the body
            for child in body {
                flatten_instruction(child, output);
            }

            // Add the end instruction
            output.push(Instruction {
                kind: InstructionKind::End,
                position: *end_position,
                original_bytes: end_bytes.clone(),
            });
        }
        StructuredInstruction::Loop {
            block_type,
            body,
            position,
            original_bytes,
            end_position,
            end_bytes,
        } => {
            // Reconstruct the loop instruction
            output.push(Instruction {
                kind: InstructionKind::Loop {
                    block_type: *block_type,
                },
                position: *position,
                original_bytes: original_bytes.clone(),
            });

            // Flatten the body
            for child in body {
                flatten_instruction(child, output);
            }

            // Add the end instruction
            output.push(Instruction {
                kind: InstructionKind::End,
                position: *end_position,
                original_bytes: end_bytes.clone(),
            });
        }
        StructuredInstruction::If {
            block_type,
            then_branch,
            else_branch,
            position,
            original_bytes,
            else_position,
            else_bytes,
            end_position,
            end_bytes,
        } => {
            // Reconstruct the if instruction
            output.push(Instruction {
                kind: InstructionKind::If {
                    block_type: *block_type,
                },
                position: *position,
                original_bytes: original_bytes.clone(),
            });

            // Flatten the then branch
            for child in then_branch {
                flatten_instruction(child, output);
            }

            // Add else if present
            if let Some(else_body) = else_branch {
                output.push(Instruction {
                    kind: InstructionKind::Else,
                    position: else_position.unwrap_or(super::instruction::ByteRange { offset: 0, length: 0 }),
                    original_bytes: else_bytes.clone().unwrap_or_default(),
                });

                // Flatten the else branch
                for child in else_body {
                    flatten_instruction(child, output);
                }
            }

            // Add the end instruction
            output.push(Instruction {
                kind: InstructionKind::End,
                position: *end_position,
                original_bytes: end_bytes.clone(),
            });
        }
    }
}

/// A structured instruction that makes control flow explicit
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum StructuredInstruction {
    /// Regular instruction (no control flow)
    Plain(Instruction),

    /// Block with pre-extracted body
    /// Branches to blocks go to the end
    Block {
        /// Type signature of the block
        block_type: BlockType,
        /// Instructions inside the block
        body: Vec<StructuredInstruction>,
        /// Position and bytes of the original block instruction
        position: super::instruction::ByteRange,
        original_bytes: Vec<u8>,
        /// Position and bytes of the matching end instruction
        end_position: super::instruction::ByteRange,
        end_bytes: Vec<u8>,
    },

    /// Loop with pre-extracted body
    /// Branches to loops go to the beginning
    Loop {
        /// Type signature of the loop
        block_type: BlockType,
        /// Instructions inside the loop
        body: Vec<StructuredInstruction>,
        /// Position and bytes of the original loop instruction
        position: super::instruction::ByteRange,
        original_bytes: Vec<u8>,
        /// Position and bytes of the matching end instruction
        end_position: super::instruction::ByteRange,
        end_bytes: Vec<u8>,
    },

    /// If with pre-extracted branches
    If {
        /// Type signature of the if
        block_type: BlockType,
        /// Instructions in the then branch
        then_branch: Vec<StructuredInstruction>,
        /// Optional else branch
        else_branch: Option<Vec<StructuredInstruction>>,
        /// Position and bytes of the original if instruction
        position: super::instruction::ByteRange,
        original_bytes: Vec<u8>,
        /// Optional position and bytes of else instruction
        else_position: Option<super::instruction::ByteRange>,
        else_bytes: Option<Vec<u8>>,
        /// Position and bytes of the matching end instruction
        end_position: super::instruction::ByteRange,
        end_bytes: Vec<u8>,
    },
}

impl fmt::Display for StructuredFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "StructuredFunction {{")?;
        writeln!(f, "  locals: {} total", self.local_count)?;
        writeln!(f, "  returns: {:?}", self.return_types)?;
        writeln!(f, "  body:")?;
        for inst in &self.body {
            write!(f, "{}", format_instruction(inst, 2))?;
        }
        writeln!(f, "}}")
    }
}

impl fmt::Display for StructuredInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format_instruction(self, 0))
    }
}

/// Helper function to format instructions with indentation
fn format_instruction(inst: &StructuredInstruction, indent: usize) -> String {
    let prefix = "  ".repeat(indent);
    match inst {
        StructuredInstruction::Plain(i) => {
            format!("{prefix}  {i}\n")
        }
        StructuredInstruction::Block { block_type, body, .. } => {
            let mut s = format!("{prefix}  block {block_type:?}\n");
            for child in body {
                s.push_str(&format_instruction(child, indent + 1));
            }
            s.push_str(&format!("{prefix}  end\n"));
            s
        }
        StructuredInstruction::Loop { block_type, body, .. } => {
            let mut s = format!("{prefix}  loop {block_type:?}\n");
            for child in body {
                s.push_str(&format_instruction(child, indent + 1));
            }
            s.push_str(&format!("{prefix}  end\n"));
            s
        }
        StructuredInstruction::If {
            block_type,
            then_branch,
            else_branch,
            ..
        } => {
            let mut s = format!("{prefix}  if {block_type:?}\n");
            for child in then_branch {
                s.push_str(&format_instruction(child, indent + 1));
            }
            if let Some(else_body) = else_branch {
                s.push_str(&format!("{prefix}  else\n"));
                for child in else_body {
                    s.push_str(&format_instruction(child, indent + 1));
                }
            }
            s.push_str(&format!("{prefix}  end\n"));
            s
        }
    }
}

/// Result of executing a block
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum BlockEnd {
    /// Normal completion - continue to next instruction
    Normal,
    /// Branch to label at given depth
    Branch(u32),
    /// Return from function
    Return,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::instruction::{ByteRange, InstructionKind};

    fn make_instruction(kind: InstructionKind) -> Instruction {
        Instruction {
            kind,
            position: ByteRange { offset: 0, length: 1 },
            original_bytes: vec![],
        }
    }

    #[test]
    fn test_display_plain() {
        let inst = StructuredInstruction::Plain(make_instruction(InstructionKind::Nop));
        let formatted = format!("{}", inst);
        assert!(formatted.contains("nop"));
    }

    #[test]
    fn test_display_block() {
        let inst = StructuredInstruction::Block {
            block_type: BlockType::Empty,
            body: vec![StructuredInstruction::Plain(make_instruction(
                InstructionKind::I32Const { value: 42 },
            ))],
            position: ByteRange { offset: 0, length: 1 },
            original_bytes: vec![],
            end_position: ByteRange { offset: 1, length: 1 },
            end_bytes: vec![],
        };
        let formatted = format!("{}", inst);
        assert!(formatted.contains("block"));
        assert!(formatted.contains("i32.const 42"));
        assert!(formatted.contains("end"));
    }

    #[test]
    fn test_display_nested() {
        let inst = StructuredInstruction::Block {
            block_type: BlockType::Empty,
            body: vec![StructuredInstruction::Loop {
                block_type: BlockType::Empty,
                body: vec![StructuredInstruction::Plain(make_instruction(InstructionKind::Nop))],
                position: ByteRange { offset: 1, length: 1 },
                original_bytes: vec![],
                end_position: ByteRange { offset: 2, length: 1 },
                end_bytes: vec![],
            }],
            position: ByteRange { offset: 0, length: 1 },
            original_bytes: vec![],
            end_position: ByteRange { offset: 3, length: 1 },
            end_bytes: vec![],
        };
        let formatted = format!("{}", inst);
        assert!(formatted.contains("block"));
        assert!(formatted.contains("loop"));
        assert!(formatted.contains("nop"));
    }

    #[test]
    fn test_flatten_simple() {
        let func = StructuredFunction {
            body: vec![
                StructuredInstruction::Plain(make_instruction(InstructionKind::I32Const { value: 42 })),
                StructuredInstruction::Plain(make_instruction(InstructionKind::Drop)),
            ],
            local_count: 0,
            return_types: vec![],
            end_instruction: Some(make_instruction(InstructionKind::End)),
        };

        let flattened = func.flatten();
        assert_eq!(flattened.len(), 3); // Including the end instruction
        assert!(matches!(flattened[0].kind, InstructionKind::I32Const { value: 42 }));
        assert!(matches!(flattened[1].kind, InstructionKind::Drop));
        assert!(matches!(flattened[2].kind, InstructionKind::End));
    }

    #[test]
    fn test_flatten_block() {
        let func = StructuredFunction {
            body: vec![StructuredInstruction::Block {
                block_type: BlockType::Empty,
                body: vec![StructuredInstruction::Plain(make_instruction(
                    InstructionKind::I32Const { value: 1 },
                ))],
                position: ByteRange { offset: 0, length: 1 },
                original_bytes: vec![0x02],
                end_position: ByteRange { offset: 2, length: 1 },
                end_bytes: vec![0x0b],
            }],
            local_count: 0,
            return_types: vec![],
            end_instruction: Some(make_instruction(InstructionKind::End)),
        };

        let flattened = func.flatten();
        assert_eq!(flattened.len(), 4); // block, i32.const, end, function end
        assert!(matches!(flattened[0].kind, InstructionKind::Block { .. }));
        assert!(matches!(flattened[1].kind, InstructionKind::I32Const { value: 1 }));
        assert!(matches!(flattened[2].kind, InstructionKind::End));
        assert!(matches!(flattened[3].kind, InstructionKind::End));
        assert_eq!(flattened[0].original_bytes, vec![0x02]);
        assert_eq!(flattened[2].original_bytes, vec![0x0b]);
    }

    #[test]
    fn test_flatten_if_else() {
        let func = StructuredFunction {
            body: vec![StructuredInstruction::If {
                block_type: BlockType::Empty,
                then_branch: vec![StructuredInstruction::Plain(make_instruction(
                    InstructionKind::I32Const { value: 1 },
                ))],
                else_branch: Some(vec![StructuredInstruction::Plain(make_instruction(
                    InstructionKind::I32Const { value: 2 },
                ))]),
                position: ByteRange { offset: 0, length: 1 },
                original_bytes: vec![0x04],
                else_position: Some(ByteRange { offset: 2, length: 1 }),
                else_bytes: Some(vec![0x05]),
                end_position: ByteRange { offset: 4, length: 1 },
                end_bytes: vec![0x0b],
            }],
            local_count: 0,
            return_types: vec![],
            end_instruction: Some(make_instruction(InstructionKind::End)),
        };

        let flattened = func.flatten();
        assert_eq!(flattened.len(), 6); // if, i32.const, else, i32.const, end, function end
        assert!(matches!(flattened[0].kind, InstructionKind::If { .. }));
        assert!(matches!(flattened[1].kind, InstructionKind::I32Const { value: 1 }));
        assert!(matches!(flattened[2].kind, InstructionKind::Else));
        assert!(matches!(flattened[3].kind, InstructionKind::I32Const { value: 2 }));
        assert!(matches!(flattened[4].kind, InstructionKind::End));
        assert!(matches!(flattened[5].kind, InstructionKind::End));
    }

    #[test]
    fn test_flatten_nested() {
        let func = StructuredFunction {
            body: vec![StructuredInstruction::Block {
                block_type: BlockType::Empty,
                body: vec![StructuredInstruction::Loop {
                    block_type: BlockType::Empty,
                    body: vec![StructuredInstruction::Plain(make_instruction(InstructionKind::Nop))],
                    position: ByteRange { offset: 1, length: 1 },
                    original_bytes: vec![],
                    end_position: ByteRange { offset: 3, length: 1 },
                    end_bytes: vec![],
                }],
                position: ByteRange { offset: 0, length: 1 },
                original_bytes: vec![],
                end_position: ByteRange { offset: 4, length: 1 },
                end_bytes: vec![],
            }],
            local_count: 0,
            return_types: vec![],
            end_instruction: Some(make_instruction(InstructionKind::End)),
        };

        let flattened = func.flatten();
        assert_eq!(flattened.len(), 6); // block, loop, nop, end, end, function end
        assert!(matches!(flattened[0].kind, InstructionKind::Block { .. }));
        assert!(matches!(flattened[1].kind, InstructionKind::Loop { .. }));
        assert!(matches!(flattened[2].kind, InstructionKind::Nop));
        assert!(matches!(flattened[3].kind, InstructionKind::End));
        assert!(matches!(flattened[4].kind, InstructionKind::End));
        assert!(matches!(flattened[5].kind, InstructionKind::End));
    }
}
