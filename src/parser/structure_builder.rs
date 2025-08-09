//! Structure builder that transforms flat instruction sequences into structured representation
//!
//! This module provides two ways to build structured representations:
//! 1. Streaming: Process instructions one at a time as they're decoded
//! 2. Batch: Process a complete vector of instructions at once
//!
//! ## Why Structured Representation?
//!
//! WebAssembly's control flow instructions (block, loop, if/else, br) form a tree structure,
//! but are encoded as a flat sequence in the binary format. Building a structured representation
//! makes execution more efficient by pre-computing branch targets and control flow relationships.
//!
//! ## Implementation
//!
//! The builder maintains a stack of block contexts during processing. As control flow
//! instructions are encountered, contexts are pushed and popped from this stack, building
//! the tree structure incrementally. This approach works whether processing instructions
//! one at a time (streaming) or all at once (batch).

use super::instruction::{BlockType, Instruction, InstructionKind};
use super::module::ValueType;
use super::structured::{StructuredFunction, StructuredInstruction};
use std::io;

/// Context for a block being built
#[allow(dead_code)]
enum BlockContext {
    /// Building a regular block
    Block {
        block_type: BlockType,
        instructions: Vec<StructuredInstruction>,
        position: super::instruction::ByteRange,
        original_bytes: Vec<u8>,
    },
    /// Building a loop
    Loop {
        block_type: BlockType,
        instructions: Vec<StructuredInstruction>,
        position: super::instruction::ByteRange,
        original_bytes: Vec<u8>,
    },
    /// Building the then branch of an if
    IfThen {
        block_type: BlockType,
        then_instructions: Vec<StructuredInstruction>,
        position: super::instruction::ByteRange,
        original_bytes: Vec<u8>,
    },
    /// Building the else branch of an if
    IfElse {
        block_type: BlockType,
        then_instructions: Vec<StructuredInstruction>,
        else_instructions: Vec<StructuredInstruction>,
        position: super::instruction::ByteRange,
        original_bytes: Vec<u8>,
        else_position: super::instruction::ByteRange,
        else_bytes: Vec<u8>,
    },
    /// Top-level function body
    Function { instructions: Vec<StructuredInstruction> },
}

/// Structure builder that transforms flat instructions into tree representation
#[allow(dead_code)]
pub struct StructureBuilder {
    /// Stack of blocks being built
    block_stack: Vec<BlockContext>,
    /// Local count for the function
    local_count: usize,
    /// Return types for the function
    return_types: Vec<ValueType>,
}

impl StructureBuilder {
    /// Create a new streaming structure builder
    pub fn new(local_count: usize, return_types: Vec<ValueType>) -> Self {
        StructureBuilder {
            block_stack: vec![BlockContext::Function {
                instructions: Vec::new(),
            }],
            local_count,
            return_types,
        }
    }

    /// Build a structured function from a complete instruction vector
    ///
    /// Processes all instructions at once to build the structured representation.
    #[allow(dead_code)]
    pub fn build_function(
        instructions: &[Instruction],
        local_count: usize,
        return_types: Vec<ValueType>,
    ) -> Result<StructuredFunction, io::Error> {
        let mut builder = Self::new(local_count, return_types);

        for instruction in instructions {
            builder.process_instruction(instruction)?;
        }

        builder.finalise()
    }

    /// Process a single instruction
    pub fn process_instruction(&mut self, instruction: &Instruction) -> Result<(), io::Error> {
        match &instruction.kind {
            InstructionKind::Block { block_type } => {
                // Start a new block context
                self.block_stack.push(BlockContext::Block {
                    block_type: *block_type,
                    instructions: Vec::new(),
                    position: instruction.position,
                    original_bytes: instruction.original_bytes.clone(),
                });
            }

            InstructionKind::Loop { block_type } => {
                // Start a new loop context
                self.block_stack.push(BlockContext::Loop {
                    block_type: *block_type,
                    instructions: Vec::new(),
                    position: instruction.position,
                    original_bytes: instruction.original_bytes.clone(),
                });
            }

            InstructionKind::If { block_type } => {
                // Start a new if context (then branch)
                self.block_stack.push(BlockContext::IfThen {
                    block_type: *block_type,
                    then_instructions: Vec::new(),
                    position: instruction.position,
                    original_bytes: instruction.original_bytes.clone(),
                });
            }

            InstructionKind::Else => {
                // Transform IfThen to IfElse
                match self.block_stack.pop() {
                    Some(BlockContext::IfThen {
                        block_type,
                        then_instructions,
                        position,
                        original_bytes,
                    }) => {
                        self.block_stack.push(BlockContext::IfElse {
                            block_type,
                            then_instructions,
                            else_instructions: Vec::new(),
                            position,
                            original_bytes,
                            else_position: instruction.position,
                            else_bytes: instruction.original_bytes.clone(),
                        });
                    }
                    _ => {
                        return Err(io::Error::new(io::ErrorKind::InvalidData, "unexpected else without if"));
                    }
                }
            }

            InstructionKind::End => {
                // Complete the current block and add it to parent
                let completed = match self.block_stack.pop() {
                    Some(BlockContext::Block {
                        block_type,
                        instructions,
                        position,
                        original_bytes,
                    }) => StructuredInstruction::Block {
                        block_type,
                        body: instructions,
                        position,
                        original_bytes,
                        end_position: instruction.position,
                        end_bytes: instruction.original_bytes.clone(),
                    },

                    Some(BlockContext::Loop {
                        block_type,
                        instructions,
                        position,
                        original_bytes,
                    }) => StructuredInstruction::Loop {
                        block_type,
                        body: instructions,
                        position,
                        original_bytes,
                        end_position: instruction.position,
                        end_bytes: instruction.original_bytes.clone(),
                    },

                    Some(BlockContext::IfThen {
                        block_type,
                        then_instructions,
                        position,
                        original_bytes,
                    }) => StructuredInstruction::If {
                        block_type,
                        then_branch: then_instructions,
                        else_branch: None,
                        position,
                        original_bytes,
                        else_position: None,
                        else_bytes: None,
                        end_position: instruction.position,
                        end_bytes: instruction.original_bytes.clone(),
                    },

                    Some(BlockContext::IfElse {
                        block_type,
                        then_instructions,
                        else_instructions,
                        position,
                        original_bytes,
                        else_position,
                        else_bytes,
                    }) => StructuredInstruction::If {
                        block_type,
                        then_branch: then_instructions,
                        else_branch: Some(else_instructions),
                        position,
                        original_bytes,
                        else_position: Some(else_position),
                        else_bytes: Some(else_bytes),
                        end_position: instruction.position,
                        end_bytes: instruction.original_bytes.clone(),
                    },

                    Some(BlockContext::Function { instructions }) => {
                        // End of function - push back and return
                        self.block_stack.push(BlockContext::Function { instructions });
                        return Ok(());
                    }

                    None => {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidData,
                            "unexpected end without block",
                        ));
                    }
                };

                // Add completed block to parent
                self.add_to_current(completed)?;
            }

            _ => {
                // Regular instruction - add to current context
                self.add_to_current(StructuredInstruction::Plain(instruction.clone()))?;
            }
        }

        Ok(())
    }

    /// Add an instruction to the current context
    fn add_to_current(&mut self, instruction: StructuredInstruction) -> Result<(), io::Error> {
        match self.block_stack.last_mut() {
            Some(BlockContext::Block { instructions, .. })
            | Some(BlockContext::Loop { instructions, .. })
            | Some(BlockContext::Function { instructions }) => {
                instructions.push(instruction);
            }

            Some(BlockContext::IfThen { then_instructions, .. }) => {
                then_instructions.push(instruction);
            }

            Some(BlockContext::IfElse { else_instructions, .. }) => {
                else_instructions.push(instruction);
            }

            None => {
                return Err(io::Error::new(io::ErrorKind::InvalidData, "no block context available"));
            }
        }
        Ok(())
    }

    /// Finalise and return the structured function
    pub fn finalise(mut self) -> Result<StructuredFunction, io::Error> {
        // Should only have the function context left
        match self.block_stack.pop() {
            Some(BlockContext::Function { instructions }) if self.block_stack.is_empty() => Ok(StructuredFunction {
                body: instructions,
                local_count: self.local_count,
                return_types: self.return_types,
            }),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "unclosed blocks at end of function",
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_instruction(kind: InstructionKind) -> Instruction {
        Instruction {
            kind,
            position: super::super::instruction::ByteRange { offset: 0, length: 1 },
            original_bytes: vec![],
        }
    }

    #[test]
    fn test_simple_block() {
        let mut builder = StructureBuilder::new(0, vec![]);

        builder
            .process_instruction(&make_instruction(InstructionKind::Block {
                block_type: BlockType::Empty,
            }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::I32Const { value: 42 }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::End))
            .unwrap();

        let func = builder.finalise().unwrap();
        assert_eq!(func.body.len(), 1);
        match &func.body[0] {
            StructuredInstruction::Block { body, .. } => {
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected block"),
        }
    }

    #[test]
    fn test_if_else() {
        let mut builder = StructureBuilder::new(0, vec![]);

        builder
            .process_instruction(&make_instruction(InstructionKind::If {
                block_type: BlockType::Empty,
            }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::I32Const { value: 1 }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::Else))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::I32Const { value: 2 }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::End))
            .unwrap();

        let func = builder.finalise().unwrap();
        assert_eq!(func.body.len(), 1);
        match &func.body[0] {
            StructuredInstruction::If {
                then_branch,
                else_branch,
                ..
            } => {
                assert_eq!(then_branch.len(), 1);
                assert!(else_branch.is_some());
                assert_eq!(else_branch.as_ref().unwrap().len(), 1);
            }
            _ => panic!("Expected if"),
        }
    }

    #[test]
    fn test_loop() {
        let mut builder = StructureBuilder::new(0, vec![]);

        builder
            .process_instruction(&make_instruction(InstructionKind::Loop {
                block_type: BlockType::Empty,
            }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::I32Const { value: 1 }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::Br { label_idx: 0 }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::End))
            .unwrap();

        let func = builder.finalise().unwrap();
        assert_eq!(func.body.len(), 1);
        match &func.body[0] {
            StructuredInstruction::Loop { body, .. } => {
                assert_eq!(body.len(), 2); // const and br
            }
            _ => panic!("Expected loop"),
        }
    }

    #[test]
    fn test_nested_blocks() {
        let mut builder = StructureBuilder::new(0, vec![]);

        // Outer block
        builder
            .process_instruction(&make_instruction(InstructionKind::Block {
                block_type: BlockType::Empty,
            }))
            .unwrap();

        // Inner block
        builder
            .process_instruction(&make_instruction(InstructionKind::Block {
                block_type: BlockType::Value(ValueType::I32),
            }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::I32Const { value: 42 }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::End))
            .unwrap();

        // Back in outer block
        builder
            .process_instruction(&make_instruction(InstructionKind::Drop))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::End))
            .unwrap();

        let func = builder.finalise().unwrap();
        assert_eq!(func.body.len(), 1);
        match &func.body[0] {
            StructuredInstruction::Block { body, .. } => {
                assert_eq!(body.len(), 2); // inner block and drop
                match &body[0] {
                    StructuredInstruction::Block {
                        block_type,
                        body: inner_body,
                        ..
                    } => {
                        assert_eq!(*block_type, BlockType::Value(ValueType::I32));
                        assert_eq!(inner_body.len(), 1);
                    }
                    _ => panic!("Expected nested block"),
                }
            }
            _ => panic!("Expected outer block"),
        }
    }

    #[test]
    fn test_if_without_else() {
        let mut builder = StructureBuilder::new(0, vec![]);

        builder
            .process_instruction(&make_instruction(InstructionKind::If {
                block_type: BlockType::Empty,
            }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::I32Const { value: 1 }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::End))
            .unwrap();

        let func = builder.finalise().unwrap();
        assert_eq!(func.body.len(), 1);
        match &func.body[0] {
            StructuredInstruction::If {
                then_branch,
                else_branch,
                ..
            } => {
                assert_eq!(then_branch.len(), 1);
                assert!(else_branch.is_none());
            }
            _ => panic!("Expected if"),
        }
    }

    #[test]
    fn test_deeply_nested_structures() {
        let mut builder = StructureBuilder::new(0, vec![]);

        // Block containing loop containing if
        builder
            .process_instruction(&make_instruction(InstructionKind::Block {
                block_type: BlockType::Empty,
            }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::Loop {
                block_type: BlockType::Empty,
            }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::If {
                block_type: BlockType::Empty,
            }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::Nop))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::End))
            .unwrap(); // end if
        builder
            .process_instruction(&make_instruction(InstructionKind::End))
            .unwrap(); // end loop
        builder
            .process_instruction(&make_instruction(InstructionKind::End))
            .unwrap(); // end block

        let func = builder.finalise().unwrap();
        assert_eq!(func.body.len(), 1);

        // Verify structure
        match &func.body[0] {
            StructuredInstruction::Block { body, .. } => {
                assert_eq!(body.len(), 1);
                match &body[0] {
                    StructuredInstruction::Loop { body: loop_body, .. } => {
                        assert_eq!(loop_body.len(), 1);
                        match &loop_body[0] {
                            StructuredInstruction::If { then_branch, .. } => {
                                assert_eq!(then_branch.len(), 1);
                            }
                            _ => panic!("Expected if inside loop"),
                        }
                    }
                    _ => panic!("Expected loop inside block"),
                }
            }
            _ => panic!("Expected block"),
        }
    }

    #[test]
    fn test_empty_function() {
        let builder = StructureBuilder::new(0, vec![]);
        // No instructions at all
        let func = builder.finalise().unwrap();
        assert_eq!(func.body.len(), 0);
        assert_eq!(func.local_count, 0);
        assert_eq!(func.return_types.len(), 0);
    }

    #[test]
    fn test_function_with_locals_and_returns() {
        let return_types = vec![ValueType::I32, ValueType::I64];
        let mut builder = StructureBuilder::new(3, return_types.clone());

        builder
            .process_instruction(&make_instruction(InstructionKind::LocalGet { local_idx: 0 }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::LocalGet { local_idx: 1 }))
            .unwrap();

        let func = builder.finalise().unwrap();
        assert_eq!(func.body.len(), 2);
        assert_eq!(func.local_count, 3);
        assert_eq!(func.return_types, return_types);
    }

    #[test]
    fn test_multiple_instructions_in_block() {
        let mut builder = StructureBuilder::new(0, vec![]);

        builder
            .process_instruction(&make_instruction(InstructionKind::Block {
                block_type: BlockType::Empty,
            }))
            .unwrap();

        // Add multiple instructions
        for i in 0..5 {
            builder
                .process_instruction(&make_instruction(InstructionKind::I32Const { value: i }))
                .unwrap();
            builder
                .process_instruction(&make_instruction(InstructionKind::Drop))
                .unwrap();
        }

        builder
            .process_instruction(&make_instruction(InstructionKind::End))
            .unwrap();

        let func = builder.finalise().unwrap();
        match &func.body[0] {
            StructuredInstruction::Block { body, .. } => {
                assert_eq!(body.len(), 10); // 5 pairs of const+drop
            }
            _ => panic!("Expected block"),
        }
    }

    #[test]
    fn test_error_else_without_if() {
        let mut builder = StructureBuilder::new(0, vec![]);

        let result = builder.process_instruction(&make_instruction(InstructionKind::Else));
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("unexpected else"));
    }

    #[test]
    fn test_error_end_without_block() {
        let mut builder = StructureBuilder::new(0, vec![]);

        builder
            .process_instruction(&make_instruction(InstructionKind::I32Const { value: 42 }))
            .unwrap();

        // This should complete the function, not error
        let result = builder.process_instruction(&make_instruction(InstructionKind::End));
        assert!(result.is_ok());

        // The function is now complete, we can finalise it
        let func = builder.finalise().unwrap();
        assert_eq!(func.body.len(), 1);
    }

    #[test]
    fn test_error_unexpected_end() {
        let mut builder = StructureBuilder::new(0, vec![]);

        // End without any context should error
        builder.block_stack.clear(); // Remove function context
        let result = builder.process_instruction(&make_instruction(InstructionKind::End));
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("unexpected end"));
    }

    #[test]
    fn test_error_unclosed_block() {
        let mut builder = StructureBuilder::new(0, vec![]);

        builder
            .process_instruction(&make_instruction(InstructionKind::Block {
                block_type: BlockType::Empty,
            }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::I32Const { value: 42 }))
            .unwrap();
        // Missing End

        let result = builder.finalise();
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("unclosed blocks"));
    }

    #[test]
    fn test_error_unclosed_if() {
        let mut builder = StructureBuilder::new(0, vec![]);

        builder
            .process_instruction(&make_instruction(InstructionKind::If {
                block_type: BlockType::Empty,
            }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::Nop))
            .unwrap();
        // Missing End

        let result = builder.finalise();
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("unclosed blocks"));
    }

    #[test]
    fn test_complex_control_flow() {
        let mut builder = StructureBuilder::new(0, vec![]);

        // if (cond) { loop { if (cond2) { break } } } else { nop }
        builder
            .process_instruction(&make_instruction(InstructionKind::If {
                block_type: BlockType::Empty,
            }))
            .unwrap();

        // Then branch with loop
        builder
            .process_instruction(&make_instruction(InstructionKind::Loop {
                block_type: BlockType::Empty,
            }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::If {
                block_type: BlockType::Empty,
            }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::Br { label_idx: 1 }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::End))
            .unwrap(); // end inner if
        builder
            .process_instruction(&make_instruction(InstructionKind::Br { label_idx: 0 }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::End))
            .unwrap(); // end loop

        // Else branch
        builder
            .process_instruction(&make_instruction(InstructionKind::Else))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::Nop))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::End))
            .unwrap(); // end if

        let func = builder.finalise().unwrap();
        assert_eq!(func.body.len(), 1);

        match &func.body[0] {
            StructuredInstruction::If {
                then_branch,
                else_branch,
                ..
            } => {
                assert_eq!(then_branch.len(), 1); // loop
                assert!(else_branch.is_some());
                assert_eq!(else_branch.as_ref().unwrap().len(), 1); // nop

                // Check loop structure
                match &then_branch[0] {
                    StructuredInstruction::Loop { body, .. } => {
                        assert_eq!(body.len(), 2); // if and br
                    }
                    _ => panic!("Expected loop in then branch"),
                }
            }
            _ => panic!("Expected if"),
        }
    }

    #[test]
    fn test_block_with_functype() {
        let mut builder = StructureBuilder::new(0, vec![]);

        builder
            .process_instruction(&make_instruction(InstructionKind::Block {
                block_type: BlockType::FuncType(0),
            }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::I32Const { value: 1 }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::I64Const { value: 2 }))
            .unwrap();
        builder
            .process_instruction(&make_instruction(InstructionKind::End))
            .unwrap();

        let func = builder.finalise().unwrap();
        match &func.body[0] {
            StructuredInstruction::Block { block_type, body, .. } => {
                assert_eq!(*block_type, BlockType::FuncType(0));
                assert_eq!(body.len(), 2);
            }
            _ => panic!("Expected block"),
        }
    }
}
