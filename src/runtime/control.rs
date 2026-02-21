//! WebAssembly label stack for control flow
//!
//! This module implements the label stack as described in the WebAssembly specification.
//! See: <https://webassembly.github.io/spec/core/exec/runtime.html#labels>
//!
//! From the spec:
//! > "Labels carry an arity n and their associated branch target, which is expressed
//! > syntactically as an instruction sequence"
//!
//! The label stack tracks active control constructs (blocks, loops, ifs) and enables
//! structured control flow through branch instructions.

use crate::parser::instruction::BlockType;
use crate::parser::module::ValueType;

/// Type of label construct as per WebAssembly specification
///
/// See: <https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions>
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LabelType {
    /// Block label - branches target the end of the block
    /// Spec: "block blocktype instr* end"
    Block,
    /// Loop label - branches target the beginning of the loop
    /// Spec: "loop blocktype instr* end"
    /// Note: "the label of a loop does not target the end, but the beginning of the loop"
    Loop,
    /// If label - conditional execution
    /// Spec: "if blocktype instr* else instr* end"
    If,
}

/// A label on the label stack
///
/// From the WebAssembly spec (4.2.8 Labels):
/// > "Labels carry an arity n and their associated branch target"
///
/// Each label represents an active control construct and contains the information
/// needed for branch instructions to properly exit or continue execution.
#[derive(Debug, Clone)]
pub struct Label {
    /// Type of this label construct
    pub label_type: LabelType,
    /// Block type defining parameters and results
    /// See: <https://webassembly.github.io/spec/core/syntax/instructions.html#block-types>
    pub block_type: BlockType,
    /// Height of value stack when entering this label
    /// Used to restore stack state on branch
    pub stack_height: usize,
    /// Whether this label is unreachable (after unconditional branch/return)
    /// See: <https://webassembly.github.io/spec/core/valid/instructions.html#control-instructions>
    pub unreachable: bool,
    /// Resolved parameter types (consumed on block entry)
    pub param_types: Vec<ValueType>,
    /// Resolved return types (produced on block exit)
    pub return_types: Vec<ValueType>,
}

/// The label stack for managing nested control structures
///
/// From the WebAssembly spec (4.2.8 Labels):
/// > "The label stack is empty when execution starts and gets extended with new labels
/// > when entering structured control instructions"
///
/// This implements the label stack that tracks active control constructs and enables
/// proper branch target resolution.
#[derive(Debug)]
pub struct LabelStack {
    labels: Vec<Label>,
}

impl Default for LabelStack {
    fn default() -> Self {
        Self::new()
    }
}

impl LabelStack {
    /// Create a new empty label stack
    ///
    /// From the spec: "The label stack is empty when execution starts"
    pub fn new() -> Self {
        LabelStack { labels: Vec::new() }
    }

    /// Create a label stack from a vector of labels
    pub fn from_vec(labels: Vec<Label>) -> Self {
        LabelStack { labels }
    }

    /// Push a new label onto the stack
    ///
    /// From the spec: "gets extended with new labels when entering structured control instructions"
    pub fn push(&mut self, label: Label) {
        self.labels.push(label);
    }

    /// Pop a label from the stack
    ///
    /// Used when exiting control constructs or when branching
    pub fn pop(&mut self) -> Option<Label> {
        self.labels.pop()
    }

    /// Get the nth label from the top (0 = top)
    ///
    /// From the spec (4.4.8 br l):
    /// > "Let L be the l-th label appearing on the stack, starting from the top and counting from zero"
    pub fn get(&self, depth: u32) -> Option<&Label> {
        let len = self.labels.len();
        if depth as usize >= len {
            return None;
        }
        self.labels.get(len - 1 - depth as usize)
    }

    /// Get mutable reference to the nth label from the top
    pub fn get_mut(&mut self, depth: u32) -> Option<&mut Label> {
        let len = self.labels.len();
        if depth as usize >= len {
            return None;
        }
        self.labels.get_mut(len - 1 - depth as usize)
    }

    /// Check if label stack is empty
    pub fn is_empty(&self) -> bool {
        self.labels.is_empty()
    }

    /// Get the current depth (number of labels)
    pub fn depth(&self) -> usize {
        self.labels.len()
    }
}

impl Label {
    /// Get the arity (number of results) for this label
    ///
    /// From the spec (4.4.8 Control Instructions):
    /// > "Let n be the arity of L"
    ///
    /// The arity determines how many values are popped from the stack when branching to this label.
    /// For loops, this returns the number of parameters (values consumed on branch TO the loop).
    /// For blocks/ifs, this returns the number of return values.
    pub fn arity(&self) -> usize {
        if self.label_type == LabelType::Loop {
            self.param_types.len()
        } else {
            self.return_types.len()
        }
    }

    /// Get the values that should be kept when branching to this label
    ///
    /// For loops: the parameter types (branching jumps to the beginning)
    /// For blocks/ifs: the return types (branching jumps to the end)
    pub fn branch_types(&self) -> &[ValueType] {
        if self.label_type == LabelType::Loop {
            &self.param_types
        } else {
            &self.return_types
        }
    }

    /// Get the result types for this label
    ///
    /// Used for type checking and determining what values this control construct produces
    pub fn results(&self) -> &[ValueType] {
        &self.return_types
    }
}
