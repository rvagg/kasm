//! WebAssembly call frame
//!
//! Represents the call frame for function execution, containing locals and control flow state

use super::{Value, control::Label};

/// Call frame for managing function calls
#[derive(Debug)]
pub struct CallFrame {
    /// Function index in the module
    pub function_idx: u32,
    /// Instruction pointer in the function body
    pub ip: usize,
    /// Local variables (parameters + declared locals)
    pub locals: Vec<Value>,
    /// Label stack for control flow within this function
    pub label_stack: Vec<Label>,
    /// Number of values this function should return
    pub return_arity: usize,
}
