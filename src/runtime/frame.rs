//! WebAssembly call frame
//!
//! Represents the call frame for function execution, containing locals and control flow state.

use super::{Value, control::Label};

/// Call frame for managing function calls
#[derive(Debug)]
pub struct CallFrame {
    /// Local variables (parameters + declared locals)
    pub locals: Vec<Value>,
    /// Label stack for control flow within this function
    pub label_stack: Vec<Label>,
}
