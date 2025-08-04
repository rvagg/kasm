//! WebAssembly execution frame
//!
//! Represents the activation frame for a function call, containing local variables

use super::Value;

/// Execution frame for a function
#[derive(Debug)]
pub struct Frame {
    /// Local variables (includes function parameters)
    pub locals: Vec<Value>,
}

impl Frame {
    /// Create a new frame with the given locals
    pub fn new(locals: Vec<Value>) -> Self {
        Frame { locals }
    }
}
