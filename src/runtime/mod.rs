//! WebAssembly runtime implementation
//!
//! This module provides the execution engine for WebAssembly modules,
//! including the stack machine, value representation, and instruction interpreter.

pub mod control;
pub mod executor;
pub mod frame;
pub mod instance;
pub mod stack;
pub mod value;

pub use instance::Instance;
pub use value::Value;

#[derive(Debug, thiserror::Error)]
pub enum RuntimeError {
    #[error("Stack underflow")]
    StackUnderflow,
    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },
    #[error("Unknown function: {0}")]
    UnknownFunction(String),
    #[error("Unknown export: {0}")]
    UnknownExport(String),
    #[error("Function index out of bounds: {0}")]
    FunctionIndexOutOfBounds(u32),
    #[error("Unimplemented instruction: {0}")]
    UnimplementedInstruction(String),
    #[error("Invalid function type")]
    InvalidFunctionType,
    #[error("Local variable index out of bounds: {0}")]
    LocalIndexOutOfBounds(u32),
}
