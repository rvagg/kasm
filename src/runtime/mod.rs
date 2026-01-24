//! WebAssembly runtime implementation
//!
//! This module provides the execution engine for WebAssembly modules,
//! including the stack machine, value representation, and instruction interpreter.

pub mod control;
pub mod executor;
pub mod frame;
pub mod implemented;
pub mod imports;
pub mod instance;
pub mod memory;
pub mod ops;
pub mod stack;
pub mod store;
pub mod table;
pub mod test_utils;
pub mod value;
pub mod wasi;

pub use imports::ImportObject;
pub use instance::Instance;
pub use memory::Memory;
pub use store::{FuncAddr, FunctionInstance, MemoryAddr, SharedMemory, SharedTable, Store, TableAddr};
pub use table::Table;
pub use value::Value;

use crate::parser::module::{FunctionType, ValueType};

/// Outcome of executing a function - either complete or needs external call
#[derive(Debug)]
pub enum ExecutionOutcome {
    /// Execution completed with these return values
    Complete(Vec<Value>),
    /// Execution paused, needs external function call before resuming
    NeedsExternalCall(ExternalCallRequest),
}

/// Request for an external function call (cross-module)
#[derive(Debug)]
pub struct ExternalCallRequest {
    /// The function address to call
    pub func_addr: FuncAddr,
    /// Arguments to pass to the function
    pub args: Vec<Value>,
    /// Expected return types (for validation)
    pub return_types: Vec<ValueType>,
    /// The function type for type checking
    pub func_type: FunctionType,
}

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
    #[error("Global variable index out of bounds: {0}")]
    GlobalIndexOutOfBounds(u32),
    #[error("Invalid label: {0}")]
    InvalidLabel(u32),
    #[error("Memory error: {0}")]
    MemoryError(String),
    #[error("Division by zero")]
    DivisionByZero,
    #[error("Integer overflow")]
    IntegerOverflow,
    #[error("Invalid conversion: {0}")]
    InvalidConversion(String),
    #[error("Trap: {0}")]
    Trap(String),
    #[error("Invalid constant expression: {0}")]
    InvalidConstExpr(String),
    #[error("Call stack overflow")]
    CallStackOverflow,
    #[error("Table index out of bounds: {0}")]
    TableIndexOutOfBounds(u32),
    #[error("Table size exceeded")]
    TableSizeExceeded,
    #[error("Undefined table element: {0}")]
    UndefinedElement(u32),
    #[error("Indirect call type mismatch: expected {expected}, got {actual}")]
    IndirectCallTypeMismatch { expected: String, actual: String },
    #[error("Element index out of bounds: {0}")]
    ElementIndexOutOfBounds(u32),
    #[error("Import type mismatch for {module}.{name}: expected {expected}, got {actual}")]
    ImportTypeMismatch {
        module: String,
        name: String,
        expected: String,
        actual: String,
    },
    #[error("Instruction budget exhausted")]
    InstructionBudgetExhausted,
}
