//! WebAssembly runtime implementation
//!
//! This module provides the execution engine for WebAssembly modules,
//! including the stack machine, value representation, and instruction interpreter.

pub mod control;
pub mod executor;
pub mod frame;
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
pub use store::{
    FuncAddr, FunctionInstance, GlobalAddr, MemoryAddr, SharedGlobal, SharedMemory, SharedTable, Store, TableAddr,
};
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
    #[error("stack underflow")]
    StackUnderflow,
    #[error("type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },
    #[error("unknown import: {0}")]
    UnknownFunction(String),
    #[error("unknown export: {0}")]
    UnknownExport(String),
    #[error("incompatible import type: {0}")]
    IncompatibleImportType(String),
    #[error("function index out of bounds: {0}")]
    FunctionIndexOutOfBounds(u32),
    #[error("unimplemented instruction: {0}")]
    UnimplementedInstruction(String),
    #[error("invalid function type")]
    InvalidFunctionType,
    #[error("local variable index out of bounds: {0}")]
    LocalIndexOutOfBounds(u32),
    #[error("global variable index out of bounds: {0}")]
    GlobalIndexOutOfBounds(u32),
    #[error("invalid label: {0}")]
    InvalidLabel(u32),
    #[error("memory error: {0}")]
    MemoryError(String),
    #[error("integer divide by zero")]
    DivisionByZero,
    #[error("integer overflow")]
    IntegerOverflow,
    #[error("invalid conversion to integer: {0}")]
    InvalidConversion(String),
    #[error("trap: {0}")]
    Trap(String),
    #[error("invalid constant expression: {0}")]
    InvalidConstExpr(String),
    #[error("call stack exhausted")]
    CallStackOverflow,
    #[error("out of bounds table access")]
    TableIndexOutOfBounds(u32),
    #[error("table size exceeded")]
    TableSizeExceeded,
    #[error("uninitialized element {0}")]
    UndefinedElement(u32),
    #[error("indirect call type mismatch: expected {expected}, got {actual}")]
    IndirectCallTypeMismatch { expected: String, actual: String },
    #[error("element index out of bounds: {0}")]
    ElementIndexOutOfBounds(u32),
    #[error("incompatible import type for {module}.{name}: expected {expected}, got {actual}")]
    ImportTypeMismatch {
        module: String,
        name: String,
        expected: String,
        actual: String,
    },
    #[error("instruction budget exhausted")]
    InstructionBudgetExhausted,
}
