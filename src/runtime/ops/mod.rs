//! WebAssembly operation implementations
//!
//! This module contains the implementation of all WebAssembly instructions,
//! organized by category according to the WebAssembly specification.

pub mod comparison;
pub mod control;
pub mod memory;
pub mod numeric;
pub mod parametric;
pub mod variable;

// Re-export commonly used types for operation implementations
pub(crate) use crate::parser::instruction::MemArg;
pub(crate) use crate::runtime::memory::Memory;
pub(crate) use crate::runtime::stack::Stack;
pub(crate) use crate::runtime::{RuntimeError, Value};
