//! Parametric operations for WebAssembly
//!
//! This module provides implementations of parametric operations
//! as specified in the WebAssembly specification section 4.4.4 (Parametric Instructions).

use super::*;

/// drop - Drop value from stack
/// spec: 4.4.4
///
/// From the spec:
/// 1. Assert: due to validation, a value is on the top of the stack.
/// 2. Pop the value val from the stack.
pub fn drop(stack: &mut Stack) -> Result<(), RuntimeError> {
    stack.pop()?;
    Ok(())
}

// Note: select and select_t will be implemented here when needed
// select - Select one of two values based on condition
// select_t - Typed select

#[cfg(test)]
mod tests {
    use crate::parser::instruction::InstructionKind;
    use crate::parser::module::ValueType;
    use crate::runtime::test_utils::test::ExecutorTest;
    use crate::runtime::Value;

    #[test]
    fn drop_single_value() {
        ExecutorTest::new()
            .inst(InstructionKind::I32Const { value: 42 })
            .inst(InstructionKind::Drop)
            .inst(InstructionKind::I32Const { value: 100 })
            .returns(vec![ValueType::I32])
            .expect_stack(vec![Value::I32(100)]);
    }

    #[test]
    fn drop_multiple_values() {
        ExecutorTest::new()
            .inst(InstructionKind::I32Const { value: 1 })
            .inst(InstructionKind::I32Const { value: 2 })
            .inst(InstructionKind::I32Const { value: 3 })
            .inst(InstructionKind::Drop)
            .inst(InstructionKind::Drop)
            .returns(vec![ValueType::I32])
            .expect_stack(vec![Value::I32(1)]);
    }

    #[test]
    fn drop_empty_stack() {
        ExecutorTest::new()
            .inst(InstructionKind::Drop)
            .expect_error("Stack underflow");
    }

    #[test]
    fn drop_different_types() {
        ExecutorTest::new()
            .inst(InstructionKind::I32Const { value: 42 })
            .inst(InstructionKind::Drop)
            .inst(InstructionKind::F64Const { value: 3.14 })
            .inst(InstructionKind::Drop)
            .inst(InstructionKind::I64Const { value: 100 })
            .returns(vec![ValueType::I64])
            .expect_stack(vec![Value::I64(100)]);
    }
}
