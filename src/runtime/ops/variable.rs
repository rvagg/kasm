//! Variable operations for WebAssembly
//!
//! This module provides implementations of variable operations
//! as specified in the WebAssembly specification section 4.4.5 (Variable Instructions).

use super::*;
use crate::runtime::frame::Frame;

/// local.get x - Get local variable
/// spec: 4.4.5
///
/// From the spec:
/// 1. Let F be the current frame.
/// 2. Assert: due to validation, F.locals[x] exists.
/// 3. Let val be the value F.locals[x].
/// 4. Push the value val to the stack.
pub fn local_get(stack: &mut Stack, frame: &Frame, local_idx: u32) -> Result<(), RuntimeError> {
    let value = frame
        .locals
        .get(local_idx as usize)
        .ok_or(RuntimeError::LocalIndexOutOfBounds(local_idx))?
        .clone();
    stack.push(value);
    Ok(())
}

/// local.set x - Set local variable
/// spec: 4.4.5
///
/// From the spec:
/// 1. Let F be the current frame.
/// 2. Assert: due to validation, F.locals[x] exists.
/// 3. Assert: due to validation, a value is on the top of the stack.
/// 4. Pop the value val from the stack.
/// 5. Replace F.locals[x] with the value val.
pub fn local_set(stack: &mut Stack, frame: &mut Frame, local_idx: u32) -> Result<(), RuntimeError> {
    let value = stack.pop()?;

    // Bounds check
    if local_idx as usize >= frame.locals.len() {
        return Err(RuntimeError::LocalIndexOutOfBounds(local_idx));
    }

    frame.locals[local_idx as usize] = value;
    Ok(())
}

/// local.tee x - Set local variable but keep value on stack
/// spec: 4.4.5
///
/// From the spec:
/// 1. Assert: due to validation, a value is on the top of the stack.
/// 2. Pop the value val from the stack.
/// 3. Push the value val to the stack.
/// 4. Push the value val to the stack.
/// 5. Execute the instruction local.set x.
///
/// Note: This is equivalent to duplicating the top of stack, then doing local.set
pub fn local_tee(stack: &mut Stack, frame: &mut Frame, local_idx: u32) -> Result<(), RuntimeError> {
    let value = stack.pop()?;

    // Bounds check
    if local_idx as usize >= frame.locals.len() {
        return Err(RuntimeError::LocalIndexOutOfBounds(local_idx));
    }

    // Push value back to stack (tee leaves it on the stack)
    stack.push(value.clone());

    // Store in local
    frame.locals[local_idx as usize] = value;
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::parser::instruction::InstructionKind;
    use crate::parser::module::ValueType;
    use crate::runtime::test_utils::test::ExecutorTest;
    use crate::runtime::Value;

    #[test]
    fn local_get_first_arg() {
        // First function argument is local 0
        ExecutorTest::new()
            .args(vec![Value::I32(42)])
            .inst(InstructionKind::LocalGet { local_idx: 0 })
            .returns(vec![ValueType::I32])
            .expect_stack(vec![Value::I32(42)]);
    }

    #[test]
    fn local_get_multiple_args() {
        // Multiple arguments become locals 0, 1, 2, etc.
        ExecutorTest::new()
            .args(vec![Value::I32(10), Value::I64(20), Value::F32(30.0)])
            .inst(InstructionKind::LocalGet { local_idx: 1 })
            .inst(InstructionKind::LocalGet { local_idx: 0 })
            .inst(InstructionKind::LocalGet { local_idx: 2 })
            .returns(vec![ValueType::I64, ValueType::I32, ValueType::F32])
            .expect_stack(vec![Value::I64(20), Value::I32(10), Value::F32(30.0)]);
    }

    #[test]
    fn local_get_same_local_twice() {
        // Getting the same local multiple times
        ExecutorTest::new()
            .args(vec![Value::I32(42)])
            .inst(InstructionKind::LocalGet { local_idx: 0 })
            .inst(InstructionKind::LocalGet { local_idx: 0 })
            .returns(vec![ValueType::I32, ValueType::I32])
            .expect_stack(vec![Value::I32(42), Value::I32(42)]);
    }

    #[test]
    fn local_get_out_of_bounds() {
        ExecutorTest::new()
            .args(vec![Value::I32(42)])
            .inst(InstructionKind::LocalGet { local_idx: 1 })
            .expect_error("Local variable index out of bounds: 1");
    }

    #[test]
    fn local_get_with_drop() {
        // Test interaction with drop
        ExecutorTest::new()
            .args(vec![Value::I32(42), Value::I64(84)])
            .inst(InstructionKind::LocalGet { local_idx: 0 })
            .inst(InstructionKind::LocalGet { local_idx: 1 })
            .inst(InstructionKind::Drop)
            .returns(vec![ValueType::I32])
            .expect_stack(vec![Value::I32(42)]);
    }

    #[test]
    fn local_set_basic() {
        // Set a local and read it back
        ExecutorTest::new()
            .args(vec![Value::I32(0)])
            .inst(InstructionKind::I32Const { value: 42 })
            .inst(InstructionKind::LocalSet { local_idx: 0 })
            .inst(InstructionKind::LocalGet { local_idx: 0 })
            .returns(vec![ValueType::I32])
            .expect_stack(vec![Value::I32(42)]);
    }

    #[test]
    fn local_set_multiple() {
        // Set multiple locals
        ExecutorTest::new()
            .args(vec![Value::I32(1), Value::I64(2), Value::F32(3.0)])
            .inst(InstructionKind::I32Const { value: 10 })
            .inst(InstructionKind::LocalSet { local_idx: 0 })
            .inst(InstructionKind::I64Const { value: 20 })
            .inst(InstructionKind::LocalSet { local_idx: 1 })
            .inst(InstructionKind::F32Const { value: 30.0 })
            .inst(InstructionKind::LocalSet { local_idx: 2 })
            .inst(InstructionKind::LocalGet { local_idx: 0 })
            .inst(InstructionKind::LocalGet { local_idx: 1 })
            .inst(InstructionKind::LocalGet { local_idx: 2 })
            .returns(vec![ValueType::I32, ValueType::I64, ValueType::F32])
            .expect_stack(vec![Value::I32(10), Value::I64(20), Value::F32(30.0)]);
    }

    #[test]
    fn local_set_out_of_bounds() {
        ExecutorTest::new()
            .args(vec![Value::I32(42)])
            .inst(InstructionKind::I32Const { value: 100 })
            .inst(InstructionKind::LocalSet { local_idx: 1 })
            .expect_error("Local variable index out of bounds: 1");
    }

    #[test]
    fn local_set_empty_stack() {
        ExecutorTest::new()
            .args(vec![Value::I32(42)])
            .inst(InstructionKind::LocalSet { local_idx: 0 })
            .expect_error("Stack underflow");
    }

    #[test]
    fn local_set_get_sequence() {
        // Complex sequence of sets and gets
        ExecutorTest::new()
            .args(vec![Value::I32(1), Value::I32(2)])
            .inst(InstructionKind::LocalGet { local_idx: 0 }) // stack: [1]
            .inst(InstructionKind::LocalGet { local_idx: 1 }) // stack: [1, 2]
            .inst(InstructionKind::LocalSet { local_idx: 0 }) // stack: [1], local[0] = 2
            .inst(InstructionKind::LocalSet { local_idx: 1 }) // stack: [], local[1] = 1
            .inst(InstructionKind::LocalGet { local_idx: 0 }) // stack: [2]
            .inst(InstructionKind::LocalGet { local_idx: 1 }) // stack: [2, 1]
            .returns(vec![ValueType::I32, ValueType::I32])
            .expect_stack(vec![Value::I32(2), Value::I32(1)]);
    }

    #[test]
    fn local_tee_basic() {
        // Tee sets local but leaves value on stack
        ExecutorTest::new()
            .args(vec![Value::I32(0)])
            .inst(InstructionKind::I32Const { value: 42 })
            .inst(InstructionKind::LocalTee { local_idx: 0 })
            .returns(vec![ValueType::I32])
            .expect_stack(vec![Value::I32(42)]);
    }

    #[test]
    fn local_tee_verify_stored() {
        // Verify that tee actually stores the value
        ExecutorTest::new()
            .args(vec![Value::I32(0)])
            .inst(InstructionKind::I32Const { value: 42 })
            .inst(InstructionKind::LocalTee { local_idx: 0 })
            .inst(InstructionKind::Drop)
            .inst(InstructionKind::LocalGet { local_idx: 0 })
            .returns(vec![ValueType::I32])
            .expect_stack(vec![Value::I32(42)]);
    }

    #[test]
    fn local_tee_multiple() {
        // Use tee with multiple locals
        ExecutorTest::new()
            .args(vec![Value::I32(1), Value::I64(2), Value::F32(3.0)])
            .inst(InstructionKind::I32Const { value: 10 })
            .inst(InstructionKind::LocalTee { local_idx: 0 })
            .inst(InstructionKind::I64Const { value: 20 })
            .inst(InstructionKind::LocalTee { local_idx: 1 })
            .inst(InstructionKind::F32Const { value: 30.0 })
            .inst(InstructionKind::LocalTee { local_idx: 2 })
            .returns(vec![ValueType::I32, ValueType::I64, ValueType::F32])
            .expect_stack(vec![Value::I32(10), Value::I64(20), Value::F32(30.0)]);
    }

    #[test]
    fn local_tee_out_of_bounds() {
        ExecutorTest::new()
            .args(vec![Value::I32(42)])
            .inst(InstructionKind::I32Const { value: 100 })
            .inst(InstructionKind::LocalTee { local_idx: 1 })
            .expect_error("Local variable index out of bounds: 1");
    }

    #[test]
    fn local_tee_empty_stack() {
        ExecutorTest::new()
            .args(vec![Value::I32(42)])
            .inst(InstructionKind::LocalTee { local_idx: 0 })
            .expect_error("Stack underflow");
    }

    #[test]
    fn local_tee_chain() {
        // Chain multiple tees
        ExecutorTest::new()
            .args(vec![Value::I32(0), Value::I32(0)])
            .inst(InstructionKind::I32Const { value: 42 })
            .inst(InstructionKind::LocalTee { local_idx: 0 })
            .inst(InstructionKind::LocalTee { local_idx: 1 })
            .inst(InstructionKind::Drop)
            .inst(InstructionKind::LocalGet { local_idx: 0 })
            .inst(InstructionKind::LocalGet { local_idx: 1 })
            .returns(vec![ValueType::I32, ValueType::I32])
            .expect_stack(vec![Value::I32(42), Value::I32(42)]);
    }
}
