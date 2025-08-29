//! Variable operations for WebAssembly
//!
//! This module provides implementations of variable operations
//! as specified in the WebAssembly specification section 4.4.5 (Variable Instructions).

use super::*;
use crate::parser::module::Module;
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

/// global.get x - Get global variable
/// spec: 4.4.5
///
/// From the spec:
/// 1. Let F be the current frame.
/// 2. Assert: due to validation, F.module.globals[x] exists.
/// 3. Let val be the value F.module.globals[x].val.
/// 4. Push the value val to the stack.
pub fn global_get(stack: &mut Stack, globals: &[Value], global_idx: u32) -> Result<(), RuntimeError> {
    let value = globals
        .get(global_idx as usize)
        .ok_or(RuntimeError::GlobalIndexOutOfBounds(global_idx))?
        .clone();
    stack.push(value);
    Ok(())
}

/// global.set x - Set global variable
/// spec: 4.4.5
///
/// From the spec:
/// 1. Let F be the current frame.
/// 2. Assert: due to validation, F.module.globals[x] exists.
/// 3. Assert: due to validation, F.module.globals[x] is mutable.
/// 4. Assert: due to validation, a value is on the top of the stack.
/// 5. Pop the value val from the stack.
/// 6. Replace F.module.globals[x].val with the value val.
pub fn global_set(
    stack: &mut Stack,
    globals: &mut [Value],
    module: &Module,
    global_idx: u32,
) -> Result<(), RuntimeError> {
    let value = stack.pop()?;

    // Bounds check
    if global_idx as usize >= globals.len() {
        return Err(RuntimeError::GlobalIndexOutOfBounds(global_idx));
    }

    // Check mutability
    if let Some(global_def) = module.globals.globals.get(global_idx as usize) {
        if !global_def.global_type.mutable {
            return Err(RuntimeError::InvalidConversion(
                "Cannot set immutable global".to_string(),
            ));
        }
    }

    globals[global_idx as usize] = value;
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

    #[test]
    fn global_get_basic() {
        // Test basic global.get with actual global
        ExecutorTest::new()
            .global(ValueType::I32, Value::I32(42), false)
            .inst(InstructionKind::GlobalGet { global_idx: 0 })
            .returns(vec![ValueType::I32])
            .expect_stack(vec![Value::I32(42)]);
    }

    #[test]
    fn global_get_multiple() {
        // Test multiple globals with different types
        ExecutorTest::new()
            .global(ValueType::I32, Value::I32(10), false)
            .global(ValueType::F64, Value::F64(3.14), false)
            .global(ValueType::I64, Value::I64(100), false)
            .inst(InstructionKind::GlobalGet { global_idx: 1 })
            .inst(InstructionKind::GlobalGet { global_idx: 0 })
            .inst(InstructionKind::GlobalGet { global_idx: 2 })
            .returns(vec![ValueType::F64, ValueType::I32, ValueType::I64])
            .expect_stack(vec![Value::F64(3.14), Value::I32(10), Value::I64(100)]);
    }

    #[test]
    fn global_get_out_of_bounds() {
        ExecutorTest::new()
            .global(ValueType::I32, Value::I32(42), false)
            .inst(InstructionKind::GlobalGet { global_idx: 1 })
            .expect_error("Global variable index out of bounds: 1");
    }

    #[test]
    fn global_set_basic() {
        // Test basic global.set with mutable global
        ExecutorTest::new()
            .global(ValueType::I32, Value::I32(0), true) // mutable
            .inst(InstructionKind::I32Const { value: 42 })
            .inst(InstructionKind::GlobalSet { global_idx: 0 })
            .inst(InstructionKind::GlobalGet { global_idx: 0 })
            .returns(vec![ValueType::I32])
            .expect_stack(vec![Value::I32(42)]);
    }

    #[test]
    fn global_set_multiple_types() {
        // Test setting different value types
        ExecutorTest::new()
            .global(ValueType::I32, Value::I32(0), true)
            .global(ValueType::F32, Value::F32(0.0), true)
            .global(ValueType::I64, Value::I64(0), true)
            .inst(InstructionKind::I32Const { value: 100 })
            .inst(InstructionKind::GlobalSet { global_idx: 0 })
            .inst(InstructionKind::F32Const { value: 2.5 })
            .inst(InstructionKind::GlobalSet { global_idx: 1 })
            .inst(InstructionKind::I64Const { value: 999 })
            .inst(InstructionKind::GlobalSet { global_idx: 2 })
            .inst(InstructionKind::GlobalGet { global_idx: 0 })
            .inst(InstructionKind::GlobalGet { global_idx: 1 })
            .inst(InstructionKind::GlobalGet { global_idx: 2 })
            .returns(vec![ValueType::I32, ValueType::F32, ValueType::I64])
            .expect_stack(vec![Value::I32(100), Value::F32(2.5), Value::I64(999)]);
    }

    #[test]
    fn global_set_out_of_bounds() {
        ExecutorTest::new()
            .global(ValueType::I32, Value::I32(42), true)
            .inst(InstructionKind::I32Const { value: 100 })
            .inst(InstructionKind::GlobalSet { global_idx: 1 })
            .expect_error("Global variable index out of bounds: 1");
    }

    #[test]
    fn global_set_empty_stack() {
        ExecutorTest::new()
            .global(ValueType::I32, Value::I32(42), true)
            .inst(InstructionKind::GlobalSet { global_idx: 0 })
            .expect_error("Stack underflow");
    }

    #[test]
    fn global_set_immutable() {
        // Test that setting immutable global fails
        ExecutorTest::new()
            .global(ValueType::I32, Value::I32(42), false) // immutable
            .inst(InstructionKind::I32Const { value: 100 })
            .inst(InstructionKind::GlobalSet { global_idx: 0 })
            .expect_error("Cannot set immutable global");
    }

    #[test]
    fn global_mixed_mutability() {
        // Test mix of mutable and immutable globals
        ExecutorTest::new()
            .global(ValueType::I32, Value::I32(10), false) // immutable
            .global(ValueType::I32, Value::I32(20), true) // mutable
            .inst(InstructionKind::I32Const { value: 100 })
            .inst(InstructionKind::GlobalSet { global_idx: 1 }) // Should work (mutable)
            .inst(InstructionKind::GlobalGet { global_idx: 0 }) // Should get immutable value
            .inst(InstructionKind::GlobalGet { global_idx: 1 }) // Should get new value
            .returns(vec![ValueType::I32, ValueType::I32])
            .expect_stack(vec![Value::I32(10), Value::I32(100)]);
    }
}
