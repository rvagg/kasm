//! Comparison operations for WebAssembly
//!
//! This module provides implementations of comparison operations
//! as specified in the WebAssembly specification section 4.4.1.4 (Test Instructions)
//! and 4.4.1.5 (Comparison Instructions).
//!
//! All comparison operations return an i32 value: 1 for true, 0 for false.

use super::*;

// ============================================================================
// Integer Comparison Operations (i32)
// ============================================================================

/// i32.eqz - Test if i32 is zero
/// spec: 4.4.1.4
pub fn i32_eqz(stack: &mut Stack) -> Result<(), RuntimeError> {
    let value = stack.pop_i32()?;
    stack.push(Value::I32(if value == 0 { 1 } else { 0 }));
    Ok(())
}

/// i32.eq - Test if two i32 values are equal
/// spec: 4.4.1.5
pub fn i32_eq(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i32()?;
    let a = stack.pop_i32()?;
    stack.push(Value::I32(if a == b { 1 } else { 0 }));
    Ok(())
}

/// i32.ne - Test if two i32 values are not equal
/// spec: 4.4.1.5
pub fn i32_ne(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i32()?;
    let a = stack.pop_i32()?;
    stack.push(Value::I32(if a != b { 1 } else { 0 }));
    Ok(())
}

/// i32.lt_s - Test if a < b (signed)
/// spec: 4.4.1.5
pub fn i32_lt_s(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i32()?;
    let a = stack.pop_i32()?;
    stack.push(Value::I32(if a < b { 1 } else { 0 }));
    Ok(())
}

/// i32.lt_u - Test if a < b (unsigned)
/// spec: 4.4.1.5
pub fn i32_lt_u(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i32()?;
    let a = stack.pop_i32()?;
    stack.push(Value::I32(if (a as u32) < (b as u32) { 1 } else { 0 }));
    Ok(())
}

/// i32.gt_s - Test if a > b (signed)
/// spec: 4.4.1.5
pub fn i32_gt_s(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i32()?;
    let a = stack.pop_i32()?;
    stack.push(Value::I32(if a > b { 1 } else { 0 }));
    Ok(())
}

/// i32.gt_u - Test if a > b (unsigned)
/// spec: 4.4.1.5
pub fn i32_gt_u(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i32()?;
    let a = stack.pop_i32()?;
    stack.push(Value::I32(if (a as u32) > (b as u32) { 1 } else { 0 }));
    Ok(())
}

/// i32.le_s - Test if a <= b (signed)
/// spec: 4.4.1.5
pub fn i32_le_s(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i32()?;
    let a = stack.pop_i32()?;
    stack.push(Value::I32(if a <= b { 1 } else { 0 }));
    Ok(())
}

/// i32.le_u - Test if a <= b (unsigned)
/// spec: 4.4.1.5
pub fn i32_le_u(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i32()?;
    let a = stack.pop_i32()?;
    stack.push(Value::I32(if (a as u32) <= (b as u32) { 1 } else { 0 }));
    Ok(())
}

/// i32.ge_s - Test if a >= b (signed)
/// spec: 4.4.1.5
pub fn i32_ge_s(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i32()?;
    let a = stack.pop_i32()?;
    stack.push(Value::I32(if a >= b { 1 } else { 0 }));
    Ok(())
}

/// i32.ge_u - Test if a >= b (unsigned)
/// spec: 4.4.1.5
pub fn i32_ge_u(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i32()?;
    let a = stack.pop_i32()?;
    stack.push(Value::I32(if (a as u32) >= (b as u32) { 1 } else { 0 }));
    Ok(())
}

// ============================================================================
// Integer Comparison Operations (i64)
// ============================================================================

/// i64.eqz - Test if i64 is zero
/// spec: 4.4.1.4
pub fn i64_eqz(stack: &mut Stack) -> Result<(), RuntimeError> {
    let value = stack.pop_i64()?;
    stack.push(Value::I32(if value == 0 { 1 } else { 0 }));
    Ok(())
}

/// i64.eq - Test if two i64 values are equal
/// spec: 4.4.1.5
pub fn i64_eq(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i64()?;
    let a = stack.pop_i64()?;
    stack.push(Value::I32(if a == b { 1 } else { 0 }));
    Ok(())
}

/// i64.ne - Test if two i64 values are not equal
/// spec: 4.4.1.5
pub fn i64_ne(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i64()?;
    let a = stack.pop_i64()?;
    stack.push(Value::I32(if a != b { 1 } else { 0 }));
    Ok(())
}

/// i64.lt_s - Test if a < b (signed)
/// spec: 4.4.1.5
pub fn i64_lt_s(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i64()?;
    let a = stack.pop_i64()?;
    stack.push(Value::I32(if a < b { 1 } else { 0 }));
    Ok(())
}

/// i64.lt_u - Test if a < b (unsigned)
/// spec: 4.4.1.5
pub fn i64_lt_u(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i64()?;
    let a = stack.pop_i64()?;
    stack.push(Value::I32(if (a as u64) < (b as u64) { 1 } else { 0 }));
    Ok(())
}

/// i64.gt_s - Test if a > b (signed)
/// spec: 4.4.1.5
pub fn i64_gt_s(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i64()?;
    let a = stack.pop_i64()?;
    stack.push(Value::I32(if a > b { 1 } else { 0 }));
    Ok(())
}

/// i64.gt_u - Test if a > b (unsigned)
/// spec: 4.4.1.5
pub fn i64_gt_u(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i64()?;
    let a = stack.pop_i64()?;
    stack.push(Value::I32(if (a as u64) > (b as u64) { 1 } else { 0 }));
    Ok(())
}

/// i64.le_s - Test if a <= b (signed)
/// spec: 4.4.1.5
pub fn i64_le_s(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i64()?;
    let a = stack.pop_i64()?;
    stack.push(Value::I32(if a <= b { 1 } else { 0 }));
    Ok(())
}

/// i64.le_u - Test if a <= b (unsigned)
/// spec: 4.4.1.5
pub fn i64_le_u(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i64()?;
    let a = stack.pop_i64()?;
    stack.push(Value::I32(if (a as u64) <= (b as u64) { 1 } else { 0 }));
    Ok(())
}

/// i64.ge_s - Test if a >= b (signed)
/// spec: 4.4.1.5
pub fn i64_ge_s(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i64()?;
    let a = stack.pop_i64()?;
    stack.push(Value::I32(if a >= b { 1 } else { 0 }));
    Ok(())
}

/// i64.ge_u - Test if a >= b (unsigned)
/// spec: 4.4.1.5
pub fn i64_ge_u(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i64()?;
    let a = stack.pop_i64()?;
    stack.push(Value::I32(if (a as u64) >= (b as u64) { 1 } else { 0 }));
    Ok(())
}

// ============================================================================
// Floating-Point Comparison Operations (f32)
// ============================================================================

/// f32.eq - Test if two f32 values are equal
/// spec: 4.4.1.5
/// Note: NaN != NaN, -0.0 == +0.0
pub fn f32_eq(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_f32()?;
    let a = stack.pop_f32()?;
    stack.push(Value::I32(if a == b { 1 } else { 0 }));
    Ok(())
}

/// f32.ne - Test if two f32 values are not equal
/// spec: 4.4.1.5
/// Note: NaN != NaN returns true
pub fn f32_ne(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_f32()?;
    let a = stack.pop_f32()?;
    stack.push(Value::I32(if a != b { 1 } else { 0 }));
    Ok(())
}

/// f32.lt - Test if a < b
/// spec: 4.4.1.5
/// Note: NaN comparisons return false
pub fn f32_lt(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_f32()?;
    let a = stack.pop_f32()?;
    stack.push(Value::I32(if a < b { 1 } else { 0 }));
    Ok(())
}

/// f32.gt - Test if a > b
/// spec: 4.4.1.5
pub fn f32_gt(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_f32()?;
    let a = stack.pop_f32()?;
    stack.push(Value::I32(if a > b { 1 } else { 0 }));
    Ok(())
}

/// f32.le - Test if a <= b
/// spec: 4.4.1.5
pub fn f32_le(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_f32()?;
    let a = stack.pop_f32()?;
    stack.push(Value::I32(if a <= b { 1 } else { 0 }));
    Ok(())
}

/// f32.ge - Test if a >= b
/// spec: 4.4.1.5
pub fn f32_ge(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_f32()?;
    let a = stack.pop_f32()?;
    stack.push(Value::I32(if a >= b { 1 } else { 0 }));
    Ok(())
}

// ============================================================================
// Floating-Point Comparison Operations (f64)
// ============================================================================

/// f64.eq - Test if two f64 values are equal
/// spec: 4.4.1.5
pub fn f64_eq(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_f64()?;
    let a = stack.pop_f64()?;
    stack.push(Value::I32(if a == b { 1 } else { 0 }));
    Ok(())
}

/// f64.ne - Test if two f64 values are not equal
/// spec: 4.4.1.5
pub fn f64_ne(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_f64()?;
    let a = stack.pop_f64()?;
    stack.push(Value::I32(if a != b { 1 } else { 0 }));
    Ok(())
}

/// f64.lt - Test if a < b
/// spec: 4.4.1.5
pub fn f64_lt(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_f64()?;
    let a = stack.pop_f64()?;
    stack.push(Value::I32(if a < b { 1 } else { 0 }));
    Ok(())
}

/// f64.gt - Test if a > b
/// spec: 4.4.1.5
pub fn f64_gt(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_f64()?;
    let a = stack.pop_f64()?;
    stack.push(Value::I32(if a > b { 1 } else { 0 }));
    Ok(())
}

/// f64.le - Test if a <= b
/// spec: 4.4.1.5
pub fn f64_le(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_f64()?;
    let a = stack.pop_f64()?;
    stack.push(Value::I32(if a <= b { 1 } else { 0 }));
    Ok(())
}

/// f64.ge - Test if a >= b
/// spec: 4.4.1.5
pub fn f64_ge(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_f64()?;
    let a = stack.pop_f64()?;
    stack.push(Value::I32(if a >= b { 1 } else { 0 }));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Value;

    // Helper to create a stack with values
    fn stack_with(values: Vec<Value>) -> Stack {
        let mut stack = Stack::new();
        for value in values {
            stack.push(value);
        }
        stack
    }

    // ============================================================================
    // i32 Test Operations
    // ============================================================================

    #[test]
    fn test_i32_eqz() {
        // Test zero
        let mut stack = stack_with(vec![Value::I32(0)]);
        i32_eqz(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // Test non-zero positive
        let mut stack = stack_with(vec![Value::I32(42)]);
        i32_eqz(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0));

        // Test non-zero negative
        let mut stack = stack_with(vec![Value::I32(-1)]);
        i32_eqz(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0));
    }

    // ============================================================================
    // i32 Comparison Operations
    // ============================================================================

    #[test]
    fn test_i32_eq() {
        // Equal values
        let mut stack = stack_with(vec![Value::I32(42), Value::I32(42)]);
        i32_eq(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // Different values
        let mut stack = stack_with(vec![Value::I32(42), Value::I32(43)]);
        i32_eq(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0));

        // Negative values
        let mut stack = stack_with(vec![Value::I32(-1), Value::I32(-1)]);
        i32_eq(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));
    }

    #[test]
    fn test_i32_ne() {
        // Different values
        let mut stack = stack_with(vec![Value::I32(42), Value::I32(43)]);
        i32_ne(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // Equal values
        let mut stack = stack_with(vec![Value::I32(42), Value::I32(42)]);
        i32_ne(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0));
    }

    #[test]
    fn test_i32_lt_s() {
        // a < b
        let mut stack = stack_with(vec![Value::I32(1), Value::I32(2)]);
        i32_lt_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // a >= b
        let mut stack = stack_with(vec![Value::I32(2), Value::I32(1)]);
        i32_lt_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0));

        // Negative comparison
        let mut stack = stack_with(vec![Value::I32(-2), Value::I32(-1)]);
        i32_lt_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));
    }

    #[test]
    fn test_i32_lt_u() {
        // Unsigned comparison where sign bit matters
        let mut stack = stack_with(vec![Value::I32(-1), Value::I32(1)]);
        i32_lt_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0)); // -1 as u32 is large

        // Normal unsigned comparison
        let mut stack = stack_with(vec![Value::I32(1), Value::I32(2)]);
        i32_lt_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));
    }

    #[test]
    fn test_i32_gt_s() {
        // a > b
        let mut stack = stack_with(vec![Value::I32(2), Value::I32(1)]);
        i32_gt_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // a <= b
        let mut stack = stack_with(vec![Value::I32(1), Value::I32(2)]);
        i32_gt_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0));
    }

    #[test]
    fn test_i32_le_s() {
        // a <= b (less)
        let mut stack = stack_with(vec![Value::I32(1), Value::I32(2)]);
        i32_le_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // a <= b (equal)
        let mut stack = stack_with(vec![Value::I32(2), Value::I32(2)]);
        i32_le_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // a > b
        let mut stack = stack_with(vec![Value::I32(3), Value::I32(2)]);
        i32_le_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0));
    }

    #[test]
    fn test_i32_ge_s() {
        // a >= b (greater)
        let mut stack = stack_with(vec![Value::I32(2), Value::I32(1)]);
        i32_ge_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // a >= b (equal)
        let mut stack = stack_with(vec![Value::I32(2), Value::I32(2)]);
        i32_ge_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // a < b
        let mut stack = stack_with(vec![Value::I32(1), Value::I32(2)]);
        i32_ge_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0));
    }

    // ============================================================================
    // i64 Tests
    // ============================================================================

    #[test]
    fn test_i64_eqz() {
        // Test zero
        let mut stack = stack_with(vec![Value::I64(0)]);
        i64_eqz(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // Test non-zero
        let mut stack = stack_with(vec![Value::I64(0x1_0000_0000)]);
        i64_eqz(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0));
    }

    #[test]
    fn test_i64_comparisons() {
        // i64.eq
        let mut stack = stack_with(vec![Value::I64(42), Value::I64(42)]);
        i64_eq(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // i64.lt_s
        let mut stack = stack_with(vec![Value::I64(-1), Value::I64(0)]);
        i64_lt_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // i64.lt_u
        let mut stack = stack_with(vec![Value::I64(-1), Value::I64(0)]);
        i64_lt_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0)); // -1 as u64 is max value
    }

    // ============================================================================
    // f32 Tests
    // ============================================================================

    #[test]
    fn test_f32_eq() {
        // Equal values
        let mut stack = stack_with(vec![Value::F32(1.0), Value::F32(1.0)]);
        f32_eq(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // NaN != NaN
        let mut stack = stack_with(vec![Value::F32(f32::NAN), Value::F32(f32::NAN)]);
        f32_eq(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0));

        // -0.0 == +0.0
        let mut stack = stack_with(vec![Value::F32(-0.0), Value::F32(0.0)]);
        f32_eq(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));
    }

    #[test]
    fn test_f32_ne() {
        // Different values
        let mut stack = stack_with(vec![Value::F32(1.0), Value::F32(2.0)]);
        f32_ne(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // NaN != NaN is true
        let mut stack = stack_with(vec![Value::F32(f32::NAN), Value::F32(f32::NAN)]);
        f32_ne(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));
    }

    #[test]
    fn test_f32_lt() {
        // a < b
        let mut stack = stack_with(vec![Value::F32(1.0), Value::F32(2.0)]);
        f32_lt(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // NaN comparison returns false
        let mut stack = stack_with(vec![Value::F32(f32::NAN), Value::F32(1.0)]);
        f32_lt(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0));

        let mut stack = stack_with(vec![Value::F32(1.0), Value::F32(f32::NAN)]);
        f32_lt(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0));
    }

    #[test]
    fn test_f32_infinity() {
        // -inf < inf
        let mut stack = stack_with(vec![Value::F32(f32::NEG_INFINITY), Value::F32(f32::INFINITY)]);
        f32_lt(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // inf > any finite
        let mut stack = stack_with(vec![Value::F32(f32::INFINITY), Value::F32(1e10)]);
        f32_gt(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));
    }

    // ============================================================================
    // f64 Tests
    // ============================================================================

    #[test]
    fn test_f64_comparisons() {
        // f64.eq
        let mut stack = stack_with(vec![Value::F64(42.0), Value::F64(42.0)]);
        f64_eq(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // f64.lt with NaN
        let mut stack = stack_with(vec![Value::F64(f64::NAN), Value::F64(1.0)]);
        f64_lt(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0));

        // f64.ge
        let mut stack = stack_with(vec![Value::F64(2.0), Value::F64(2.0)]);
        f64_ge(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));
    }
}
