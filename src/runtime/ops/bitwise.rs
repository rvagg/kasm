//! Bitwise operations for WebAssembly
//!
//! This module provides implementations of bitwise operations
//! as specified in the WebAssembly specification section 4.4.1.3 (Binary Operations).
//!
//! All shift and rotate operations mask the shift count to the appropriate bit width.

use super::*;

// ============================================================================
// i32 Bitwise Operations
// ============================================================================

/// i32.and - Bitwise AND
/// spec: 4.4.1.3
pub fn i32_and(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i32()?;
    let a = stack.pop_i32()?;
    stack.push(Value::I32(a & b));
    Ok(())
}

/// i32.or - Bitwise OR
/// spec: 4.4.1.3
pub fn i32_or(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i32()?;
    let a = stack.pop_i32()?;
    stack.push(Value::I32(a | b));
    Ok(())
}

/// i32.xor - Bitwise XOR
/// spec: 4.4.1.3
pub fn i32_xor(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i32()?;
    let a = stack.pop_i32()?;
    stack.push(Value::I32(a ^ b));
    Ok(())
}

/// i32.shl - Shift left
/// spec: 4.4.1.3
/// The shift count is taken modulo 32
pub fn i32_shl(stack: &mut Stack) -> Result<(), RuntimeError> {
    let count = stack.pop_i32()?;
    let value = stack.pop_i32()?;
    // WebAssembly spec: shift count is modulo 32
    let shift = (count & 31) as u32;
    stack.push(Value::I32(value << shift));
    Ok(())
}

/// i32.shr_s - Arithmetic shift right (sign-extending)
/// spec: 4.4.1.3
/// The shift count is taken modulo 32
pub fn i32_shr_s(stack: &mut Stack) -> Result<(), RuntimeError> {
    let count = stack.pop_i32()?;
    let value = stack.pop_i32()?;
    // WebAssembly spec: shift count is modulo 32
    let shift = (count & 31) as u32;
    stack.push(Value::I32(value >> shift));
    Ok(())
}

/// i32.shr_u - Logical shift right (zero-extending)
/// spec: 4.4.1.3
/// The shift count is taken modulo 32
pub fn i32_shr_u(stack: &mut Stack) -> Result<(), RuntimeError> {
    let count = stack.pop_i32()?;
    let value = stack.pop_i32()? as u32;
    // WebAssembly spec: shift count is modulo 32
    let shift = (count & 31) as u32;
    stack.push(Value::I32((value >> shift) as i32));
    Ok(())
}

/// i32.rotl - Rotate left
/// spec: 4.4.1.3
/// The rotation count is taken modulo 32
pub fn i32_rotl(stack: &mut Stack) -> Result<(), RuntimeError> {
    let count = stack.pop_i32()?;
    let value = stack.pop_i32()? as u32;
    // WebAssembly spec: rotation count is modulo 32
    let rotation = (count & 31) as u32;
    let result = value.rotate_left(rotation);
    stack.push(Value::I32(result as i32));
    Ok(())
}

/// i32.rotr - Rotate right
/// spec: 4.4.1.3
/// The rotation count is taken modulo 32
pub fn i32_rotr(stack: &mut Stack) -> Result<(), RuntimeError> {
    let count = stack.pop_i32()?;
    let value = stack.pop_i32()? as u32;
    // WebAssembly spec: rotation count is modulo 32
    let rotation = (count & 31) as u32;
    let result = value.rotate_right(rotation);
    stack.push(Value::I32(result as i32));
    Ok(())
}

// ============================================================================
// i64 Bitwise Operations
// ============================================================================

/// i64.and - Bitwise AND
/// spec: 4.4.1.3
pub fn i64_and(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i64()?;
    let a = stack.pop_i64()?;
    stack.push(Value::I64(a & b));
    Ok(())
}

/// i64.or - Bitwise OR
/// spec: 4.4.1.3
pub fn i64_or(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i64()?;
    let a = stack.pop_i64()?;
    stack.push(Value::I64(a | b));
    Ok(())
}

/// i64.xor - Bitwise XOR
/// spec: 4.4.1.3
pub fn i64_xor(stack: &mut Stack) -> Result<(), RuntimeError> {
    let b = stack.pop_i64()?;
    let a = stack.pop_i64()?;
    stack.push(Value::I64(a ^ b));
    Ok(())
}

/// i64.shl - Shift left
/// spec: 4.4.1.3
/// The shift count is taken modulo 64
pub fn i64_shl(stack: &mut Stack) -> Result<(), RuntimeError> {
    let count = stack.pop_i64()?;
    let value = stack.pop_i64()?;
    // WebAssembly spec: shift count is modulo 64
    let shift = (count & 63) as u32;
    stack.push(Value::I64(value << shift));
    Ok(())
}

/// i64.shr_s - Arithmetic shift right (sign-extending)
/// spec: 4.4.1.3
/// The shift count is taken modulo 64
pub fn i64_shr_s(stack: &mut Stack) -> Result<(), RuntimeError> {
    let count = stack.pop_i64()?;
    let value = stack.pop_i64()?;
    // WebAssembly spec: shift count is modulo 64
    let shift = (count & 63) as u32;
    stack.push(Value::I64(value >> shift));
    Ok(())
}

/// i64.shr_u - Logical shift right (zero-extending)
/// spec: 4.4.1.3
/// The shift count is taken modulo 64
pub fn i64_shr_u(stack: &mut Stack) -> Result<(), RuntimeError> {
    let count = stack.pop_i64()?;
    let value = stack.pop_i64()? as u64;
    // WebAssembly spec: shift count is modulo 64
    let shift = (count & 63) as u32;
    stack.push(Value::I64((value >> shift) as i64));
    Ok(())
}

/// i64.rotl - Rotate left
/// spec: 4.4.1.3
/// The rotation count is taken modulo 64
pub fn i64_rotl(stack: &mut Stack) -> Result<(), RuntimeError> {
    let count = stack.pop_i64()?;
    let value = stack.pop_i64()? as u64;
    // WebAssembly spec: rotation count is modulo 64
    let rotation = (count & 63) as u32;
    let result = value.rotate_left(rotation);
    stack.push(Value::I64(result as i64));
    Ok(())
}

/// i64.rotr - Rotate right
/// spec: 4.4.1.3
/// The rotation count is taken modulo 64
pub fn i64_rotr(stack: &mut Stack) -> Result<(), RuntimeError> {
    let count = stack.pop_i64()?;
    let value = stack.pop_i64()? as u64;
    // WebAssembly spec: rotation count is modulo 64
    let rotation = (count & 63) as u32;
    let result = value.rotate_right(rotation);
    stack.push(Value::I64(result as i64));
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
    // i32 Bitwise Tests
    // ============================================================================

    #[test]
    fn test_i32_and() {
        let mut stack = stack_with(vec![Value::I32(0xFF00FF00u32 as i32), Value::I32(0x0F0F0F0F)]);
        i32_and(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x0F000F00));

        // Test with negative numbers
        let mut stack = stack_with(vec![Value::I32(-1), Value::I32(0x12345678)]);
        i32_and(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x12345678));
    }

    #[test]
    fn test_i32_bitwise_masks() {
        // Test masking operations
        // Clear high byte: x & 0x00FFFFFF
        let mut stack = stack_with(vec![Value::I32(0x12345678), Value::I32(0x00FFFFFF)]);
        i32_and(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x00345678));

        // Extract nibble with AND
        let mut stack = stack_with(vec![Value::I32(0x12345678), Value::I32(0x0000F000)]);
        i32_and(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x00005000));

        // Set bits with OR
        let mut stack = stack_with(vec![Value::I32(0x12340000), Value::I32(0x00005678)]);
        i32_or(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x12345678));

        // Toggle bits with XOR
        let mut stack = stack_with(vec![Value::I32(0x12345678), Value::I32(0xFF00FF00u32 as i32)]);
        i32_xor(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0xED34A978u32 as i32));

        // Clear bit pattern: x & ~pattern
        let mut stack = stack_with(vec![Value::I32(0xFFFFFFFFu32 as i32), Value::I32(0x80808080u32 as i32)]);
        i32_and(&mut stack).unwrap();
        let _result = stack.pop().unwrap();
        // Now XOR to clear those bits from another value
        let mut stack = stack_with(vec![Value::I32(0xFFFFFFFFu32 as i32), Value::I32(0x80808080u32 as i32)]);
        i32_xor(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x7F7F7F7F));
    }

    #[test]
    fn test_i32_or() {
        let mut stack = stack_with(vec![Value::I32(0xFF00FF00u32 as i32), Value::I32(0x0F0F0F0F)]);
        i32_or(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0xFF0FFF0Fu32 as i32));

        // Test with zero
        let mut stack = stack_with(vec![Value::I32(0), Value::I32(0x12345678)]);
        i32_or(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x12345678));
    }

    #[test]
    fn test_i32_xor() {
        let mut stack = stack_with(vec![Value::I32(0xFF00FF00u32 as i32), Value::I32(0x0F0F0F0F)]);
        i32_xor(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0xF00FF00Fu32 as i32));

        // XOR with self should be zero
        let mut stack = stack_with(vec![Value::I32(0x12345678), Value::I32(0x12345678)]);
        i32_xor(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0));
    }

    #[test]
    fn test_i32_shl() {
        // Basic shift
        let mut stack = stack_with(vec![Value::I32(1), Value::I32(4)]);
        i32_shl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(16));

        // Shift by 0
        let mut stack = stack_with(vec![Value::I32(42), Value::I32(0)]);
        i32_shl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(42));

        // Shift by 32 (should wrap to 0)
        let mut stack = stack_with(vec![Value::I32(1), Value::I32(32)]);
        i32_shl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // Shift by 33 (should wrap to 1)
        let mut stack = stack_with(vec![Value::I32(1), Value::I32(33)]);
        i32_shl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(2));
    }

    #[test]
    fn test_i32_shl_overflow() {
        // Test that bits are lost when shifted out
        // 0x80000000 << 1 should be 0 (MSB lost)
        let mut stack = stack_with(vec![Value::I32(0x80000000u32 as i32), Value::I32(1)]);
        i32_shl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0));

        // 0xFFFFFFFF << 1 should be 0xFFFFFFFE (LSB becomes 0)
        let mut stack = stack_with(vec![Value::I32(-1), Value::I32(1)]);
        i32_shl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(-2));

        // 0x40000000 << 1 should be 0x80000000 (becomes negative)
        let mut stack = stack_with(vec![Value::I32(0x40000000), Value::I32(1)]);
        i32_shl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x80000000u32 as i32));

        // Test large shift counts with modulo
        // 0xFF << 100 = 0xFF << (100 % 32) = 0xFF << 4 = 0xFF0
        let mut stack = stack_with(vec![Value::I32(0xFF), Value::I32(100)]);
        i32_shl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0xFF0));
    }

    #[test]
    fn test_i32_shr_s() {
        // Positive number
        let mut stack = stack_with(vec![Value::I32(16), Value::I32(4)]);
        i32_shr_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // Negative number (sign extension)
        let mut stack = stack_with(vec![Value::I32(-16), Value::I32(4)]);
        i32_shr_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(-1));

        // Shift by 32 (should wrap to 0)
        let mut stack = stack_with(vec![Value::I32(0x80000000u32 as i32), Value::I32(32)]);
        i32_shr_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x80000000u32 as i32));
    }

    #[test]
    fn test_i32_shr_s_sign_extension() {
        // Test that all high bits are set when shifting negative numbers
        // -1 >> 1 should be -1 (all bits set)
        let mut stack = stack_with(vec![Value::I32(-1), Value::I32(1)]);
        i32_shr_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(-1));

        // -1 >> 31 should be -1 (sign bit propagated)
        let mut stack = stack_with(vec![Value::I32(-1), Value::I32(31)]);
        i32_shr_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(-1));

        // 0x80000000 >> 1 should be 0xC0000000 (sign extended)
        let mut stack = stack_with(vec![Value::I32(0x80000000u32 as i32), Value::I32(1)]);
        i32_shr_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0xC0000000u32 as i32));

        // 0x80000000 >> 31 should be -1 (all bits from sign)
        let mut stack = stack_with(vec![Value::I32(0x80000000u32 as i32), Value::I32(31)]);
        i32_shr_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(-1));

        // Test boundary: largest negative >> 1
        let mut stack = stack_with(vec![Value::I32(i32::MIN), Value::I32(1)]);
        i32_shr_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(-1073741824)); // 0xC0000000
    }

    #[test]
    fn test_i32_shr_u() {
        // Basic shift
        let mut stack = stack_with(vec![Value::I32(16), Value::I32(4)]);
        i32_shr_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // Negative number (no sign extension)
        let mut stack = stack_with(vec![Value::I32(-16), Value::I32(4)]);
        i32_shr_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x0FFFFFFFu32 as i32));

        // High bit set
        let mut stack = stack_with(vec![Value::I32(0x80000000u32 as i32), Value::I32(1)]);
        i32_shr_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x40000000));
    }

    #[test]
    fn test_i32_shr_u_zero_extension() {
        // Test that high bits are always zero-filled
        // -1 >>> 1 should be 0x7FFFFFFF (zero extended)
        let mut stack = stack_with(vec![Value::I32(-1), Value::I32(1)]);
        i32_shr_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x7FFFFFFF));

        // -1 >>> 31 should be 1 (only LSB remains)
        let mut stack = stack_with(vec![Value::I32(-1), Value::I32(31)]);
        i32_shr_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // 0x80000000 >>> 31 should be 1 (only MSB shifts to LSB)
        let mut stack = stack_with(vec![Value::I32(0x80000000u32 as i32), Value::I32(31)]);
        i32_shr_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(1));

        // Test i32::MIN >>> 1 = 0x40000000
        let mut stack = stack_with(vec![Value::I32(i32::MIN), Value::I32(1)]);
        i32_shr_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x40000000));

        // -2 >>> 1 should be 0x7FFFFFFF (not -1)
        let mut stack = stack_with(vec![Value::I32(-2), Value::I32(1)]);
        i32_shr_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x7FFFFFFF));
    }

    #[test]
    fn test_i32_rotl() {
        // Basic rotation
        let mut stack = stack_with(vec![Value::I32(0x80000001u32 as i32), Value::I32(1)]);
        i32_rotl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x00000003));

        // Rotate by 0
        let mut stack = stack_with(vec![Value::I32(0x12345678), Value::I32(0)]);
        i32_rotl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x12345678));

        // Rotate by 32 (full rotation)
        let mut stack = stack_with(vec![Value::I32(0x12345678), Value::I32(32)]);
        i32_rotl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x12345678));

        // Rotate by 4
        let mut stack = stack_with(vec![Value::I32(0x12345678), Value::I32(4)]);
        i32_rotl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x23456781));
    }

    #[test]
    fn test_i32_rotl_large_counts() {
        // Test rotation with counts > 32 (should wrap)
        // Rotate by 33 = rotate by 1
        let mut stack = stack_with(vec![Value::I32(0x80000001u32 as i32), Value::I32(33)]);
        i32_rotl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x00000003));

        // Rotate by 100 = rotate by 4 (100 % 32)
        let mut stack = stack_with(vec![Value::I32(0x12345678), Value::I32(100)]);
        i32_rotl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x23456781));

        // Test with negative rotation count (treated as large unsigned)
        // -1 as u32 = 0xFFFFFFFF, modulo 32 = 31
        // Rotating left by 31 is same as rotating right by 1
        let mut stack = stack_with(vec![Value::I32(0x80000001u32 as i32), Value::I32(-1)]);
        i32_rotl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0xC0000000u32 as i32));
    }

    #[test]
    fn test_i32_rotr() {
        // Basic rotation
        let mut stack = stack_with(vec![Value::I32(0x80000001u32 as i32), Value::I32(1)]);
        i32_rotr(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0xC0000000u32 as i32));

        // Rotate by 4
        let mut stack = stack_with(vec![Value::I32(0x12345678), Value::I32(4)]);
        i32_rotr(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x81234567u32 as i32));
    }

    #[test]
    fn test_i32_rotr_edge_cases() {
        // Rotate all ones
        let mut stack = stack_with(vec![Value::I32(-1), Value::I32(1)]);
        i32_rotr(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(-1)); // All bits stay set

        // Rotate by 31 (almost full rotation)
        let mut stack = stack_with(vec![Value::I32(0x80000001u32 as i32), Value::I32(31)]);
        i32_rotr(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0x00000003));

        // Large rotation count
        let mut stack = stack_with(vec![Value::I32(0x80000001u32 as i32), Value::I32(65)]);
        i32_rotr(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I32(0xC0000000u32 as i32)); // Same as rotate by 1
    }

    // ============================================================================
    // i64 Bitwise Tests
    // ============================================================================

    #[test]
    fn test_i64_and() {
        let mut stack = stack_with(vec![
            Value::I64(0xFF00FF00FF00FF00u64 as i64),
            Value::I64(0x0F0F0F0F0F0F0F0F),
        ]);
        i64_and(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x0F000F000F000F00));
    }

    #[test]
    fn test_i64_or() {
        let mut stack = stack_with(vec![Value::I64(0), Value::I64(0x123456789ABCDEF0u64 as i64)]);
        i64_or(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x123456789ABCDEF0u64 as i64));
    }

    #[test]
    fn test_i64_xor() {
        let mut stack = stack_with(vec![Value::I64(-1), Value::I64(0x123456789ABCDEF0u64 as i64)]);
        i64_xor(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0xEDCBA9876543210Fu64 as i64));
    }

    #[test]
    fn test_i64_bitwise_patterns() {
        // Test 64-bit specific patterns
        // Swap halves using rotations and masks
        let value = 0x123456789ABCDEF0u64 as i64;

        // Extract high 32 bits: (x >> 32) & 0xFFFFFFFF
        let mut stack = stack_with(vec![Value::I64(value), Value::I64(32)]);
        i64_shr_u(&mut stack).unwrap();
        let high = stack.pop().unwrap();
        assert_eq!(high, Value::I64(0x12345678));

        // Extract low 32 bits: x & 0xFFFFFFFF
        let mut stack = stack_with(vec![Value::I64(value), Value::I64(0xFFFFFFFF)]);
        i64_and(&mut stack).unwrap();
        let low = stack.pop().unwrap();
        assert_eq!(low, Value::I64(0x9ABCDEF0u32 as i64));

        // Test sign bit manipulation
        // Clear sign bit: x & 0x7FFFFFFFFFFFFFFF
        let mut stack = stack_with(vec![Value::I64(-1), Value::I64(0x7FFFFFFFFFFFFFFF)]);
        i64_and(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x7FFFFFFFFFFFFFFF));

        // Set sign bit: x | 0x8000000000000000
        let mut stack = stack_with(vec![Value::I64(1), Value::I64(0x8000000000000000u64 as i64)]);
        i64_or(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x8000000000000001u64 as i64));

        // Toggle sign bit: x ^ 0x8000000000000000
        let mut stack = stack_with(vec![
            Value::I64(0x7FFFFFFFFFFFFFFF),
            Value::I64(0x8000000000000000u64 as i64),
        ]);
        i64_xor(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(-1));
    }

    #[test]
    fn test_i64_shl() {
        // Basic shift
        let mut stack = stack_with(vec![Value::I64(1), Value::I64(8)]);
        i64_shl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(256));

        // Shift by 64 (should wrap to 0)
        let mut stack = stack_with(vec![Value::I64(1), Value::I64(64)]);
        i64_shl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(1));

        // Shift by 65 (should wrap to 1)
        let mut stack = stack_with(vec![Value::I64(1), Value::I64(65)]);
        i64_shl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(2));
    }

    #[test]
    fn test_i64_shl_overflow() {
        // MSB lost when shifted
        let mut stack = stack_with(vec![Value::I64(0x8000000000000000u64 as i64), Value::I64(1)]);
        i64_shl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0));

        // -1 << 1 = -2
        let mut stack = stack_with(vec![Value::I64(-1), Value::I64(1)]);
        i64_shl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(-2));

        // Test sign change
        let mut stack = stack_with(vec![Value::I64(0x4000000000000000), Value::I64(1)]);
        i64_shl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x8000000000000000u64 as i64));

        // Large shift count with modulo
        let mut stack = stack_with(vec![Value::I64(0xFF), Value::I64(200)]);
        i64_shl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0xFF00)); // 200 % 64 = 8
    }

    #[test]
    fn test_i64_shr_s() {
        // Positive number
        let mut stack = stack_with(vec![Value::I64(256), Value::I64(8)]);
        i64_shr_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(1));

        // Negative number (sign extension)
        let mut stack = stack_with(vec![Value::I64(-256), Value::I64(8)]);
        i64_shr_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(-1));
    }

    #[test]
    fn test_i64_shr_s_sign_extension() {
        // -1 >> 1 should be -1 (all bits set)
        let mut stack = stack_with(vec![Value::I64(-1), Value::I64(1)]);
        i64_shr_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(-1));

        // -1 >> 63 should be -1 (sign bit propagated)
        let mut stack = stack_with(vec![Value::I64(-1), Value::I64(63)]);
        i64_shr_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(-1));

        // 0x8000000000000000 >> 1 should be 0xC000000000000000
        let mut stack = stack_with(vec![Value::I64(0x8000000000000000u64 as i64), Value::I64(1)]);
        i64_shr_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0xC000000000000000u64 as i64));

        // 0x8000000000000000 >> 63 should be -1
        let mut stack = stack_with(vec![Value::I64(0x8000000000000000u64 as i64), Value::I64(63)]);
        i64_shr_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(-1));

        // i64::MIN >> 1
        let mut stack = stack_with(vec![Value::I64(i64::MIN), Value::I64(1)]);
        i64_shr_s(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(-4611686018427387904i64));
    }

    #[test]
    fn test_i64_shr_u() {
        // High bit set
        let mut stack = stack_with(vec![Value::I64(0x8000000000000000u64 as i64), Value::I64(1)]);
        i64_shr_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x4000000000000000));

        // Negative number (no sign extension)
        let mut stack = stack_with(vec![Value::I64(-256), Value::I64(8)]);
        i64_shr_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x00FFFFFFFFFFFFFFu64 as i64));
    }

    #[test]
    fn test_i64_shr_u_zero_extension() {
        // -1 >>> 1 should be 0x7FFFFFFFFFFFFFFF
        let mut stack = stack_with(vec![Value::I64(-1), Value::I64(1)]);
        i64_shr_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x7FFFFFFFFFFFFFFF));

        // -1 >>> 63 should be 1
        let mut stack = stack_with(vec![Value::I64(-1), Value::I64(63)]);
        i64_shr_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(1));

        // 0x8000000000000000 >>> 63 should be 1
        let mut stack = stack_with(vec![Value::I64(0x8000000000000000u64 as i64), Value::I64(63)]);
        i64_shr_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(1));

        // i64::MIN >>> 1 = 0x4000000000000000
        let mut stack = stack_with(vec![Value::I64(i64::MIN), Value::I64(1)]);
        i64_shr_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x4000000000000000));

        // -2 >>> 1 should be 0x7FFFFFFFFFFFFFFF
        let mut stack = stack_with(vec![Value::I64(-2), Value::I64(1)]);
        i64_shr_u(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x7FFFFFFFFFFFFFFF));
    }

    #[test]
    fn test_i64_rotl() {
        // Rotate by 8
        let mut stack = stack_with(vec![Value::I64(0x123456789ABCDEF0u64 as i64), Value::I64(8)]);
        i64_rotl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x3456789ABCDEF012u64 as i64));
    }

    #[test]
    fn test_i64_rotl_edge_cases() {
        // Rotate by 64 (full rotation)
        let mut stack = stack_with(vec![Value::I64(0x123456789ABCDEF0u64 as i64), Value::I64(64)]);
        i64_rotl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x123456789ABCDEF0u64 as i64));

        // Rotate by 65 (same as rotate by 1)
        let mut stack = stack_with(vec![Value::I64(0x8000000000000001u64 as i64), Value::I64(65)]);
        i64_rotl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x0000000000000003));

        // Test with all ones
        let mut stack = stack_with(vec![Value::I64(-1), Value::I64(32)]);
        i64_rotl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(-1));

        // Large rotation count
        let mut stack = stack_with(vec![Value::I64(0x123456789ABCDEF0u64 as i64), Value::I64(200)]);
        i64_rotl(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x3456789ABCDEF012u64 as i64));
        // 200 % 64 = 8
    }

    #[test]
    fn test_i64_rotr() {
        // Rotate by 8
        let mut stack = stack_with(vec![Value::I64(0x123456789ABCDEF0u64 as i64), Value::I64(8)]);
        i64_rotr(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0xF0123456789ABCDEu64 as i64));
    }

    #[test]
    fn test_i64_rotr_edge_cases() {
        // Rotate by 63 (almost full rotation)
        let mut stack = stack_with(vec![Value::I64(0x8000000000000001u64 as i64), Value::I64(63)]);
        i64_rotr(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x0000000000000003));

        // Test with alternating bits
        let mut stack = stack_with(vec![Value::I64(0xAAAAAAAAAAAAAAAAu64 as i64), Value::I64(1)]);
        i64_rotr(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x5555555555555555));

        // Negative rotation count (wraps around)
        let mut stack = stack_with(vec![Value::I64(0x8000000000000001u64 as i64), Value::I64(-1)]);
        i64_rotr(&mut stack).unwrap();
        assert_eq!(stack.pop().unwrap(), Value::I64(0x0000000000000003));
    }
}
