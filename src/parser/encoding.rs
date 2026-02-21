//! Binary encoding primitives for WebAssembly values.
//!
//! Provides LEB128 integer encoding, IEEE 754 float encoding, and byte vector
//! encoding as specified by the WebAssembly binary format.
//!
//! All functions write directly into a caller-provided `&mut Vec<u8>` buffer,
//! avoiding intermediate allocations.

use byteorder::{LittleEndian, WriteBytesExt};
use std::io;

// ---------------------------------------------------------------------------
// WebAssembly binary format constants (spec section 5)
// ---------------------------------------------------------------------------

// Section IDs (§5.5.2)
pub const SECTION_CUSTOM: u8 = 0;
pub const SECTION_TYPE: u8 = 1;
pub const SECTION_IMPORT: u8 = 2;
pub const SECTION_FUNCTION: u8 = 3;
pub const SECTION_TABLE: u8 = 4;
pub const SECTION_MEMORY: u8 = 5;
pub const SECTION_GLOBAL: u8 = 6;
pub const SECTION_EXPORT: u8 = 7;
pub const SECTION_START: u8 = 8;
pub const SECTION_ELEMENT: u8 = 9;
pub const SECTION_CODE: u8 = 10;
pub const SECTION_DATA: u8 = 11;
pub const SECTION_DATA_COUNT: u8 = 12;

// Type constructors (§5.3.6)
pub const TYPE_FUNC: u8 = 0x60;

// Import/export descriptor kinds (§5.5.5, §5.5.10)
pub const DESC_FUNC: u8 = 0x00;
pub const DESC_TABLE: u8 = 0x01;
pub const DESC_MEMORY: u8 = 0x02;
pub const DESC_GLOBAL: u8 = 0x03;

// Element segment elemkind (§5.5.12)
pub const ELEMKIND_FUNCREF: u8 = 0x00;

// Element segment flags (§5.5.12)
// 3-bit encoding: bit 0 = non-active mode, bit 1 = explicit table, bit 2 = expressions
pub const ELEM_ACTIVE_FUNCS: u32 = 0; // active, table 0, func indices
pub const ELEM_PASSIVE_FUNCS: u32 = 1; // passive, elemkind, func indices
pub const ELEM_ACTIVE_TABLE_FUNCS: u32 = 2; // active, explicit table, elemkind, func indices
pub const ELEM_DECLARATIVE_FUNCS: u32 = 3; // declarative, elemkind, func indices
pub const ELEM_ACTIVE_EXPRS: u32 = 4; // active, table 0, expressions
pub const ELEM_PASSIVE_EXPRS: u32 = 5; // passive, reftype, expressions
pub const ELEM_ACTIVE_TABLE_EXPRS: u32 = 6; // active, explicit table, reftype, expressions
pub const ELEM_DECLARATIVE_EXPRS: u32 = 7; // declarative, reftype, expressions

// Data segment flags (§5.5.14)
pub const DATA_ACTIVE: u32 = 0;
pub const DATA_PASSIVE: u32 = 1;
pub const DATA_ACTIVE_EXPLICIT: u32 = 2;

// Expression terminator (§5.4.9)
pub const OP_END: u8 = 0x0B;

// Block type: empty (§5.4.1)
pub const BLOCK_TYPE_EMPTY: u8 = 0x40;

// ---------------------------------------------------------------------------
// Unsigned LEB128
// ---------------------------------------------------------------------------

/// Appends the unsigned LEB128 encoding of a u64 value to `buf`.
fn write_vu(buf: &mut Vec<u8>, mut value: u64) {
    loop {
        let mut byte = (value & 0x7f) as u8;
        value >>= 7;
        if value == 0 {
            buf.push(byte);
            break;
        }
        byte |= 0x80;
        buf.push(byte);
    }
}

/// Appends the unsigned LEB128 encoding of a u32 value to `buf`.
pub fn write_vu32(buf: &mut Vec<u8>, v: u32) {
    write_vu(buf, v as u64);
}

/// Appends the unsigned LEB128 encoding of a u64 value to `buf`.
pub fn write_vu64(buf: &mut Vec<u8>, v: u64) {
    write_vu(buf, v);
}

/// Appends a single-bit boolean as a one-byte LEB128 value (0x00 or 0x01).
pub fn write_vu1(buf: &mut Vec<u8>, v: bool) {
    buf.push(if v { 1 } else { 0 });
}

// ---------------------------------------------------------------------------
// Signed LEB128
// ---------------------------------------------------------------------------

/// Appends the signed LEB128 encoding of an i64 value to `buf`.
fn write_vs(buf: &mut Vec<u8>, mut value: i64) {
    loop {
        let mut byte = (value & 0x7f) as u8;
        value >>= 7;
        if (value == 0 && (byte & 0x40) == 0) || (value == -1 && (byte & 0x40) != 0) {
            buf.push(byte);
            break;
        }
        byte |= 0x80;
        buf.push(byte);
    }
}

/// Appends the signed LEB128 encoding of an i32 value to `buf`.
pub fn write_vs32(buf: &mut Vec<u8>, v: i32) {
    write_vs(buf, v as i64);
}

/// Appends the signed LEB128 encoding of an i64 value to `buf`.
pub fn write_vs64(buf: &mut Vec<u8>, v: i64) {
    write_vs(buf, v);
}

// ---------------------------------------------------------------------------
// IEEE 754 floats (little-endian)
// ---------------------------------------------------------------------------

/// Appends the little-endian IEEE 754 encoding of an f32 value to `buf`.
pub fn write_f32(buf: &mut Vec<u8>, v: f32) {
    let mut bytes = [0u8; 4];
    let mut wtr = io::Cursor::new(&mut bytes[..]);
    wtr.write_f32::<LittleEndian>(v).unwrap();
    buf.extend_from_slice(&bytes);
}

/// Appends the little-endian IEEE 754 encoding of an f64 value to `buf`.
pub fn write_f64(buf: &mut Vec<u8>, v: f64) {
    let mut bytes = [0u8; 8];
    let mut wtr = io::Cursor::new(&mut bytes[..]);
    wtr.write_f64::<LittleEndian>(v).unwrap();
    buf.extend_from_slice(&bytes);
}

// ---------------------------------------------------------------------------
// v128 (16-byte SIMD vector, raw bytes)
// ---------------------------------------------------------------------------

/// Appends 16 raw bytes to `buf`.
pub fn write_v128(buf: &mut Vec<u8>, v: [u8; 16]) {
    buf.extend_from_slice(&v);
}

// ---------------------------------------------------------------------------
// Length-prefixed byte vector
// ---------------------------------------------------------------------------

/// Appends a length-prefixed byte vector (vu32 length + raw bytes) to `buf`.
pub fn write_u8vec(buf: &mut Vec<u8>, v: &[u8]) {
    write_vu32(buf, v.len() as u32);
    buf.extend_from_slice(v);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_eq_with_diag<T: std::fmt::Debug + std::cmp::PartialEq>(actual: T, expected: T) {
        assert!(
            actual == expected,
            "Assertion failed. Actual: {actual:?}, Expected: {expected:?}",
        );
    }

    /// Encodes a u32 via write_vu32 and returns the resulting bytes.
    fn encode_vu32(v: u32) -> Vec<u8> {
        let mut buf = Vec::new();
        write_vu32(&mut buf, v);
        buf
    }

    /// Encodes a u64 via write_vu64 and returns the resulting bytes.
    fn encode_vu64(v: u64) -> Vec<u8> {
        let mut buf = Vec::new();
        write_vu64(&mut buf, v);
        buf
    }

    /// Encodes an i32 via write_vs32 and returns the resulting bytes.
    fn encode_vs32(v: i32) -> Vec<u8> {
        let mut buf = Vec::new();
        write_vs32(&mut buf, v);
        buf
    }

    /// Encodes an i64 via write_vs64 and returns the resulting bytes.
    fn encode_vs64(v: i64) -> Vec<u8> {
        let mut buf = Vec::new();
        write_vs64(&mut buf, v);
        buf
    }

    // -- Unsigned LEB128 --

    #[test]
    fn test_write_vu32() {
        assert_eq_with_diag(encode_vu32(0), vec![0]);
        assert_eq_with_diag(encode_vu32(1), vec![1]);
        assert_eq_with_diag(encode_vu32(624485), vec![0b11100101, 0b10001110, 0b00100110]);
        assert_eq_with_diag(encode_vu32(127), vec![0x7f]);
        assert_eq_with_diag(encode_vu32(16256), vec![0x80, 0x7f]);
        assert_eq_with_diag(encode_vu32(0x3b4), vec![0xb4, 0x07]);
        assert_eq_with_diag(encode_vu32(0x40c), vec![0x8c, 0x08]);
        assert_eq_with_diag(encode_vu32(0xffffffff), vec![0xff, 0xff, 0xff, 0xff, 0xf]);
        assert_eq_with_diag(encode_vu32(0x80000000), vec![128, 128, 128, 128, 8]);
    }

    #[test]
    fn test_rt_vu32() {
        use crate::parser::reader::Reader;

        let mut test_values = vec![0, 1, u32::MAX, u32::MIN, 128, 129, 130, 624485];

        for i in 0..31 {
            let value = 1u32 << i;
            test_values.push(value);
            test_values.push(value + 1);
            test_values.push(value - 1);
        }

        for i in 1..4 {
            let max_value = (1u32 << (i * 8)) - 1;
            test_values.push(max_value);
        }

        use rand::RngExt;
        let mut rng = rand::rng();
        for _ in 0..100 {
            test_values.push(rng.random::<u32>());
        }

        for i in 0..1000 {
            test_values.push(i);
        }

        for &expected in &test_values {
            let byts = encode_vu32(expected);
            let mut reader = Reader::new(byts);
            let actual = reader.read_vu32().unwrap_or_else(|_| {
                panic!("Failed to read vu32");
            });
            assert_eq_with_diag(actual, expected);
        }
    }

    #[test]
    fn test_write_vu64() {
        assert_eq_with_diag(encode_vu64(0), vec![0]);
        assert_eq_with_diag(encode_vu64(1), vec![1]);
        assert_eq_with_diag(encode_vu64(624485), vec![0b11100101, 0b10001110, 0b00100110]);
        assert_eq_with_diag(encode_vu64(127), vec![0x7f]);
        assert_eq_with_diag(encode_vu64(16256), vec![0x80, 0x7f]);
        assert_eq_with_diag(encode_vu64(0x3b4), vec![0xb4, 0x07]);
        assert_eq_with_diag(encode_vu64(0x40c), vec![0x8c, 0x08]);
        assert_eq_with_diag(encode_vu64(0xffffffff), vec![0xff, 0xff, 0xff, 0xff, 0xf]);
        assert_eq_with_diag(encode_vu64(0x80000000), vec![128, 128, 128, 128, 8]);
    }

    #[test]
    fn test_rt_vu64() {
        use crate::parser::reader::Reader;

        let mut test_values = vec![0, 1, u64::MAX, u64::MIN, 128, 129, 130, 624485];

        for i in 0..63 {
            let value = 1u64 << i;
            test_values.push(value);
            test_values.push(value + 1);
            test_values.push(value - 1);
        }

        for i in 1..8 {
            let max_value = (1u64 << (i * 8)) - 1;
            test_values.push(max_value);
        }

        use rand::RngExt;
        let mut rng = rand::rng();
        for _ in 0..100 {
            test_values.push(rng.random::<u64>());
        }

        for i in 0..1000 {
            test_values.push(i);
        }

        for &expected in &test_values {
            let byts = encode_vu64(expected);
            let mut reader = Reader::new(byts);
            let actual = reader.read_vu64().unwrap_or_else(|_| {
                panic!("Failed to read vu64");
            });
            assert_eq_with_diag(actual, expected);
        }
    }

    #[test]
    fn test_write_vu1() {
        let mut buf = Vec::new();
        write_vu1(&mut buf, false);
        write_vu1(&mut buf, true);
        assert_eq!(buf, vec![0, 1]);
    }

    // -- Signed LEB128 --

    #[test]
    fn test_write_vs32() {
        assert_eq_with_diag(encode_vs32(0), vec![0]);
        assert_eq_with_diag(encode_vs32(1), vec![1]);
        assert_eq_with_diag(encode_vs32(624485), vec![0b11100101, 0b10001110, 0b00100110]);
        assert_eq_with_diag(encode_vs32(0x3b4), vec![0xb4, 0x07]);
        assert_eq_with_diag(encode_vs32(0x40c), vec![0x8c, 0x08]);
        assert_eq_with_diag(encode_vs32(-1), vec![0x7f]);
        assert_eq_with_diag(encode_vs32(-128), vec![0x80, 0x7f]);
        assert_eq_with_diag(encode_vs32(-624485), vec![0b10011011, 0b11110001, 0b01011001]);
        assert_eq_with_diag(encode_vs32(0x80000000u32 as i32), vec![128, 128, 128, 128, 120]);
    }

    #[test]
    fn test_rt_vs32() {
        use crate::parser::reader::Reader;

        let mut test_values = vec![
            0,
            1,
            -1,
            i32::MAX,
            i32::MIN,
            128,
            -128,
            129,
            -129,
            130,
            -130,
            624485,
            -624485,
        ];

        for i in 0..31 {
            let value = 1i32 << i;
            test_values.push(value);
            test_values.push(-value);
            test_values.push(value + 1);
            test_values.push(-value - 1);
            test_values.push(value - 1);
            test_values.push(-value + 1);
        }

        for i in 1..4 {
            let max_value = (1i32 << (i * 8)) - 1;
            let min_value = -(1i32 << (i * 8));
            test_values.push(max_value);
            test_values.push(min_value);
        }

        use rand::RngExt;
        let mut rng = rand::rng();
        for _ in 0..100 {
            test_values.push(rng.random::<i32>());
        }

        for i in -1000..1000 {
            test_values.push(i);
        }

        for &expected in &test_values {
            let byts = encode_vs32(expected);
            let mut reader = Reader::new(byts);
            let actual = reader.read_vs32().unwrap_or_else(|_| {
                panic!("Failed to read vs32");
            });
            assert_eq_with_diag(actual, expected);
        }
    }

    #[test]
    fn test_write_vs64() {
        assert_eq_with_diag(encode_vs64(0), vec![0]);
        assert_eq_with_diag(encode_vs64(1), vec![1]);
        assert_eq_with_diag(encode_vs64(624485), vec![0b11100101, 0b10001110, 0b00100110]);
        assert_eq_with_diag(encode_vs64(0x3b4), vec![0xb4, 0x07]);
        assert_eq_with_diag(encode_vs64(0x40c), vec![0x8c, 0x08]);
        assert_eq_with_diag(encode_vs64(-1), vec![0x7f]);
        assert_eq_with_diag(encode_vs64(-128), vec![0x80, 0x7f]);
        assert_eq_with_diag(encode_vs64(-624485), vec![0b10011011, 0b11110001, 0b01011001]);
        assert_eq_with_diag(
            encode_vs64(0x7ff8000000000000),
            vec![128, 128, 128, 128, 128, 128, 128, 252, 255, 0],
        );
        assert_eq_with_diag(
            encode_vs64(0x8000000000000000u64 as i64),
            vec![128, 128, 128, 128, 128, 128, 128, 128, 128, 127],
        );
    }

    #[test]
    fn test_rt_vs64() {
        use crate::parser::reader::Reader;

        let mut test_values = vec![
            0,
            1,
            -1,
            i64::MAX,
            i64::MIN,
            128,
            -128,
            129,
            -129,
            130,
            -130,
            624485,
            -624485,
        ];

        for i in 0..63 {
            let value = 1i64 << i;
            test_values.push(value);
            test_values.push(-value);
            test_values.push(value + 1);
            test_values.push(-value - 1);
            test_values.push(value - 1);
            test_values.push(-value + 1);
        }

        for i in 1..8 {
            let max_value = (1i64 << (i * 8)) - 1;
            let min_value = -(1i64 << (i * 8));
            test_values.push(max_value);
            test_values.push(min_value);
        }

        use rand::RngExt;
        let mut rng = rand::rng();
        for _ in 0..100 {
            test_values.push(rng.random::<i64>());
        }

        for i in -1000..1000 {
            test_values.push(i);
        }

        for &expected in &test_values {
            let byts = encode_vs64(expected);
            let mut reader = Reader::new(byts);
            let actual = reader.read_vs64().unwrap_or_else(|_| {
                panic!("Failed to read vs64");
            });
            assert_eq_with_diag(actual, expected);
        }
    }

    // -- Direct buffer tests --

    #[test]
    fn test_write_vu32_into_buffer() {
        let mut buf = vec![0xAA];
        write_vu32(&mut buf, 624485);
        assert_eq!(buf, vec![0xAA, 0b11100101, 0b10001110, 0b00100110]);
    }

    #[test]
    fn test_write_vs32_into_buffer() {
        let mut buf = vec![0xBB];
        write_vs32(&mut buf, -1);
        assert_eq!(buf, vec![0xBB, 0x7f]);
    }

    #[test]
    fn test_write_f32_into_buffer() {
        let mut buf = Vec::new();
        write_f32(&mut buf, 6.283_185_5);
        assert_eq!(buf, vec![219, 15, 201, 64]);
    }

    #[test]
    fn test_write_f64_into_buffer() {
        let mut buf = Vec::new();
        write_f64(&mut buf, std::f64::consts::TAU);
        assert_eq!(buf, vec![24, 45, 68, 84, 251, 33, 25, 64]);
    }

    #[test]
    fn test_write_v128_into_buffer() {
        let mut buf = Vec::new();
        write_v128(&mut buf, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]);
        assert_eq!(buf, vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]);
    }

    #[test]
    fn test_write_u8vec_into_buffer() {
        let mut buf = Vec::new();
        write_u8vec(&mut buf, &[0xDE, 0xAD]);
        assert_eq!(buf, vec![2, 0xDE, 0xAD]);
    }
}
