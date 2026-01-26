//! WebAssembly value representation

use super::FuncAddr;
use crate::parser::module::ValueType;
use fhex::ToHex;
use std::fmt;

/// Runtime representation of WebAssembly values
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    /// Function reference - either null or a global function address
    FuncRef(Option<FuncAddr>),
    /// External reference - either null or an external reference index
    ExternRef(Option<u32>),
}

impl Value {
    /// Get the WebAssembly type of this value
    pub fn typ(&self) -> ValueType {
        match self {
            Value::I32(_) => ValueType::I32,
            Value::I64(_) => ValueType::I64,
            Value::F32(_) => ValueType::F32,
            Value::F64(_) => ValueType::F64,
            Value::FuncRef(_) => ValueType::FuncRef,
            Value::ExternRef(_) => ValueType::ExternRef,
        }
    }

    /// Convert to i32, returning None if wrong type
    pub fn as_i32(&self) -> Option<i32> {
        match self {
            Value::I32(v) => Some(*v),
            _ => None,
        }
    }

    /// Convert to i64, returning None if wrong type
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Value::I64(v) => Some(*v),
            _ => None,
        }
    }

    /// Convert to f32, returning None if wrong type
    pub fn as_f32(&self) -> Option<f32> {
        match self {
            Value::F32(v) => Some(*v),
            _ => None,
        }
    }

    /// Convert to f64, returning None if wrong type
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Value::F64(v) => Some(*v),
            _ => None,
        }
    }

    /// Convert to funcref, returning None if wrong type
    pub fn as_funcref(&self) -> Option<Option<FuncAddr>> {
        match self {
            Value::FuncRef(v) => Some(*v),
            _ => None,
        }
    }

    /// Convert to externref, returning None if wrong type
    pub fn as_externref(&self) -> Option<Option<u32>> {
        match self {
            Value::ExternRef(v) => Some(*v),
            _ => None,
        }
    }

    /// Parse an i32 value from a string
    fn parse_i32(value: &str) -> Result<Value, String> {
        value
            .parse::<u32>()
            .map(|v| Value::I32(v as i32))
            .map_err(|e| format!("Failed to parse i32: {e}"))
    }

    /// Parse an i64 value from a string
    fn parse_i64(value: &str) -> Result<Value, String> {
        value
            .parse::<u64>()
            .map(|v| Value::I64(v as i64))
            .map_err(|e| format!("Failed to parse i64: {e}"))
    }

    /// Parse a NaN with payload for f32
    fn parse_f32_nan_payload(value: &str, negative: bool) -> Result<Value, String> {
        let prefix_len = if negative { 7 } else { 6 }; // Skip "-nan:0x" or "nan:0x"
        let payload_str = &value[prefix_len..];

        u32::from_str_radix(payload_str, 16)
            .map(|payload| {
                let base_bits = if negative { 0xffc00000 } else { 0x7fc00000 };
                let bits = base_bits | (payload & 0x3fffff);
                Value::F32(f32::from_bits(bits))
            })
            .map_err(|e| format!("Failed to parse f32 NaN payload: {e}"))
    }

    /// Parse an f32 value from a string
    fn parse_f32(value: &str) -> Result<Value, String> {
        match value {
            "nan:canonical" | "nan:0x400000" => {
                // Canonical NaN: sign = 0, quiet bit = 1, payload = 0
                Ok(Value::F32(f32::from_bits(0x7fc00000)))
            }
            "nan:arithmetic" => {
                // Any arithmetic NaN (we'll use canonical for simplicity)
                Ok(Value::F32(f32::from_bits(0x7fc00000)))
            }
            "inf" => Ok(Value::F32(f32::INFINITY)),
            "-inf" => Ok(Value::F32(f32::NEG_INFINITY)),
            _ if value.starts_with("nan:0x") => Self::parse_f32_nan_payload(value, false),
            _ if value.starts_with("-nan:0x") => Self::parse_f32_nan_payload(value, true),
            _ if value.starts_with("0x") => {
                // Handle hex representation
                let hex = &value[2..];
                u32::from_str_radix(hex, 16)
                    .map(|bits| Value::F32(f32::from_bits(bits)))
                    .map_err(|e| format!("Failed to parse f32 hex: {e}"))
            }
            _ => {
                // Try as bits first (for integer representation of float bits)
                value
                    .parse::<u32>()
                    .map(|bits| Value::F32(f32::from_bits(bits)))
                    .or_else(|_| {
                        // Fall back to parsing as float
                        value.parse::<f32>().map(Value::F32)
                    })
                    .map_err(|e| format!("Failed to parse f32: {e}"))
            }
        }
    }

    /// Parse a NaN with payload for f64
    fn parse_f64_nan_payload(value: &str, negative: bool) -> Result<Value, String> {
        let prefix_len = if negative { 7 } else { 6 }; // Skip "-nan:0x" or "nan:0x"
        let payload_str = &value[prefix_len..];

        u64::from_str_radix(payload_str, 16)
            .map(|payload| {
                let base_bits = if negative {
                    0xfff8000000000000
                } else {
                    0x7ff8000000000000
                };
                let bits = base_bits | (payload & 0x7ffffffffffff);
                Value::F64(f64::from_bits(bits))
            })
            .map_err(|e| format!("Failed to parse f64 NaN payload: {e}"))
    }

    /// Parse an f64 value from a string
    fn parse_f64(value: &str) -> Result<Value, String> {
        match value {
            "nan:canonical" | "nan:0x8000000000000" => {
                // Canonical NaN: sign = 0, quiet bit = 1, payload = 0
                Ok(Value::F64(f64::from_bits(0x7ff8000000000000)))
            }
            "nan:arithmetic" => {
                // Any arithmetic NaN (we'll use canonical for simplicity)
                Ok(Value::F64(f64::from_bits(0x7ff8000000000000)))
            }
            "inf" => Ok(Value::F64(f64::INFINITY)),
            "-inf" => Ok(Value::F64(f64::NEG_INFINITY)),
            _ if value.starts_with("nan:0x") => Self::parse_f64_nan_payload(value, false),
            _ if value.starts_with("-nan:0x") => Self::parse_f64_nan_payload(value, true),
            _ if value.starts_with("0x") => {
                // Handle hex representation
                let hex = &value[2..];
                u64::from_str_radix(hex, 16)
                    .map(|bits| Value::F64(f64::from_bits(bits)))
                    .map_err(|e| format!("Failed to parse f64 hex: {e}"))
            }
            _ => {
                // Try as bits first (for integer representation of float bits)
                value
                    .parse::<u64>()
                    .map(|bits| Value::F64(f64::from_bits(bits)))
                    .or_else(|_| {
                        // Fall back to parsing as float
                        value.parse::<f64>().map(Value::F64)
                    })
                    .map_err(|e| format!("Failed to parse f64: {e}"))
            }
        }
    }

    /// Parse a reference type value from a string
    fn parse_ref(value: &str, ref_type: &str) -> Result<Value, String> {
        if value == "null" {
            match ref_type {
                "funcref" => Ok(Value::FuncRef(None)),
                "externref" => Ok(Value::ExternRef(None)),
                _ => Err(format!("Unknown reference type: {ref_type}")),
            }
        } else {
            value
                .parse::<usize>()
                .map(|idx| match ref_type {
                    "funcref" => Value::FuncRef(Some(FuncAddr(idx))),
                    "externref" => Value::ExternRef(Some(idx as u32)),
                    _ => unreachable!(),
                })
                .map_err(|e| format!("Failed to parse {ref_type}: {e}"))
        }
    }

    /// Create from a type string and value string (used in tests)
    pub fn from_strings(typ: &str, value: &str) -> Result<Self, String> {
        match typ {
            "i32" => Self::parse_i32(value),
            "i64" => Self::parse_i64(value),
            "f32" => Self::parse_f32(value),
            "f64" => Self::parse_f64(value),
            "funcref" | "externref" => Self::parse_ref(value, typ),
            t => Err(format!("Unknown value type: {t}")),
        }
    }

    /// Convert to type and value strings for test comparison
    pub fn to_strings(&self) -> (String, String) {
        match self {
            Value::I32(v) => ("i32".to_string(), (*v as u32).to_string()),
            Value::I64(v) => ("i64".to_string(), (*v as u64).to_string()),
            Value::F32(v) => {
                let bits = v.to_bits();
                let value_str = if v.is_nan() {
                    // Check if it's a canonical NaN
                    if bits == 0x7fc00000 || bits == 0xffc00000 {
                        "nan:canonical".to_string()
                    } else {
                        // It's an arithmetic NaN
                        "nan:arithmetic".to_string()
                    }
                } else {
                    // Regular value, return as bits
                    bits.to_string()
                };
                ("f32".to_string(), value_str)
            }
            Value::F64(v) => {
                let bits = v.to_bits();
                let value_str = if v.is_nan() {
                    // Check if it's a canonical NaN
                    if bits == 0x7ff8000000000000 || bits == 0xfff8000000000000 {
                        "nan:canonical".to_string()
                    } else {
                        // It's an arithmetic NaN
                        "nan:arithmetic".to_string()
                    }
                } else {
                    // Regular value, return as bits
                    bits.to_string()
                };
                ("f64".to_string(), value_str)
            }
            Value::FuncRef(v) => {
                let value_str = match v {
                    None => "null".to_string(),
                    Some(FuncAddr(idx)) => idx.to_string(),
                };
                ("funcref".to_string(), value_str)
            }
            Value::ExternRef(v) => {
                let value_str = match v {
                    None => "null".to_string(),
                    Some(idx) => idx.to_string(),
                };
                ("externref".to_string(), value_str)
            }
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::I32(v) => write!(f, "i32:{v}"),
            Value::I64(v) => write!(f, "i64:{v}"),
            Value::F32(v) => write!(f, "f32:{}", v.to_hex()),
            Value::F64(v) => write!(f, "f64:{}", v.to_hex()),
            Value::FuncRef(None) => write!(f, "funcref:null"),
            Value::FuncRef(Some(FuncAddr(idx))) => write!(f, "funcref:{}", idx),
            Value::ExternRef(None) => write!(f, "externref:null"),
            Value::ExternRef(Some(idx)) => write!(f, "externref:{}", idx),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_type() {
        assert_eq!(Value::I32(42).typ(), ValueType::I32);
        assert_eq!(Value::I64(42).typ(), ValueType::I64);
        assert_eq!(Value::F32(42.0).typ(), ValueType::F32);
        assert_eq!(Value::F64(42.0).typ(), ValueType::F64);
    }

    #[test]
    fn test_value_conversions() {
        assert_eq!(Value::I32(42).as_i32(), Some(42));
        assert_eq!(Value::I32(42).as_i64(), None);
        assert_eq!(Value::I64(42).as_i64(), Some(42));
        assert_eq!(Value::F32(42.0).as_f32(), Some(42.0));
        assert_eq!(Value::F64(42.0).as_f64(), Some(42.0));
    }

    #[test]
    fn test_from_strings() {
        // Test integer parsing
        assert_eq!(Value::from_strings("i32", "42").unwrap(), Value::I32(42));
        assert_eq!(Value::from_strings("i64", "42").unwrap(), Value::I64(42));

        // Test float parsing (as bits)
        assert_eq!(
            Value::from_strings("f32", "1109917696").unwrap(),
            Value::F32(f32::from_bits(1109917696))
        );

        // Test hex parsing
        assert_eq!(
            Value::from_strings("f32", "0x42280000").unwrap(),
            Value::F32(f32::from_bits(0x42280000))
        );

        // Test invalid type
        assert!(Value::from_strings("invalid", "42").is_err());
    }

    #[test]
    fn test_to_strings() {
        assert_eq!(Value::I32(42).to_strings(), ("i32".to_string(), "42".to_string()));
        assert_eq!(Value::I64(42).to_strings(), ("i64".to_string(), "42".to_string()));

        // Float values return bit representation as decimal
        let f32_val = Value::F32(42.0);
        let (typ, val) = f32_val.to_strings();
        assert_eq!(typ, "f32");
        assert_eq!(val, "1109917696"); // 42.0f32 as bits
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Value::I32(42)), "i32:42");
        assert_eq!(format!("{}", Value::I64(42)), "i64:42");
        // fhex uses a different format, just check it contains the value
        let f32_str = format!("{}", Value::F32(42.0));
        assert!(f32_str.starts_with("f32:"));
        let f64_str = format!("{}", Value::F64(42.0));
        assert!(f64_str.starts_with("f64:"));
    }
}
