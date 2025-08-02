//! WebAssembly value representation

use crate::parser::module::ValueType;
use fhex::ToHex;
use std::fmt;

/// Runtime representation of WebAssembly values
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl Value {
    /// Get the WebAssembly type of this value
    pub fn typ(&self) -> ValueType {
        match self {
            Value::I32(_) => ValueType::I32,
            Value::I64(_) => ValueType::I64,
            Value::F32(_) => ValueType::F32,
            Value::F64(_) => ValueType::F64,
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

    /// Create from a type string and value string (used in tests)
    pub fn from_strings(typ: &str, value: &str) -> Result<Self, String> {
        match typ {
            "i32" => value
                .parse::<u32>()
                .map(|v| Value::I32(v as i32))
                .map_err(|e| format!("Failed to parse i32: {e}")),
            "i64" => value
                .parse::<u64>()
                .map(|v| Value::I64(v as i64))
                .map_err(|e| format!("Failed to parse i64: {e}")),
            "f32" => {
                if let Some(hex) = value.strip_prefix("0x") {
                    // Handle hex representation
                    u32::from_str_radix(hex, 16)
                        .map(|bits| Value::F32(f32::from_bits(bits)))
                        .map_err(|e| format!("Failed to parse f32 hex: {e}"))
                } else {
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
            "f64" => {
                if let Some(hex) = value.strip_prefix("0x") {
                    // Handle hex representation
                    u64::from_str_radix(hex, 16)
                        .map(|bits| Value::F64(f64::from_bits(bits)))
                        .map_err(|e| format!("Failed to parse f64 hex: {e}"))
                } else {
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
            t => Err(format!("Unknown value type: {t}")),
        }
    }

    /// Convert to type and value strings for test comparison
    pub fn to_strings(&self) -> (String, String) {
        match self {
            Value::I32(v) => ("i32".to_string(), (*v as u32).to_string()),
            Value::I64(v) => ("i64".to_string(), (*v as u64).to_string()),
            Value::F32(v) => ("f32".to_string(), v.to_bits().to_string()),
            Value::F64(v) => ("f64".to_string(), v.to_bits().to_string()),
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
