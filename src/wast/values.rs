//! Value conversion and comparison for .wast test execution.
//!
//! Provides methods to convert `.wast` typed constants ([`WastValue`]) to runtime
//! [`Value`]s, and to compare runtime results against expected `.wast` values with
//! proper NaN tolerance as required by the WebAssembly specification.

use super::command::{WastFloat, WastLane, WastValue};
use crate::runtime::Value;

/// Convert a slice of [`WastValue`]s to runtime [`Value`]s (for function arguments).
pub fn convert_args(args: &[WastValue]) -> Result<Vec<Value>, String> {
    args.iter().map(|v| v.to_value()).collect()
}

/// Compare a vector of runtime results against expected `.wast` values.
///
/// Returns `Ok(())` if all match, or `Err(description)` for the first mismatch.
pub fn match_results(results: &[Value], expected: &[WastValue]) -> Result<(), String> {
    if results.len() != expected.len() {
        return Err(format!(
            "result count mismatch: expected {}, got {}",
            expected.len(),
            results.len()
        ));
    }
    for (i, (result, exp)) in results.iter().zip(expected).enumerate() {
        exp.matches(result).map_err(|e| format!("result {i}: {e}"))?;
    }
    Ok(())
}

impl WastValue {
    /// Convert this `.wast` value to a runtime [`Value`].
    ///
    /// NaN patterns (`NanCanonical`, `NanArithmetic`) produce representative bit
    /// patterns suitable for use as function arguments.
    pub fn to_value(&self) -> Result<Value, String> {
        match self {
            WastValue::I32(bits) => Ok(Value::I32(*bits as i32)),
            WastValue::I64(bits) => Ok(Value::I64(*bits as i64)),
            WastValue::F32(wf) => match wf {
                WastFloat::Value(bits) => Ok(Value::F32(f32::from_bits(*bits))),
                WastFloat::NanCanonical => Ok(Value::F32(f32::from_bits(0x7fc00000))),
                WastFloat::NanArithmetic => Ok(Value::F32(f32::from_bits(0x7fc00001))),
            },
            WastValue::F64(wf) => match wf {
                WastFloat::Value(bits) => Ok(Value::F64(f64::from_bits(*bits))),
                WastFloat::NanCanonical => Ok(Value::F64(f64::from_bits(0x7ff8000000000000))),
                WastFloat::NanArithmetic => Ok(Value::F64(f64::from_bits(0x7ff8000000000001))),
            },
            WastValue::V128 { lane_type, lanes } => {
                let base_type = normalize_lane_type(lane_type);
                let lane_strings: Vec<String> = lanes
                    .iter()
                    .map(|l| match l {
                        WastLane::Integer(v) => v.to_string(),
                        WastLane::F32(WastFloat::Value(bits)) => bits.to_string(),
                        WastLane::F32(WastFloat::NanCanonical) => 0x7fc00000u32.to_string(),
                        WastLane::F32(WastFloat::NanArithmetic) => 0x7fc00001u32.to_string(),
                        WastLane::F64(WastFloat::Value(bits)) => bits.to_string(),
                        WastLane::F64(WastFloat::NanCanonical) => 0x7ff8000000000000u64.to_string(),
                        WastLane::F64(WastFloat::NanArithmetic) => 0x7ff8000000000001u64.to_string(),
                    })
                    .collect();
                Value::from_v128_lanes(base_type, &lane_strings)
            }
            WastValue::RefNull(ref_type) => match ref_type.as_str() {
                "extern" | "externref" => Ok(Value::ExternRef(None)),
                _ => Ok(Value::FuncRef(None)),
            },
            WastValue::RefFunc => Ok(Value::FuncRef(None)),
            WastValue::RefExtern(idx) => Ok(Value::ExternRef(*idx)),
        }
    }

    /// Check whether a runtime [`Value`] matches this expected value.
    ///
    /// Returns `Ok(())` on match, or `Err(description)` describing the mismatch.
    /// Implements WebAssembly spec NaN comparison semantics: NaN sign bits are
    /// non-deterministic, `nan:arithmetic` accepts any quiet NaN, and
    /// `nan:canonical` requires the canonical NaN payload.
    pub fn matches(&self, actual: &Value) -> Result<(), String> {
        match self {
            WastValue::I32(bits) => {
                let (rtype, rval) = actual.to_strings();
                if rtype != "i32" {
                    return Err(format!("type mismatch: expected i32, got {rtype}"));
                }
                let expected_str = bits.to_string();
                if rval != expected_str {
                    return Err(format!("expected {expected_str}, got {rval}"));
                }
                Ok(())
            }
            WastValue::I64(bits) => {
                let (rtype, rval) = actual.to_strings();
                if rtype != "i64" {
                    return Err(format!("type mismatch: expected i64, got {rtype}"));
                }
                let expected_str = bits.to_string();
                if rval != expected_str {
                    return Err(format!("expected {expected_str}, got {rval}"));
                }
                Ok(())
            }
            WastValue::F32(wf) => {
                let (rtype, rval) = actual.to_strings();
                if rtype != "f32" {
                    return Err(format!("type mismatch: expected f32, got {rtype}"));
                }
                match_f32(wf, actual, &rval)
            }
            WastValue::F64(wf) => {
                let (rtype, rval) = actual.to_strings();
                if rtype != "f64" {
                    return Err(format!("type mismatch: expected f64, got {rtype}"));
                }
                match_f64(wf, actual, &rval)
            }
            WastValue::V128 { lane_type, lanes } => match_v128(lane_type, lanes, actual),
            WastValue::RefNull(_) => match actual {
                Value::FuncRef(None) | Value::ExternRef(None) => Ok(()),
                _ => Err(format!("expected ref.null, got {actual:?}")),
            },
            WastValue::RefFunc => match actual {
                Value::FuncRef(_) => Ok(()),
                _ => Err(format!("expected ref.func, got {actual:?}")),
            },
            WastValue::RefExtern(expected_idx) => match actual {
                Value::ExternRef(actual_idx) if actual_idx == expected_idx => Ok(()),
                _ => Err(format!("expected ref.extern({expected_idx:?}), got {actual:?}")),
            },
        }
    }
}

fn match_f32(wf: &WastFloat<u32>, actual: &Value, rval: &str) -> Result<(), String> {
    match wf {
        WastFloat::Value(bits) => {
            let expected_str = bits.to_string();
            if rval == expected_str {
                return Ok(());
            }
            // Try normalised form (e.g. float display vs raw bits)
            let expected_val = Value::F32(f32::from_bits(*bits));
            let (_, normalised) = expected_val.to_strings();
            if rval == normalised {
                return Ok(());
            }
            // NaN sign bit is non-deterministic — compare ignoring sign
            let actual_bits: u32 = match actual {
                Value::F32(f) => f.to_bits(),
                _ => unreachable!(),
            };
            if f32::from_bits(*bits).is_nan()
                && f32::from_bits(actual_bits).is_nan()
                && (actual_bits & 0x7FFFFFFF) == (*bits & 0x7FFFFFFF)
            {
                return Ok(());
            }
            Err(format!("expected {expected_str}, got {rval}"))
        }
        WastFloat::NanCanonical => {
            if rval == "nan:canonical" {
                Ok(())
            } else {
                Err(format!("expected nan:canonical, got {rval}"))
            }
        }
        WastFloat::NanArithmetic => {
            if rval == "nan:canonical" || rval == "nan:arithmetic" {
                Ok(())
            } else {
                Err(format!("expected nan:arithmetic, got {rval}"))
            }
        }
    }
}

fn match_f64(wf: &WastFloat<u64>, actual: &Value, rval: &str) -> Result<(), String> {
    match wf {
        WastFloat::Value(bits) => {
            let expected_str = bits.to_string();
            if rval == expected_str {
                return Ok(());
            }
            let expected_val = Value::F64(f64::from_bits(*bits));
            let (_, normalised) = expected_val.to_strings();
            if rval == normalised {
                return Ok(());
            }
            let actual_bits: u64 = match actual {
                Value::F64(f) => f.to_bits(),
                _ => unreachable!(),
            };
            if f64::from_bits(*bits).is_nan()
                && f64::from_bits(actual_bits).is_nan()
                && (actual_bits & 0x7FFFFFFFFFFFFFFF) == (*bits & 0x7FFFFFFFFFFFFFFF)
            {
                return Ok(());
            }
            Err(format!("expected {expected_str}, got {rval}"))
        }
        WastFloat::NanCanonical => {
            if rval == "nan:canonical" {
                Ok(())
            } else {
                Err(format!("expected nan:canonical, got {rval}"))
            }
        }
        WastFloat::NanArithmetic => {
            if rval == "nan:canonical" || rval == "nan:arithmetic" {
                Ok(())
            } else {
                Err(format!("expected nan:arithmetic, got {rval}"))
            }
        }
    }
}

fn match_v128(lane_type: &str, lanes: &[WastLane], actual: &Value) -> Result<(), String> {
    let base_type = normalize_lane_type(lane_type);
    let result_lanes = actual.to_v128_lanes(base_type)?;

    if result_lanes.len() != lanes.len() {
        return Err(format!(
            "v128 lane count mismatch: expected {}, got {}",
            lanes.len(),
            result_lanes.len()
        ));
    }

    for (j, (rl, el)) in result_lanes.iter().zip(lanes).enumerate() {
        let mismatch = |el: &WastLane, rl: &str| Err(format!("v128 lane {j}: expected {el:?}, got {rl}"));
        let parse_err = |rl: &str, ty: &str| Err(format!("v128 lane {j}: failed to parse '{rl}' as {ty}"));
        match el {
            WastLane::Integer(v) => {
                let actual_val: u64 = match rl.parse() {
                    Ok(v) => v,
                    Err(_) => return parse_err(rl, "u64"),
                };
                let mask = lane_mask(base_type);
                if actual_val != (*v & mask) {
                    return mismatch(el, rl);
                }
            }
            WastLane::F32(WastFloat::Value(bits)) => {
                let actual_val: u32 = match rl.parse() {
                    Ok(v) => v,
                    Err(_) => return parse_err(rl, "u32"),
                };
                if actual_val != *bits {
                    return mismatch(el, rl);
                }
            }
            WastLane::F32(WastFloat::NanCanonical) => {
                if !lane_is_canonical_nan(rl, "f32") {
                    return mismatch(el, rl);
                }
            }
            WastLane::F32(WastFloat::NanArithmetic) => {
                if !lane_is_arithmetic_nan(rl, "f32") {
                    return mismatch(el, rl);
                }
            }
            WastLane::F64(WastFloat::Value(bits)) => {
                let actual_val: u64 = match rl.parse() {
                    Ok(v) => v,
                    Err(_) => return parse_err(rl, "u64"),
                };
                if actual_val != *bits {
                    return mismatch(el, rl);
                }
            }
            WastLane::F64(WastFloat::NanCanonical) => {
                if !lane_is_canonical_nan(rl, "f64") {
                    return mismatch(el, rl);
                }
            }
            WastLane::F64(WastFloat::NanArithmetic) => {
                if !lane_is_arithmetic_nan(rl, "f64") {
                    return mismatch(el, rl);
                }
            }
        }
    }
    Ok(())
}

/// Bit mask for truncating a u64 expected value to the lane's width.
fn lane_mask(base_type: &str) -> u64 {
    match base_type {
        "i8" => 0xFF,
        "i16" => 0xFFFF,
        "i32" => 0xFFFF_FFFF,
        _ => u64::MAX,
    }
}

/// Strip lane count from v128 lane type: `"i32x4"` → `"i32"`.
fn normalize_lane_type(lane_type: &str) -> &str {
    match lane_type {
        "i8x16" => "i8",
        "i16x8" => "i16",
        "i32x4" => "i32",
        "i64x2" => "i64",
        "f32x4" => "f32",
        "f64x2" => "f64",
        other => other,
    }
}

/// Check whether a v128 lane (as bit-string) is a canonical NaN.
fn lane_is_canonical_nan(lane_bits: &str, lane_type: &str) -> bool {
    match lane_type {
        "f32" => {
            let bits: u32 = match lane_bits.parse() {
                Ok(v) => v,
                Err(_) => return false,
            };
            bits == 0x7fc00000 || bits == 0xffc00000
        }
        "f64" => {
            let bits: u64 = match lane_bits.parse() {
                Ok(v) => v,
                Err(_) => return false,
            };
            bits == 0x7ff8000000000000 || bits == 0xfff8000000000000
        }
        _ => false,
    }
}

/// Check whether a v128 lane (as bit-string) is an arithmetic NaN (quiet bit set).
fn lane_is_arithmetic_nan(lane_bits: &str, lane_type: &str) -> bool {
    match lane_type {
        "f32" => {
            let bits: u32 = match lane_bits.parse() {
                Ok(v) => v,
                Err(_) => return false,
            };
            f32::from_bits(bits).is_nan() && (bits & 0x00400000) != 0
        }
        "f64" => {
            let bits: u64 = match lane_bits.parse() {
                Ok(v) => v,
                Err(_) => return false,
            };
            f64::from_bits(bits).is_nan() && (bits & 0x0008000000000000) != 0
        }
        _ => false,
    }
}
