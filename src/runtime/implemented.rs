//! Single source of truth for which WebAssembly instructions are implemented
//!
//! This module provides a centralized place to track which instructions
//! the runtime currently supports. This is used by both the test framework
//! and the coverage analysis tool.

use crate::parser::instruction::InstructionKind;
use std::collections::HashSet;

/// Returns a set of all implemented instruction mnemonics
pub fn get_implemented_instructions() -> HashSet<String> {
    let mut implemented = HashSet::new();

    // Constants
    implemented.insert("i32.const".to_string());
    implemented.insert("i64.const".to_string());
    implemented.insert("f32.const".to_string());
    implemented.insert("f64.const".to_string());

    // Basic
    implemented.insert("nop".to_string());
    implemented.insert("drop".to_string());
    implemented.insert("end".to_string());
    implemented.insert("unreachable".to_string());

    // Local variables
    implemented.insert("local.get".to_string());
    implemented.insert("local.set".to_string());
    implemented.insert("local.tee".to_string());

    // Global variables
    implemented.insert("global.get".to_string());
    implemented.insert("global.set".to_string());

    // Parametric instructions
    implemented.insert("select".to_string());

    // Control flow
    implemented.insert("block".to_string());
    implemented.insert("loop".to_string());
    implemented.insert("if".to_string());
    implemented.insert("else".to_string());
    implemented.insert("br".to_string());
    implemented.insert("br_if".to_string());
    implemented.insert("br_table".to_string());
    implemented.insert("return".to_string());

    // Function calls
    implemented.insert("call".to_string());
    // call_indirect requires tables, not yet implemented

    // Memory operations
    implemented.insert("memory.size".to_string());
    implemented.insert("memory.grow".to_string());

    // Memory load operations
    implemented.insert("i32.load".to_string());
    implemented.insert("i64.load".to_string());
    implemented.insert("f32.load".to_string());
    implemented.insert("f64.load".to_string());
    implemented.insert("i32.load8_s".to_string());
    implemented.insert("i32.load8_u".to_string());
    implemented.insert("i32.load16_s".to_string());
    implemented.insert("i32.load16_u".to_string());
    implemented.insert("i64.load8_s".to_string());
    implemented.insert("i64.load8_u".to_string());
    implemented.insert("i64.load16_s".to_string());
    implemented.insert("i64.load16_u".to_string());
    implemented.insert("i64.load32_s".to_string());
    implemented.insert("i64.load32_u".to_string());

    // Memory store operations
    implemented.insert("i32.store".to_string());
    implemented.insert("i64.store".to_string());
    implemented.insert("f32.store".to_string());
    implemented.insert("f64.store".to_string());
    implemented.insert("i32.store8".to_string());
    implemented.insert("i32.store16".to_string());
    implemented.insert("i64.store8".to_string());
    implemented.insert("i64.store16".to_string());
    implemented.insert("i64.store32".to_string());

    // Unary operations
    implemented.insert("i32.clz".to_string());
    implemented.insert("i32.ctz".to_string());
    implemented.insert("i32.popcnt".to_string());
    implemented.insert("i64.clz".to_string());
    implemented.insert("i64.ctz".to_string());
    implemented.insert("i64.popcnt".to_string());
    implemented.insert("f32.abs".to_string());
    implemented.insert("f32.neg".to_string());
    implemented.insert("f32.sqrt".to_string());
    implemented.insert("f32.ceil".to_string());
    implemented.insert("f32.floor".to_string());
    implemented.insert("f32.trunc".to_string());
    implemented.insert("f32.nearest".to_string());
    implemented.insert("f64.abs".to_string());
    implemented.insert("f64.neg".to_string());
    implemented.insert("f64.sqrt".to_string());
    implemented.insert("f64.ceil".to_string());
    implemented.insert("f64.floor".to_string());
    implemented.insert("f64.trunc".to_string());
    implemented.insert("f64.nearest".to_string());

    // Binary operations - floating point
    implemented.insert("f32.add".to_string());
    implemented.insert("f32.sub".to_string());
    implemented.insert("f32.mul".to_string());
    implemented.insert("f32.div".to_string());
    implemented.insert("f32.min".to_string());
    implemented.insert("f32.max".to_string());
    implemented.insert("f32.copysign".to_string());
    implemented.insert("f64.add".to_string());
    implemented.insert("f64.sub".to_string());
    implemented.insert("f64.mul".to_string());
    implemented.insert("f64.div".to_string());
    implemented.insert("f64.min".to_string());
    implemented.insert("f64.max".to_string());
    implemented.insert("f64.copysign".to_string());

    // Binary operations - integers
    implemented.insert("i32.add".to_string());
    implemented.insert("i32.sub".to_string());
    implemented.insert("i32.mul".to_string());
    implemented.insert("i32.div_s".to_string());
    implemented.insert("i32.div_u".to_string());
    implemented.insert("i32.rem_s".to_string());
    implemented.insert("i32.rem_u".to_string());
    implemented.insert("i64.add".to_string());
    implemented.insert("i64.sub".to_string());
    implemented.insert("i64.mul".to_string());
    implemented.insert("i64.div_s".to_string());
    implemented.insert("i64.div_u".to_string());
    implemented.insert("i64.rem_s".to_string());
    implemented.insert("i64.rem_u".to_string());

    // Comparison instructions
    implemented.insert("i32.eqz".to_string());
    implemented.insert("i32.eq".to_string());
    implemented.insert("i32.ne".to_string());
    implemented.insert("i32.lt_s".to_string());
    implemented.insert("i32.lt_u".to_string());
    implemented.insert("i32.gt_s".to_string());
    implemented.insert("i32.gt_u".to_string());
    implemented.insert("i32.le_s".to_string());
    implemented.insert("i32.le_u".to_string());
    implemented.insert("i32.ge_s".to_string());
    implemented.insert("i32.ge_u".to_string());

    implemented.insert("i64.eqz".to_string());
    implemented.insert("i64.eq".to_string());
    implemented.insert("i64.ne".to_string());
    implemented.insert("i64.lt_s".to_string());
    implemented.insert("i64.lt_u".to_string());
    implemented.insert("i64.gt_s".to_string());
    implemented.insert("i64.gt_u".to_string());
    implemented.insert("i64.le_s".to_string());
    implemented.insert("i64.le_u".to_string());
    implemented.insert("i64.ge_s".to_string());
    implemented.insert("i64.ge_u".to_string());

    implemented.insert("f32.eq".to_string());
    implemented.insert("f32.ne".to_string());
    implemented.insert("f32.lt".to_string());
    implemented.insert("f32.gt".to_string());
    implemented.insert("f32.le".to_string());
    implemented.insert("f32.ge".to_string());

    implemented.insert("f64.eq".to_string());
    implemented.insert("f64.ne".to_string());
    implemented.insert("f64.lt".to_string());
    implemented.insert("f64.gt".to_string());
    implemented.insert("f64.le".to_string());
    implemented.insert("f64.ge".to_string());

    // Bitwise operations
    implemented.insert("i32.and".to_string());
    implemented.insert("i32.or".to_string());
    implemented.insert("i32.xor".to_string());
    implemented.insert("i32.shl".to_string());
    implemented.insert("i32.shr_s".to_string());
    implemented.insert("i32.shr_u".to_string());
    implemented.insert("i32.rotl".to_string());
    implemented.insert("i32.rotr".to_string());

    implemented.insert("i64.and".to_string());
    implemented.insert("i64.or".to_string());
    implemented.insert("i64.xor".to_string());
    implemented.insert("i64.shl".to_string());
    implemented.insert("i64.shr_s".to_string());
    implemented.insert("i64.shr_u".to_string());
    implemented.insert("i64.rotl".to_string());
    implemented.insert("i64.rotr".to_string());

    // Conversion operations
    implemented.insert("i32.wrap_i64".to_string());
    implemented.insert("i64.extend_i32_s".to_string());
    implemented.insert("i64.extend_i32_u".to_string());

    // Sign extension operations
    implemented.insert("i32.extend8_s".to_string());
    implemented.insert("i32.extend16_s".to_string());
    implemented.insert("i64.extend8_s".to_string());
    implemented.insert("i64.extend16_s".to_string());
    implemented.insert("i64.extend32_s".to_string());

    // Float width conversions
    implemented.insert("f32.demote_f64".to_string());
    implemented.insert("f64.promote_f32".to_string());

    // Reinterpret/bit casting
    implemented.insert("i32.reinterpret_f32".to_string());
    implemented.insert("i64.reinterpret_f64".to_string());
    implemented.insert("f32.reinterpret_i32".to_string());
    implemented.insert("f64.reinterpret_i64".to_string());

    // Integer to float conversions
    implemented.insert("f32.convert_i32_s".to_string());
    implemented.insert("f32.convert_i32_u".to_string());
    implemented.insert("f32.convert_i64_s".to_string());
    implemented.insert("f32.convert_i64_u".to_string());
    implemented.insert("f64.convert_i32_s".to_string());
    implemented.insert("f64.convert_i32_u".to_string());
    implemented.insert("f64.convert_i64_s".to_string());
    implemented.insert("f64.convert_i64_u".to_string());

    // Float to integer truncation (trapping)
    implemented.insert("i32.trunc_f32_s".to_string());
    implemented.insert("i32.trunc_f32_u".to_string());
    implemented.insert("i32.trunc_f64_s".to_string());
    implemented.insert("i32.trunc_f64_u".to_string());
    implemented.insert("i64.trunc_f32_s".to_string());
    implemented.insert("i64.trunc_f32_u".to_string());
    implemented.insert("i64.trunc_f64_s".to_string());
    implemented.insert("i64.trunc_f64_u".to_string());

    // Saturating truncation (non-trapping)
    implemented.insert("i32.trunc_sat_f32_s".to_string());
    implemented.insert("i32.trunc_sat_f32_u".to_string());
    implemented.insert("i32.trunc_sat_f64_s".to_string());
    implemented.insert("i32.trunc_sat_f64_u".to_string());
    implemented.insert("i64.trunc_sat_f32_s".to_string());
    implemented.insert("i64.trunc_sat_f32_u".to_string());
    implemented.insert("i64.trunc_sat_f64_s".to_string());
    implemented.insert("i64.trunc_sat_f64_u".to_string());

    implemented
}

/// Check if a specific instruction is implemented
pub fn is_instruction_implemented(inst: &InstructionKind) -> bool {
    let implemented = get_implemented_instructions();
    let mnemonic = inst.mnemonic();
    implemented.contains(mnemonic)
}
