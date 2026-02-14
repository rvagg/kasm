//! Single source of truth for which WebAssembly instructions are implemented
//!
//! This module provides a centralised place to track which instructions
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
    implemented.insert("call_indirect".to_string());

    // Table operations
    implemented.insert("table.get".to_string());
    implemented.insert("table.set".to_string());
    implemented.insert("table.size".to_string());
    implemented.insert("table.grow".to_string());
    implemented.insert("table.init".to_string());
    implemented.insert("table.copy".to_string());
    implemented.insert("table.fill".to_string());
    implemented.insert("elem.drop".to_string());

    // Reference operations
    implemented.insert("ref.null".to_string());
    implemented.insert("ref.is_null".to_string());
    implemented.insert("ref.func".to_string());

    // Memory operations
    implemented.insert("memory.size".to_string());
    implemented.insert("memory.grow".to_string());
    implemented.insert("memory.init".to_string());
    implemented.insert("memory.copy".to_string());
    implemented.insert("memory.fill".to_string());
    implemented.insert("data.drop".to_string());

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

    // SIMD v128 foundation
    implemented.insert("v128.const".to_string());
    implemented.insert("v128.load".to_string());
    implemented.insert("v128.store".to_string());

    // SIMD f32x4 operations
    implemented.insert("f32x4.abs".to_string());
    implemented.insert("f32x4.neg".to_string());
    implemented.insert("f32x4.sqrt".to_string());
    implemented.insert("f32x4.ceil".to_string());
    implemented.insert("f32x4.floor".to_string());
    implemented.insert("f32x4.trunc".to_string());
    implemented.insert("f32x4.nearest".to_string());
    implemented.insert("f32x4.add".to_string());
    implemented.insert("f32x4.sub".to_string());
    implemented.insert("f32x4.mul".to_string());
    implemented.insert("f32x4.div".to_string());
    implemented.insert("f32x4.min".to_string());
    implemented.insert("f32x4.max".to_string());
    implemented.insert("f32x4.pmin".to_string());
    implemented.insert("f32x4.pmax".to_string());
    implemented.insert("f32x4.eq".to_string());
    implemented.insert("f32x4.ne".to_string());
    implemented.insert("f32x4.lt".to_string());
    implemented.insert("f32x4.gt".to_string());
    implemented.insert("f32x4.le".to_string());
    implemented.insert("f32x4.ge".to_string());

    // SIMD f64x2 operations
    implemented.insert("f64x2.abs".to_string());
    implemented.insert("f64x2.neg".to_string());
    implemented.insert("f64x2.sqrt".to_string());
    implemented.insert("f64x2.ceil".to_string());
    implemented.insert("f64x2.floor".to_string());
    implemented.insert("f64x2.trunc".to_string());
    implemented.insert("f64x2.nearest".to_string());
    implemented.insert("f64x2.add".to_string());
    implemented.insert("f64x2.sub".to_string());
    implemented.insert("f64x2.mul".to_string());
    implemented.insert("f64x2.div".to_string());
    implemented.insert("f64x2.min".to_string());
    implemented.insert("f64x2.max".to_string());
    implemented.insert("f64x2.pmin".to_string());
    implemented.insert("f64x2.pmax".to_string());
    implemented.insert("f64x2.eq".to_string());
    implemented.insert("f64x2.ne".to_string());
    implemented.insert("f64x2.lt".to_string());
    implemented.insert("f64x2.gt".to_string());
    implemented.insert("f64x2.le".to_string());
    implemented.insert("f64x2.ge".to_string());

    // SIMD memory operations
    implemented.insert("v128.load8x8_s".to_string());
    implemented.insert("v128.load8x8_u".to_string());
    implemented.insert("v128.load16x4_s".to_string());
    implemented.insert("v128.load16x4_u".to_string());
    implemented.insert("v128.load32x2_s".to_string());
    implemented.insert("v128.load32x2_u".to_string());
    implemented.insert("v128.load8_splat".to_string());
    implemented.insert("v128.load16_splat".to_string());
    implemented.insert("v128.load32_splat".to_string());
    implemented.insert("v128.load64_splat".to_string());
    implemented.insert("v128.load32_zero".to_string());
    implemented.insert("v128.load64_zero".to_string());
    implemented.insert("v128.load8_lane".to_string());
    implemented.insert("v128.load16_lane".to_string());
    implemented.insert("v128.load32_lane".to_string());
    implemented.insert("v128.load64_lane".to_string());
    implemented.insert("v128.store8_lane".to_string());
    implemented.insert("v128.store16_lane".to_string());
    implemented.insert("v128.store32_lane".to_string());
    implemented.insert("v128.store64_lane".to_string());

    // SIMD shuffle/swizzle
    implemented.insert("i8x16.shuffle".to_string());
    implemented.insert("i8x16.swizzle".to_string());

    // SIMD splat
    implemented.insert("i8x16.splat".to_string());
    implemented.insert("i16x8.splat".to_string());
    implemented.insert("i32x4.splat".to_string());
    implemented.insert("i64x2.splat".to_string());
    implemented.insert("f32x4.splat".to_string());
    implemented.insert("f64x2.splat".to_string());

    // SIMD extract/replace lane
    implemented.insert("i8x16.extract_lane_s".to_string());
    implemented.insert("i8x16.extract_lane_u".to_string());
    implemented.insert("i8x16.replace_lane".to_string());
    implemented.insert("i16x8.extract_lane_s".to_string());
    implemented.insert("i16x8.extract_lane_u".to_string());
    implemented.insert("i16x8.replace_lane".to_string());
    implemented.insert("i32x4.extract_lane".to_string());
    implemented.insert("i32x4.replace_lane".to_string());
    implemented.insert("i64x2.extract_lane".to_string());
    implemented.insert("i64x2.replace_lane".to_string());
    implemented.insert("f32x4.extract_lane".to_string());
    implemented.insert("f32x4.replace_lane".to_string());
    implemented.insert("f64x2.extract_lane".to_string());
    implemented.insert("f64x2.replace_lane".to_string());

    // SIMD i8x16 comparisons
    implemented.insert("i8x16.eq".to_string());
    implemented.insert("i8x16.ne".to_string());
    implemented.insert("i8x16.lt_s".to_string());
    implemented.insert("i8x16.lt_u".to_string());
    implemented.insert("i8x16.gt_s".to_string());
    implemented.insert("i8x16.gt_u".to_string());
    implemented.insert("i8x16.le_s".to_string());
    implemented.insert("i8x16.le_u".to_string());
    implemented.insert("i8x16.ge_s".to_string());
    implemented.insert("i8x16.ge_u".to_string());

    // SIMD i16x8 comparisons
    implemented.insert("i16x8.eq".to_string());
    implemented.insert("i16x8.ne".to_string());
    implemented.insert("i16x8.lt_s".to_string());
    implemented.insert("i16x8.lt_u".to_string());
    implemented.insert("i16x8.gt_s".to_string());
    implemented.insert("i16x8.gt_u".to_string());
    implemented.insert("i16x8.le_s".to_string());
    implemented.insert("i16x8.le_u".to_string());
    implemented.insert("i16x8.ge_s".to_string());
    implemented.insert("i16x8.ge_u".to_string());

    // SIMD i32x4 comparisons
    implemented.insert("i32x4.eq".to_string());
    implemented.insert("i32x4.ne".to_string());
    implemented.insert("i32x4.lt_s".to_string());
    implemented.insert("i32x4.lt_u".to_string());
    implemented.insert("i32x4.gt_s".to_string());
    implemented.insert("i32x4.gt_u".to_string());
    implemented.insert("i32x4.le_s".to_string());
    implemented.insert("i32x4.le_u".to_string());
    implemented.insert("i32x4.ge_s".to_string());
    implemented.insert("i32x4.ge_u".to_string());

    // SIMD i64x2 comparisons
    implemented.insert("i64x2.eq".to_string());
    implemented.insert("i64x2.ne".to_string());
    implemented.insert("i64x2.lt_s".to_string());
    implemented.insert("i64x2.gt_s".to_string());
    implemented.insert("i64x2.le_s".to_string());
    implemented.insert("i64x2.ge_s".to_string());

    // SIMD v128 bitwise
    implemented.insert("v128.not".to_string());
    implemented.insert("v128.and".to_string());
    implemented.insert("v128.andnot".to_string());
    implemented.insert("v128.or".to_string());
    implemented.insert("v128.xor".to_string());
    implemented.insert("v128.bitselect".to_string());
    implemented.insert("v128.any_true".to_string());

    // SIMD i8x16 operations
    implemented.insert("i8x16.abs".to_string());
    implemented.insert("i8x16.neg".to_string());
    implemented.insert("i8x16.popcnt".to_string());
    implemented.insert("i8x16.all_true".to_string());
    implemented.insert("i8x16.bitmask".to_string());
    implemented.insert("i8x16.narrow_i16x8_s".to_string());
    implemented.insert("i8x16.narrow_i16x8_u".to_string());
    implemented.insert("i8x16.shl".to_string());
    implemented.insert("i8x16.shr_s".to_string());
    implemented.insert("i8x16.shr_u".to_string());
    implemented.insert("i8x16.add".to_string());
    implemented.insert("i8x16.add_sat_s".to_string());
    implemented.insert("i8x16.add_sat_u".to_string());
    implemented.insert("i8x16.sub".to_string());
    implemented.insert("i8x16.sub_sat_s".to_string());
    implemented.insert("i8x16.sub_sat_u".to_string());
    implemented.insert("i8x16.min_s".to_string());
    implemented.insert("i8x16.min_u".to_string());
    implemented.insert("i8x16.max_s".to_string());
    implemented.insert("i8x16.max_u".to_string());
    implemented.insert("i8x16.avgr_u".to_string());

    // SIMD i16x8 operations
    implemented.insert("i16x8.abs".to_string());
    implemented.insert("i16x8.neg".to_string());
    implemented.insert("i16x8.q15mulr_sat_s".to_string());
    implemented.insert("i16x8.all_true".to_string());
    implemented.insert("i16x8.bitmask".to_string());
    implemented.insert("i16x8.narrow_i32x4_s".to_string());
    implemented.insert("i16x8.narrow_i32x4_u".to_string());
    implemented.insert("i16x8.extend_low_i8x16_s".to_string());
    implemented.insert("i16x8.extend_high_i8x16_s".to_string());
    implemented.insert("i16x8.extend_low_i8x16_u".to_string());
    implemented.insert("i16x8.extend_high_i8x16_u".to_string());
    implemented.insert("i16x8.shl".to_string());
    implemented.insert("i16x8.shr_s".to_string());
    implemented.insert("i16x8.shr_u".to_string());
    implemented.insert("i16x8.add".to_string());
    implemented.insert("i16x8.add_sat_s".to_string());
    implemented.insert("i16x8.add_sat_u".to_string());
    implemented.insert("i16x8.sub".to_string());
    implemented.insert("i16x8.sub_sat_s".to_string());
    implemented.insert("i16x8.sub_sat_u".to_string());
    implemented.insert("i16x8.mul".to_string());
    implemented.insert("i16x8.min_s".to_string());
    implemented.insert("i16x8.min_u".to_string());
    implemented.insert("i16x8.max_s".to_string());
    implemented.insert("i16x8.max_u".to_string());
    implemented.insert("i16x8.avgr_u".to_string());
    implemented.insert("i16x8.extmul_low_i8x16_s".to_string());
    implemented.insert("i16x8.extmul_high_i8x16_s".to_string());
    implemented.insert("i16x8.extmul_low_i8x16_u".to_string());
    implemented.insert("i16x8.extmul_high_i8x16_u".to_string());
    implemented.insert("i16x8.extadd_pairwise_i8x16_s".to_string());
    implemented.insert("i16x8.extadd_pairwise_i8x16_u".to_string());

    // SIMD i32x4 operations
    implemented.insert("i32x4.abs".to_string());
    implemented.insert("i32x4.neg".to_string());
    implemented.insert("i32x4.all_true".to_string());
    implemented.insert("i32x4.bitmask".to_string());
    implemented.insert("i32x4.extend_low_i16x8_s".to_string());
    implemented.insert("i32x4.extend_high_i16x8_s".to_string());
    implemented.insert("i32x4.extend_low_i16x8_u".to_string());
    implemented.insert("i32x4.extend_high_i16x8_u".to_string());
    implemented.insert("i32x4.shl".to_string());
    implemented.insert("i32x4.shr_s".to_string());
    implemented.insert("i32x4.shr_u".to_string());
    implemented.insert("i32x4.add".to_string());
    implemented.insert("i32x4.sub".to_string());
    implemented.insert("i32x4.mul".to_string());
    implemented.insert("i32x4.min_s".to_string());
    implemented.insert("i32x4.min_u".to_string());
    implemented.insert("i32x4.max_s".to_string());
    implemented.insert("i32x4.max_u".to_string());
    implemented.insert("i32x4.dot_i16x8_s".to_string());
    implemented.insert("i32x4.extmul_low_i16x8_s".to_string());
    implemented.insert("i32x4.extmul_high_i16x8_s".to_string());
    implemented.insert("i32x4.extmul_low_i16x8_u".to_string());
    implemented.insert("i32x4.extmul_high_i16x8_u".to_string());
    implemented.insert("i32x4.extadd_pairwise_i16x8_s".to_string());
    implemented.insert("i32x4.extadd_pairwise_i16x8_u".to_string());

    // SIMD i64x2 operations
    implemented.insert("i64x2.abs".to_string());
    implemented.insert("i64x2.neg".to_string());
    implemented.insert("i64x2.all_true".to_string());
    implemented.insert("i64x2.bitmask".to_string());
    implemented.insert("i64x2.extend_low_i32x4_s".to_string());
    implemented.insert("i64x2.extend_high_i32x4_s".to_string());
    implemented.insert("i64x2.extend_low_i32x4_u".to_string());
    implemented.insert("i64x2.extend_high_i32x4_u".to_string());
    implemented.insert("i64x2.shl".to_string());
    implemented.insert("i64x2.shr_s".to_string());
    implemented.insert("i64x2.shr_u".to_string());
    implemented.insert("i64x2.add".to_string());
    implemented.insert("i64x2.sub".to_string());
    implemented.insert("i64x2.mul".to_string());
    implemented.insert("i64x2.extmul_low_i32x4_s".to_string());
    implemented.insert("i64x2.extmul_high_i32x4_s".to_string());
    implemented.insert("i64x2.extmul_low_i32x4_u".to_string());
    implemented.insert("i64x2.extmul_high_i32x4_u".to_string());

    // SIMD conversion operations
    implemented.insert("i32x4.trunc_sat_f32x4_s".to_string());
    implemented.insert("i32x4.trunc_sat_f32x4_u".to_string());
    implemented.insert("f32x4.convert_i32x4_s".to_string());
    implemented.insert("f32x4.convert_i32x4_u".to_string());
    implemented.insert("i32x4.trunc_sat_f64x2_s_zero".to_string());
    implemented.insert("i32x4.trunc_sat_f64x2_u_zero".to_string());
    implemented.insert("f64x2.convert_low_i32x4_s".to_string());
    implemented.insert("f64x2.convert_low_i32x4_u".to_string());
    implemented.insert("f32x4.demote_f64x2_zero".to_string());
    implemented.insert("f64x2.promote_low_f32x4".to_string());

    implemented
}

/// Check if a specific instruction is implemented
pub fn is_instruction_implemented(inst: &InstructionKind) -> bool {
    let implemented = get_implemented_instructions();
    let mnemonic = inst.mnemonic();
    implemented.contains(mnemonic)
}

/// Tests that should be skipped due to missing features or debug mode limitations
pub fn should_skip_test(_test_name: &str) -> bool {
    false
}
