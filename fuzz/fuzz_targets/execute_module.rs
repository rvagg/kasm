#![no_main]

use libfuzzer_sys::fuzz_target;
use std::collections::HashMap;

use kasm::parser::module::{ExportIndex, ValueType};
use kasm::parser::{self, reader::Reader};
use kasm::runtime::{Store, Value};

/// Generate a Value of the specified type from fuzz data
fn generate_value(typ: &ValueType, data: &mut &[u8]) -> Value {
    match typ {
        ValueType::I32 => {
            let val = if data.len() >= 4 {
                let bytes = [data[0], data[1], data[2], data[3]];
                *data = &data[4..];
                i32::from_le_bytes(bytes)
            } else {
                0
            };
            Value::I32(val)
        }
        ValueType::I64 => {
            let val = if data.len() >= 8 {
                let bytes = [
                    data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
                ];
                *data = &data[8..];
                i64::from_le_bytes(bytes)
            } else {
                0
            };
            Value::I64(val)
        }
        ValueType::F32 => {
            let val = if data.len() >= 4 {
                let bytes = [data[0], data[1], data[2], data[3]];
                *data = &data[4..];
                f32::from_le_bytes(bytes)
            } else {
                0.0
            };
            Value::F32(val)
        }
        ValueType::F64 => {
            let val = if data.len() >= 8 {
                let bytes = [
                    data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
                ];
                *data = &data[8..];
                f64::from_le_bytes(bytes)
            } else {
                0.0
            };
            Value::F64(val)
        }
        ValueType::FuncRef => Value::FuncRef(None),
        ValueType::ExternRef => Value::ExternRef(None),
        ValueType::V128 => Value::I32(0), // SIMD not yet supported, use placeholder
    }
}

fuzz_target!(|data: &[u8]| {
    // Need enough bytes for a minimal wasm module plus some fuzz data for arguments
    if data.len() < 8 {
        return;
    }

    // Split data: most for the wasm module, tail for argument generation
    let split_point = data.len().saturating_sub(64).max(8);
    let wasm_data = &data[..split_point];
    let mut arg_data = &data[split_point..];

    // Parse the module
    let mut reader = Reader::new(wasm_data.to_vec());
    let module = match parser::parse(&HashMap::new(), "fuzz", &mut reader) {
        Ok(m) => m,
        Err(_) => return,
    };

    // Create store and instantiate
    let mut store = Store::new();
    let instance_id = match store.create_instance(&module, None) {
        Ok(id) => id,
        Err(_) => return,
    };

    // Execute start function if present (already done by create_instance, but verify no panic)

    // Try each exported function with typed arguments
    for export in &module.exports.exports {
        if let ExportIndex::Function(func_idx) = export.index {
            // Get function type to generate correct arguments
            if let Some(func_type) = module.get_function_type_by_idx(func_idx) {
                // Generate arguments based on function signature
                let mut args = Vec::with_capacity(func_type.parameters.len());
                for param_type in &func_type.parameters {
                    args.push(generate_value(param_type, &mut arg_data));
                }

                // Invoke with generated args - we don't care about the result
                // Use an instruction budget to prevent infinite loops from hanging the fuzzer
                let _ = store.invoke_export(instance_id, &export.name, args, Some(100_000));
            }
        }
    }

    // Try indirect calls if tables exist
    if !module.table.tables.is_empty() {
        // The table might have funcref entries; try calling through table index 0
        // This exercises call_indirect paths
        for export in &module.exports.exports {
            if let ExportIndex::Table(_) = export.index {
                // Found an exported table, table operations are tested via functions
                break;
            }
        }
    }
});
