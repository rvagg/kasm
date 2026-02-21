//! AssemblyScript runtime support
//!
//! AssemblyScript is a TypeScript-like language that compiles to WebAssembly.
//! Unlike pure WASI modules, AssemblyScript requires additional host functions
//! from the "env" module that are not part of the WASI specification.
//!
//! This module provides these AssemblyScript-specific imports. They are kept
//! separate from WASI functions to clearly distinguish between standard WASI
//! and AssemblyScript's runtime requirements.
//!
//! # Background
//!
//! When running in a browser or Node.js, AssemblyScript modules are loaded with
//! JavaScript glue code that provides these functions. When running in a WASI
//! runtime like kasm, we must provide them ourselves.
//!
//! # Usage
//!
//! ```ignore
//! use kasm::runtime::wasi::{create_wasi_imports, add_assemblyscript_imports};
//!
//! let mut imports = create_wasi_imports(&mut store, ctx.clone());
//! add_assemblyscript_imports(&mut store, &mut imports, ctx.clone());
//! ```

use super::WasiContext;
use crate::parser::module::{FunctionType, ValueType};
use crate::runtime::store::{FunctionInstance, Store};
use crate::runtime::{ImportObject, RuntimeError, Value};
use std::sync::Arc;

/// Add AssemblyScript-specific imports to an ImportObject.
///
/// This registers the `env.abort` function required by AssemblyScript's runtime.
/// Call this after `create_wasi_imports` if your module is compiled from AssemblyScript.
pub fn add_assemblyscript_imports(store: &mut Store<'_>, imports: &mut ImportObject, ctx: Arc<WasiContext>) {
    // env.abort: AssemblyScript abort function
    // Called when an assertion fails or abort() is called explicitly.
    // Signature: (message: i32, fileName: i32, line: i32, column: i32) -> void
    let abort_type = FunctionType {
        parameters: vec![ValueType::I32, ValueType::I32, ValueType::I32, ValueType::I32],
        return_types: vec![],
    };
    let abort_addr = store.allocate_function(FunctionInstance::Host {
        func: Box::new(move |args| env_abort(&ctx, args)),
        func_type: abort_type,
    });
    imports.add_function("env", "abort", abort_addr);
}

/// Extract an i32 value from function arguments at the given index.
fn extract_i32(args: &[Value], index: usize) -> Option<i32> {
    match args.get(index) {
        Some(Value::I32(v)) => Some(*v),
        _ => None,
    }
}

/// Read an AssemblyScript string from memory.
///
/// AssemblyScript strings are stored as:
/// - ptr-4: length in bytes (u32)
/// - ptr: UTF-16LE encoded string data
///
/// Returns None if the pointer is 0 (null) or if reading fails.
fn read_as_string(ctx: &WasiContext, ptr: u32) -> Option<String> {
    if ptr == 0 {
        return None;
    }

    // Read length from ptr-4
    let len_bytes = ctx.read_u32(ptr.wrapping_sub(4)).ok()?;

    // Sanity check: length should be reasonable and even (UTF-16)
    if len_bytes == 0 || len_bytes > 10000 || len_bytes % 2 != 0 {
        return None;
    }

    // Read UTF-16LE bytes
    let bytes = ctx.read_bytes(ptr, len_bytes as usize).ok()?;

    // Convert from UTF-16LE to String
    let utf16: Vec<u16> = bytes
        .chunks_exact(2)
        .map(|chunk| u16::from_le_bytes([chunk[0], chunk[1]]))
        .collect();

    String::from_utf16(&utf16).ok()
}

/// AssemblyScript abort function.
///
/// Called when an assertion fails or when `abort()` is called explicitly in
/// AssemblyScript code. The message and filename pointers reference UTF-16
/// encoded strings in linear memory (AssemblyScript's native string format).
fn env_abort(ctx: &WasiContext, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
    let message_ptr = extract_i32(&args, 0).unwrap_or(0) as u32;
    let filename_ptr = extract_i32(&args, 1).unwrap_or(0) as u32;
    let line = extract_i32(&args, 2).unwrap_or(0);
    let column = extract_i32(&args, 3).unwrap_or(0);

    // Try to read the message and filename from memory
    let message = read_as_string(ctx, message_ptr);
    let filename = read_as_string(ctx, filename_ptr);

    // Set exit code to 1 (abnormal termination)
    ctx.set_exit_code(1);

    // Build error message with available information
    let error_msg = match (message, filename) {
        (Some(msg), Some(file)) => {
            format!("abort: {} at {}:{}:{}", msg, file, line, column)
        }
        (Some(msg), None) => {
            format!("abort: {} at line {}, column {}", msg, line, column)
        }
        (None, Some(file)) => {
            format!("abort at {}:{}:{}", file, line, column)
        }
        (None, None) => {
            format!("abort at line {}, column {}", line, column)
        }
    };

    Err(RuntimeError::Trap(error_msg))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Memory;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn test_add_assemblyscript_imports() {
        let mut store = Store::new();
        let ctx = Arc::new(WasiContext::builder().build());
        let mut imports = ImportObject::new();

        add_assemblyscript_imports(&mut store, &mut imports, ctx);

        assert!(imports.get_function("env", "abort").is_ok());
    }

    #[test]
    fn test_env_abort_sets_exit_code() {
        let ctx = WasiContext::builder().build();

        // Bind memory (required for context)
        let memory = Memory::new(1, None).unwrap();
        let shared_memory = Rc::new(RefCell::new(memory));
        ctx.bind_memory(shared_memory);

        let args = vec![
            Value::I32(0),  // message ptr (null)
            Value::I32(0),  // filename ptr (null)
            Value::I32(42), // line
            Value::I32(10), // column
        ];
        let result = env_abort(&ctx, args);

        assert!(result.is_err());
        assert_eq!(ctx.exit_code(), Some(1));

        if let Err(RuntimeError::Trap(msg)) = result {
            assert!(msg.contains("line 42"));
            assert!(msg.contains("column 10"));
        } else {
            panic!("Expected Trap error");
        }
    }

    #[test]
    fn test_env_abort_reads_strings() {
        let ctx = WasiContext::builder().build();

        let memory = Memory::new(1, None).unwrap();
        let shared_memory = Rc::new(RefCell::new(memory));
        ctx.bind_memory(shared_memory);

        // Write an AssemblyScript string "Error!" at address 100
        // Format: length at ptr-4, UTF-16LE data at ptr
        // "Error!" = 6 chars = 12 bytes in UTF-16
        let message = "Error!";
        let utf16: Vec<u16> = message.encode_utf16().collect();
        let len_bytes = (utf16.len() * 2) as u32;

        // Write length at address 96 (100 - 4)
        ctx.write_u32(96, len_bytes).unwrap();

        // Write UTF-16LE data at address 100
        let mut utf16_bytes = Vec::new();
        for ch in &utf16 {
            utf16_bytes.extend_from_slice(&ch.to_le_bytes());
        }
        ctx.write_bytes(100, &utf16_bytes).unwrap();

        // Write filename "test.ts" at address 200
        let filename = "test.ts";
        let utf16_fn: Vec<u16> = filename.encode_utf16().collect();
        let len_fn = (utf16_fn.len() * 2) as u32;

        ctx.write_u32(196, len_fn).unwrap();
        let mut utf16_fn_bytes = Vec::new();
        for ch in &utf16_fn {
            utf16_fn_bytes.extend_from_slice(&ch.to_le_bytes());
        }
        ctx.write_bytes(200, &utf16_fn_bytes).unwrap();

        let args = vec![
            Value::I32(100), // message ptr
            Value::I32(200), // filename ptr
            Value::I32(42),  // line
            Value::I32(10),  // column
        ];
        let result = env_abort(&ctx, args);

        assert!(result.is_err());
        if let Err(RuntimeError::Trap(msg)) = result {
            assert!(msg.contains("Error!"), "Expected 'Error!' in: {}", msg);
            assert!(msg.contains("test.ts"), "Expected 'test.ts' in: {}", msg);
            assert!(msg.contains("42"), "Expected line 42 in: {}", msg);
            assert!(msg.contains("10"), "Expected column 10 in: {}", msg);
        } else {
            panic!("Expected Trap error");
        }
    }

    #[test]
    fn test_read_as_string() {
        let ctx = WasiContext::builder().build();

        let memory = Memory::new(1, None).unwrap();
        let shared_memory = Rc::new(RefCell::new(memory));
        ctx.bind_memory(shared_memory);

        // Null pointer returns None
        assert_eq!(read_as_string(&ctx, 0), None);

        // Write "Hello" as AssemblyScript string at address 104 (length at 100)
        let text = "Hello";
        let utf16: Vec<u16> = text.encode_utf16().collect();
        let len_bytes = (utf16.len() * 2) as u32;

        ctx.write_u32(100, len_bytes).unwrap();
        let mut utf16_bytes = Vec::new();
        for ch in &utf16 {
            utf16_bytes.extend_from_slice(&ch.to_le_bytes());
        }
        ctx.write_bytes(104, &utf16_bytes).unwrap();

        assert_eq!(read_as_string(&ctx, 104), Some("Hello".to_string()));
    }
}
