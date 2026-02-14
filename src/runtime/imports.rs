//! Import resolution for WebAssembly modules
//!
//! This module provides import resolution for WebAssembly modules, supporting
//! global and function imports.

use super::{FuncAddr, MemoryAddr, RuntimeError, TableAddr, Value};
use crate::parser::module::{ExternalKind, Module, ValueType};
use std::collections::HashMap;

/// Container for imported values that a module can reference
#[derive(Debug, Clone, Default)]
pub struct ImportObject {
    /// Imported global variables mapped by (module_name, field_name)
    pub globals: HashMap<(String, String), Value>,
    /// Imported functions mapped by (module_name, field_name)
    pub functions: HashMap<(String, String), FuncAddr>,
    /// Imported memories mapped by (module_name, field_name)
    pub memories: HashMap<(String, String), MemoryAddr>,
    /// Imported tables mapped by (module_name, field_name)
    pub tables: HashMap<(String, String), TableAddr>,
}

impl ImportObject {
    /// Create a new empty import object
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            functions: HashMap::new(),
            memories: HashMap::new(),
            tables: HashMap::new(),
        }
    }

    /// Add a function import
    pub fn add_function(&mut self, module: impl Into<String>, name: impl Into<String>, addr: FuncAddr) {
        self.functions.insert((module.into(), name.into()), addr);
    }

    /// Get a function import
    ///
    /// # Errors
    /// Returns `UnknownFunction` error if the import doesn't exist
    pub fn get_function(&self, module: &str, name: &str) -> Result<FuncAddr, RuntimeError> {
        self.functions
            .get(&(module.to_string(), name.to_string()))
            .copied()
            .ok_or_else(|| RuntimeError::UnknownFunction(format!("{}.{}", module, name)))
    }

    /// Add a global import
    pub fn add_global(&mut self, module: impl Into<String>, name: impl Into<String>, value: Value) {
        self.globals.insert((module.into(), name.into()), value);
    }

    /// Get a global import
    pub fn get_global(&self, module: &str, name: &str) -> Option<&Value> {
        self.globals.get(&(module.to_string(), name.to_string()))
    }

    /// Add a memory import
    pub fn add_memory(&mut self, module: impl Into<String>, name: impl Into<String>, addr: MemoryAddr) {
        self.memories.insert((module.into(), name.into()), addr);
    }

    /// Get a memory import
    ///
    /// # Errors
    /// Returns `UnknownExport` error if the import doesn't exist
    pub fn get_memory(&self, module: &str, name: &str) -> Result<MemoryAddr, RuntimeError> {
        self.memories
            .get(&(module.to_string(), name.to_string()))
            .copied()
            .ok_or_else(|| RuntimeError::UnknownExport(format!("{}.{}", module, name)))
    }

    /// Add a table import
    pub fn add_table(&mut self, module: impl Into<String>, name: impl Into<String>, addr: TableAddr) {
        self.tables.insert((module.into(), name.into()), addr);
    }

    /// Get a table import
    ///
    /// # Errors
    /// Returns `UnknownExport` error if the import doesn't exist
    pub fn get_table(&self, module: &str, name: &str) -> Result<TableAddr, RuntimeError> {
        self.tables
            .get(&(module.to_string(), name.to_string()))
            .copied()
            .ok_or_else(|| RuntimeError::UnknownExport(format!("{}.{}", module, name)))
    }

    /// Get an imported global value with type validation, or return a default value for the type
    ///
    /// This handles the common pattern of looking up an imported global, validating its type,
    /// and falling back to a default value if not found.
    pub fn get_or_default(
        &self,
        module_name: &str,
        field_name: &str,
        expected_type: ValueType,
    ) -> Result<Value, RuntimeError> {
        if let Some(value) = self.get_global(module_name, field_name) {
            // Validate type matches
            if !validate_type(value, expected_type) {
                return Err(RuntimeError::TypeMismatch {
                    expected: format!("{:?}", expected_type),
                    actual: format!("{:?}", value),
                });
            }
            return Ok(*value);
        }

        // Import not found - return default for type
        default_value_for_type(expected_type)
    }
}

/// Validate that a value matches the expected type
pub fn validate_type(value: &Value, expected_type: ValueType) -> bool {
    matches!(
        (value, expected_type),
        (Value::I32(_), ValueType::I32)
            | (Value::I64(_), ValueType::I64)
            | (Value::F32(_), ValueType::F32)
            | (Value::F64(_), ValueType::F64)
            | (Value::FuncRef(_), ValueType::FuncRef)
            | (Value::ExternRef(_), ValueType::ExternRef)
    )
}

/// Get the default value for a type
pub fn default_value_for_type(value_type: ValueType) -> Result<Value, RuntimeError> {
    Ok(match value_type {
        ValueType::I32 => Value::I32(0),
        ValueType::I64 => Value::I64(0),
        ValueType::F32 => Value::F32(0.0),
        ValueType::F64 => Value::F64(0.0),
        ValueType::FuncRef => Value::FuncRef(None),
        ValueType::ExternRef => Value::ExternRef(None),
        ValueType::V128 => Value::V128([0u8; 16]),
    })
}

/// Count the number of imported globals in a module
pub fn count_imported_globals(module: &Module) -> usize {
    module
        .imports
        .imports
        .iter()
        .filter(|imp| matches!(imp.external_kind, ExternalKind::Global(_)))
        .count()
}

/// Check if a global is mutable (handles both imported and module-defined globals)
pub fn is_global_mutable(module: &Module, global_idx: u32) -> Result<bool, RuntimeError> {
    let num_imported_globals = count_imported_globals(module);

    if (global_idx as usize) < num_imported_globals {
        // This is an imported global - check the import declaration
        let import = module
            .imports
            .imports
            .iter()
            .filter(|imp| matches!(imp.external_kind, ExternalKind::Global(_)))
            .nth(global_idx as usize)
            .ok_or(RuntimeError::GlobalIndexOutOfBounds(global_idx))?;

        if let ExternalKind::Global(global_type) = &import.external_kind {
            Ok(global_type.mutable)
        } else {
            Ok(false)
        }
    } else {
        // This is a module-defined global
        let module_global_idx = (global_idx as usize - num_imported_globals) as u32;
        Ok(module
            .globals
            .get(module_global_idx)
            .map(|g| g.global_type.mutable)
            .unwrap_or(false))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_import_object_globals() {
        let mut imports = ImportObject::new();

        // Add some globals
        imports.add_global("env", "global1", Value::I32(42));
        imports.add_global("env", "global2", Value::F64(3.14));
        imports.add_global("js", "counter", Value::I64(100));

        // Retrieve them
        assert_eq!(imports.get_global("env", "global1"), Some(&Value::I32(42)));
        assert_eq!(imports.get_global("env", "global2"), Some(&Value::F64(3.14)));
        assert_eq!(imports.get_global("js", "counter"), Some(&Value::I64(100)));
        assert_eq!(imports.get_global("env", "missing"), None);
    }

    #[test]
    fn test_type_validation() {
        assert!(validate_type(&Value::I32(42), ValueType::I32));
        assert!(validate_type(&Value::I64(42), ValueType::I64));
        assert!(validate_type(&Value::F32(3.14), ValueType::F32));
        assert!(validate_type(&Value::F64(3.14), ValueType::F64));

        // Type mismatches
        assert!(!validate_type(&Value::I32(42), ValueType::I64));
        assert!(!validate_type(&Value::F32(3.14), ValueType::F64));
    }
}
