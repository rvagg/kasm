//! Import resolution for WebAssembly modules
//!
//! This module provides import resolution for WebAssembly modules, supporting
//! global and function imports.

use super::{FuncAddr, GlobalAddr, MemoryAddr, RuntimeError, TableAddr, Value};
use crate::parser::module::{ExternalKind, Module, ValueType};
use std::collections::HashMap;

/// Container for imported values that a module can reference
#[derive(Debug, Clone, Default)]
pub struct ImportObject {
    /// Imported global variables mapped by (module_name, field_name) -> (GlobalAddr, type, mutable)
    pub globals: HashMap<(String, String), (GlobalAddr, ValueType, bool)>,
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

    /// Check if an import exists under any kind
    pub fn has_import(&self, module: &str, name: &str) -> bool {
        let key = (module.to_string(), name.to_string());
        self.functions.contains_key(&key)
            || self.globals.contains_key(&key)
            || self.memories.contains_key(&key)
            || self.tables.contains_key(&key)
    }

    /// Return "incompatible import type" if name exists under different kind,
    /// or "unknown import" if it doesn't exist at all.
    fn import_error(&self, module: &str, name: &str) -> RuntimeError {
        if self.has_import(module, name) {
            RuntimeError::IncompatibleImportType(format!("{module}.{name}"))
        } else {
            RuntimeError::UnknownExport(format!("{module}.{name}"))
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
            .ok_or_else(|| self.import_error(module, name))
    }

    /// Add a global import with its Store address, type, and mutability
    pub fn add_global(
        &mut self,
        module: impl Into<String>,
        name: impl Into<String>,
        addr: GlobalAddr,
        value_type: ValueType,
        mutable: bool,
    ) {
        self.globals
            .insert((module.into(), name.into()), (addr, value_type, mutable));
    }

    /// Get a global import's address
    pub fn get_global_addr(&self, module: &str, name: &str) -> Result<GlobalAddr, RuntimeError> {
        self.globals
            .get(&(module.to_string(), name.to_string()))
            .map(|(addr, _, _)| *addr)
            .ok_or_else(|| self.import_error(module, name))
    }

    /// Validate that a global import matches expected type and mutability
    pub fn validate_global(
        &self,
        module: &str,
        name: &str,
        expected_type: ValueType,
        expected_mutable: bool,
    ) -> Result<(), RuntimeError> {
        if let Some((_, vtype, mutable)) = self.globals.get(&(module.to_string(), name.to_string())) {
            if *mutable != expected_mutable {
                return Err(RuntimeError::IncompatibleImportType(format!("{module}.{name}")));
            }
            if *vtype != expected_type {
                return Err(RuntimeError::IncompatibleImportType(format!("{module}.{name}")));
            }
            return Ok(());
        }

        // Import not found — check if it exists under a different kind
        if self.has_import(module, name) {
            return Err(RuntimeError::IncompatibleImportType(format!("{module}.{name}")));
        }

        Err(RuntimeError::UnknownExport(format!("{module}.{name}")))
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
            .ok_or_else(|| self.import_error(module, name))
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
            .ok_or_else(|| self.import_error(module, name))
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
            | (Value::V128(_), ValueType::V128)
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

/// Get the value type of a global (handles both imported and module-defined globals)
pub fn global_value_type(module: &Module, global_idx: u32) -> Result<ValueType, RuntimeError> {
    let num_imported_globals = count_imported_globals(module);

    if (global_idx as usize) < num_imported_globals {
        let import = module
            .imports
            .imports
            .iter()
            .filter(|imp| matches!(imp.external_kind, ExternalKind::Global(_)))
            .nth(global_idx as usize)
            .ok_or(RuntimeError::GlobalIndexOutOfBounds(global_idx))?;

        if let ExternalKind::Global(global_type) = &import.external_kind {
            Ok(global_type.value_type)
        } else {
            Err(RuntimeError::GlobalIndexOutOfBounds(global_idx))
        }
    } else {
        let module_global_idx = (global_idx as usize - num_imported_globals) as u32;
        module
            .globals
            .get(module_global_idx)
            .map(|g| g.global_type.value_type)
            .ok_or(RuntimeError::GlobalIndexOutOfBounds(global_idx))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Store;

    #[test]
    fn test_import_object_globals() {
        let mut store = Store::new();
        let mut imports = ImportObject::new();

        // Add some globals (allocate in store first)
        let addr1 = store.allocate_global(Value::I32(42));
        let addr2 = store.allocate_global(Value::F64(3.14));
        let addr3 = store.allocate_global(Value::I64(100));

        imports.add_global("env", "global1", addr1, ValueType::I32, false);
        imports.add_global("env", "global2", addr2, ValueType::F64, true);
        imports.add_global("js", "counter", addr3, ValueType::I64, false);

        // Retrieve addresses
        assert_eq!(imports.get_global_addr("env", "global1").unwrap(), addr1);
        assert_eq!(imports.get_global_addr("env", "global2").unwrap(), addr2);
        assert_eq!(imports.get_global_addr("js", "counter").unwrap(), addr3);
        assert!(imports.get_global_addr("env", "missing").is_err());
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
