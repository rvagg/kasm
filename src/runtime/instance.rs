//! WebAssembly module instance

use super::{executor::Executor, RuntimeError, Value};
use crate::parser::module::{ExportIndex, Module};
use std::collections::HashMap;

/// A WebAssembly module instance
pub struct Instance<'a> {
    module: &'a Module,
    exports: HashMap<String, u32>, // Maps export name to function index
}

impl<'a> Instance<'a> {
    /// Create a new instance from a parsed module
    pub fn new(module: &'a Module) -> Self {
        let mut exports = HashMap::new();

        // Build export map
        for export in &module.exports.exports {
            if let ExportIndex::Function(idx) = export.index {
                exports.insert(export.name.clone(), idx);
            }
        }

        Instance { module, exports }
    }

    /// Invoke an exported function by name
    pub fn invoke(&self, name: &str, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
        // Find the export
        let export_index = self
            .exports
            .get(name)
            .ok_or_else(|| RuntimeError::UnknownExport(name.to_string()))?;

        // Get the function index
        let func_idx = *export_index;

        // Get the function type
        let func = self
            .module
            .functions
            .functions
            .get(func_idx as usize)
            .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))?;

        let func_type = self
            .module
            .types
            .types
            .get(func.ftype_index as usize)
            .ok_or(RuntimeError::InvalidFunctionType)?;

        // Check argument count
        if args.len() != func_type.parameters.len() {
            return Err(RuntimeError::TypeMismatch {
                expected: format!("{} arguments", func_type.parameters.len()),
                actual: format!("{} arguments", args.len()),
            });
        }

        // Check argument types
        for (i, (arg, expected_type)) in args.iter().zip(&func_type.parameters).enumerate() {
            if arg.typ() != *expected_type {
                return Err(RuntimeError::TypeMismatch {
                    expected: format!("{expected_type:?} for argument {i}"),
                    actual: format!("{:?}", arg.typ()),
                });
            }
        }

        // Get the function body
        let body = self
            .module
            .code
            .code
            .get(func_idx as usize)
            .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))?;

        // Execute the function
        let mut executor = Executor::new(self.module);
        executor.execute_function(func_idx, &body.instructions, args, &func_type.return_types)
    }

    /// Get the module reference
    pub fn module(&self) -> &Module {
        self.module
    }
}
