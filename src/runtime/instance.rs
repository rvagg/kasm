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

        // Calculate the number of imported functions
        let num_imported_functions = self
            .module
            .imports
            .imports
            .iter()
            .filter(|import| matches!(import.external_kind, crate::parser::module::ExternalKind::Function(_)))
            .count();

        // Check if this is an imported function (which we can't execute)
        if (func_idx as usize) < num_imported_functions {
            return Err(RuntimeError::UnimplementedInstruction(
                "Cannot execute imported functions".to_string(),
            ));
        }

        // Calculate the code section index (func_idx - num_imported)
        let code_idx = func_idx as usize - num_imported_functions;

        // Get the function declaration
        let func = self
            .module
            .functions
            .functions
            .get(code_idx)
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

        // Get the function body from the code section
        let body = self
            .module
            .code
            .code
            .get(code_idx)
            .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))?;

        // Use the pre-built structured representation from FunctionBody
        let structured_func = &body.body;

        // Execute the function
        let mut executor = Executor::new(self.module)?;
        executor.execute_function(structured_func, args, &func_type.return_types)
    }

    /// Get the module reference
    pub fn module(&self) -> &Module {
        self.module
    }
}
