//! WebAssembly module instance

use super::{executor::Executor, imports::ImportObject, ExecutionOutcome, FuncAddr, RuntimeError, Value};
use crate::parser::module::{ExportIndex, Module, Positional};
use std::collections::HashMap;

/// A WebAssembly module instance
pub struct Instance<'a> {
    module: &'a Module,
    exports: HashMap<String, u32>, // Maps export name to function index
    /// Maps local function index to global FuncAddr
    function_addresses: Vec<FuncAddr>,
    executor: Executor<'a>, // Persistent executor to maintain state
}

impl<'a> Instance<'a> {
    /// Create a new instance without function address linking
    ///
    /// This is called by Store::create_instance(). Use Store::create_instance() instead
    /// of calling this directly.
    pub(super) fn new_unlinked(module: &'a Module, imports: Option<&ImportObject>) -> Result<Self, RuntimeError> {
        let mut exports = HashMap::new();

        // Build export map
        for export in &module.exports.exports {
            if let ExportIndex::Function(idx) = export.index {
                exports.insert(export.name.clone(), idx);
            }
        }

        // Create persistent executor - this initialises memory and globals
        let executor = Executor::new(module, imports)?;

        Ok(Instance {
            module,
            exports,
            function_addresses: Vec::new(),
            executor,
        })
    }

    /// Link function addresses after instance creation
    ///
    /// This is called by Store::create_instance() to populate function addresses.
    pub(super) fn link_functions(&mut self, function_addresses: Vec<FuncAddr>) -> Result<(), RuntimeError> {
        self.executor.link_function_addresses(function_addresses.clone());
        self.function_addresses = function_addresses;

        // Now that function addresses are linked, initialise globals with their init expressions
        // (they may contain ref.func instructions that need the address mapping)
        self.executor.initialise_globals()?;

        // Then initialise element segments (which may also contain ref.func instructions)
        self.executor.initialise_element_segments()?;

        Ok(())
    }

    /// Execute the start function if present
    ///
    /// This should be called after linking functions.
    pub(super) fn execute_start(&mut self) -> Result<(), RuntimeError> {
        if self.module.start.has_position() {
            let start_func_idx = self.module.start.start;
            let num_imported_functions = self.module.imports.function_count();

            if (start_func_idx as usize) < num_imported_functions {
                return Err(RuntimeError::UnimplementedInstruction(
                    "Cannot execute imported start function".to_string(),
                ));
            }

            let code_idx = start_func_idx as usize - num_imported_functions;
            let func_body = self
                .module
                .code
                .get(code_idx as u32)
                .ok_or(RuntimeError::FunctionIndexOutOfBounds(start_func_idx))?;

            self.executor
                .execute_function_with_locals(&func_body.body, vec![], &[], Some(&func_body.locals))?;
        }
        Ok(())
    }

    /// Invoke an exported function by name
    ///
    /// Note: This method cannot handle cross-module calls. Use `Store::invoke_export()`
    /// for functions that may call into other modules.
    pub fn invoke(&mut self, name: &str, args: Vec<Value>) -> Result<ExecutionOutcome, RuntimeError> {
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

        // Execute the function with locals information using the persistent executor
        self.executor
            .execute_function_with_locals(structured_func, args, &func_type.return_types, Some(&body.locals))
    }

    /// Invoke a function by its local function index
    ///
    /// This is used by Store to execute functions. The func_idx is the local
    /// function index (includes both imported and local functions).
    /// Returns `ExecutionOutcome` which may indicate completion or need for external call.
    pub(super) fn invoke_by_index(
        &mut self,
        func_idx: u32,
        args: Vec<Value>,
    ) -> Result<ExecutionOutcome, RuntimeError> {
        let num_imported_functions = self.module.imports.function_count();

        // For imported functions, we can't execute them directly
        // The Store should have already resolved these, so this is an error
        if (func_idx as usize) < num_imported_functions {
            return Err(RuntimeError::UnimplementedInstruction(format!(
                "Cannot execute imported function at index {func_idx}"
            )));
        }

        // Calculate the code section index
        let code_idx = func_idx as usize - num_imported_functions;

        // Get function type
        let func = self
            .module
            .functions
            .get((code_idx) as u32)
            .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))?;

        let func_type = self
            .module
            .types
            .get(func.ftype_index)
            .ok_or(RuntimeError::InvalidFunctionType)?;

        // Type check arguments
        if args.len() != func_type.parameters.len() {
            return Err(RuntimeError::TypeMismatch {
                expected: format!("{} arguments", func_type.parameters.len()),
                actual: format!("{} arguments", args.len()),
            });
        }

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
            .get(code_idx as u32)
            .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))?;

        // Execute
        self.executor
            .execute_function_with_locals(&body.body, args, &func_type.return_types, Some(&body.locals))
    }

    /// Resume execution after an external call completes
    ///
    /// This is called by Store after an external function returns.
    pub(super) fn resume_with_results(&mut self, results: Vec<Value>) -> Result<ExecutionOutcome, RuntimeError> {
        self.executor.resume_with_results(results)
    }

    /// Get the FuncAddr for an exported function by name
    ///
    /// # Errors
    /// - Returns `UnknownExport` if the export doesn't exist or isn't a function
    pub fn get_function_addr(&self, name: &str) -> Result<FuncAddr, RuntimeError> {
        let func_idx = self
            .exports
            .get(name)
            .ok_or_else(|| RuntimeError::UnknownExport(name.to_string()))?;

        self.function_addresses
            .get(*func_idx as usize)
            .copied()
            .ok_or_else(|| RuntimeError::UnknownExport(format!("Function {} not linked", name)))
    }

    /// Get the module reference
    pub fn module(&self) -> &Module {
        self.module
    }

    /// Get an exported global value by name
    ///
    /// # Errors
    /// - Returns `UnknownExport` if the export doesn't exist or isn't a global
    pub fn get_global_export(&self, name: &str) -> Result<Value, RuntimeError> {
        // Find the export using the helper
        let export = self
            .module
            .exports
            .get_by_name(name)
            .ok_or_else(|| RuntimeError::UnknownExport(name.to_string()))?;

        // Check it's a global export
        if let ExportIndex::Global(global_idx) = export.index {
            self.executor.get_global(global_idx)
        } else {
            Err(RuntimeError::UnknownExport(format!("{} is not a global export", name)))
        }
    }
}
