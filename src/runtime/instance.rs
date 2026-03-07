//! WebAssembly module instance

use super::{
    ExecutionOutcome, FuncAddr, GlobalAddr, MemoryAddr, RuntimeError, TableAddr, Value, executor::Executor,
    store::Resources,
};
use crate::parser::module::{ExportIndex, Module, Positional};
use std::collections::HashMap;
use std::sync::Arc;

/// A WebAssembly module instance
pub struct Instance {
    module: Arc<Module>,
    exports: HashMap<String, u32>, // Maps export name to function index
    /// Maps local function index to global FuncAddr
    function_addresses: Vec<FuncAddr>,
    /// Maps local memory index to global MemoryAddr
    pub(super) memory_addresses: Vec<MemoryAddr>,
    /// Maps local table index to global TableAddr
    table_addresses: Vec<TableAddr>,
    /// Maps local global index to global GlobalAddr
    global_addresses: Vec<GlobalAddr>,
    executor: Executor,
}

impl Instance {
    /// Create a new unlinked instance with resource address maps
    ///
    /// Resources (memories, tables, globals) live in the Store. The instance
    /// holds address maps that translate module-local indices to global addresses.
    /// Function addresses are linked separately via link_functions().
    pub(super) fn new_unlinked(
        module: Arc<Module>,
        memory_addresses: Vec<MemoryAddr>,
        table_addresses: Vec<TableAddr>,
        global_addresses: Vec<GlobalAddr>,
    ) -> Result<Self, RuntimeError> {
        let mut exports = HashMap::new();

        for export in &module.exports.exports {
            if let ExportIndex::Function(idx) = export.index {
                exports.insert(export.name.clone(), idx);
            }
        }

        let executor = Executor::new_unlinked(
            Arc::clone(&module),
            memory_addresses.clone(),
            table_addresses.clone(),
            global_addresses.clone(),
        )?;

        Ok(Instance {
            module,
            exports,
            function_addresses: Vec::new(),
            memory_addresses,
            table_addresses,
            global_addresses,
            executor,
        })
    }

    /// Link function addresses and initialise the instance
    ///
    /// Populates function addresses, then initialises globals, element segments,
    /// and data sections. This must happen after linking because init expressions
    /// can contain ref.func instructions that require the function address mapping.
    pub(super) fn link_functions(
        &mut self,
        function_addresses: Vec<FuncAddr>,
        resources: &mut Resources,
    ) -> Result<(), RuntimeError> {
        self.executor.link_function_addresses(function_addresses.clone());
        self.function_addresses = function_addresses;

        // Initialise in dependency order: globals first (element segments may
        // reference them), then element segments, then data sections.
        self.executor.initialise_globals(resources)?;
        self.executor.initialise_element_segments(resources)?;
        self.executor.initialise_data_sections(resources)?;

        Ok(())
    }

    /// Execute the start function if present.
    ///
    /// Returns `Some(FuncAddr)` if the start function is imported and needs
    /// external execution. Returns `None` if handled locally or no start function.
    pub(super) fn execute_start(&mut self, resources: &mut Resources) -> Result<Option<FuncAddr>, RuntimeError> {
        if self.module.start.has_position() {
            let start_func_idx = self.module.start.start;
            let num_imported_functions = self.module.imports.function_count();

            if (start_func_idx as usize) < num_imported_functions {
                // Imported start function — return FuncAddr for the Store to execute
                let func_addr = self
                    .function_addresses
                    .get(start_func_idx as usize)
                    .copied()
                    .ok_or(RuntimeError::FunctionIndexOutOfBounds(start_func_idx))?;
                return Ok(Some(func_addr));
            }

            let code_idx = start_func_idx as usize - num_imported_functions;
            let func_body = self
                .module
                .code
                .get(code_idx as u32)
                .ok_or(RuntimeError::FunctionIndexOutOfBounds(start_func_idx))?;

            self.executor.execute_function_with_locals(
                &func_body.body,
                vec![],
                &[],
                Some(&func_body.locals),
                resources,
            )?;
        }
        Ok(None)
    }

    /// Invoke an exported function by name
    ///
    /// Resolves the export, validates argument types, and delegates to the executor.
    /// Cannot handle cross-module calls — use `Store::invoke_export()` for that.
    ///
    /// # Errors
    /// - `UnknownExport` if the export doesn't exist
    /// - `TypeMismatch` if argument count or types don't match
    /// - `FunctionIndexOutOfBounds` if the function index is invalid
    pub fn invoke(
        &mut self,
        name: &str,
        args: Vec<Value>,
        resources: &mut Resources,
    ) -> Result<ExecutionOutcome, RuntimeError> {
        // Find the export
        let export_index = self
            .exports
            .get(name)
            .ok_or_else(|| RuntimeError::UnknownExport(name.to_string()))?;

        let func_idx = *export_index;
        let num_imported_functions = self.module.imports.function_count();

        // Imported functions can't be executed directly by the instance
        if (func_idx as usize) < num_imported_functions {
            return Err(RuntimeError::UnimplementedInstruction(
                "Cannot execute imported functions".to_string(),
            ));
        }

        // Functions section only contains non-imported functions
        let code_idx = func_idx as usize - num_imported_functions;

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

        for (i, (arg, expected_type)) in args.iter().zip(&func_type.parameters).enumerate() {
            if arg.typ() != *expected_type {
                return Err(RuntimeError::TypeMismatch {
                    expected: format!("{expected_type:?} for argument {i}"),
                    actual: format!("{:?}", arg.typ()),
                });
            }
        }

        let body = self
            .module
            .code
            .code
            .get(code_idx)
            .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))?;

        let structured_func = &body.body;

        self.executor.execute_function_with_locals(
            structured_func,
            args,
            &func_type.return_types,
            Some(&body.locals),
            resources,
        )
    }

    /// Invoke a function by its local function index
    ///
    /// Used by Store to execute functions. The func_idx includes both imported
    /// and local functions in the module's function index space.
    ///
    /// # Errors
    /// - `UnimplementedInstruction` if func_idx refers to an imported function
    /// - `FunctionIndexOutOfBounds` if the index is invalid
    /// - `TypeMismatch` if argument count or types don't match
    pub(super) fn invoke_by_index(
        &mut self,
        func_idx: u32,
        args: Vec<Value>,
        resources: &mut Resources,
    ) -> Result<ExecutionOutcome, RuntimeError> {
        let num_imported_functions = self.module.imports.function_count();

        if (func_idx as usize) < num_imported_functions {
            return Err(RuntimeError::UnimplementedInstruction(format!(
                "Cannot execute imported function at index {func_idx}"
            )));
        }

        let code_idx = func_idx as usize - num_imported_functions;

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

        let body = self
            .module
            .code
            .get(code_idx as u32)
            .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))?;

        self.executor.execute_function_with_locals(
            &body.body,
            args,
            &func_type.return_types,
            Some(&body.locals),
            resources,
        )
    }

    /// Resume execution after an external call completes
    ///
    /// Called by Store when a cross-module call returns with results.
    pub(super) fn resume_with_results(
        &mut self,
        results: Vec<Value>,
        resources: &mut Resources,
    ) -> Result<ExecutionOutcome, RuntimeError> {
        self.executor.resume_with_results(results, resources)
    }

    /// Get the FuncAddr for an exported function by name
    ///
    /// # Errors
    /// - `UnknownExport` if the export doesn't exist or isn't a function
    pub fn get_function_addr(&self, name: &str) -> Result<FuncAddr, RuntimeError> {
        let func_idx = self
            .exports
            .get(name)
            .ok_or_else(|| RuntimeError::UnknownExport(name.to_string()))?;

        self.function_addresses
            .get(*func_idx as usize)
            .copied()
            .ok_or_else(|| RuntimeError::UnknownExport(format!("function {} not linked", name)))
    }

    /// Get the module reference
    pub fn module(&self) -> &Module {
        &self.module
    }

    /// Get an exported global value by name
    ///
    /// # Errors
    /// - `UnknownExport` if the export doesn't exist or isn't a global
    pub fn get_global_export(&self, name: &str, resources: &Resources) -> Result<Value, RuntimeError> {
        let export = self
            .module
            .exports
            .get_by_name(name)
            .ok_or_else(|| RuntimeError::UnknownExport(name.to_string()))?;

        if let ExportIndex::Global(global_idx) = export.index {
            self.executor.get_global(global_idx, resources)
        } else {
            Err(RuntimeError::UnknownExport(format!("{} is not a global export", name)))
        }
    }

    /// Get the GlobalAddr for an exported global by name
    ///
    /// # Errors
    /// - `UnknownExport` if the export doesn't exist or isn't a global
    pub fn get_global_addr(&self, name: &str) -> Result<GlobalAddr, RuntimeError> {
        let export = self
            .module
            .exports
            .get_by_name(name)
            .ok_or_else(|| RuntimeError::UnknownExport(name.to_string()))?;

        if let ExportIndex::Global(global_idx) = export.index {
            self.global_addresses
                .get(global_idx as usize)
                .copied()
                .ok_or_else(|| RuntimeError::UnknownExport(format!("global {} not found", name)))
        } else {
            Err(RuntimeError::UnknownExport(format!("{} is not a global export", name)))
        }
    }

    /// Get the MemoryAddr for an exported memory by name
    ///
    /// # Errors
    /// - `UnknownExport` if the export doesn't exist or isn't a memory
    pub fn get_memory_addr(&self, name: &str) -> Result<MemoryAddr, RuntimeError> {
        let export = self
            .module
            .exports
            .get_by_name(name)
            .ok_or_else(|| RuntimeError::UnknownExport(name.to_string()))?;

        if let ExportIndex::Memory(mem_idx) = export.index {
            self.memory_addresses
                .get(mem_idx as usize)
                .copied()
                .ok_or_else(|| RuntimeError::UnknownExport(format!("memory {} not found", name)))
        } else {
            Err(RuntimeError::UnknownExport(format!("{} is not a memory export", name)))
        }
    }

    /// Get the TableAddr for an exported table by name
    ///
    /// # Errors
    /// - `UnknownExport` if the export doesn't exist or isn't a table
    pub fn get_table_addr(&self, name: &str) -> Result<TableAddr, RuntimeError> {
        let export = self
            .module
            .exports
            .get_by_name(name)
            .ok_or_else(|| RuntimeError::UnknownExport(name.to_string()))?;

        if let ExportIndex::Table(table_idx) = export.index {
            self.table_addresses
                .get(table_idx as usize)
                .copied()
                .ok_or_else(|| RuntimeError::UnknownExport(format!("table {} not found", name)))
        } else {
            Err(RuntimeError::UnknownExport(format!("{} is not a table export", name)))
        }
    }

    /// Set an instruction budget limit for execution
    ///
    /// When set, execution returns `RuntimeError::InstructionBudgetExhausted`
    /// after the given number of instructions have been executed. Pass `None`
    /// to remove the limit.
    pub fn set_instruction_budget(&mut self, budget: Option<u64>) {
        self.executor.set_instruction_budget(budget);
    }
}
