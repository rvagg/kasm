//! WebAssembly Store - manages runtime instances and provides global function addressing
//!
//! The Store is the central runtime component that owns all module instances and provides
//! a global address space for functions. This enables proper cross-module function references
//! as required by the WebAssembly specification.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │                          Store                              │
//! │  ┌────────────────────────────────────────────────────────┐ │
//! │  │ Function Space (FuncAddr -> FunctionInstance)          │ │
//! │  │  [0]: Host { print }                                   │ │
//! │  │  [1]: Host { print_i32 }                               │ │
//! │  │  [2]: Wasm { instance: 0, func: 0 }                    │ │
//! │  │  [3]: Wasm { instance: 1, func: 0 }                    │ │
//! │  └────────────────────────────────────────────────────────┘ │
//! │  ┌────────────────────────────────────────────────────────┐ │
//! │  │ Instance Registry                                      │ │
//! │  │  [0]: module_a                                         │ │
//! │  │  [1]: module_b (imports func from module_a)            │ │
//! │  └────────────────────────────────────────────────────────┘ │
//! └─────────────────────────────────────────────────────────────┘
//! ```
//!
//! # Key Design Decisions
//!
//! - **FuncAddr is globally unique**: Allocated by Store, works across module boundaries
//! - **Store owns all instances**: Proper lifecycle management
//! - **Execution through Store**: All calls routed through Store.execute()
//! - **Resumable execution**: Cross-module calls return `NeedsExternalCall`, Store handles delegation
//!
//! # Cross-Module Execution Flow
//!
//! When execution encounters a function from another module (via import or call_indirect):
//!
//! 1. Executor returns `NeedsExternalCall` with the target FuncAddr
//! 2. Store.execute() pushes the calling instance onto a call stack
//! 3. Store dispatches to the target (wasm instance or host function)
//! 4. When target completes, Store pops the call stack and resumes the caller
//! 5. This continues until the call stack is empty
//!
//! The call stack enables arbitrarily deep cross-module chains (A -> B -> C -> ...)
//! where each module can perform computation before/after its external calls.

use super::imports::{global_value_type, is_global_mutable};
use super::{ExecutionOutcome, Instance, Memory, RuntimeError, Table, Value};
use crate::parser::module::{ExportIndex, ExternalKind, FunctionType, Limits};
use std::cell::{Cell, RefCell};
use std::rc::Rc;

/// Type alias for host function implementations
type HostFunc = Box<dyn Fn(Vec<Value>) -> Result<Vec<Value>, RuntimeError>>;

/// The next action in the cross-module execution loop.
///
/// Alternates between calling a new function and resuming a suspended instance
/// with the results of a completed call.
enum PendingAction {
    Call(FuncAddr, Vec<Value>),
    Resume(usize, Vec<Value>),
}

/// Global function address - index into the Store's function space
///
/// FuncAddr provides stable, globally-unique identifiers for functions that work
/// across module boundaries, enabling proper funcref semantics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncAddr(pub usize);

/// Global memory address - index into the Store's memory registry
///
/// MemoryAddr provides stable, globally-unique identifiers for memory instances
/// that can be shared across module boundaries for memory imports.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MemoryAddr(pub usize);

/// Global table address - index into the Store's table registry
///
/// TableAddr provides stable, globally-unique identifiers for table instances
/// that can be shared across module boundaries for table imports.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TableAddr(pub usize);

/// Global address - index into the Store's global registry
///
/// GlobalAddr provides stable, globally-unique identifiers for global instances
/// that can be shared across module boundaries for mutable global imports.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalAddr(pub usize);

/// Shared global instance for cross-module mutable global aliasing
///
/// Uses Rc<Cell<>> since WebAssembly is single-threaded and Value is Copy.
/// When module A exports a mutable global and module B imports it, both
/// modules share the same Rc<Cell<Value>>, so mutations are visible to both.
pub type SharedGlobal = Rc<Cell<Value>>;

/// Shared memory instance for cross-module access
///
/// Uses Rc<RefCell<>> since WebAssembly is single-threaded and Memory is not Copy.
/// When module A exports memory and module B imports it, both share the same
/// Rc<RefCell<Memory>>, so mutations are visible to both.
pub type SharedMemory = Rc<RefCell<Memory>>;

/// Shared table instance for cross-module access
///
/// Uses Rc<RefCell<>> since WebAssembly is single-threaded and Table is not Copy.
/// When module A exports a table and module B imports it, both share the same
/// Rc<RefCell<Table>>, so mutations are visible to both.
pub type SharedTable = Rc<RefCell<Table>>;

/// A function instance in the Store
///
/// Functions can either be WebAssembly functions (executed by an instance's executor)
/// or host functions (native Rust functions provided for imports).
pub enum FunctionInstance {
    /// WebAssembly function - reference to function within an instance
    Wasm {
        /// Index of the instance in Store.instances
        instance_id: usize,
        /// Function index within the instance's function space (includes imports)
        func_idx: u32,
        /// Cached function type for quick access
        func_type: FunctionType,
    },
    /// Host function - native Rust function
    Host {
        /// The host function implementation
        func: HostFunc,
        /// Function type signature
        func_type: FunctionType,
    },
}

/// Validate that actual limits are compatible with expected limits for an import.
///
/// The imported entity's minimum must be >= the declared minimum, and if a maximum
/// is declared, the imported entity must also have a maximum that is <= the declared one.
fn validate_import_limits(
    actual_min: u32,
    actual_max: Option<u32>,
    expected: &Limits,
    module_name: &str,
    field_name: &str,
) -> Result<(), RuntimeError> {
    if actual_min < expected.min {
        return Err(RuntimeError::IncompatibleImportType(format!(
            "{}.{}",
            module_name, field_name
        )));
    }
    if let Some(expected_max) = expected.max {
        match actual_max {
            Some(actual_max) if actual_max <= expected_max => {}
            _ => {
                return Err(RuntimeError::IncompatibleImportType(format!(
                    "{}.{}",
                    module_name, field_name
                )));
            }
        }
    }
    Ok(())
}

/// The WebAssembly Store - owns all instances and provides execution context
///
/// The Store manages the lifetime of all module instances and provides a global
/// function address space. All function calls are routed through the Store, which
/// delegates to the appropriate instance.
pub struct Store<'a> {
    /// All function instances, indexed by FuncAddr
    functions: Vec<FunctionInstance>,

    /// All module instances owned by this store
    instances: Vec<Instance<'a>>,

    /// All memory instances, indexed by MemoryAddr
    /// Shared via Rc<RefCell<>> to enable cross-module memory imports
    memories: Vec<SharedMemory>,

    /// All table instances, indexed by TableAddr
    /// Shared via Rc<RefCell<>> to enable cross-module table imports
    tables: Vec<SharedTable>,

    /// All global instances, indexed by GlobalAddr
    /// Shared via Rc<Cell<>> to enable cross-module mutable global aliasing
    globals: Vec<SharedGlobal>,
}

impl<'a> Store<'a> {
    /// Create a new empty Store
    pub fn new() -> Self {
        Store {
            functions: Vec::new(),
            instances: Vec::new(),
            memories: Vec::new(),
            tables: Vec::new(),
            globals: Vec::new(),
        }
    }

    /// Allocate a new function address and register a function instance
    ///
    /// Returns the FuncAddr that can be used to call this function.
    pub fn allocate_function(&mut self, func: FunctionInstance) -> FuncAddr {
        let addr = FuncAddr(self.functions.len());
        self.functions.push(func);
        addr
    }

    /// Allocate a new memory address and register a memory instance
    ///
    /// The memory is wrapped in Rc<RefCell<>> for shared access across modules.
    /// Returns the MemoryAddr that can be used to reference this memory.
    pub fn allocate_memory(&mut self, memory: Memory) -> MemoryAddr {
        let addr = MemoryAddr(self.memories.len());
        self.memories.push(Rc::new(RefCell::new(memory)));
        addr
    }

    /// Allocate a new table address and register a table instance
    ///
    /// The table is wrapped in Rc<RefCell<>> for shared access across modules.
    /// Returns the TableAddr that can be used to reference this table.
    pub fn allocate_table(&mut self, table: Table) -> TableAddr {
        let addr = TableAddr(self.tables.len());
        self.tables.push(Rc::new(RefCell::new(table)));
        addr
    }

    /// Get a shared reference to a memory by its address
    ///
    /// Returns None if the address is invalid.
    pub fn get_memory(&self, addr: MemoryAddr) -> Option<&SharedMemory> {
        self.memories.get(addr.0)
    }

    /// Get a shared reference to a table by its address
    ///
    /// Returns None if the address is invalid.
    pub fn get_table(&self, addr: TableAddr) -> Option<&SharedTable> {
        self.tables.get(addr.0)
    }

    /// Allocate a new global in the Store
    ///
    /// Returns the GlobalAddr that can be used to reference this global.
    pub fn allocate_global(&mut self, value: Value) -> GlobalAddr {
        let addr = GlobalAddr(self.globals.len());
        self.globals.push(Rc::new(Cell::new(value)));
        addr
    }

    /// Get a shared global cell by its address
    ///
    /// Returns None if the address is invalid.
    pub fn get_shared_global(&self, addr: GlobalAddr) -> Option<&SharedGlobal> {
        self.globals.get(addr.0)
    }

    /// Create and register a new instance in the Store
    ///
    /// Resolves imports, creates local resources, links functions, and initialises
    /// the instance. Returns the instance ID.
    pub fn create_instance(
        &mut self,
        module: &'a crate::parser::module::Module,
        imports: Option<&super::ImportObject>,
    ) -> Result<usize, RuntimeError> {
        let instance_id = self.instances.len();

        let (memories, memory_addresses) = self.resolve_memories(module, imports)?;
        let (tables, table_addresses) = self.resolve_tables(module, imports)?;
        let (globals, global_addresses) = self.resolve_globals(module, imports)?;

        let mut instance = Instance::new_unlinked(
            module,
            memories,
            tables,
            globals,
            memory_addresses,
            table_addresses,
            global_addresses,
        )?;

        let function_addresses = self.resolve_functions(module, imports, instance_id)?;

        // Link functions, then push instance before propagating errors.
        // The spec requires side effects on shared tables/memories to persist
        // even when instantiation fails (e.g., OOB element segments).
        let link_result = instance.link_functions(function_addresses);
        self.instances.push(instance);
        link_result?;

        self.execute_start_function(instance_id)?;

        Ok(instance_id)
    }

    /// Resolve memory imports or create local memories.
    ///
    /// WebAssembly 1.0 allows at most one memory, either imported or locally defined.
    fn resolve_memories(
        &mut self,
        module: &crate::parser::module::Module,
        imports: Option<&super::ImportObject>,
    ) -> Result<(Vec<SharedMemory>, Vec<MemoryAddr>), RuntimeError> {
        let mut memories = Vec::new();
        let mut addresses = Vec::new();

        let has_memory_import = module
            .imports
            .imports
            .iter()
            .any(|imp| matches!(imp.external_kind, ExternalKind::Memory(_)));

        if has_memory_import {
            for import in &module.imports.imports {
                if let ExternalKind::Memory(expected_limits) = &import.external_kind {
                    let import_obj = imports.ok_or_else(|| {
                        RuntimeError::MemoryError(format!("memory import {}.{} not found", import.module, import.name))
                    })?;

                    let mem_addr = import_obj.get_memory(&import.module, &import.name)?;
                    let shared_mem = self
                        .get_memory(mem_addr)
                        .ok_or_else(|| {
                            RuntimeError::MemoryError(format!(
                                "memory import {}.{} not found in store",
                                import.module, import.name
                            ))
                        })?
                        .clone();

                    let mem = shared_mem.borrow();
                    validate_import_limits(
                        mem.size(),
                        mem.max_pages(),
                        expected_limits,
                        &import.module,
                        &import.name,
                    )?;
                    drop(mem);

                    memories.push(shared_mem);
                    addresses.push(mem_addr);
                }
            }
        } else {
            for mem_def in &module.memory.memory {
                let memory = Memory::new(mem_def.limits.min, mem_def.limits.max)?;
                let addr = self.allocate_memory(memory);
                memories.push(self.get_memory(addr).unwrap().clone());
                addresses.push(addr);
            }
        }

        Ok((memories, addresses))
    }

    /// Resolve table imports and create local tables.
    ///
    /// Unlike memories, tables can have both imports and locals (sequential in the index space).
    fn resolve_tables(
        &mut self,
        module: &crate::parser::module::Module,
        imports: Option<&super::ImportObject>,
    ) -> Result<(Vec<SharedTable>, Vec<TableAddr>), RuntimeError> {
        let mut tables = Vec::new();
        let mut addresses = Vec::new();

        for import in &module.imports.imports {
            if let ExternalKind::Table(expected_table_type) = &import.external_kind {
                let import_obj = imports.ok_or_else(|| {
                    RuntimeError::MemoryError(format!("table import {}.{} not found", import.module, import.name))
                })?;

                let table_addr = import_obj.get_table(&import.module, &import.name)?;
                let shared_table = self
                    .get_table(table_addr)
                    .ok_or_else(|| {
                        RuntimeError::MemoryError(format!(
                            "table import {}.{} not found in store",
                            import.module, import.name
                        ))
                    })?
                    .clone();

                let tbl = shared_table.borrow();
                if tbl.ref_type() != expected_table_type.ref_type {
                    return Err(RuntimeError::IncompatibleImportType(format!(
                        "{}.{}",
                        import.module, import.name
                    )));
                }
                let actual_limits = tbl.limits();
                validate_import_limits(
                    actual_limits.min,
                    actual_limits.max,
                    &expected_table_type.limits,
                    &import.module,
                    &import.name,
                )?;
                drop(tbl);

                tables.push(shared_table);
                addresses.push(table_addr);
            }
        }

        for table_type in &module.table.tables {
            let table = Table::new(table_type.ref_type, table_type.limits)?;
            let addr = self.allocate_table(table);
            tables.push(self.get_table(addr).unwrap().clone());
            addresses.push(addr);
        }

        Ok((tables, addresses))
    }

    /// Resolve global imports and create local globals.
    ///
    /// Imported globals share the same Rc<Cell<Value>> so mutations are visible across modules.
    /// Local globals are allocated with default values; init expressions are evaluated later.
    fn resolve_globals(
        &mut self,
        module: &crate::parser::module::Module,
        imports: Option<&super::ImportObject>,
    ) -> Result<(Vec<SharedGlobal>, Vec<GlobalAddr>), RuntimeError> {
        let mut globals = Vec::new();
        let mut addresses = Vec::new();

        for import in &module.imports.imports {
            if let ExternalKind::Global(global_type) = &import.external_kind {
                let import_obj = imports.ok_or_else(|| {
                    RuntimeError::MemoryError(format!("global import {}.{} not found", import.module, import.name))
                })?;

                let global_addr = import_obj.get_global_addr(&import.module, &import.name)?;
                import_obj.validate_global(
                    &import.module,
                    &import.name,
                    global_type.value_type,
                    global_type.mutable,
                )?;

                let shared = self
                    .get_shared_global(global_addr)
                    .ok_or_else(|| {
                        RuntimeError::MemoryError(format!(
                            "global import {}.{} not found in store",
                            import.module, import.name
                        ))
                    })?
                    .clone();

                globals.push(shared);
                addresses.push(global_addr);
            }
        }

        for global in &module.globals.globals {
            let default_value = super::imports::default_value_for_type(global.global_type.value_type)?;
            let addr = self.allocate_global(default_value);
            globals.push(self.get_shared_global(addr).unwrap().clone());
            addresses.push(addr);
        }

        Ok((globals, addresses))
    }

    /// Resolve function imports and allocate local function addresses.
    ///
    /// Imported functions are validated against their expected type signatures.
    /// Local functions are registered as Wasm function instances in the Store.
    fn resolve_functions(
        &mut self,
        module: &crate::parser::module::Module,
        imports: Option<&super::ImportObject>,
        instance_id: usize,
    ) -> Result<Vec<FuncAddr>, RuntimeError> {
        let num_imported_funcs = module.imports.function_count();
        let mut addresses = Vec::new();

        for import in &module.imports.imports {
            if let ExternalKind::Function(type_idx) = &import.external_kind {
                let import_obj = imports.ok_or_else(|| {
                    RuntimeError::UnknownFunction(format!("import {}.{} not found", import.module, import.name))
                })?;

                let addr = import_obj.get_function(&import.module, &import.name)?;
                let expected_type = module.types.get(*type_idx).ok_or(RuntimeError::InvalidFunctionType)?;
                let actual_type = self.get_function_type(addr)?;

                if expected_type != actual_type {
                    return Err(RuntimeError::ImportTypeMismatch {
                        module: import.module.clone(),
                        name: import.name.clone(),
                        expected: format!("{:?}", expected_type),
                        actual: format!("{:?}", actual_type),
                    });
                }

                addresses.push(addr);
            }
        }

        for (local_idx, func) in module.functions.functions.iter().enumerate() {
            let func_idx = (num_imported_funcs + local_idx) as u32;
            let func_type = module
                .types
                .get(func.ftype_index)
                .ok_or(RuntimeError::InvalidFunctionType)?
                .clone();

            let addr = self.allocate_function(FunctionInstance::Wasm {
                instance_id,
                func_idx,
                func_type,
            });
            addresses.push(addr);
        }

        Ok(addresses)
    }

    /// Execute the start function for a newly instantiated module.
    fn execute_start_function(&mut self, instance_id: usize) -> Result<(), RuntimeError> {
        match self.instances[instance_id].execute_start()? {
            Some(func_addr) => {
                // Imported start function — dispatch through Store
                match self.functions.get(func_addr.0) {
                    Some(FunctionInstance::Host { func, .. }) => func(vec![]).map(|_| ()),
                    Some(FunctionInstance::Wasm { .. }) => Err(RuntimeError::UnimplementedInstruction(
                        "cross-module wasm start function".to_string(),
                    )),
                    None => Err(RuntimeError::FunctionIndexOutOfBounds(func_addr.0 as u32)),
                }
            }
            None => Ok(()),
        }
    }

    /// Get a reference to an instance by ID
    pub fn get_instance(&self, instance_id: usize) -> Option<&Instance<'a>> {
        self.instances.get(instance_id)
    }

    /// Get a mutable reference to an instance by ID
    pub fn get_instance_mut(&mut self, instance_id: usize) -> Option<&mut Instance<'a>> {
        self.instances.get_mut(instance_id)
    }

    /// Get the function type for a FuncAddr
    ///
    /// # Errors
    /// - Returns error if FuncAddr is invalid
    pub fn get_function_type(&self, addr: FuncAddr) -> Result<&FunctionType, RuntimeError> {
        match self.functions.get(addr.0) {
            Some(FunctionInstance::Wasm { func_type, .. }) => Ok(func_type),
            Some(FunctionInstance::Host { func_type, .. }) => Ok(func_type),
            None => Err(RuntimeError::FunctionIndexOutOfBounds(addr.0 as u32)),
        }
    }

    /// Execute a single function (host or wasm) and return its outcome
    ///
    /// This is a helper that handles the dispatch to either host functions
    /// or wasm instance functions.
    fn execute_one(
        &mut self,
        addr: FuncAddr,
        args: Vec<Value>,
    ) -> Result<(ExecutionOutcome, Option<usize>), RuntimeError> {
        match self.functions.get(addr.0) {
            Some(FunctionInstance::Wasm {
                instance_id, func_idx, ..
            }) => {
                let instance_id = *instance_id;
                let func_idx = *func_idx;
                let instance = self
                    .get_instance_mut(instance_id)
                    .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))?;
                let outcome = instance.invoke_by_index(func_idx, args)?;
                Ok((outcome, Some(instance_id)))
            }
            Some(FunctionInstance::Host { func, .. }) => {
                let results = func(args)?;
                Ok((ExecutionOutcome::Complete(results), None))
            }
            None => Err(RuntimeError::FunctionIndexOutOfBounds(addr.0 as u32)),
        }
    }

    /// Execute a function by its address, handling cross-module calls.
    ///
    /// Execution alternates between two actions: calling a new function, or resuming
    /// a suspended instance with results from a completed call. The call stack tracks
    /// instances waiting for results.
    ///
    /// ```text
    /// Call(A) -> A completes -> return results   (simple case)
    /// Call(A) -> A needs B   -> push A, Call(B)
    ///                        -> B completes -> pop A, Resume(A, results)
    ///                                       -> A completes -> return results
    /// ```
    pub fn execute(&mut self, addr: FuncAddr, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
        let mut call_stack: Vec<usize> = Vec::new();
        let mut action = PendingAction::Call(addr, args);

        loop {
            let (outcome, source_instance) = match action {
                PendingAction::Call(addr, args) => self.execute_one(addr, args)?,
                PendingAction::Resume(instance_id, results) => {
                    let instance = self
                        .get_instance_mut(instance_id)
                        .ok_or(RuntimeError::FunctionIndexOutOfBounds(0))?;
                    let outcome = instance.resume_with_results(results)?;
                    (outcome, Some(instance_id))
                }
            };

            match outcome {
                ExecutionOutcome::Complete(results) => {
                    if let Some(caller_id) = call_stack.pop() {
                        action = PendingAction::Resume(caller_id, results);
                    } else {
                        return Ok(results);
                    }
                }
                ExecutionOutcome::NeedsExternalCall(request) => {
                    if let Some(instance_id) = source_instance {
                        call_stack.push(instance_id);
                    }
                    action = PendingAction::Call(request.func_addr, request.args);
                }
            }
        }
    }

    /// Invoke an exported function by name on a specific instance
    ///
    /// This is the recommended way to invoke functions when cross-module calls may occur.
    ///
    /// If `instruction_budget` is `Some(n)`, execution will stop with
    /// `RuntimeError::InstructionBudgetExhausted` after `n` instructions.
    pub fn invoke_export(
        &mut self,
        instance_id: usize,
        name: &str,
        args: Vec<Value>,
        instruction_budget: Option<u64>,
    ) -> Result<Vec<Value>, RuntimeError> {
        // Set instruction budget on the instance if specified
        if let Some(instance) = self.get_instance_mut(instance_id) {
            instance.set_instruction_budget(instruction_budget);
        }

        let func_addr = {
            let instance = self
                .get_instance(instance_id)
                .ok_or(RuntimeError::FunctionIndexOutOfBounds(0))?;
            instance.get_function_addr(name)?
        };

        let result = self.execute(func_addr, args);

        // Clear the budget after execution
        if let Some(instance) = self.get_instance_mut(instance_id) {
            instance.set_instruction_budget(None);
        }

        result
    }

    /// Register all exports from an instance as imports under a given module name.
    ///
    /// This implements the `.wast` `(register "name")` directive: every function,
    /// global, memory, and table exported by the instance becomes available for
    /// import under `as_name`.
    pub fn register_exports(
        &self,
        instance_id: usize,
        as_name: &str,
        imports: &mut super::ImportObject,
    ) -> Result<(), RuntimeError> {
        let instance = self
            .get_instance(instance_id)
            .ok_or(RuntimeError::FunctionIndexOutOfBounds(0))?;
        let module = instance.module();

        for export in &module.exports.exports {
            match export.index {
                ExportIndex::Function(_) => {
                    if let Ok(addr) = instance.get_function_addr(&export.name) {
                        imports.add_function(as_name, &export.name, addr);
                    }
                }
                ExportIndex::Global(global_idx) => {
                    if let Ok(addr) = instance.get_global_addr(&export.name) {
                        let mutable = is_global_mutable(module, global_idx).unwrap_or(false);
                        let vtype =
                            global_value_type(module, global_idx).unwrap_or(crate::parser::module::ValueType::I32);
                        imports.add_global(as_name, &export.name, addr, vtype, mutable);
                    }
                }
                ExportIndex::Memory(_) => {
                    if let Ok(addr) = instance.get_memory_addr(&export.name) {
                        imports.add_memory(as_name, &export.name, addr);
                    }
                }
                ExportIndex::Table(_) => {
                    if let Ok(addr) = instance.get_table_addr(&export.name) {
                        imports.add_table(as_name, &export.name, addr);
                    }
                }
            }
        }
        Ok(())
    }
}

impl<'a> Default for Store<'a> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::instruction::{ByteRange, Instruction, InstructionKind};
    use crate::parser::module::{
        CodeSection, Export, ExportIndex, ExportSection, Function, FunctionBody, FunctionSection, Import,
        ImportSection, Locals, Module, SectionPosition, TypeSection, ValueType,
    };
    use crate::parser::structure_builder::StructureBuilder;
    use crate::parser::structured::StructuredFunction;
    use crate::runtime::ImportObject;

    /// Create a module that imports a function with the given type signature
    fn module_with_function_import(
        module_name: &str,
        func_name: &str,
        type_idx: u32,
        param_types: Vec<ValueType>,
        return_types: Vec<ValueType>,
    ) -> Module {
        let mut module = Module::new("test");

        // Add the function type
        module.types = TypeSection {
            types: vec![FunctionType {
                parameters: param_types,
                return_types,
            }],
            position: SectionPosition { start: 0, end: 0 },
        };

        // Add the function import
        module.imports = ImportSection {
            imports: vec![Import {
                module: module_name.to_string(),
                name: func_name.to_string(),
                external_kind: ExternalKind::Function(type_idx),
            }],
            position: SectionPosition { start: 0, end: 0 },
        };

        module
    }

    #[test]
    fn test_import_type_match_succeeds() {
        let mut store = Store::new();

        // Register a host function: (i32) -> i32
        let host_func_type = FunctionType {
            parameters: vec![ValueType::I32],
            return_types: vec![ValueType::I32],
        };
        let addr = store.allocate_function(FunctionInstance::Host {
            func: Box::new(|args| Ok(args)),
            func_type: host_func_type,
        });

        // Create import object
        let mut imports = ImportObject::new();
        imports.add_function("env", "add_one", addr);

        // Create a module that imports (i32) -> i32 - should match
        let module = module_with_function_import("env", "add_one", 0, vec![ValueType::I32], vec![ValueType::I32]);

        // Instantiation should succeed
        let result = store.create_instance(&module, Some(&imports));
        assert!(result.is_ok(), "Expected instantiation to succeed");
    }

    #[test]
    fn test_import_type_mismatch_parameter() {
        let mut store = Store::new();

        // Register a host function: (i32) -> i32
        let host_func_type = FunctionType {
            parameters: vec![ValueType::I32],
            return_types: vec![ValueType::I32],
        };
        let addr = store.allocate_function(FunctionInstance::Host {
            func: Box::new(|args| Ok(args)),
            func_type: host_func_type,
        });

        // Create import object
        let mut imports = ImportObject::new();
        imports.add_function("env", "my_func", addr);

        // Create a module that imports (i64) -> i32 - parameter type mismatch
        let module = module_with_function_import(
            "env",
            "my_func",
            0,
            vec![ValueType::I64], // Module expects i64, but host provides i32
            vec![ValueType::I32],
        );

        // Instantiation should fail with type mismatch
        let result = store.create_instance(&module, Some(&imports));
        assert!(result.is_err(), "Expected instantiation to fail");

        let err = result.unwrap_err();
        match err {
            RuntimeError::ImportTypeMismatch {
                module,
                name,
                expected,
                actual,
            } => {
                assert_eq!(module, "env");
                assert_eq!(name, "my_func");
                assert!(expected.contains("I64"), "Expected type should mention I64");
                assert!(actual.contains("I32"), "Actual type should mention I32");
            }
            _ => panic!("Expected ImportTypeMismatch error, got: {:?}", err),
        }
    }

    #[test]
    fn test_import_type_mismatch_return() {
        let mut store = Store::new();

        // Register a host function: () -> i32
        let host_func_type = FunctionType {
            parameters: vec![],
            return_types: vec![ValueType::I32],
        };
        let addr = store.allocate_function(FunctionInstance::Host {
            func: Box::new(|_| Ok(vec![Value::I32(42)])),
            func_type: host_func_type,
        });

        // Create import object
        let mut imports = ImportObject::new();
        imports.add_function("env", "get_value", addr);

        // Create a module that imports () -> i64 - return type mismatch
        let module = module_with_function_import(
            "env",
            "get_value",
            0,
            vec![],
            vec![ValueType::I64], // Module expects i64 return, but host returns i32
        );

        // Instantiation should fail with type mismatch
        let result = store.create_instance(&module, Some(&imports));
        assert!(result.is_err(), "Expected instantiation to fail");

        let err = result.unwrap_err();
        assert!(
            matches!(err, RuntimeError::ImportTypeMismatch { .. }),
            "Expected ImportTypeMismatch error, got: {:?}",
            err
        );
    }

    #[test]
    fn test_import_type_mismatch_arity() {
        let mut store = Store::new();

        // Register a host function: (i32, i32) -> i32
        let host_func_type = FunctionType {
            parameters: vec![ValueType::I32, ValueType::I32],
            return_types: vec![ValueType::I32],
        };
        let addr = store.allocate_function(FunctionInstance::Host {
            func: Box::new(|_| Ok(vec![Value::I32(0)])),
            func_type: host_func_type,
        });

        // Create import object
        let mut imports = ImportObject::new();
        imports.add_function("env", "binary_op", addr);

        // Create a module that imports (i32) -> i32 - different arity
        let module = module_with_function_import(
            "env",
            "binary_op",
            0,
            vec![ValueType::I32], // Module expects 1 param, host has 2
            vec![ValueType::I32],
        );

        // Instantiation should fail with type mismatch
        let result = store.create_instance(&module, Some(&imports));
        assert!(result.is_err(), "Expected instantiation to fail");

        let err = result.unwrap_err();
        assert!(
            matches!(err, RuntimeError::ImportTypeMismatch { .. }),
            "Expected ImportTypeMismatch error, got: {:?}",
            err
        );
    }

    // === Cross-module call chain tests ===

    fn make_instruction(kind: InstructionKind) -> Instruction {
        Instruction {
            kind,
            position: ByteRange { offset: 0, length: 1 },
            original_bytes: vec![],
        }
    }

    fn build_structured_function(
        instructions: Vec<InstructionKind>,
        local_count: usize,
        return_types: Vec<ValueType>,
    ) -> StructuredFunction {
        let instrs: Vec<Instruction> = instructions.into_iter().map(make_instruction).collect();
        StructureBuilder::build_function(&instrs, local_count, return_types).expect("Structure building should succeed")
    }

    /// Create a module with a single function that returns a constant i32
    fn module_returning_constant(value: i32, export_name: &str) -> Module {
        let mut module = Module::new("const_module");

        // Type: () -> i32
        module.types = TypeSection {
            types: vec![FunctionType {
                parameters: vec![],
                return_types: vec![ValueType::I32],
            }],
            position: SectionPosition { start: 0, end: 0 },
        };

        // Function declaration
        module.functions = FunctionSection {
            functions: vec![Function { ftype_index: 0 }],
            position: SectionPosition { start: 0, end: 0 },
        };

        // Function body: i32.const value, end
        let body = build_structured_function(
            vec![InstructionKind::I32Const { value }, InstructionKind::End],
            0,
            vec![ValueType::I32],
        );
        module.code = CodeSection {
            code: vec![FunctionBody {
                locals: Locals::empty(),
                body,
                position: SectionPosition { start: 0, end: 0 },
            }],
            position: SectionPosition { start: 0, end: 0 },
        };

        // Export the function
        module.exports = ExportSection {
            exports: vec![Export {
                name: export_name.to_string(),
                index: ExportIndex::Function(0),
            }],
            position: SectionPosition { start: 0, end: 0 },
        };

        module
    }

    /// Create a module that imports a () -> i32 function and calls it, returning the result
    fn module_calling_import(import_module: &str, import_name: &str, export_name: &str) -> Module {
        let mut module = Module::new("caller_module");

        // Type: () -> i32 (used for both import and local function)
        module.types = TypeSection {
            types: vec![FunctionType {
                parameters: vec![],
                return_types: vec![ValueType::I32],
            }],
            position: SectionPosition { start: 0, end: 0 },
        };

        // Import the function
        module.imports = ImportSection {
            imports: vec![Import {
                module: import_module.to_string(),
                name: import_name.to_string(),
                external_kind: ExternalKind::Function(0),
            }],
            position: SectionPosition { start: 0, end: 0 },
        };

        // Local function declaration (type 0)
        module.functions = FunctionSection {
            functions: vec![Function { ftype_index: 0 }],
            position: SectionPosition { start: 0, end: 0 },
        };

        // Function body: call 0 (the imported function), end
        // Function index 0 is the import, our local function is index 1
        let body = build_structured_function(
            vec![InstructionKind::Call { func_idx: 0 }, InstructionKind::End],
            0,
            vec![ValueType::I32],
        );
        module.code = CodeSection {
            code: vec![FunctionBody {
                locals: Locals::empty(),
                body,
                position: SectionPosition { start: 0, end: 0 },
            }],
            position: SectionPosition { start: 0, end: 0 },
        };

        // Export the local function (index 1, since import is index 0)
        module.exports = ExportSection {
            exports: vec![Export {
                name: export_name.to_string(),
                index: ExportIndex::Function(1),
            }],
            position: SectionPosition { start: 0, end: 0 },
        };

        module
    }

    #[test]
    fn test_wasm_to_host_call() {
        // Simple test: wasm function calls host function
        let mut store = Store::new();

        // Host function that returns 42
        let host_addr = store.allocate_function(FunctionInstance::Host {
            func: Box::new(|_| Ok(vec![Value::I32(42)])),
            func_type: FunctionType {
                parameters: vec![],
                return_types: vec![ValueType::I32],
            },
        });

        let mut imports = ImportObject::new();
        imports.add_function("host", "get_value", host_addr);

        let module = module_calling_import("host", "get_value", "call_host");
        let instance_id = store
            .create_instance(&module, Some(&imports))
            .expect("Instance creation should succeed");

        let result = store
            .invoke_export(instance_id, "call_host", vec![], None)
            .expect("Execution should succeed");

        assert_eq!(result, vec![Value::I32(42)]);
    }

    #[test]
    fn test_wasm_to_wasm_call() {
        // Module B exports a function, Module A imports and calls it
        let mut store = Store::new();

        // Create Module B: exports "get_value" returning 100
        let module_b = module_returning_constant(100, "get_value");
        let instance_b = store
            .create_instance(&module_b, None)
            .expect("Module B creation should succeed");

        // Get the FuncAddr for B's exported function
        let func_addr_b = store
            .get_instance(instance_b)
            .unwrap()
            .get_function_addr("get_value")
            .expect("Should find get_value export");

        // Create imports for Module A
        let mut imports_a = ImportObject::new();
        imports_a.add_function("module_b", "get_value", func_addr_b);

        // Create Module A: imports and calls module_b.get_value
        let module_a = module_calling_import("module_b", "get_value", "call_b");
        let instance_a = store
            .create_instance(&module_a, Some(&imports_a))
            .expect("Module A creation should succeed");

        let result = store
            .invoke_export(instance_a, "call_b", vec![], None)
            .expect("Execution should succeed");

        assert_eq!(result, vec![Value::I32(100)]);
    }

    #[test]
    fn test_three_module_chain() {
        // A calls B, B calls C - tests the call stack bug
        // C returns 999, B forwards it, A should receive 999
        let mut store = Store::new();

        // Module C: exports "get_value" returning 999
        let module_c = module_returning_constant(999, "get_value");
        let instance_c = store
            .create_instance(&module_c, None)
            .expect("Module C creation should succeed");
        let func_addr_c = store
            .get_instance(instance_c)
            .unwrap()
            .get_function_addr("get_value")
            .unwrap();

        // Module B: imports C's function, calls it, exports result
        let mut imports_b = ImportObject::new();
        imports_b.add_function("module_c", "get_value", func_addr_c);
        let module_b = module_calling_import("module_c", "get_value", "call_c");
        let instance_b = store
            .create_instance(&module_b, Some(&imports_b))
            .expect("Module B creation should succeed");
        let func_addr_b = store
            .get_instance(instance_b)
            .unwrap()
            .get_function_addr("call_c")
            .unwrap();

        // Module A: imports B's function, calls it
        let mut imports_a = ImportObject::new();
        imports_a.add_function("module_b", "call_c", func_addr_b);
        let module_a = module_calling_import("module_b", "call_c", "call_chain");
        let instance_a = store
            .create_instance(&module_a, Some(&imports_a))
            .expect("Module A creation should succeed");

        // This is the critical test - A -> B -> C chain
        let result = store
            .invoke_export(instance_a, "call_chain", vec![], None)
            .expect("Three-module chain should succeed");

        assert_eq!(result, vec![Value::I32(999)]);
    }

    /// Create a module that imports a () -> i32 function, calls it, adds a constant, and returns
    fn module_calling_import_and_add(
        import_module: &str,
        import_name: &str,
        add_value: i32,
        export_name: &str,
    ) -> Module {
        let mut module = Module::new("caller_add_module");

        // Type: () -> i32
        module.types = TypeSection {
            types: vec![FunctionType {
                parameters: vec![],
                return_types: vec![ValueType::I32],
            }],
            position: SectionPosition { start: 0, end: 0 },
        };

        // Import the function
        module.imports = ImportSection {
            imports: vec![Import {
                module: import_module.to_string(),
                name: import_name.to_string(),
                external_kind: ExternalKind::Function(0),
            }],
            position: SectionPosition { start: 0, end: 0 },
        };

        // Local function declaration
        module.functions = FunctionSection {
            functions: vec![Function { ftype_index: 0 }],
            position: SectionPosition { start: 0, end: 0 },
        };

        // Function body: call 0; i32.const add_value; i32.add; end
        let body = build_structured_function(
            vec![
                InstructionKind::Call { func_idx: 0 },
                InstructionKind::I32Const { value: add_value },
                InstructionKind::I32Add,
                InstructionKind::End,
            ],
            0,
            vec![ValueType::I32],
        );
        module.code = CodeSection {
            code: vec![FunctionBody {
                locals: Locals::empty(),
                body,
                position: SectionPosition { start: 0, end: 0 },
            }],
            position: SectionPosition { start: 0, end: 0 },
        };

        // Export the local function
        module.exports = ExportSection {
            exports: vec![Export {
                name: export_name.to_string(),
                index: ExportIndex::Function(1),
            }],
            position: SectionPosition { start: 0, end: 0 },
        };

        module
    }

    #[test]
    fn test_three_module_chain_with_computation() {
        // This test catches the call stack bug:
        // C returns 10
        // B calls C and adds 100 (should return 110)
        // A calls B and adds 1000 (should return 1110)
        // If the bug exists: A never resumes, we get 110 instead of 1110
        let mut store = Store::new();

        // Module C: returns 10
        let module_c = module_returning_constant(10, "get_value");
        let instance_c = store.create_instance(&module_c, None).unwrap();
        let func_addr_c = store
            .get_instance(instance_c)
            .unwrap()
            .get_function_addr("get_value")
            .unwrap();

        // Module B: calls C, adds 100
        let mut imports_b = ImportObject::new();
        imports_b.add_function("module_c", "get_value", func_addr_c);
        let module_b = module_calling_import_and_add("module_c", "get_value", 100, "call_c");
        let instance_b = store.create_instance(&module_b, Some(&imports_b)).unwrap();
        let func_addr_b = store
            .get_instance(instance_b)
            .unwrap()
            .get_function_addr("call_c")
            .unwrap();

        // Module A: calls B, adds 1000
        let mut imports_a = ImportObject::new();
        imports_a.add_function("module_b", "call_c", func_addr_b);
        let module_a = module_calling_import_and_add("module_b", "call_c", 1000, "call_chain");
        let instance_a = store.create_instance(&module_a, Some(&imports_a)).unwrap();

        // A -> B -> C chain with computation at each level
        // Expected: 10 + 100 + 1000 = 1110
        let result = store
            .invoke_export(instance_a, "call_chain", vec![], None)
            .expect("Chain should succeed");

        assert_eq!(
            result,
            vec![Value::I32(1110)],
            "Expected 1110 (10 + 100 + 1000), got {:?}. If 110, the call stack bug exists.",
            result
        );
    }

    #[test]
    fn test_wasm_host_wasm_chain() {
        // A calls Host, Host was called from B
        // This tests: B (wasm) -> Host -> resume B
        // Then: A (wasm) -> B (wasm) -> Host -> resume B -> resume A
        let mut store = Store::new();

        // Host function
        let host_addr = store.allocate_function(FunctionInstance::Host {
            func: Box::new(|_| Ok(vec![Value::I32(777)])),
            func_type: FunctionType {
                parameters: vec![],
                return_types: vec![ValueType::I32],
            },
        });

        // Module B: calls host function
        let mut imports_b = ImportObject::new();
        imports_b.add_function("host", "get_value", host_addr);
        let module_b = module_calling_import("host", "get_value", "call_host");
        let instance_b = store
            .create_instance(&module_b, Some(&imports_b))
            .expect("Module B creation should succeed");
        let func_addr_b = store
            .get_instance(instance_b)
            .unwrap()
            .get_function_addr("call_host")
            .unwrap();

        // Module A: calls B
        let mut imports_a = ImportObject::new();
        imports_a.add_function("module_b", "call_host", func_addr_b);
        let module_a = module_calling_import("module_b", "call_host", "start_chain");
        let instance_a = store
            .create_instance(&module_a, Some(&imports_a))
            .expect("Module A creation should succeed");

        // A -> B -> Host chain
        let result = store
            .invoke_export(instance_a, "start_chain", vec![], None)
            .expect("Wasm-Host-Wasm chain should succeed");

        assert_eq!(result, vec![Value::I32(777)]);
    }

    // === Memory/Table allocation tests ===

    #[test]
    fn test_memory_allocation() {
        let mut store = Store::new();

        // Allocate a memory (1 page initial, 10 pages max)
        let memory = Memory::new(1, Some(10)).unwrap();
        let addr = store.allocate_memory(memory);

        assert_eq!(addr, MemoryAddr(0));

        // Verify we can retrieve it
        let shared_mem = store.get_memory(addr);
        assert!(shared_mem.is_some());

        // Verify the memory properties through the RefCell
        let mem_guard = shared_mem.unwrap().borrow();
        assert_eq!(mem_guard.size(), 1);
    }

    #[test]
    fn test_table_allocation() {
        use crate::parser::module::{Limits, RefType};

        let mut store = Store::new();

        // Allocate a table
        let table = Table::new(RefType::FuncRef, Limits { min: 10, max: Some(20) }).unwrap();
        let addr = store.allocate_table(table);

        assert_eq!(addr, TableAddr(0));

        // Verify we can retrieve it
        let shared_table = store.get_table(addr);
        assert!(shared_table.is_some());

        // Verify the table properties through the RefCell
        let table_guard = shared_table.unwrap().borrow();
        assert_eq!(table_guard.size(), 10);
    }

    #[test]
    fn test_multiple_memory_allocations() {
        let mut store = Store::new();

        // Allocate multiple memories
        let mem1 = Memory::new(1, Some(5)).unwrap();
        let mem2 = Memory::new(2, Some(10)).unwrap();
        let mem3 = Memory::new(3, None).unwrap();

        let addr1 = store.allocate_memory(mem1);
        let addr2 = store.allocate_memory(mem2);
        let addr3 = store.allocate_memory(mem3);

        // Each gets a unique address
        assert_eq!(addr1, MemoryAddr(0));
        assert_eq!(addr2, MemoryAddr(1));
        assert_eq!(addr3, MemoryAddr(2));

        // All are retrievable with correct properties
        assert_eq!(store.get_memory(addr1).unwrap().borrow().size(), 1);
        assert_eq!(store.get_memory(addr2).unwrap().borrow().size(), 2);
        assert_eq!(store.get_memory(addr3).unwrap().borrow().size(), 3);
    }

    #[test]
    fn test_multiple_table_allocations() {
        use crate::parser::module::{Limits, RefType};

        let mut store = Store::new();

        // Allocate multiple tables
        let table1 = Table::new(RefType::FuncRef, Limits { min: 5, max: Some(10) }).unwrap();
        let table2 = Table::new(RefType::ExternRef, Limits { min: 8, max: None }).unwrap();

        let addr1 = store.allocate_table(table1);
        let addr2 = store.allocate_table(table2);

        // Each gets a unique address
        assert_eq!(addr1, TableAddr(0));
        assert_eq!(addr2, TableAddr(1));

        // All are retrievable with correct properties
        assert_eq!(store.get_table(addr1).unwrap().borrow().size(), 5);
        assert_eq!(store.get_table(addr2).unwrap().borrow().size(), 8);
    }

    #[test]
    fn test_invalid_memory_address() {
        let store = Store::new();

        // Trying to get a memory that doesn't exist returns None
        assert!(store.get_memory(MemoryAddr(0)).is_none());
        assert!(store.get_memory(MemoryAddr(999)).is_none());
    }

    #[test]
    fn test_invalid_table_address() {
        let store = Store::new();

        // Trying to get a table that doesn't exist returns None
        assert!(store.get_table(TableAddr(0)).is_none());
        assert!(store.get_table(TableAddr(999)).is_none());
    }

    #[test]
    fn test_shared_memory_modification() {
        let mut store = Store::new();

        let memory = Memory::new(1, Some(10)).unwrap();
        let addr = store.allocate_memory(memory);

        // Get two references to the same memory
        let ref1 = store.get_memory(addr).unwrap().clone();
        let ref2 = store.get_memory(addr).unwrap().clone();

        // Modify through one reference
        {
            let mut mem = ref1.borrow_mut();
            mem.write_u32(0, 0xDEADBEEF).unwrap();
        }

        // Read through the other reference - should see the modification
        {
            let mem = ref2.borrow();
            let value = mem.read_u32(0).unwrap();
            assert_eq!(value, 0xDEADBEEF);
        }
    }

    #[test]
    fn test_shared_table_modification() {
        use crate::parser::module::{Limits, RefType};
        use crate::runtime::FuncAddr;

        let mut store = Store::new();

        let table = Table::new(RefType::FuncRef, Limits { min: 10, max: Some(20) }).unwrap();
        let addr = store.allocate_table(table);

        // Get two references to the same table
        let ref1 = store.get_table(addr).unwrap().clone();
        let ref2 = store.get_table(addr).unwrap().clone();

        // Modify through one reference
        {
            let mut tbl = ref1.borrow_mut();
            tbl.set(0, Some(Value::FuncRef(Some(FuncAddr(42))))).unwrap();
        }

        // Read through the other reference - should see the modification
        {
            let tbl = ref2.borrow();
            let value = tbl.get(0).unwrap();
            assert_eq!(value, Value::FuncRef(Some(FuncAddr(42))));
        }
    }
}
