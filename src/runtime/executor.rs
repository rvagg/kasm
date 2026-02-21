//! WebAssembly instruction executor

use super::{
    ExecutionOutcome, ExternalCallRequest, RuntimeError, Value,
    control::{Label, LabelStack, LabelType},
    frame::CallFrame,
    imports::{default_value_for_type, is_global_mutable},
    memory::Memory,
    ops,
    stack::Stack,
    store::{SharedGlobal, SharedMemory, SharedTable},
    table::Table,
};
use crate::parser::instruction::{BlockType, Instruction, InstructionKind, SimdOp};
use crate::parser::module::{DataMode, ElementMode, ExternalKind, FunctionType, Locals, Module, ValueType};
use crate::parser::structured::{BlockEnd, StructuredFunction, StructuredInstruction};
use std::cell::{Cell, RefCell};
use std::collections::HashSet;
use std::rc::Rc;

/// Maximum call stack depth to prevent stack overflow
const MAX_CALL_DEPTH: usize = 1000;

/// Execution state for the state machine executor
#[derive(Debug)]
enum ExecutionState<'a> {
    /// Continue to next instruction
    Continue,
    /// Enter a nested instruction sequence (block/loop/if body)
    EnterNested(&'a [StructuredInstruction]),
    /// Call a local function (by local func_idx)
    CallFunction(u32),
    /// Call an external function (cross-module, by FuncAddr)
    CallExternalFunction {
        func_addr: super::FuncAddr,
        func_type: FunctionType,
    },
    /// Return from current function
    ReturnFromFunction,
    /// Branch to label at given depth
    BranchTo(u32),
}

/// Execution context representing a sequence of instructions being executed
#[derive(Debug, Clone, Copy)]
struct ExecutionContext<'a> {
    /// Borrowed instruction slice (from module, never cloned)
    instructions: &'a [StructuredInstruction],
    /// Current position in instruction sequence
    position: usize,
    /// What to do when this context completes
    on_complete: ContextCompletion,
}

/// What to do when an execution context completes
#[derive(Debug, Clone, Copy)]
enum ContextCompletion {
    /// Pop a label when done (for blocks/loops/ifs)
    PopLabel,
    /// Return from function when done
    ReturnFunction,
}

/// Result of handling a function call
enum CallHandled {
    /// Call was to a local function, context updated
    LocalCall,
    /// Call requires external dispatch
    NeedsExternal(ExternalCallRequest),
}

/// Executes WebAssembly instructions
pub struct Executor<'a> {
    module: &'a Module,
    stack: Stack,
    /// Call stack for managing function calls (always has at least one frame during execution)
    call_stack: Vec<CallFrame>,
    /// Memory instances (shared for cross-module access)
    /// WebAssembly 1.0 supports only 1 memory per module
    memories: Vec<SharedMemory>,
    /// Global variable values (shared via Rc<Cell<Value>> for cross-module mutable global aliasing)
    globals: Vec<SharedGlobal>,
    /// Table instances (shared for cross-module access)
    tables: Vec<SharedTable>,
    /// Element segments (for table.init)
    element_segments: Vec<Vec<Option<Value>>>,
    /// Number of imported functions (they have no local bodies)
    num_imported_functions: usize,
    /// Function types for quick access
    function_types: Vec<FunctionType>,
    /// Maps local function index to global FuncAddr (empty until linked)
    function_addresses: Vec<super::FuncAddr>,
    /// Module's global definitions (for initialisation after linking)
    module_globals: Vec<crate::parser::module::Global>,
    /// Saved execution contexts when paused for external call
    saved_contexts: Option<Vec<ExecutionContext<'a>>>,
    /// Expected return types when resuming from external call
    saved_return_types: Vec<ValueType>,
    /// Optional instruction budget - execution stops when exhausted
    instruction_budget: Option<u64>,
    /// Data segments that have been dropped (data.drop)
    dropped_data: HashSet<u32>,
}

impl<'a> Executor<'a> {
    /// Create a new executor for a module
    ///
    /// # Errors
    /// - If the module has more than one memory (WebAssembly 1.0 limitation)
    /// - If memory initialisation fails
    pub fn new(module: &'a Module) -> Result<Self, RuntimeError> {
        let (memories, tables) = Self::create_memories_and_tables(module)?;

        // Build shared globals (standalone mode — no Store, so each global is independent)
        let mut globals: Vec<SharedGlobal> = Vec::new();

        // Imported globals
        for import in &module.imports.imports {
            if let ExternalKind::Global(global_type) = &import.external_kind {
                let initial_value = default_value_for_type(global_type.value_type)?;
                globals.push(Rc::new(Cell::new(initial_value)));
            }
        }

        // Local globals (default values, initialised later)
        for global in &module.globals.globals {
            let default_value = default_value_for_type(global.global_type.value_type)?;
            globals.push(Rc::new(Cell::new(default_value)));
        }

        Self::new_with_shared(module, memories, tables, globals)
    }

    /// Create a new executor with pre-provided shared memories, tables, and globals
    ///
    /// This is used by Store::create_instance(). All shared resources must be in the
    /// correct order: imported first, then local.
    pub fn new_with_shared(
        module: &'a Module,
        memories: Vec<SharedMemory>,
        tables: Vec<SharedTable>,
        globals: Vec<SharedGlobal>,
    ) -> Result<Self, RuntimeError> {
        // Store the module's global definitions for later initialisation
        // Init expressions can contain ref.func, so they must be evaluated after function_addresses are linked
        let module_globals = module.globals.globals.clone();

        // Count imported functions (they come first in the index space, but have no local bodies)
        let num_imported_functions = module
            .imports
            .imports
            .iter()
            .filter(|import| matches!(import.external_kind, ExternalKind::Function(_)))
            .count();

        // Collect function types for quick access
        let function_types = module.types.types.clone();

        // Create executor instance
        let mut executor = Executor {
            module,
            stack: Stack::new(),
            call_stack: Vec::new(),
            memories,
            globals,
            tables,
            element_segments: Vec::new(), // Will be populated after linking
            num_imported_functions,
            function_types,
            function_addresses: Vec::new(), // Will be populated when instance is linked
            module_globals,                 // Store for later initialisation
            saved_contexts: None,           // For resumable execution
            saved_return_types: Vec::new(), // For resumable execution
            instruction_budget: None,       // No budget limit by default
            dropped_data: HashSet::new(),
        };

        // Note: Globals and element segments are NOT initialised here because their init
        // expressions may contain ref.func instructions that require function_addresses to be linked first.
        // They will be initialised by Instance::link_functions() after linking.

        // Initialise memory with data sections
        executor.initialise_data_sections()?;

        Ok(executor)
    }

    /// Create memories and tables from module definition
    ///
    /// This is used when no external memories/tables are provided.
    /// Imported memories/tables are created as new instances (for backward compatibility).
    fn create_memories_and_tables(module: &Module) -> Result<(Vec<SharedMemory>, Vec<SharedTable>), RuntimeError> {
        // Initialise memories from module definition
        let memories: Vec<SharedMemory> = if module.memory.memory.is_empty() {
            vec![]
        } else {
            // WebAssembly 1.0: Only one memory allowed per module
            if module.memory.memory.len() > 1 {
                return Err(RuntimeError::MemoryError("multiple memories not supported".to_string()));
            }

            let mem_def = &module.memory.memory[0];
            match Memory::new(mem_def.limits.min, mem_def.limits.max) {
                Ok(memory) => vec![Rc::new(RefCell::new(memory))],
                Err(e) => return Err(e),
            }
        };

        // Initialise tables from module definition
        let mut tables: Vec<SharedTable> = Vec::new();

        // First, initialise imported tables (they come first in the index space)
        for import in &module.imports.imports {
            if let ExternalKind::Table(table_type) = &import.external_kind {
                // Create default table for import
                let table = Table::new(table_type.ref_type, table_type.limits)?;
                tables.push(Rc::new(RefCell::new(table)));
            }
        }

        // Then, add locally defined tables
        for table_type in &module.table.tables {
            let table = Table::new(table_type.ref_type, table_type.limits)?;
            tables.push(Rc::new(RefCell::new(table)));
        }

        Ok((memories, tables))
    }

    /// Initialise module globals with their init expressions
    ///
    /// This must be called after function_addresses have been linked, as global init
    /// expressions can contain ref.func instructions that need the address mapping.
    pub(super) fn initialise_globals(&mut self) -> Result<(), RuntimeError> {
        // Calculate how many imported globals there are
        let num_imported_globals = self
            .module
            .imports
            .imports
            .iter()
            .filter(|import| matches!(import.external_kind, ExternalKind::Global(_)))
            .count();

        // Initialise module's own globals with their init expressions
        // Globals can reference previously defined globals in their init expressions,
        // so we need to evaluate them in order
        for (idx, global) in self.module_globals.iter().enumerate() {
            let global_idx = num_imported_globals + idx;

            let initial_value = if global.init.is_empty() {
                // No init expression, keep the default value that was already set
                continue;
            } else {
                // Evaluate the init expression with the current state of globals
                // This allows later globals to reference earlier ones
                self.evaluate_const_expr(&global.init)?
            };

            // Update the global value via the shared cell
            self.globals[global_idx].set(initial_value);
        }

        Ok(())
    }

    /// Initialise tables with element segments
    ///
    /// This must be called after function_addresses have been linked, as element
    /// segments can contain ref.func instructions that need the address mapping.
    pub(super) fn initialise_element_segments(&mut self) -> Result<(), RuntimeError> {
        // Process each element segment
        for element in &self.module.elements.elements {
            // Evaluate init expressions to get values
            let mut values = Vec::new();
            for init_expr in &element.init {
                let val = self.evaluate_const_expr(init_expr)?;
                values.push(Some(val));
            }

            match &element.mode {
                ElementMode::Active { table_index, offset } => {
                    // Evaluate offset expression
                    let offset_val = self.evaluate_const_expr(offset)?;
                    let start_idx = match offset_val {
                        Value::I32(v) => v as u32,
                        _ => return Err(RuntimeError::InvalidConstExpr("element offset must be i32".to_string())),
                    };

                    // Check table exists
                    let table = self
                        .tables
                        .get(*table_index as usize)
                        .ok_or(RuntimeError::TableIndexOutOfBounds(*table_index))?;

                    // Initialize table with element values
                    {
                        let mut table_guard = table.borrow_mut();
                        table_guard.init(start_idx, &values, 0, values.len() as u32)?;
                    }

                    // Active segments are dropped after instantiation per spec
                    self.element_segments.push(Vec::new());
                }
                ElementMode::Declarative => {
                    // Declarative segments are dropped immediately per spec
                    self.element_segments.push(Vec::new());
                }
                ElementMode::Passive => {
                    // Passive segments are available for table.init
                    self.element_segments.push(values);
                }
            }
        }

        Ok(())
    }

    /// Initialise memory with data from data sections
    fn initialise_data_sections(&mut self) -> Result<(), RuntimeError> {
        // Process each data segment
        for (seg_idx, data_segment) in self.module.data.data.iter().enumerate() {
            match &data_segment.mode {
                DataMode::Active { memory_index, offset } => {
                    // Check memory index is valid (WebAssembly 1.0 only supports one memory)
                    if *memory_index != 0 {
                        return Err(RuntimeError::MemoryError(format!(
                            "invalid memory index {} in data segment",
                            memory_index
                        )));
                    }

                    // Check we have a memory
                    let memory = self.memories.first().ok_or_else(|| {
                        RuntimeError::MemoryError("data segment requires memory but none exists".to_string())
                    })?;

                    // Evaluate the offset expression to get the starting address
                    // The offset expression should be a constant expression
                    let offset_value = self.evaluate_const_expr(offset)?;

                    // Extract the offset as u32
                    let offset_addr = match offset_value {
                        Value::I32(v) => v as u32,
                        _ => {
                            return Err(RuntimeError::MemoryError(
                                "data segment offset must be an i32".to_string(),
                            ));
                        }
                    };

                    // Write the data to memory
                    let mut mem_guard = memory.borrow_mut();
                    let data = &data_segment.init;

                    // Check if the data fits in memory
                    let end_addr = offset_addr as usize + data.len();
                    let memory_size_bytes = (mem_guard.size() as usize) * 65536; // Convert pages to bytes
                    if end_addr > memory_size_bytes {
                        return Err(RuntimeError::MemoryError("out of bounds memory access".to_string()));
                    }

                    // Copy the data into memory using the shared helper
                    ops::memory::copy_to_memory(&mut mem_guard, offset_addr, data)?;

                    // Active segments are logically dropped after initialisation
                    self.dropped_data.insert(seg_idx as u32);
                }
                DataMode::Passive => {
                    // Passive data segments are not automatically initialised
                    // They're used with memory.init instruction
                }
            }
        }

        Ok(())
    }

    /// Evaluate a constant expression (used for data/element segment offsets and global init)
    fn evaluate_const_expr(&self, instructions: &[Instruction]) -> Result<Value, RuntimeError> {
        // Constant expressions are limited to a small set of instructions
        // They must end with an End instruction
        if instructions.is_empty() {
            return Err(RuntimeError::InvalidConstExpr("empty constant expression".to_string()));
        }

        // Check that the last instruction is End
        match instructions.last() {
            Some(inst) if matches!(inst.kind, InstructionKind::End) => {}
            _ => {
                return Err(RuntimeError::InvalidConstExpr(
                    "constant expression must end with end instruction".to_string(),
                ));
            }
        }

        // For now, handle the common cases (single instruction + End)
        if instructions.len() == 2 {
            match &instructions[0].kind {
                InstructionKind::I32Const { value } => Ok(Value::I32(*value)),
                InstructionKind::I64Const { value } => Ok(Value::I64(*value)),
                InstructionKind::F32Const { value } => Ok(Value::F32(*value)),
                InstructionKind::F64Const { value } => Ok(Value::F64(*value)),
                InstructionKind::GlobalGet { global_idx } => {
                    // Get the global value from the shared cell
                    if (*global_idx as usize) >= self.globals.len() {
                        return Err(RuntimeError::GlobalIndexOutOfBounds(*global_idx));
                    }
                    Ok(self.globals[*global_idx as usize].get())
                }
                InstructionKind::RefNull { ref_type } => match ref_type {
                    ValueType::FuncRef => Ok(Value::FuncRef(None)),
                    ValueType::ExternRef => Ok(Value::ExternRef(None)),
                    _ => Err(RuntimeError::InvalidConstExpr(format!(
                        "invalid reference type for ref.null: {:?}",
                        ref_type
                    ))),
                },
                InstructionKind::Simd(SimdOp::V128Const { value }) => Ok(Value::V128(*value)),
                InstructionKind::RefFunc { func_idx } => {
                    // Validate function exists
                    let total_functions = self.module.imports.function_count() + self.module.functions.functions.len();
                    if (*func_idx as usize) >= total_functions {
                        return Err(RuntimeError::FunctionIndexOutOfBounds(*func_idx));
                    }
                    // Map local func_idx to global FuncAddr
                    let func_addr = self
                        .function_addresses
                        .get(*func_idx as usize)
                        .copied()
                        .ok_or(RuntimeError::FunctionIndexOutOfBounds(*func_idx))?;
                    Ok(Value::FuncRef(Some(func_addr)))
                }
                _ => Err(RuntimeError::InvalidConstExpr(format!(
                    "unsupported instruction in constant expression: {:?}",
                    instructions[0].kind
                ))),
            }
        } else if instructions.len() == 1 && matches!(instructions[0].kind, InstructionKind::End) {
            // Just an End instruction - this shouldn't happen in valid WebAssembly
            Err(RuntimeError::InvalidConstExpr(
                "constant expression cannot be just end".to_string(),
            ))
        } else {
            // TODO: Support more complex constant expressions (e.g., i32.add with two consts)
            Err(RuntimeError::InvalidConstExpr(format!(
                "unsupported constant expression with {} instructions",
                instructions.len()
            )))
        }
    }

    /// Push a new call frame for a function call
    fn push_call_frame(&mut self, func_idx: u32, args: Vec<Value>, return_arity: usize) -> Result<(), RuntimeError> {
        // Check call stack depth
        if self.call_stack.len() >= MAX_CALL_DEPTH {
            return Err(RuntimeError::CallStackOverflow);
        }

        // Calculate the number of imported functions
        let num_imported_functions = self.module.imports.function_count();

        // Get the function declaration from the functions section
        // Note: functions section only contains non-imported functions,
        // so we need to subtract the number of imports if this is not an imported function
        let func_decl_idx = if (func_idx as usize) < num_imported_functions {
            // This shouldn't happen as we check for imports in handle_call, but be safe
            return Err(RuntimeError::UnimplementedInstruction(format!(
                "Cannot push frame for imported function at index {func_idx}"
            )));
        } else {
            func_idx as usize - num_imported_functions
        };

        let func = self
            .module
            .functions
            .get(func_decl_idx as u32)
            .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))?;

        let func_type = self
            .function_types
            .get(func.ftype_index as usize)
            .ok_or(RuntimeError::InvalidFunctionType)?;

        // Verify argument count and types match
        if args.len() != func_type.parameters.len() {
            return Err(RuntimeError::TypeMismatch {
                expected: format!("{} arguments", func_type.parameters.len()),
                actual: format!("{} arguments", args.len()),
            });
        }

        // Verify each argument type
        for (i, (arg, expected_type)) in args.iter().zip(&func_type.parameters).enumerate() {
            if arg.typ() != *expected_type {
                return Err(RuntimeError::TypeMismatch {
                    expected: format!("{expected_type:?} for argument {i}"),
                    actual: format!("{:?}", arg.typ()),
                });
            }
        }

        // Get the function body from the code section
        // The code section also only contains non-imported functions
        let body = self
            .module
            .code
            .get(func_decl_idx as u32)
            .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))?;

        // Initialise locals: parameters first, then local declarations
        let mut locals = args;
        for (count, local_type) in body.locals.iter() {
            for _ in 0..*count {
                // Initialise locals to zero (or null for reference types)
                let zero_value = match local_type {
                    ValueType::I32 => Value::I32(0),
                    ValueType::I64 => Value::I64(0),
                    ValueType::F32 => Value::F32(0.0),
                    ValueType::F64 => Value::F64(0.0),
                    ValueType::FuncRef => Value::FuncRef(None),
                    ValueType::ExternRef => Value::ExternRef(None),
                    ValueType::V128 => Value::V128([0u8; 16]),
                };
                locals.push(zero_value);
            }
        }

        // Create and push the new frame
        let frame = CallFrame {
            function_idx: func_idx,
            ip: 0,
            locals,
            label_stack: Vec::new(),
            return_arity,
        };

        self.call_stack.push(frame);
        Ok(())
    }

    /// Get the current frame's locals (mutable)
    fn current_locals_mut(&mut self) -> Result<&mut Vec<Value>, RuntimeError> {
        self.call_stack
            .last_mut()
            .map(|frame| &mut frame.locals)
            .ok_or(RuntimeError::InvalidFunctionType)
    }

    /// Get the current frame's locals (immutable)
    fn current_locals(&self) -> Result<&Vec<Value>, RuntimeError> {
        self.call_stack
            .last()
            .map(|frame| &frame.locals)
            .ok_or(RuntimeError::InvalidFunctionType)
    }

    /// Get the function type for a given function index
    ///
    /// Handles both imported and local functions by delegating to Module.
    ///
    /// # Errors
    /// - Returns `FunctionIndexOutOfBounds` if func_idx is invalid
    fn get_function_type(&self, func_idx: u32) -> Result<&FunctionType, RuntimeError> {
        self.module
            .get_function_type_by_idx(func_idx)
            .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))
    }

    /// Get a global value by index
    ///
    /// # Errors
    /// - Returns `GlobalIndexOutOfBounds` if global_idx is invalid
    pub fn get_global(&self, global_idx: u32) -> Result<Value, RuntimeError> {
        self.globals
            .get(global_idx as usize)
            .map(|g| g.get())
            .ok_or(RuntimeError::GlobalIndexOutOfBounds(global_idx))
    }

    /// Link function addresses after instance creation
    ///
    /// This is called by Instance::link_functions() to provide the executor
    /// with the mapping from local function indices to global FuncAddr.
    pub(super) fn link_function_addresses(&mut self, function_addresses: Vec<super::FuncAddr>) {
        self.function_addresses = function_addresses;
    }

    /// Get the current label stack (mutable)
    fn current_label_stack_mut(&mut self) -> Result<&mut Vec<Label>, RuntimeError> {
        self.call_stack
            .last_mut()
            .map(|frame| &mut frame.label_stack)
            .ok_or(RuntimeError::InvalidFunctionType)
    }

    /// Get the current label stack (immutable)
    fn current_label_stack(&self) -> Result<&Vec<Label>, RuntimeError> {
        self.call_stack
            .last()
            .map(|frame| &frame.label_stack)
            .ok_or(RuntimeError::InvalidFunctionType)
    }

    /// Create a function label for the implicit function block
    /// In WebAssembly, a function body is an implicit block
    fn create_function_label(&self, return_types: &[ValueType]) -> Label {
        let block_type = match return_types.len() {
            0 => BlockType::Empty,
            1 => BlockType::Value(return_types[0]),
            _ => {
                // For multi-value returns, we'd ideally use BlockType::FuncType
                // but we don't have a function type index here.
                // This is a temporary workaround - proper support would need BlockType::FuncType
                BlockType::Empty
            }
        };

        Label {
            label_type: LabelType::Block, // Function body is a block
            block_type,
            stack_height: self.stack.len(),
            unreachable: false,
            param_types: vec![], // Functions don't consume parameters from stack
            return_types: return_types.to_vec(),
        }
    }

    /// Set a global value for testing purposes
    #[cfg(test)]
    pub fn set_global_for_test(&mut self, global_idx: u32, value: Value) -> Result<(), RuntimeError> {
        if global_idx as usize >= self.globals.len() {
            return Err(RuntimeError::GlobalIndexOutOfBounds(global_idx));
        }
        self.globals[global_idx as usize].set(value);
        Ok(())
    }

    /// Set an instruction budget limit for execution
    ///
    /// When set, the executor will count instructions and return
    /// `RuntimeError::InstructionBudgetExhausted` when the budget is exhausted.
    /// Pass `None` to disable the limit.
    pub fn set_instruction_budget(&mut self, budget: Option<u64>) {
        self.instruction_budget = budget;
    }

    /// Execute a function
    pub fn execute_function(
        &mut self,
        func: &'a StructuredFunction,
        args: Vec<Value>,
        return_types: &[ValueType],
    ) -> Result<ExecutionOutcome, RuntimeError> {
        self.execute_function_with_locals(func, args, return_types, None)
    }

    /// Execute a function with explicit locals information
    ///
    /// Returns `ExecutionOutcome::Complete` if the function completes, or
    /// `ExecutionOutcome::NeedsExternalCall` if a cross-module call is needed.
    pub fn execute_function_with_locals(
        &mut self,
        func: &'a StructuredFunction,
        args: Vec<Value>,
        return_types: &[ValueType],
        locals_info: Option<&Locals>,
    ) -> Result<ExecutionOutcome, RuntimeError> {
        // Initialise locals: parameters first, then local declarations
        let mut locals = args;

        // Add declared locals if provided
        if let Some(locals_info) = locals_info {
            for (count, local_type) in locals_info.iter() {
                for _ in 0..*count {
                    let zero_value = match local_type {
                        ValueType::I32 => Value::I32(0),
                        ValueType::I64 => Value::I64(0),
                        ValueType::F32 => Value::F32(0.0),
                        ValueType::F64 => Value::F64(0.0),
                        ValueType::FuncRef => Value::FuncRef(None),
                        ValueType::ExternRef => Value::ExternRef(None),
                        ValueType::V128 => Value::V128([0u8; 16]),
                    };
                    locals.push(zero_value);
                }
            }
        }

        // Save call stack depth so we can restore on error
        let saved_call_depth = self.call_stack.len();

        // Create initial call frame
        let initial_frame = CallFrame {
            function_idx: 0,
            ip: 0,
            locals,
            label_stack: Vec::new(),
            return_arity: return_types.len(),
        };
        self.call_stack.push(initial_frame);

        // Push implicit function block label
        let func_label = self.create_function_label(return_types);
        self.current_label_stack_mut()?.push(func_label);

        // Create initial execution context (borrow from module, zero-copy)
        let contexts = vec![ExecutionContext {
            instructions: &func.body,
            position: 0,
            on_complete: ContextCompletion::ReturnFunction,
        }];

        let result = self.run_execution_loop(contexts, return_types.to_vec());

        // On error, restore call stack to pre-call state
        if result.is_err() {
            self.call_stack.truncate(saved_call_depth);
            self.stack.clear();
        }

        result
    }

    /// Resume execution after an external call completes
    ///
    /// This method is called when a cross-module call returns. It pushes the
    /// results onto the operand stack and continues execution.
    pub fn resume_with_results(&mut self, results: Vec<Value>) -> Result<ExecutionOutcome, RuntimeError> {
        // Restore saved contexts
        let contexts = self.saved_contexts.take().ok_or(RuntimeError::InvalidFunctionType)?;
        let return_types = std::mem::take(&mut self.saved_return_types);

        // Push results onto the operand stack
        for value in results {
            self.stack.push(value);
        }

        self.run_execution_loop(contexts, return_types)
    }

    /// Main execution loop shared by execute_function_with_locals and resume_with_results
    fn run_execution_loop(
        &mut self,
        mut contexts: Vec<ExecutionContext<'a>>,
        return_types: Vec<ValueType>,
    ) -> Result<ExecutionOutcome, RuntimeError> {
        'execution: loop {
            // Get current context
            let context = match contexts.last_mut() {
                Some(ctx) => ctx,
                None => break 'execution,
            };

            // Check if we've reached end of current context
            if context.position >= context.instructions.len() {
                let completion = context.on_complete;
                contexts.pop();

                match completion {
                    ContextCompletion::PopLabel => {
                        self.current_label_stack_mut()?.pop();
                    }
                    ContextCompletion::ReturnFunction => {
                        if self.call_stack.len() > 1 {
                            self.current_label_stack_mut()?.pop();
                            self.call_stack.pop();
                        } else {
                            break 'execution;
                        }
                    }
                }
                continue 'execution;
            }

            // Check instruction budget before executing
            if let Some(ref mut remaining) = self.instruction_budget {
                if *remaining == 0 {
                    return Err(RuntimeError::InstructionBudgetExhausted);
                }
                *remaining -= 1;
            }

            // Execute current instruction
            let instruction = &context.instructions[context.position];

            let state = self.execute_instruction_state_machine(instruction)?;

            match state {
                ExecutionState::Continue => {
                    context.position += 1;
                }

                ExecutionState::EnterNested(instructions) => {
                    context.position += 1;
                    contexts.push(ExecutionContext {
                        instructions,
                        position: 0,
                        on_complete: ContextCompletion::PopLabel,
                    });
                }

                ExecutionState::CallFunction(func_idx) => {
                    context.position += 1;
                    match self.handle_call_function(func_idx, &mut contexts)? {
                        CallHandled::LocalCall => {}
                        CallHandled::NeedsExternal(request) => {
                            self.saved_contexts = Some(contexts);
                            self.saved_return_types = return_types;
                            return Ok(ExecutionOutcome::NeedsExternalCall(request));
                        }
                    }
                }

                ExecutionState::CallExternalFunction { func_addr, func_type } => {
                    context.position += 1;
                    let args = self.pop_args_for_call(&func_type)?;
                    self.saved_contexts = Some(contexts);
                    self.saved_return_types = return_types;
                    return Ok(ExecutionOutcome::NeedsExternalCall(ExternalCallRequest {
                        func_addr,
                        args,
                        return_types: func_type.return_types.clone(),
                        func_type,
                    }));
                }

                ExecutionState::ReturnFromFunction => {
                    if self.handle_return_from_function(&mut contexts)? {
                        break 'execution;
                    }
                }

                ExecutionState::BranchTo(depth) => {
                    self.handle_branch(&mut contexts, depth)?;
                }
            }
        }

        // Cleanup and collect results
        self.current_label_stack_mut()?.pop();
        self.call_stack.pop();

        let mut results = Vec::new();
        for return_type in return_types.iter().rev() {
            let value = self.stack.pop_typed(*return_type)?;
            results.push(value);
        }
        results.reverse();
        Ok(ExecutionOutcome::Complete(results))
    }

    /// Handle a local or imported function call
    ///
    /// Returns `CallHandled::NeedsExternal` for imported functions,
    /// or `CallHandled::LocalCall` after setting up local function context.
    fn handle_call_function(
        &mut self,
        func_idx: u32,
        contexts: &mut Vec<ExecutionContext<'a>>,
    ) -> Result<CallHandled, RuntimeError> {
        // Check if this is an imported function (needs external call)
        if (func_idx as usize) < self.num_imported_functions {
            let import = &self.module.imports.imports[func_idx as usize];
            let type_idx = match &import.external_kind {
                ExternalKind::Function(idx) => *idx,
                _ => return Err(RuntimeError::InvalidFunctionType),
            };
            let func_type = self
                .function_types
                .get(type_idx as usize)
                .ok_or(RuntimeError::InvalidFunctionType)?
                .clone();

            let args = self.pop_args_for_call(&func_type)?;

            let func_addr = self
                .function_addresses
                .get(func_idx as usize)
                .copied()
                .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))?;

            return Ok(CallHandled::NeedsExternal(ExternalCallRequest {
                func_addr,
                args,
                return_types: func_type.return_types.clone(),
                func_type,
            }));
        }

        // Local function call - get body directly from module (no cloning)
        let local_func_idx = func_idx as usize - self.num_imported_functions;
        let func = self
            .module
            .functions
            .get(local_func_idx as u32)
            .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))?;

        let func_type = self
            .function_types
            .get(func.ftype_index as usize)
            .ok_or(RuntimeError::InvalidFunctionType)?
            .clone();

        if self.call_stack.len() >= MAX_CALL_DEPTH {
            return Err(RuntimeError::CallStackOverflow);
        }

        let args = self.pop_args_for_call(&func_type)?;
        self.push_call_frame(func_idx, args, func_type.return_types.len())?;

        // Borrow function body directly from module (zero-copy)
        let function_body = self
            .module
            .code
            .code
            .get(local_func_idx)
            .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))?;

        let func_label = self.create_function_label(&func_type.return_types);
        self.current_label_stack_mut()?.push(func_label);

        contexts.push(ExecutionContext {
            instructions: &function_body.body.body,
            position: 0,
            on_complete: ContextCompletion::ReturnFunction,
        });

        Ok(CallHandled::LocalCall)
    }

    /// Pop arguments from stack for a function call
    fn pop_args_for_call(&mut self, func_type: &FunctionType) -> Result<Vec<Value>, RuntimeError> {
        let mut args = Vec::new();
        for param_type in func_type.parameters.iter().rev() {
            let value = self.stack.pop_typed(*param_type)?;
            args.push(value);
        }
        args.reverse();
        Ok(args)
    }

    /// Handle return from function, returns true if execution should end
    fn handle_return_from_function(&mut self, contexts: &mut Vec<ExecutionContext<'a>>) -> Result<bool, RuntimeError> {
        let mut found_function_boundary = false;

        while !contexts.is_empty() {
            let ctx = contexts.last().unwrap();
            let is_function = matches!(ctx.on_complete, ContextCompletion::ReturnFunction);
            contexts.pop();

            if is_function {
                found_function_boundary = true;
                if !self.current_label_stack()?.is_empty() {
                    self.current_label_stack_mut()?.pop();
                }
                // Pop the call frame when returning from a function
                if self.call_stack.len() > 1 {
                    self.call_stack.pop();
                } else {
                    // This is the top-level function, execution should end
                    return Ok(true);
                }
                break;
            } else if !self.current_label_stack()?.is_empty() {
                self.current_label_stack_mut()?.pop();
            }
        }

        Ok(!found_function_boundary && contexts.is_empty())
    }

    /// Execute a single instruction and return execution state (for state machine)
    fn execute_instruction_state_machine(
        &mut self,
        instruction: &'a StructuredInstruction,
    ) -> Result<ExecutionState<'a>, RuntimeError> {
        match instruction {
            StructuredInstruction::Plain(inst) => {
                // Special handling for Call instruction
                if let InstructionKind::Call { func_idx } = &inst.kind {
                    // Just prepare the call and return the state
                    return Ok(ExecutionState::CallFunction(*func_idx));
                }

                // Special handling for CallIndirect instruction
                if let InstructionKind::CallIndirect { type_idx, table_idx } = &inst.kind {
                    // Pop table element index from stack
                    let table_elem_idx = self.stack.pop_i32()? as u32;

                    // Get table and lock it
                    let table = self
                        .tables
                        .get(*table_idx as usize)
                        .ok_or(RuntimeError::TableIndexOutOfBounds(*table_idx))?;
                    let table_guard = table.borrow();

                    // Get function reference from table
                    // Spec: call_indirect OOB is "undefined element", not "out of bounds table access"
                    let func_ref = table_guard.get(table_elem_idx).map_err(|e| match e {
                        RuntimeError::TableIndexOutOfBounds(_) => RuntimeError::UndefinedElement(table_elem_idx),
                        other => other,
                    })?;

                    // Extract function address from reference
                    let func_addr = match func_ref {
                        Value::FuncRef(Some(addr)) => addr,
                        Value::FuncRef(None) => {
                            return Err(RuntimeError::UndefinedElement(table_elem_idx));
                        }
                        _ => {
                            return Err(RuntimeError::TypeMismatch {
                                expected: "funcref".to_string(),
                                actual: format!("{:?}", func_ref.typ()),
                            });
                        }
                    };

                    // Get expected function type from type_idx
                    let expected_type = self
                        .function_types
                        .get(*type_idx as usize)
                        .ok_or(RuntimeError::InvalidFunctionType)?
                        .clone();

                    // Try to find FuncAddr in local function_addresses
                    let local_func_idx = self.function_addresses.iter().position(|addr| *addr == func_addr);

                    if let Some(func_idx) = local_func_idx {
                        // Local call - do type checking here
                        let actual_type = self.get_function_type(func_idx as u32)?;

                        // CRITICAL: Type check must match exactly for security
                        if &expected_type != actual_type {
                            return Err(RuntimeError::IndirectCallTypeMismatch {
                                expected: format!("{:?}", expected_type),
                                actual: format!("{:?}", actual_type),
                            });
                        }

                        // Type check passed - proceed with local call
                        return Ok(ExecutionState::CallFunction(func_idx as u32));
                    }

                    // Cross-module call - return external call request
                    // Type checking will be done by the Store using the expected_type
                    return Ok(ExecutionState::CallExternalFunction {
                        func_addr,
                        func_type: expected_type,
                    });
                }

                // Execute as normal and convert BlockEnd to ExecutionState
                match self.execute_plain_instruction(inst)? {
                    BlockEnd::Normal => Ok(ExecutionState::Continue),
                    BlockEnd::Return => Ok(ExecutionState::ReturnFromFunction),
                    BlockEnd::Branch(depth) => Ok(ExecutionState::BranchTo(depth)),
                }
            }

            StructuredInstruction::Block { block_type, body, .. } => {
                // Setup block label and parameters
                self.setup_block_structure(LabelType::Block, *block_type)?;

                // Return state to enter block body (borrow, no clone)
                Ok(ExecutionState::EnterNested(body))
            }

            StructuredInstruction::Loop { block_type, body, .. } => {
                // Setup loop label
                self.setup_block_structure(LabelType::Loop, *block_type)?;

                // Return state to enter loop body (borrow, no clone)
                Ok(ExecutionState::EnterNested(body))
            }

            StructuredInstruction::If {
                block_type,
                then_branch,
                else_branch,
                ..
            } => {
                // Pop condition
                let condition_value = self.stack.pop()?;
                let condition = condition_value.as_i32().ok_or(RuntimeError::TypeMismatch {
                    expected: "i32".to_string(),
                    actual: format!("{:?}", condition_value.typ()),
                })?;

                // Setup if label
                self.setup_block_structure(LabelType::Block, *block_type)?;

                // Choose branch (borrow, no clone)
                let body: &'a [StructuredInstruction] = if condition != 0 {
                    then_branch
                } else {
                    match else_branch {
                        Some(v) => v,
                        None => &[],
                    }
                };

                Ok(ExecutionState::EnterNested(body))
            }
        }
    }

    /// Handle branching by unwinding contexts (for state machine)
    fn handle_branch(&mut self, contexts: &mut Vec<ExecutionContext<'a>>, depth: u32) -> Result<(), RuntimeError> {
        let mut labels_to_pop = depth as usize;

        // Pop contexts and labels until we reach the target
        while labels_to_pop > 0 && !contexts.is_empty() {
            let ctx = contexts.last().unwrap();

            match ctx.on_complete {
                ContextCompletion::PopLabel => {
                    contexts.pop();
                    self.current_label_stack_mut()?.pop();
                    labels_to_pop -= 1;
                }
                ContextCompletion::ReturnFunction => {
                    // Can't branch across function boundary
                    return Err(RuntimeError::InvalidLabel(depth));
                }
            }
        }

        // Now we're at the target label
        // Check what type it is and handle accordingly
        let label_stack = self.current_label_stack()?;
        if let Some(target_label) = label_stack.last() {
            if target_label.label_type == LabelType::Loop {
                // Restart the loop by resetting position
                if let Some(ctx) = contexts.last_mut() {
                    ctx.position = 0;
                }
            } else {
                // For blocks and ifs, exit the context
                if !contexts.is_empty() {
                    contexts.pop();
                    self.current_label_stack_mut()?.pop();
                }
            }
        }

        Ok(())
    }

    /// Resolve a block type to its parameter and return types
    fn resolve_block_type(&self, block_type: &BlockType) -> Result<(Vec<ValueType>, Vec<ValueType>), RuntimeError> {
        match block_type {
            BlockType::Empty => Ok((vec![], vec![])),
            BlockType::Value(vt) => Ok((vec![], vec![*vt])),
            BlockType::FuncType(idx) => {
                let func_type = self
                    .module
                    .types
                    .types
                    .get(*idx as usize)
                    .ok_or(RuntimeError::InvalidFunctionType)?;
                Ok((func_type.parameters.clone(), func_type.return_types.clone()))
            }
        }
    }

    /// Create a label with resolved type information
    fn create_label(
        &self,
        label_type: LabelType,
        block_type: BlockType,
        stack_height: usize,
    ) -> Result<Label, RuntimeError> {
        let (param_types, return_types) = self.resolve_block_type(&block_type)?;

        Ok(Label {
            label_type,
            block_type,
            stack_height,
            unreachable: false,
            param_types,
            return_types,
        })
    }

    /// Common setup for block-like structures (block, loop, if)
    /// 1. Resolves block type to get parameter types
    /// 2. Pops parameters from stack (blocks consume their parameters)
    /// 3. Creates and pushes label
    /// 4. Pushes parameters back for the body to use
    fn setup_block_structure(&mut self, label_type: LabelType, block_type: BlockType) -> Result<(), RuntimeError> {
        // Resolve block type to get parameter types
        let (param_types, _) = self.resolve_block_type(&block_type)?;

        // Pop parameters from stack (blocks consume their parameters)
        let mut params = Vec::with_capacity(param_types.len());
        for _ in &param_types {
            params.push(self.stack.pop()?);
        }
        params.reverse();

        // Create and push label (stack height is after params consumed)
        let label = self.create_label(label_type, block_type, self.stack.len())?;
        self.current_label_stack_mut()?.push(label);

        // Push parameters back for the body to use
        for param in params {
            self.stack.push(param);
        }

        Ok(())
    }

    /// Execute a plain (non-control-flow) instruction
    fn execute_plain_instruction(&mut self, inst: &Instruction) -> Result<BlockEnd, RuntimeError> {
        use InstructionKind::*;

        macro_rules! with_memory {
            (load $op:ident($memarg:expr)) => {{
                let memory = self
                    .memories
                    .first()
                    .ok_or_else(|| RuntimeError::MemoryError("no memory instance available".to_string()))?;
                let mem_guard = memory.borrow();
                ops::memory::$op(&mut self.stack, &mem_guard, $memarg)?;
                Ok(BlockEnd::Normal)
            }};
            (store $op:ident($memarg:expr)) => {{
                let memory = self
                    .memories
                    .first()
                    .ok_or_else(|| RuntimeError::MemoryError("no memory instance available".to_string()))?;
                let mut mem_guard = memory.borrow_mut();
                ops::memory::$op(&mut self.stack, &mut mem_guard, $memarg)?;
                Ok(BlockEnd::Normal)
            }};
            (size $op:ident()) => {{
                let memory = self
                    .memories
                    .first()
                    .ok_or_else(|| RuntimeError::MemoryError("no memory instance available".to_string()))?;
                let mem_guard = memory.borrow();
                ops::memory::$op(&mut self.stack, &mem_guard)?;
                Ok(BlockEnd::Normal)
            }};
            (grow $op:ident()) => {{
                let memory = self
                    .memories
                    .first()
                    .ok_or_else(|| RuntimeError::MemoryError("no memory instance available".to_string()))?;
                let mut mem_guard = memory.borrow_mut();
                ops::memory::$op(&mut self.stack, &mut mem_guard)?;
                Ok(BlockEnd::Normal)
            }};
        }

        match &inst.kind {
            // 4.4.1 Numeric Instructions - Constants
            I32Const { value } => {
                ops::numeric::i32_const(&mut self.stack, *value)?;
                Ok(BlockEnd::Normal)
            }
            I64Const { value } => {
                ops::numeric::i64_const(&mut self.stack, *value)?;
                Ok(BlockEnd::Normal)
            }
            F32Const { value } => {
                ops::numeric::f32_const(&mut self.stack, *value)?;
                Ok(BlockEnd::Normal)
            }
            F64Const { value } => {
                ops::numeric::f64_const(&mut self.stack, *value)?;
                Ok(BlockEnd::Normal)
            }

            // ----------------------------------------------------------------
            // 4.4.8 Control Instructions
            //

            // unreachable - Trap immediately
            // spec: 4.4.8
            // [t1*] → [t2*]
            //
            // From the spec:
            // 1. Trap.
            //
            // The unreachable instruction causes an immediate trap.
            // It is typically used to indicate unreachable code.
            Unreachable => ops::control::unreachable(),

            // nop - No operation
            // spec: 4.4.8
            // [] → []
            //
            // From the spec:
            // 1. Do nothing.
            //
            // The nop instruction does nothing.
            Nop => Ok(BlockEnd::Normal),

            // ----------------------------------------------------------------
            // 4.4.3 Reference Instructions
            //

            // ref.null t - Push null reference
            // spec: 4.4.3
            // [] → [t]
            //
            // From the spec:
            // 1. Push the value ref.null t to the stack.
            //
            // The instruction produces a null value of the given reference type.
            RefNull { ref_type } => {
                match ref_type {
                    ValueType::FuncRef => self.stack.push(Value::FuncRef(None)),
                    ValueType::ExternRef => self.stack.push(Value::ExternRef(None)),
                    _ => {
                        return Err(RuntimeError::InvalidConversion(format!(
                            "invalid reference type for ref.null: {:?}",
                            ref_type
                        )));
                    }
                }
                Ok(BlockEnd::Normal)
            }

            // ref.is_null - Test if reference is null
            // spec: 4.4.3
            // [t] → [i32]
            //
            // From the spec:
            // 1. Assert: due to validation, a reference value is on the top of the stack.
            // 2. Pop the value ref from the stack.
            // 3. If ref is ref.null t, then:
            //    a. Push the value i32.const 1 to the stack.
            // 4. Else:
            //    a. Push the value i32.const 0 to the stack.
            RefIsNull => {
                let value = self.stack.pop()?;
                let is_null = match value {
                    Value::FuncRef(None) | Value::ExternRef(None) => 1,
                    Value::FuncRef(Some(_)) | Value::ExternRef(Some(_)) => 0,
                    _ => {
                        return Err(RuntimeError::TypeMismatch {
                            expected: "reference type".to_string(),
                            actual: format!("{:?}", value.typ()),
                        });
                    }
                };
                self.stack.push(Value::I32(is_null));
                Ok(BlockEnd::Normal)
            }

            // ref.func x - Create function reference
            // spec: 4.4.3
            // [] → [funcref]
            //
            // From the spec:
            // 1. Let F be the current frame.
            // 2. Assert: due to validation, F.module.funcaddrs[x] exists.
            // 3. Let a be the function address F.module.funcaddrs[x].
            // 4. Push the value ref.func a to the stack.
            //
            // The instruction produces a non-null function reference to the function
            // at the given index.
            RefFunc { func_idx } => {
                // Validate function index
                let total_functions = self.module.imports.function_count() + self.module.functions.functions.len();
                if *func_idx as usize >= total_functions {
                    return Err(RuntimeError::FunctionIndexOutOfBounds(*func_idx));
                }
                // Map local func_idx to global FuncAddr
                let func_addr = self
                    .function_addresses
                    .get(*func_idx as usize)
                    .copied()
                    .ok_or(RuntimeError::FunctionIndexOutOfBounds(*func_idx))?;
                self.stack.push(Value::FuncRef(Some(func_addr)));
                Ok(BlockEnd::Normal)
            }

            // 4.4.4 Parametric Instructions
            //
            Drop => {
                ops::parametric::drop(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            Select => {
                ops::parametric::select(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            SelectTyped { val_types } => {
                ops::parametric::select_typed(&mut self.stack, val_types)?;
                Ok(BlockEnd::Normal)
            }

            // 4.4.5 Variable Instructions
            // ---------------------------------------------------------------
            // local.get x - Get local variable
            // spec: 4.4.5
            //
            // From the spec:
            // 1. Let F be the current frame.
            // 2. Assert: due to validation, F.locals[x] exists.
            // 3. Let val be the value F.locals[x].
            // 4. Push the value val to the stack.
            LocalGet { local_idx } => {
                let locals = self.current_locals()?;
                let value = *locals
                    .get(*local_idx as usize)
                    .ok_or(RuntimeError::LocalIndexOutOfBounds(*local_idx))?;
                self.stack.push(value);
                Ok(BlockEnd::Normal)
            }

            // local.set x - Set local variable
            // spec: 4.4.5
            //
            // From the spec:
            // 1. Let F be the current frame.
            // 2. Assert: due to validation, F.locals[x] exists.
            // 3. Assert: due to validation, a value is on the top of the stack.
            // 4. Pop the value val from the stack.
            // 5. Replace F.locals[x] with the value val.
            LocalSet { local_idx } => {
                let value = self.stack.pop()?;
                let locals = self.current_locals_mut()?;
                *locals
                    .get_mut(*local_idx as usize)
                    .ok_or(RuntimeError::LocalIndexOutOfBounds(*local_idx))? = value;
                Ok(BlockEnd::Normal)
            }

            // local.tee x - Set local variable but keep value on stack
            // spec: 4.4.5
            //
            // From the spec:
            // 1. Assert: due to validation, a value is on the top of the stack.
            // 2. Pop the value val from the stack.
            // 3. Push the value val to the stack.
            // 4. Push the value val to the stack.
            // 5. Execute the instruction local.set x.
            //
            // Note: This is equivalent to duplicating the top of stack, then doing local.set
            LocalTee { local_idx } => {
                let value = *self.stack.peek().ok_or(RuntimeError::StackUnderflow)?;
                let locals = self.current_locals_mut()?;
                *locals
                    .get_mut(*local_idx as usize)
                    .ok_or(RuntimeError::LocalIndexOutOfBounds(*local_idx))? = value;
                Ok(BlockEnd::Normal)
            }

            // global.get x - Get global variable
            // spec: 4.4.5
            //
            // From the spec:
            // 1. Let F be the current frame.
            // 2. Assert: due to validation, F.module.globals[x] exists.
            // 3. Let val be the value F.module.globals[x].val.
            // 4. Push the value val to the stack.
            GlobalGet { global_idx } => {
                let shared = self
                    .globals
                    .get(*global_idx as usize)
                    .ok_or(RuntimeError::GlobalIndexOutOfBounds(*global_idx))?;
                self.stack.push(shared.get());
                Ok(BlockEnd::Normal)
            }

            // global.set x - Set global variable
            // spec: 4.4.5
            //
            // From the spec:
            // 1. Let F be the current frame.
            // 2. Assert: due to validation, F.module.globals[x] exists.
            // 3. Assert: due to validation, F.module.globals[x] is mutable.
            // 4. Assert: due to validation, a value is on the top of the stack.
            // 5. Pop the value val from the stack.
            // 6. Replace F.module.globals[x].val with the value val.
            GlobalSet { global_idx } => {
                let value = self.stack.pop()?;

                // Bounds check
                if *global_idx as usize >= self.globals.len() {
                    return Err(RuntimeError::GlobalIndexOutOfBounds(*global_idx));
                }

                // Check mutability
                if !is_global_mutable(self.module, *global_idx)? {
                    return Err(RuntimeError::InvalidConversion(
                        "cannot set immutable global".to_string(),
                    ));
                }

                self.globals[*global_idx as usize].set(value);
                Ok(BlockEnd::Normal)
            }

            // 4.4.8 Control Instructions

            // br l - Unconditional branch
            // spec: 4.4.8
            // [t*] → [t*]
            //
            // Branches to the l-th enclosing block
            Br { label_idx } => {
                let label_stack = self.current_label_stack()?;
                let label_stack = LabelStack::from_vec(label_stack.clone());
                ops::control::br(&mut self.stack, &label_stack, *label_idx)
            }

            // br_if l - Conditional branch
            // spec: 4.4.8
            // [t* i32] → [t*]
            //
            // Branches to the l-th enclosing block if condition is non-zero
            BrIf { label_idx } => {
                let label_stack = self.current_label_stack()?;
                let label_stack = LabelStack::from_vec(label_stack.clone());
                ops::control::br_if(&mut self.stack, &label_stack, *label_idx)
            }

            // br_table l* lN - Indirect branch via table
            // spec: 4.4.8
            // [t* i32] → [t*]
            //
            // Branches to label indexed by operand, with default
            BrTable { labels, default } => {
                let label_stack = self.current_label_stack()?;
                let label_stack = LabelStack::from_vec(label_stack.clone());
                ops::control::br_table(&mut self.stack, &label_stack, labels, *default)
            }

            // return - Return from function
            // spec: 4.4.8
            // [t*] → [t*]
            //
            // Returns from the current function with values on stack
            Return => ops::control::return_op(),

            // call x - Direct function call
            // spec: 4.4.8
            // [t1*] → [t2*]
            //
            // Calls function at index x
            Call { func_idx: _ } => {
                // This should never be reached - the state machine intercepts Call instructions
                // before they get to execute_plain_instruction. If we reach here, it's a bug.
                unreachable!("Call instruction should have been intercepted by state machine")
            }

            // ----------------------------------------------------------------
            // Memory Instructions
            //
            // 4.4.7.3 Memory Size Operations
            MemorySize => with_memory!(size memory_size()),
            MemoryGrow => with_memory!(grow memory_grow()),

            // 4.4.7.1 Memory Load Operations
            I32Load { memarg } => with_memory!(load i32_load(memarg)),

            // 4.4.7.2 Memory Store Operations
            I32Store { memarg } => with_memory!(store i32_store(memarg)),
            I64Store { memarg } => with_memory!(store i64_store(memarg)),
            F32Store { memarg } => with_memory!(store f32_store(memarg)),
            F64Store { memarg } => with_memory!(store f64_store(memarg)),

            // Additional load operations
            I64Load { memarg } => with_memory!(load i64_load(memarg)),
            F32Load { memarg } => with_memory!(load f32_load(memarg)),
            F64Load { memarg } => with_memory!(load f64_load(memarg)),

            // Sized memory loads
            I32Load8U { memarg } => with_memory!(load i32_load8_u(memarg)),
            I32Load8S { memarg } => with_memory!(load i32_load8_s(memarg)),
            I32Load16U { memarg } => with_memory!(load i32_load16_u(memarg)),
            I32Load16S { memarg } => with_memory!(load i32_load16_s(memarg)),
            I64Load8U { memarg } => with_memory!(load i64_load8_u(memarg)),
            I64Load8S { memarg } => with_memory!(load i64_load8_s(memarg)),
            I64Load16U { memarg } => with_memory!(load i64_load16_u(memarg)),
            I64Load16S { memarg } => with_memory!(load i64_load16_s(memarg)),
            I64Load32U { memarg } => with_memory!(load i64_load32_u(memarg)),
            I64Load32S { memarg } => with_memory!(load i64_load32_s(memarg)),

            // Sized memory stores
            I32Store8 { memarg } => with_memory!(store i32_store8(memarg)),
            I32Store16 { memarg } => with_memory!(store i32_store16(memarg)),
            I64Store8 { memarg } => with_memory!(store i64_store8(memarg)),
            I64Store16 { memarg } => with_memory!(store i64_store16(memarg)),
            I64Store32 { memarg } => with_memory!(store i64_store32(memarg)),

            // 4.4.7.4 Bulk Memory Operations
            // spec: bulk memory operations proposal (now standard)

            // memory.init x - Initialise memory from data segment
            // [i32 i32 i32] → []
            // Stack: [dest_addr, src_offset, length]
            MemoryInit { data_idx } => {
                let memory = self
                    .memories
                    .first()
                    .ok_or_else(|| RuntimeError::MemoryError("no memory available".to_string()))?;
                let mut mem_guard = memory.borrow_mut();
                ops::memory::memory_init(
                    &mut self.stack,
                    &mut mem_guard,
                    *data_idx,
                    &self.module.data.data,
                    &self.dropped_data,
                )?;
                Ok(BlockEnd::Normal)
            }

            // memory.copy - Copy memory within the same memory
            // [i32 i32 i32] → []
            // Stack: [dest_addr, src_addr, length]
            MemoryCopy => {
                let memory = self
                    .memories
                    .first()
                    .ok_or_else(|| RuntimeError::MemoryError("no memory available".to_string()))?;
                let mut mem_guard = memory.borrow_mut();
                ops::memory::memory_copy(&mut self.stack, &mut mem_guard)?;
                Ok(BlockEnd::Normal)
            }

            // memory.fill - Fill memory with a byte value
            // [i32 i32 i32] → []
            // Stack: [dest_addr, value, length]
            MemoryFill => {
                let memory = self
                    .memories
                    .first()
                    .ok_or_else(|| RuntimeError::MemoryError("no memory available".to_string()))?;
                let mut mem_guard = memory.borrow_mut();
                ops::memory::memory_fill(&mut self.stack, &mut mem_guard)?;
                Ok(BlockEnd::Normal)
            }

            // data.drop x - Drop a data segment
            // [] → []
            // Prevents further use of data segment x
            DataDrop { data_idx } => {
                self.dropped_data.insert(*data_idx);
                Ok(BlockEnd::Normal)
            }

            // ----------------------------------------------------------------
            // 4.4.1 Numeric Instructions - Unary Operations

            // i32.clz - Count leading zeros
            I32Clz => {
                ops::numeric::i32_clz(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // i32.ctz - Count trailing zeros
            I32Ctz => {
                ops::numeric::i32_ctz(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // i32.popcnt - Population count
            I32Popcnt => {
                ops::numeric::i32_popcnt(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // i64.clz - Count leading zeros
            I64Clz => {
                ops::numeric::i64_clz(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // i64.ctz - Count trailing zeros
            I64Ctz => {
                ops::numeric::i64_ctz(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // i64.popcnt - Population count
            I64Popcnt => {
                ops::numeric::i64_popcnt(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f32.abs - Absolute value
            F32Abs => {
                ops::numeric::f32_abs(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f32.neg - Negation
            F32Neg => {
                ops::numeric::f32_neg(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f32.sqrt - Square root
            F32Sqrt => {
                ops::numeric::f32_sqrt(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f32.ceil - Ceiling
            F32Ceil => {
                ops::numeric::f32_ceil(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f32.floor - Floor
            F32Floor => {
                ops::numeric::f32_floor(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f32.trunc - Truncate
            F32Trunc => {
                ops::numeric::f32_trunc(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f32.nearest - Round to nearest even
            F32Nearest => {
                ops::numeric::f32_nearest(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f64.abs - Absolute value
            F64Abs => {
                ops::numeric::f64_abs(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f64.neg - Negation
            F64Neg => {
                ops::numeric::f64_neg(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f64.sqrt - Square root
            F64Sqrt => {
                ops::numeric::f64_sqrt(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f64.ceil - Ceiling
            F64Ceil => {
                ops::numeric::f64_ceil(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f64.floor - Floor
            F64Floor => {
                ops::numeric::f64_floor(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f64.trunc - Truncate
            F64Trunc => {
                ops::numeric::f64_trunc(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f64.nearest - Round to nearest even
            F64Nearest => {
                ops::numeric::f64_nearest(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // ----------------------------------------------------------------
            // 4.4.1.3 Numeric Instructions - Binary Operations

            // f32.add
            F32Add => {
                ops::numeric::f32_add(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f32.sub
            F32Sub => {
                ops::numeric::f32_sub(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f32.mul
            F32Mul => {
                ops::numeric::f32_mul(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f32.div
            F32Div => {
                ops::numeric::f32_div(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f32.min
            F32Min => {
                ops::numeric::f32_min(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f32.max
            F32Max => {
                ops::numeric::f32_max(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f32.copysign
            F32Copysign => {
                ops::numeric::f32_copysign(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f64.add
            F64Add => {
                ops::numeric::f64_add(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f64.sub
            F64Sub => {
                ops::numeric::f64_sub(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f64.mul
            F64Mul => {
                ops::numeric::f64_mul(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f64.div
            F64Div => {
                ops::numeric::f64_div(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f64.min
            F64Min => {
                ops::numeric::f64_min(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f64.max
            F64Max => {
                ops::numeric::f64_max(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f64.copysign
            F64Copysign => {
                ops::numeric::f64_copysign(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            I32Add => {
                ops::numeric::i32_add(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            I32Sub => {
                ops::numeric::i32_sub(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            I32Mul => {
                ops::numeric::i32_mul(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            I32DivS => {
                ops::numeric::i32_div_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            I32DivU => {
                ops::numeric::i32_div_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            I32RemS => {
                ops::numeric::i32_rem_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            I32RemU => {
                ops::numeric::i32_rem_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            I64Add => {
                ops::numeric::i64_add(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            I64Sub => {
                ops::numeric::i64_sub(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            I64Mul => {
                ops::numeric::i64_mul(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            I64DivS => {
                ops::numeric::i64_div_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            I64DivU => {
                ops::numeric::i64_div_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            I64RemS => {
                ops::numeric::i64_rem_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            I64RemU => {
                ops::numeric::i64_rem_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // ----------------------------------------------------------------
            // 4.4.1.3 Bitwise Operations

            // i32 bitwise operations
            I32And => {
                ops::bitwise::i32_and(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32Or => {
                ops::bitwise::i32_or(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32Xor => {
                ops::bitwise::i32_xor(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32Shl => {
                ops::bitwise::i32_shl(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32ShrS => {
                ops::bitwise::i32_shr_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32ShrU => {
                ops::bitwise::i32_shr_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32Rotl => {
                ops::bitwise::i32_rotl(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32Rotr => {
                ops::bitwise::i32_rotr(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // i64 bitwise operations
            I64And => {
                ops::bitwise::i64_and(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64Or => {
                ops::bitwise::i64_or(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64Xor => {
                ops::bitwise::i64_xor(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64Shl => {
                ops::bitwise::i64_shl(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64ShrS => {
                ops::bitwise::i64_shr_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64ShrU => {
                ops::bitwise::i64_shr_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64Rotl => {
                ops::bitwise::i64_rotl(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64Rotr => {
                ops::bitwise::i64_rotr(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // ----------------------------------------------------------------
            // 4.4.1.4 Test Instructions & 4.4.1.5 Comparison Instructions

            // i32 test and comparison
            I32Eqz => {
                ops::comparison::i32_eqz(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32Eq => {
                ops::comparison::i32_eq(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32Ne => {
                ops::comparison::i32_ne(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32LtS => {
                ops::comparison::i32_lt_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32LtU => {
                ops::comparison::i32_lt_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32GtS => {
                ops::comparison::i32_gt_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32GtU => {
                ops::comparison::i32_gt_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32LeS => {
                ops::comparison::i32_le_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32LeU => {
                ops::comparison::i32_le_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32GeS => {
                ops::comparison::i32_ge_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32GeU => {
                ops::comparison::i32_ge_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // i64 test and comparison
            I64Eqz => {
                ops::comparison::i64_eqz(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64Eq => {
                ops::comparison::i64_eq(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64Ne => {
                ops::comparison::i64_ne(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64LtS => {
                ops::comparison::i64_lt_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64LtU => {
                ops::comparison::i64_lt_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64GtS => {
                ops::comparison::i64_gt_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64GtU => {
                ops::comparison::i64_gt_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64LeS => {
                ops::comparison::i64_le_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64LeU => {
                ops::comparison::i64_le_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64GeS => {
                ops::comparison::i64_ge_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64GeU => {
                ops::comparison::i64_ge_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f32 comparison
            F32Eq => {
                ops::comparison::f32_eq(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F32Ne => {
                ops::comparison::f32_ne(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F32Lt => {
                ops::comparison::f32_lt(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F32Gt => {
                ops::comparison::f32_gt(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F32Le => {
                ops::comparison::f32_le(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F32Ge => {
                ops::comparison::f32_ge(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // f64 comparison
            F64Eq => {
                ops::comparison::f64_eq(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F64Ne => {
                ops::comparison::f64_ne(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F64Lt => {
                ops::comparison::f64_lt(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F64Gt => {
                ops::comparison::f64_gt(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F64Le => {
                ops::comparison::f64_le(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F64Ge => {
                ops::comparison::f64_ge(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // ----------------------------------------------------------------
            // 4.4.1.6 Conversion Instructions

            // Integer width conversions
            I32WrapI64 => {
                ops::conversion::i32_wrap_i64(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64ExtendI32S => {
                ops::conversion::i64_extend_i32_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64ExtendI32U => {
                ops::conversion::i64_extend_i32_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // Sign extension operations
            I32Extend8S => {
                ops::conversion::i32_extend8_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32Extend16S => {
                ops::conversion::i32_extend16_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64Extend8S => {
                ops::conversion::i64_extend8_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64Extend16S => {
                ops::conversion::i64_extend16_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64Extend32S => {
                ops::conversion::i64_extend32_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // Float width conversions
            F32DemoteF64 => {
                ops::conversion::f32_demote_f64(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F64PromoteF32 => {
                ops::conversion::f64_promote_f32(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // Reinterpret/bit casting
            I32ReinterpretF32 => {
                ops::conversion::i32_reinterpret_f32(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64ReinterpretF64 => {
                ops::conversion::i64_reinterpret_f64(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F32ReinterpretI32 => {
                ops::conversion::f32_reinterpret_i32(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F64ReinterpretI64 => {
                ops::conversion::f64_reinterpret_i64(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // Integer to float conversions
            F32ConvertI32S => {
                ops::conversion::f32_convert_i32_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F32ConvertI32U => {
                ops::conversion::f32_convert_i32_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F32ConvertI64S => {
                ops::conversion::f32_convert_i64_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F32ConvertI64U => {
                ops::conversion::f32_convert_i64_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F64ConvertI32S => {
                ops::conversion::f64_convert_i32_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F64ConvertI32U => {
                ops::conversion::f64_convert_i32_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F64ConvertI64S => {
                ops::conversion::f64_convert_i64_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            F64ConvertI64U => {
                ops::conversion::f64_convert_i64_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // Float to integer truncation (trapping)
            I32TruncF32S => {
                ops::conversion::i32_trunc_f32_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32TruncF32U => {
                ops::conversion::i32_trunc_f32_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32TruncF64S => {
                ops::conversion::i32_trunc_f64_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32TruncF64U => {
                ops::conversion::i32_trunc_f64_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64TruncF32S => {
                ops::conversion::i64_trunc_f32_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64TruncF32U => {
                ops::conversion::i64_trunc_f32_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64TruncF64S => {
                ops::conversion::i64_trunc_f64_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64TruncF64U => {
                ops::conversion::i64_trunc_f64_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // Saturating truncation (non-trapping)
            I32TruncSatF32S => {
                ops::conversion::i32_trunc_sat_f32_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32TruncSatF32U => {
                ops::conversion::i32_trunc_sat_f32_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32TruncSatF64S => {
                ops::conversion::i32_trunc_sat_f64_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I32TruncSatF64U => {
                ops::conversion::i32_trunc_sat_f64_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64TruncSatF32S => {
                ops::conversion::i64_trunc_sat_f32_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64TruncSatF32U => {
                ops::conversion::i64_trunc_sat_f32_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64TruncSatF64S => {
                ops::conversion::i64_trunc_sat_f64_s(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }
            I64TruncSatF64U => {
                ops::conversion::i64_trunc_sat_f64_u(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // ----------------------------------------------------------------
            // Table Instructions
            TableGet { table_idx } => {
                let idx = self.stack.pop_i32()?;
                let table = self
                    .tables
                    .get(*table_idx as usize)
                    .ok_or(RuntimeError::TableIndexOutOfBounds(*table_idx))?;
                let table_guard = table.borrow();

                let value = table_guard.get(idx as u32)?;

                self.stack.push(value);
                Ok(BlockEnd::Normal)
            }
            TableSet { table_idx } => {
                let value = self.stack.pop()?;
                let idx = self.stack.pop_i32()?;

                let table = self
                    .tables
                    .get(*table_idx as usize)
                    .ok_or(RuntimeError::TableIndexOutOfBounds(*table_idx))?;
                let mut table_guard = table.borrow_mut();

                table_guard.set(idx as u32, Some(value))?;
                Ok(BlockEnd::Normal)
            }
            TableSize { table_idx } => {
                let table = self
                    .tables
                    .get(*table_idx as usize)
                    .ok_or(RuntimeError::TableIndexOutOfBounds(*table_idx))?;
                let table_guard = table.borrow();
                self.stack.push(Value::I32(table_guard.size() as i32));
                Ok(BlockEnd::Normal)
            }
            TableGrow { table_idx } => {
                let delta = self.stack.pop_i32()?;
                let init_value = self.stack.pop()?;

                let table = self
                    .tables
                    .get(*table_idx as usize)
                    .ok_or(RuntimeError::TableIndexOutOfBounds(*table_idx))?;
                let mut table_guard = table.borrow_mut();

                let result = table_guard.grow(delta as u32, Some(init_value))?;
                self.stack.push(Value::I32(result as i32));
                Ok(BlockEnd::Normal)
            }
            TableInit { elem_idx, table_idx } => {
                let count = self.stack.pop_i32()? as u32;
                let src_idx = self.stack.pop_i32()? as u32;
                let dst_idx = self.stack.pop_i32()? as u32;

                let src_segment = self
                    .element_segments
                    .get(*elem_idx as usize)
                    .ok_or(RuntimeError::ElementIndexOutOfBounds(*elem_idx))?;

                let table = self
                    .tables
                    .get(*table_idx as usize)
                    .ok_or(RuntimeError::TableIndexOutOfBounds(*table_idx))?;
                let mut table_guard = table.borrow_mut();

                table_guard.init(dst_idx, src_segment, src_idx, count)?;
                Ok(BlockEnd::Normal)
            }
            TableCopy { dst_table, src_table } => {
                let count = self.stack.pop_i32()? as u32;
                let src_idx = self.stack.pop_i32()? as u32;
                let dst_idx = self.stack.pop_i32()? as u32;

                if dst_table == src_table {
                    // Same table - use copy_within
                    let table = self
                        .tables
                        .get(*dst_table as usize)
                        .ok_or(RuntimeError::TableIndexOutOfBounds(*dst_table))?;
                    let mut table_guard = table.borrow_mut();
                    table_guard.copy_within(dst_idx, src_idx, count)?;
                } else {
                    // Different tables - borrow both independently
                    let src_table_rc = self
                        .tables
                        .get(*src_table as usize)
                        .ok_or(RuntimeError::TableIndexOutOfBounds(*src_table))?;
                    let dst_table_rc = self
                        .tables
                        .get(*dst_table as usize)
                        .ok_or(RuntimeError::TableIndexOutOfBounds(*dst_table))?;

                    let src_guard = src_table_rc.borrow();
                    let mut dst_guard = dst_table_rc.borrow_mut();

                    dst_guard.copy_from(dst_idx, &src_guard, src_idx, count)?;
                }

                Ok(BlockEnd::Normal)
            }
            TableFill { table_idx } => {
                let count = self.stack.pop_i32()? as u32;
                let value = self.stack.pop()?;
                let start = self.stack.pop_i32()? as u32;

                let table = self
                    .tables
                    .get(*table_idx as usize)
                    .ok_or(RuntimeError::TableIndexOutOfBounds(*table_idx))?;
                let mut table_guard = table.borrow_mut();

                table_guard.fill(start, count, Some(value))?;
                Ok(BlockEnd::Normal)
            }
            ElemDrop { elem_idx } => {
                if (*elem_idx as usize) >= self.element_segments.len() {
                    return Err(RuntimeError::ElementIndexOutOfBounds(*elem_idx));
                }
                // Clear the segment
                self.element_segments[*elem_idx as usize].clear();
                Ok(BlockEnd::Normal)
            }

            // SIMD instructions
            Simd(op) => {
                macro_rules! simd_mem {
                    (load $fn:ident($memarg:expr)) => {{
                        let memory = self
                            .memories
                            .first()
                            .ok_or_else(|| RuntimeError::MemoryError("no memory instance available".to_string()))?;
                        let mem_guard = memory.borrow();
                        ops::simd::$fn(&mut self.stack, &mem_guard, $memarg)?;
                        Ok(BlockEnd::Normal)
                    }};
                    (store $fn:ident($memarg:expr)) => {{
                        let memory = self
                            .memories
                            .first()
                            .ok_or_else(|| RuntimeError::MemoryError("no memory instance available".to_string()))?;
                        let mut mem_guard = memory.borrow_mut();
                        ops::simd::$fn(&mut self.stack, &mut mem_guard, $memarg)?;
                        Ok(BlockEnd::Normal)
                    }};
                }
                // Dispatch stack-only SIMD ops via macro to avoid rustfmt bloat
                macro_rules! simd_op {
                    ($fn:ident) => {{
                        ops::simd::$fn(&mut self.stack)?;
                        Ok(BlockEnd::Normal)
                    }};
                }
                // Dispatch SIMD ops that take a lane index
                macro_rules! simd_lane_op {
                    ($fn:ident, $lane:expr) => {{
                        ops::simd::$fn(&mut self.stack, $lane)?;
                        Ok(BlockEnd::Normal)
                    }};
                }
                // Dispatch SIMD load-lane ops (memarg + lane + v128 + i32 on stack)
                macro_rules! simd_mem_lane {
                    (load $fn:ident($memarg:expr, $lane:expr)) => {{
                        let memory = self
                            .memories
                            .first()
                            .ok_or_else(|| RuntimeError::MemoryError("no memory instance available".to_string()))?;
                        let mem_guard = memory.borrow();
                        ops::simd::$fn(&mut self.stack, &mem_guard, $memarg, *$lane)?;
                        Ok(BlockEnd::Normal)
                    }};
                    (store $fn:ident($memarg:expr, $lane:expr)) => {{
                        let memory = self
                            .memories
                            .first()
                            .ok_or_else(|| RuntimeError::MemoryError("no memory instance available".to_string()))?;
                        let mut mem_guard = memory.borrow_mut();
                        ops::simd::$fn(&mut self.stack, &mut mem_guard, $memarg, *$lane)?;
                        Ok(BlockEnd::Normal)
                    }};
                }
                match op {
                    // Constant
                    SimdOp::V128Const { value } => {
                        self.stack.push(Value::V128(*value));
                        Ok(BlockEnd::Normal)
                    }

                    // Memory load/store
                    SimdOp::V128Load { memarg } => simd_mem!(load v128_load(memarg)),
                    SimdOp::V128Load8x8S { memarg } => simd_mem!(load v128_load8x8_s(memarg)),
                    SimdOp::V128Load8x8U { memarg } => simd_mem!(load v128_load8x8_u(memarg)),
                    SimdOp::V128Load16x4S { memarg } => simd_mem!(load v128_load16x4_s(memarg)),
                    SimdOp::V128Load16x4U { memarg } => simd_mem!(load v128_load16x4_u(memarg)),
                    SimdOp::V128Load32x2S { memarg } => simd_mem!(load v128_load32x2_s(memarg)),
                    SimdOp::V128Load32x2U { memarg } => simd_mem!(load v128_load32x2_u(memarg)),
                    SimdOp::V128Load8Splat { memarg } => simd_mem!(load v128_load8_splat(memarg)),
                    SimdOp::V128Load16Splat { memarg } => simd_mem!(load v128_load16_splat(memarg)),
                    SimdOp::V128Load32Splat { memarg } => simd_mem!(load v128_load32_splat(memarg)),
                    SimdOp::V128Load64Splat { memarg } => simd_mem!(load v128_load64_splat(memarg)),
                    SimdOp::V128Load32Zero { memarg } => simd_mem!(load v128_load32_zero(memarg)),
                    SimdOp::V128Load64Zero { memarg } => simd_mem!(load v128_load64_zero(memarg)),
                    SimdOp::V128Store { memarg } => simd_mem!(store v128_store(memarg)),

                    // Memory load/store lane
                    SimdOp::V128Load8Lane { memarg, lane } => simd_mem_lane!(load v128_load8_lane(memarg, lane)),
                    SimdOp::V128Load16Lane { memarg, lane } => simd_mem_lane!(load v128_load16_lane(memarg, lane)),
                    SimdOp::V128Load32Lane { memarg, lane } => simd_mem_lane!(load v128_load32_lane(memarg, lane)),
                    SimdOp::V128Load64Lane { memarg, lane } => simd_mem_lane!(load v128_load64_lane(memarg, lane)),
                    SimdOp::V128Store8Lane { memarg, lane } => simd_mem_lane!(store v128_store8_lane(memarg, lane)),
                    SimdOp::V128Store16Lane { memarg, lane } => simd_mem_lane!(store v128_store16_lane(memarg, lane)),
                    SimdOp::V128Store32Lane { memarg, lane } => simd_mem_lane!(store v128_store32_lane(memarg, lane)),
                    SimdOp::V128Store64Lane { memarg, lane } => simd_mem_lane!(store v128_store64_lane(memarg, lane)),

                    // Shuffle and swizzle
                    SimdOp::I8x16Shuffle { lanes } => {
                        ops::simd::i8x16_shuffle(&mut self.stack, lanes)?;
                        Ok(BlockEnd::Normal)
                    }
                    SimdOp::I8x16Swizzle => simd_op!(i8x16_swizzle),

                    // Splat
                    SimdOp::I8x16Splat => simd_op!(i8x16_splat),
                    SimdOp::I16x8Splat => simd_op!(i16x8_splat),
                    SimdOp::I32x4Splat => simd_op!(i32x4_splat),
                    SimdOp::I64x2Splat => simd_op!(i64x2_splat),
                    SimdOp::F32x4Splat => simd_op!(f32x4_splat),
                    SimdOp::F64x2Splat => simd_op!(f64x2_splat),

                    // Lane extraction
                    SimdOp::I8x16ExtractLaneS { lane } => simd_lane_op!(i8x16_extract_lane_s, *lane),
                    SimdOp::I8x16ExtractLaneU { lane } => simd_lane_op!(i8x16_extract_lane_u, *lane),
                    SimdOp::I8x16ReplaceLane { lane } => simd_lane_op!(i8x16_replace_lane, *lane),
                    SimdOp::I16x8ExtractLaneS { lane } => simd_lane_op!(i16x8_extract_lane_s, *lane),
                    SimdOp::I16x8ExtractLaneU { lane } => simd_lane_op!(i16x8_extract_lane_u, *lane),
                    SimdOp::I16x8ReplaceLane { lane } => simd_lane_op!(i16x8_replace_lane, *lane),
                    SimdOp::I32x4ExtractLane { lane } => simd_lane_op!(i32x4_extract_lane, *lane),
                    SimdOp::I32x4ReplaceLane { lane } => simd_lane_op!(i32x4_replace_lane, *lane),
                    SimdOp::I64x2ExtractLane { lane } => simd_lane_op!(i64x2_extract_lane, *lane),
                    SimdOp::I64x2ReplaceLane { lane } => simd_lane_op!(i64x2_replace_lane, *lane),
                    SimdOp::F32x4ExtractLane { lane } => simd_lane_op!(f32x4_extract_lane, *lane),
                    SimdOp::F32x4ReplaceLane { lane } => simd_lane_op!(f32x4_replace_lane, *lane),
                    SimdOp::F64x2ExtractLane { lane } => simd_lane_op!(f64x2_extract_lane, *lane),
                    SimdOp::F64x2ReplaceLane { lane } => simd_lane_op!(f64x2_replace_lane, *lane),

                    // i8x16 comparisons
                    SimdOp::I8x16Eq => simd_op!(i8x16_eq),
                    SimdOp::I8x16Ne => simd_op!(i8x16_ne),
                    SimdOp::I8x16LtS => simd_op!(i8x16_lt_s),
                    SimdOp::I8x16LtU => simd_op!(i8x16_lt_u),
                    SimdOp::I8x16GtS => simd_op!(i8x16_gt_s),
                    SimdOp::I8x16GtU => simd_op!(i8x16_gt_u),
                    SimdOp::I8x16LeS => simd_op!(i8x16_le_s),
                    SimdOp::I8x16LeU => simd_op!(i8x16_le_u),
                    SimdOp::I8x16GeS => simd_op!(i8x16_ge_s),
                    SimdOp::I8x16GeU => simd_op!(i8x16_ge_u),

                    // i16x8 comparisons
                    SimdOp::I16x8Eq => simd_op!(i16x8_eq),
                    SimdOp::I16x8Ne => simd_op!(i16x8_ne),
                    SimdOp::I16x8LtS => simd_op!(i16x8_lt_s),
                    SimdOp::I16x8LtU => simd_op!(i16x8_lt_u),
                    SimdOp::I16x8GtS => simd_op!(i16x8_gt_s),
                    SimdOp::I16x8GtU => simd_op!(i16x8_gt_u),
                    SimdOp::I16x8LeS => simd_op!(i16x8_le_s),
                    SimdOp::I16x8LeU => simd_op!(i16x8_le_u),
                    SimdOp::I16x8GeS => simd_op!(i16x8_ge_s),
                    SimdOp::I16x8GeU => simd_op!(i16x8_ge_u),

                    // i32x4 comparisons
                    SimdOp::I32x4Eq => simd_op!(i32x4_eq),
                    SimdOp::I32x4Ne => simd_op!(i32x4_ne),
                    SimdOp::I32x4LtS => simd_op!(i32x4_lt_s),
                    SimdOp::I32x4LtU => simd_op!(i32x4_lt_u),
                    SimdOp::I32x4GtS => simd_op!(i32x4_gt_s),
                    SimdOp::I32x4GtU => simd_op!(i32x4_gt_u),
                    SimdOp::I32x4LeS => simd_op!(i32x4_le_s),
                    SimdOp::I32x4LeU => simd_op!(i32x4_le_u),
                    SimdOp::I32x4GeS => simd_op!(i32x4_ge_s),
                    SimdOp::I32x4GeU => simd_op!(i32x4_ge_u),

                    // i64x2 comparisons
                    SimdOp::I64x2Eq => simd_op!(i64x2_eq),
                    SimdOp::I64x2Ne => simd_op!(i64x2_ne),
                    SimdOp::I64x2LtS => simd_op!(i64x2_lt_s),
                    SimdOp::I64x2GtS => simd_op!(i64x2_gt_s),
                    SimdOp::I64x2LeS => simd_op!(i64x2_le_s),
                    SimdOp::I64x2GeS => simd_op!(i64x2_ge_s),

                    // f32x4 comparisons
                    SimdOp::F32x4Eq => simd_op!(f32x4_eq),
                    SimdOp::F32x4Ne => simd_op!(f32x4_ne),
                    SimdOp::F32x4Lt => simd_op!(f32x4_lt),
                    SimdOp::F32x4Gt => simd_op!(f32x4_gt),
                    SimdOp::F32x4Le => simd_op!(f32x4_le),
                    SimdOp::F32x4Ge => simd_op!(f32x4_ge),

                    // f64x2 comparisons
                    SimdOp::F64x2Eq => simd_op!(f64x2_eq),
                    SimdOp::F64x2Ne => simd_op!(f64x2_ne),
                    SimdOp::F64x2Lt => simd_op!(f64x2_lt),
                    SimdOp::F64x2Gt => simd_op!(f64x2_gt),
                    SimdOp::F64x2Le => simd_op!(f64x2_le),
                    SimdOp::F64x2Ge => simd_op!(f64x2_ge),

                    // v128 bitwise
                    SimdOp::V128Not => simd_op!(v128_not),
                    SimdOp::V128And => simd_op!(v128_and),
                    SimdOp::V128AndNot => simd_op!(v128_andnot),
                    SimdOp::V128Or => simd_op!(v128_or),
                    SimdOp::V128Xor => simd_op!(v128_xor),
                    SimdOp::V128Bitselect => simd_op!(v128_bitselect),
                    SimdOp::V128AnyTrue => simd_op!(v128_any_true),

                    // i8x16 operations
                    SimdOp::I8x16Abs => simd_op!(i8x16_abs),
                    SimdOp::I8x16Neg => simd_op!(i8x16_neg),
                    SimdOp::I8x16Popcnt => simd_op!(i8x16_popcnt),
                    SimdOp::I8x16AllTrue => simd_op!(i8x16_all_true),
                    SimdOp::I8x16Bitmask => simd_op!(i8x16_bitmask),
                    SimdOp::I8x16NarrowI16x8S => simd_op!(i8x16_narrow_i16x8_s),
                    SimdOp::I8x16NarrowI16x8U => simd_op!(i8x16_narrow_i16x8_u),
                    SimdOp::I8x16Shl => simd_op!(i8x16_shl),
                    SimdOp::I8x16ShrS => simd_op!(i8x16_shr_s),
                    SimdOp::I8x16ShrU => simd_op!(i8x16_shr_u),
                    SimdOp::I8x16Add => simd_op!(i8x16_add),
                    SimdOp::I8x16AddSatS => simd_op!(i8x16_add_sat_s),
                    SimdOp::I8x16AddSatU => simd_op!(i8x16_add_sat_u),
                    SimdOp::I8x16Sub => simd_op!(i8x16_sub),
                    SimdOp::I8x16SubSatS => simd_op!(i8x16_sub_sat_s),
                    SimdOp::I8x16SubSatU => simd_op!(i8x16_sub_sat_u),
                    SimdOp::I8x16MinS => simd_op!(i8x16_min_s),
                    SimdOp::I8x16MinU => simd_op!(i8x16_min_u),
                    SimdOp::I8x16MaxS => simd_op!(i8x16_max_s),
                    SimdOp::I8x16MaxU => simd_op!(i8x16_max_u),
                    SimdOp::I8x16AvgrU => simd_op!(i8x16_avgr_u),

                    // i16x8 operations
                    SimdOp::I16x8ExtAddPairwiseI8x16S => simd_op!(i16x8_extadd_pairwise_i8x16_s),
                    SimdOp::I16x8ExtAddPairwiseI8x16U => simd_op!(i16x8_extadd_pairwise_i8x16_u),
                    SimdOp::I16x8Abs => simd_op!(i16x8_abs),
                    SimdOp::I16x8Neg => simd_op!(i16x8_neg),
                    SimdOp::I16x8Q15MulrSatS => simd_op!(i16x8_q15mulr_sat_s),
                    SimdOp::I16x8AllTrue => simd_op!(i16x8_all_true),
                    SimdOp::I16x8Bitmask => simd_op!(i16x8_bitmask),
                    SimdOp::I16x8NarrowI32x4S => simd_op!(i16x8_narrow_i32x4_s),
                    SimdOp::I16x8NarrowI32x4U => simd_op!(i16x8_narrow_i32x4_u),
                    SimdOp::I16x8ExtendLowI8x16S => simd_op!(i16x8_extend_low_i8x16_s),
                    SimdOp::I16x8ExtendHighI8x16S => simd_op!(i16x8_extend_high_i8x16_s),
                    SimdOp::I16x8ExtendLowI8x16U => simd_op!(i16x8_extend_low_i8x16_u),
                    SimdOp::I16x8ExtendHighI8x16U => simd_op!(i16x8_extend_high_i8x16_u),
                    SimdOp::I16x8Shl => simd_op!(i16x8_shl),
                    SimdOp::I16x8ShrS => simd_op!(i16x8_shr_s),
                    SimdOp::I16x8ShrU => simd_op!(i16x8_shr_u),
                    SimdOp::I16x8Add => simd_op!(i16x8_add),
                    SimdOp::I16x8AddSatS => simd_op!(i16x8_add_sat_s),
                    SimdOp::I16x8AddSatU => simd_op!(i16x8_add_sat_u),
                    SimdOp::I16x8Sub => simd_op!(i16x8_sub),
                    SimdOp::I16x8SubSatS => simd_op!(i16x8_sub_sat_s),
                    SimdOp::I16x8SubSatU => simd_op!(i16x8_sub_sat_u),
                    SimdOp::I16x8Mul => simd_op!(i16x8_mul),
                    SimdOp::I16x8MinS => simd_op!(i16x8_min_s),
                    SimdOp::I16x8MinU => simd_op!(i16x8_min_u),
                    SimdOp::I16x8MaxS => simd_op!(i16x8_max_s),
                    SimdOp::I16x8MaxU => simd_op!(i16x8_max_u),
                    SimdOp::I16x8AvgrU => simd_op!(i16x8_avgr_u),
                    SimdOp::I16x8ExtMulLowI8x16S => simd_op!(i16x8_extmul_low_i8x16_s),
                    SimdOp::I16x8ExtMulHighI8x16S => simd_op!(i16x8_extmul_high_i8x16_s),
                    SimdOp::I16x8ExtMulLowI8x16U => simd_op!(i16x8_extmul_low_i8x16_u),
                    SimdOp::I16x8ExtMulHighI8x16U => simd_op!(i16x8_extmul_high_i8x16_u),

                    // i32x4 operations
                    SimdOp::I32x4ExtAddPairwiseI16x8S => simd_op!(i32x4_extadd_pairwise_i16x8_s),
                    SimdOp::I32x4ExtAddPairwiseI16x8U => simd_op!(i32x4_extadd_pairwise_i16x8_u),
                    SimdOp::I32x4Abs => simd_op!(i32x4_abs),
                    SimdOp::I32x4Neg => simd_op!(i32x4_neg),
                    SimdOp::I32x4AllTrue => simd_op!(i32x4_all_true),
                    SimdOp::I32x4Bitmask => simd_op!(i32x4_bitmask),
                    SimdOp::I32x4ExtendLowI16x8S => simd_op!(i32x4_extend_low_i16x8_s),
                    SimdOp::I32x4ExtendHighI16x8S => simd_op!(i32x4_extend_high_i16x8_s),
                    SimdOp::I32x4ExtendLowI16x8U => simd_op!(i32x4_extend_low_i16x8_u),
                    SimdOp::I32x4ExtendHighI16x8U => simd_op!(i32x4_extend_high_i16x8_u),
                    SimdOp::I32x4Shl => simd_op!(i32x4_shl),
                    SimdOp::I32x4ShrS => simd_op!(i32x4_shr_s),
                    SimdOp::I32x4ShrU => simd_op!(i32x4_shr_u),
                    SimdOp::I32x4Add => simd_op!(i32x4_add),
                    SimdOp::I32x4Sub => simd_op!(i32x4_sub),
                    SimdOp::I32x4Mul => simd_op!(i32x4_mul),
                    SimdOp::I32x4MinS => simd_op!(i32x4_min_s),
                    SimdOp::I32x4MinU => simd_op!(i32x4_min_u),
                    SimdOp::I32x4MaxS => simd_op!(i32x4_max_s),
                    SimdOp::I32x4MaxU => simd_op!(i32x4_max_u),
                    SimdOp::I32x4DotI16x8S => simd_op!(i32x4_dot_i16x8_s),
                    SimdOp::I32x4ExtMulLowI16x8S => simd_op!(i32x4_extmul_low_i16x8_s),
                    SimdOp::I32x4ExtMulHighI16x8S => simd_op!(i32x4_extmul_high_i16x8_s),
                    SimdOp::I32x4ExtMulLowI16x8U => simd_op!(i32x4_extmul_low_i16x8_u),
                    SimdOp::I32x4ExtMulHighI16x8U => simd_op!(i32x4_extmul_high_i16x8_u),

                    // i64x2 operations
                    SimdOp::I64x2Abs => simd_op!(i64x2_abs),
                    SimdOp::I64x2Neg => simd_op!(i64x2_neg),
                    SimdOp::I64x2AllTrue => simd_op!(i64x2_all_true),
                    SimdOp::I64x2Bitmask => simd_op!(i64x2_bitmask),
                    SimdOp::I64x2ExtendLowI32x4S => simd_op!(i64x2_extend_low_i32x4_s),
                    SimdOp::I64x2ExtendHighI32x4S => simd_op!(i64x2_extend_high_i32x4_s),
                    SimdOp::I64x2ExtendLowI32x4U => simd_op!(i64x2_extend_low_i32x4_u),
                    SimdOp::I64x2ExtendHighI32x4U => simd_op!(i64x2_extend_high_i32x4_u),
                    SimdOp::I64x2Shl => simd_op!(i64x2_shl),
                    SimdOp::I64x2ShrS => simd_op!(i64x2_shr_s),
                    SimdOp::I64x2ShrU => simd_op!(i64x2_shr_u),
                    SimdOp::I64x2Add => simd_op!(i64x2_add),
                    SimdOp::I64x2Sub => simd_op!(i64x2_sub),
                    SimdOp::I64x2Mul => simd_op!(i64x2_mul),
                    SimdOp::I64x2ExtMulLowI32x4S => simd_op!(i64x2_extmul_low_i32x4_s),
                    SimdOp::I64x2ExtMulHighI32x4S => simd_op!(i64x2_extmul_high_i32x4_s),
                    SimdOp::I64x2ExtMulLowI32x4U => simd_op!(i64x2_extmul_low_i32x4_u),
                    SimdOp::I64x2ExtMulHighI32x4U => simd_op!(i64x2_extmul_high_i32x4_u),

                    // f32x4 operations
                    SimdOp::F32x4Abs => simd_op!(f32x4_abs),
                    SimdOp::F32x4Neg => simd_op!(f32x4_neg),
                    SimdOp::F32x4Sqrt => simd_op!(f32x4_sqrt),
                    SimdOp::F32x4Ceil => simd_op!(f32x4_ceil),
                    SimdOp::F32x4Floor => simd_op!(f32x4_floor),
                    SimdOp::F32x4Trunc => simd_op!(f32x4_trunc),
                    SimdOp::F32x4Nearest => simd_op!(f32x4_nearest),
                    SimdOp::F32x4Add => simd_op!(f32x4_add),
                    SimdOp::F32x4Sub => simd_op!(f32x4_sub),
                    SimdOp::F32x4Mul => simd_op!(f32x4_mul),
                    SimdOp::F32x4Div => simd_op!(f32x4_div),
                    SimdOp::F32x4Min => simd_op!(f32x4_min),
                    SimdOp::F32x4Max => simd_op!(f32x4_max),
                    SimdOp::F32x4PMin => simd_op!(f32x4_pmin),
                    SimdOp::F32x4PMax => simd_op!(f32x4_pmax),

                    // f64x2 operations
                    SimdOp::F64x2Abs => simd_op!(f64x2_abs),
                    SimdOp::F64x2Neg => simd_op!(f64x2_neg),
                    SimdOp::F64x2Sqrt => simd_op!(f64x2_sqrt),
                    SimdOp::F64x2Ceil => simd_op!(f64x2_ceil),
                    SimdOp::F64x2Floor => simd_op!(f64x2_floor),
                    SimdOp::F64x2Trunc => simd_op!(f64x2_trunc),
                    SimdOp::F64x2Nearest => simd_op!(f64x2_nearest),
                    SimdOp::F64x2Add => simd_op!(f64x2_add),
                    SimdOp::F64x2Sub => simd_op!(f64x2_sub),
                    SimdOp::F64x2Mul => simd_op!(f64x2_mul),
                    SimdOp::F64x2Div => simd_op!(f64x2_div),
                    SimdOp::F64x2Min => simd_op!(f64x2_min),
                    SimdOp::F64x2Max => simd_op!(f64x2_max),
                    SimdOp::F64x2PMin => simd_op!(f64x2_pmin),
                    SimdOp::F64x2PMax => simd_op!(f64x2_pmax),

                    // Truncation/conversion
                    SimdOp::I32x4TruncSatF32x4S => simd_op!(i32x4_trunc_sat_f32x4_s),
                    SimdOp::I32x4TruncSatF32x4U => simd_op!(i32x4_trunc_sat_f32x4_u),
                    SimdOp::F32x4ConvertI32x4S => simd_op!(f32x4_convert_i32x4_s),
                    SimdOp::F32x4ConvertI32x4U => simd_op!(f32x4_convert_i32x4_u),
                    SimdOp::I32x4TruncSatF64x2SZero => simd_op!(i32x4_trunc_sat_f64x2_s_zero),
                    SimdOp::I32x4TruncSatF64x2UZero => simd_op!(i32x4_trunc_sat_f64x2_u_zero),
                    SimdOp::F64x2ConvertLowI32x4S => simd_op!(f64x2_convert_low_i32x4_s),
                    SimdOp::F64x2ConvertLowI32x4U => simd_op!(f64x2_convert_low_i32x4_u),
                    SimdOp::F32x4DemoteF64x2Zero => simd_op!(f32x4_demote_f64x2_zero),
                    SimdOp::F64x2PromoteLowF32x4 => simd_op!(f64x2_promote_low_f32x4),
                }
            }

            // ----------------------------------------------------------------
            // Unimplemented instructions
            kind => Err(RuntimeError::UnimplementedInstruction(kind.mnemonic().to_string())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::instruction::{BlockType, InstructionKind};
    use crate::parser::module::{Module, ValueType};
    use crate::parser::structure_builder::StructureBuilder;
    use crate::runtime::Value;
    use crate::runtime::test_utils::test::{ExecutorTest, make_instruction};

    // ============================================================================
    // Simple Control Instruction Tests
    // ============================================================================
    mod control {
        use super::*;

        #[test]
        fn nop_single() {
            ExecutorTest::new()
                .inst(InstructionKind::Nop)
                .inst(InstructionKind::I32Const { value: 42 })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn nop_multiple() {
            ExecutorTest::new()
                .inst(InstructionKind::Nop)
                .inst(InstructionKind::Nop)
                .inst(InstructionKind::Nop)
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::Nop)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn nop_with_args() {
            ExecutorTest::new()
                .args(vec![Value::I32(42)])
                .inst(InstructionKind::Nop)
                .inst(InstructionKind::LocalGet { local_idx: 0 })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn end_implicit() {
            // End is added automatically by the test framework
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 42 })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }
    }

    // ============================================================================
    // Function Argument Tests
    // ============================================================================
    mod function_arguments {
        use super::*;

        #[test]
        fn single_arg_passthrough() {
            // Arguments are stored as locals
            ExecutorTest::new()
                .args(vec![Value::I32(42)])
                .inst(InstructionKind::LocalGet { local_idx: 0 })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn multiple_args_passthrough() {
            // Arguments are stored as locals in order
            ExecutorTest::new()
                .args(vec![Value::I32(1), Value::I64(2), Value::F32(3.0)])
                .inst(InstructionKind::LocalGet { local_idx: 0 })
                .inst(InstructionKind::LocalGet { local_idx: 1 })
                .inst(InstructionKind::LocalGet { local_idx: 2 })
                .returns(vec![ValueType::I32, ValueType::I64, ValueType::F32])
                .expect_stack(vec![Value::I32(1), Value::I64(2), Value::F32(3.0)]);
        }

        #[test]
        fn args_with_operations() {
            ExecutorTest::new()
                .args(vec![Value::I32(42)])
                .inst(InstructionKind::LocalGet { local_idx: 0 })
                .inst(InstructionKind::I32Const { value: 100 })
                .returns(vec![ValueType::I32, ValueType::I32])
                .expect_stack(vec![Value::I32(42), Value::I32(100)]);
        }
    }

    // ============================================================================
    // Error Handling Tests
    // ============================================================================
    mod error_handling {
        use super::*;

        // Call instruction tests moved to function_calls module below

        #[test]
        fn return_type_mismatch() {
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 42 })
                .returns(vec![ValueType::I64])
                .expect_error("type mismatch");
        }

        #[test]
        fn missing_return_value() {
            ExecutorTest::new()
                .returns(vec![ValueType::I32])
                .expect_error("stack underflow");
        }

        #[test]
        fn too_few_return_values() {
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 42 })
                .returns(vec![ValueType::I32, ValueType::I32])
                .expect_error("stack underflow");
        }
    }

    // ============================================================================
    // Structured Execution Tests
    // ============================================================================
    mod structured_execution {
        use super::*;

        /// Helper to extract results from ExecutionOutcome for tests
        fn unwrap_complete(outcome: super::super::ExecutionOutcome) -> Vec<Value> {
            match outcome {
                super::super::ExecutionOutcome::Complete(results) => results,
                super::super::ExecutionOutcome::NeedsExternalCall(_) => {
                    panic!("Test unexpectedly needs external call");
                }
            }
        }

        #[test]
        fn test_structured_block() {
            // Test a simple block using structured execution
            let instructions = vec![
                make_instruction(InstructionKind::Block {
                    block_type: BlockType::Empty,
                }),
                make_instruction(InstructionKind::I32Const { value: 42 }),
                make_instruction(InstructionKind::End),
            ];

            // Build structured representation
            let func = StructureBuilder::build_function(&instructions, 0, vec![ValueType::I32])
                .expect("Failed to build structured function");

            // Execute using structured executor
            let module = Module::new("test");
            let mut executor = Executor::new(&module).expect("Executor creation should succeed");
            let outcome = executor
                .execute_function(&func, vec![], &[ValueType::I32])
                .expect("Execution should succeed");

            assert_eq!(unwrap_complete(outcome), vec![Value::I32(42)]);
        }

        #[test]
        fn test_structured_if_then_else() {
            // Test if-then-else using structured execution
            let instructions = vec![
                make_instruction(InstructionKind::I32Const { value: 1 }), // condition
                make_instruction(InstructionKind::If {
                    block_type: BlockType::Value(ValueType::I32),
                }),
                make_instruction(InstructionKind::I32Const { value: 10 }), // then branch
                make_instruction(InstructionKind::Else),
                make_instruction(InstructionKind::I32Const { value: 20 }), // else branch
                make_instruction(InstructionKind::End),
            ];

            // Build structured representation
            let func = StructureBuilder::build_function(&instructions, 0, vec![ValueType::I32])
                .expect("Failed to build structured function");

            // Execute using structured executor
            let module = Module::new("test");
            let mut executor = Executor::new(&module).expect("Executor creation should succeed");
            let outcome = executor
                .execute_function(&func, vec![], &[ValueType::I32])
                .expect("Execution should succeed");

            assert_eq!(unwrap_complete(outcome), vec![Value::I32(10)]); // Should take then branch
        }

        #[test]
        fn test_structured_loop_with_branch() {
            // Test loop with conditional branch using structured execution
            let instructions = vec![
                make_instruction(InstructionKind::I32Const { value: 0 }), // counter
                make_instruction(InstructionKind::Loop {
                    block_type: BlockType::Value(ValueType::I32),
                }),
                make_instruction(InstructionKind::I32Const { value: 1 }), // increment
                make_instruction(InstructionKind::I32Add),
                make_instruction(InstructionKind::LocalTee { local_idx: 0 }), // save counter
                make_instruction(InstructionKind::I32Const { value: 3 }),
                make_instruction(InstructionKind::I32LtS), // counter < 3?
                make_instruction(InstructionKind::BrIf { label_idx: 0 }), // branch to loop start if true
                make_instruction(InstructionKind::End),
            ];

            // Note: This test would need locals support to work properly
            // For now, just verify it builds successfully
            let func = StructureBuilder::build_function(&instructions, 1, vec![ValueType::I32])
                .expect("Failed to build structured function");

            // Verify structure
            assert_eq!(func.body.len(), 2); // const + loop
        }
    }

    // ============================================================================
    // Function Call Tests
    // ============================================================================
    mod function_calls {
        use super::*;
        use crate::parser::module::{
            Export, ExportIndex, ExternalKind, Function, FunctionBody, Import, Locals, SectionPosition,
        };
        use crate::runtime::{FunctionInstance, ImportObject, Store};

        #[test]
        fn test_call_with_imports() {
            // Test that function calls work correctly when there are imported functions
            // This tests the fix for function index mapping

            // Create a module with:
            // - Import: function 0 (imported, no body)
            // - Function 1: helper that returns 42
            // - Function 2: main that calls function 1

            let mut module = Module::new("test");

            // Type section: define function signatures
            module.types.types.push(FunctionType {
                parameters: vec![ValueType::I32],
                return_types: vec![],
            }); // Type 0: imported function
            module.types.types.push(FunctionType {
                parameters: vec![],
                return_types: vec![ValueType::I32],
            }); // Type 1: helper and main functions

            // Import section: import one function
            module.imports.imports.push(Import {
                module: "env".to_string(),
                name: "log".to_string(),
                external_kind: ExternalKind::Function(0), // Uses type 0
            });

            // Function section: declare our defined functions
            module.functions.functions.push(Function {
                ftype_index: 1, // helper uses type 1
            });
            module.functions.functions.push(Function {
                ftype_index: 1, // main uses type 1
            });

            // Code section: bodies for our defined functions
            // Helper function (function index 1, code index 0)
            let helper_instructions = vec![make_instruction(InstructionKind::I32Const { value: 42 })];
            let helper_body = StructureBuilder::build_function(&helper_instructions, 0, vec![ValueType::I32])
                .expect("Failed to build helper function");
            module.code.code.push(FunctionBody {
                locals: Locals::empty(),
                body: helper_body,
                position: SectionPosition::new(0, 0),
            });

            // Main function (function index 2, code index 1)
            let main_instructions = vec![
                make_instruction(InstructionKind::Call { func_idx: 1 }), // Call helper
            ];
            let main_body = StructureBuilder::build_function(&main_instructions, 0, vec![ValueType::I32])
                .expect("Failed to build main function");
            module.code.code.push(FunctionBody {
                locals: Locals::empty(),
                body: main_body,
                position: SectionPosition::new(0, 0),
            });

            // Export main function
            module.exports.exports.push(Export {
                name: "main".to_string(),
                index: ExportIndex::Function(2), // Function index 2
            });

            // Create store and import object with the required import
            let mut store = Store::new();
            let mut imports = ImportObject::new();

            // Add the imported env.log function as a host function
            let log_addr = store.allocate_function(FunctionInstance::Host {
                func: Box::new(|_args| Ok(vec![])), // No-op log function
                func_type: module.types.types[0].clone(),
            });
            imports.add_function("env", "log", log_addr);

            let instance_id = store
                .create_instance(&module, Some(&imports))
                .expect("Instance creation should succeed");
            let result = store
                .invoke_export(instance_id, "main", vec![], None)
                .expect("Function call should succeed");

            assert_eq!(result, vec![Value::I32(42)], "Main should call helper and return 42");
        }

        #[test]
        fn test_recursive_factorial() {
            // Test recursive function calls
            let mut module = Module::new("test");

            // Type: (i32) -> i32
            module.types.types.push(FunctionType {
                parameters: vec![ValueType::I32],
                return_types: vec![ValueType::I32],
            });

            // Single function that calls itself
            module.functions.functions.push(Function { ftype_index: 0 });

            // Factorial function: if n <= 1 return 1, else return n * factorial(n-1)
            let instructions = vec![
                make_instruction(InstructionKind::LocalGet { local_idx: 0 }), // n
                make_instruction(InstructionKind::I32Const { value: 1 }),
                make_instruction(InstructionKind::I32LeS), // n <= 1?
                make_instruction(InstructionKind::If {
                    block_type: BlockType::Value(ValueType::I32),
                }),
                make_instruction(InstructionKind::I32Const { value: 1 }), // then: return 1
                make_instruction(InstructionKind::Else),
                make_instruction(InstructionKind::LocalGet { local_idx: 0 }), // else: n
                make_instruction(InstructionKind::LocalGet { local_idx: 0 }), // n
                make_instruction(InstructionKind::I32Const { value: 1 }),
                make_instruction(InstructionKind::I32Sub), // n - 1
                make_instruction(InstructionKind::Call { func_idx: 0 }), // factorial(n-1)
                make_instruction(InstructionKind::I32Mul), // n * factorial(n-1)
                make_instruction(InstructionKind::End),
            ];

            let body = StructureBuilder::build_function(&instructions, 1, vec![ValueType::I32])
                .expect("Failed to build factorial function");
            module.code.code.push(FunctionBody {
                locals: Locals::empty(),
                body,
                position: SectionPosition::new(0, 0),
            });

            module.exports.exports.push(Export {
                name: "factorial".to_string(),
                index: ExportIndex::Function(0),
            });

            // Create store and instance
            let mut store = Store::new();
            let instance_id = store
                .create_instance(&module, None)
                .expect("Instance creation should succeed");

            // Test factorial(5) = 120
            let result = store
                .invoke_export(instance_id, "factorial", vec![Value::I32(5)], None)
                .expect("Factorial should succeed");
            assert_eq!(result, vec![Value::I32(120)]);

            // Test factorial(0) = 1
            let result = store
                .invoke_export(instance_id, "factorial", vec![Value::I32(0)], None)
                .expect("Factorial should succeed");
            assert_eq!(result, vec![Value::I32(1)]);
        }

        #[test]
        fn test_return_from_nested_block_preserves_caller_locals() {
            // This tests the fix for a bug where returning from within a nested block
            // did not properly pop the call frame, causing the caller's locals to be
            // corrupted by the callee's execution context.
            //
            // Scenario:
            // - Caller: stores 100 in local 0, calls helper, returns local 0
            // - Helper: has a block, returns 42 from within the block
            // - Expected: caller should return 100 (its local 0, not corrupted)

            let mut module = Module::new("test");

            // Type section: helper returns i32, caller returns i32
            module.types.types.push(FunctionType {
                parameters: vec![],
                return_types: vec![ValueType::I32],
            }); // Type 0: () -> i32

            // Function section
            module.functions.functions.push(Function {
                ftype_index: 0, // helper
            });
            module.functions.functions.push(Function {
                ftype_index: 0, // caller
            });

            // Helper function (func 0): block { return 42 }
            // This tests that returning from within a block properly cleans up
            let helper_instructions = vec![
                make_instruction(InstructionKind::Block {
                    block_type: BlockType::Empty,
                }),
                make_instruction(InstructionKind::Block {
                    block_type: BlockType::Empty,
                }),
                make_instruction(InstructionKind::I32Const { value: 42 }),
                make_instruction(InstructionKind::Return),
                make_instruction(InstructionKind::End),
                make_instruction(InstructionKind::End),
                make_instruction(InstructionKind::I32Const { value: 0 }), // unreachable
            ];
            let helper_body = StructureBuilder::build_function(&helper_instructions, 0, vec![ValueType::I32])
                .expect("Failed to build helper function");
            module.code.code.push(FunctionBody {
                locals: Locals::empty(),
                body: helper_body,
                position: SectionPosition::new(0, 0),
            });

            // Caller function (func 1): local 0 = 100; call helper; return local 0
            // The caller has a local variable. After calling helper, local 0 should
            // still be 100, not corrupted by helper's execution.
            let caller_instructions = vec![
                make_instruction(InstructionKind::I32Const { value: 100 }),
                make_instruction(InstructionKind::LocalSet { local_idx: 0 }),
                make_instruction(InstructionKind::Call { func_idx: 0 }), // call helper
                make_instruction(InstructionKind::Drop),                 // drop helper's return value
                make_instruction(InstructionKind::LocalGet { local_idx: 0 }), // return our local
            ];
            let caller_body = StructureBuilder::build_function(&caller_instructions, 1, vec![ValueType::I32])
                .expect("Failed to build caller function");
            module.code.code.push(FunctionBody {
                locals: Locals::new(vec![(1, ValueType::I32)]), // one i32 local
                body: caller_body,
                position: SectionPosition::new(0, 0),
            });

            module.exports.exports.push(Export {
                name: "main".to_string(),
                index: ExportIndex::Function(1), // export caller
            });

            // Create store and instance
            let mut store = Store::new();
            let instance_id = store
                .create_instance(&module, None)
                .expect("Instance creation should succeed");

            // Call main - should return 100 (caller's local 0)
            // Before the fix, this would fail because the return from within helper's
            // nested blocks didn't pop the call frame, causing execution to continue
            // with corrupted state.
            let result = store
                .invoke_export(instance_id, "main", vec![], None)
                .expect("Main should succeed");
            assert_eq!(
                result,
                vec![Value::I32(100)],
                "Caller's local should be preserved after callee returns from nested block"
            );
        }
    }

    // ============================================================================
    // Variable Tests (Local and Global)
    // ============================================================================
    mod variables {
        use super::*;

        // ============================================================================
        // Local Variable Tests
        // ============================================================================

        #[test]
        fn local_get_first_arg() {
            // First function argument is local 0
            ExecutorTest::new()
                .args(vec![Value::I32(42)])
                .inst(InstructionKind::LocalGet { local_idx: 0 })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn local_get_multiple_args() {
            // Multiple arguments become locals 0, 1, 2, etc.
            ExecutorTest::new()
                .args(vec![Value::I32(10), Value::I64(20), Value::F32(30.0)])
                .inst(InstructionKind::LocalGet { local_idx: 1 })
                .inst(InstructionKind::LocalGet { local_idx: 0 })
                .inst(InstructionKind::LocalGet { local_idx: 2 })
                .returns(vec![ValueType::I64, ValueType::I32, ValueType::F32])
                .expect_stack(vec![Value::I64(20), Value::I32(10), Value::F32(30.0)]);
        }

        #[test]
        fn local_get_same_local_twice() {
            // Getting the same local multiple times
            ExecutorTest::new()
                .args(vec![Value::I32(42)])
                .inst(InstructionKind::LocalGet { local_idx: 0 })
                .inst(InstructionKind::LocalGet { local_idx: 0 })
                .returns(vec![ValueType::I32, ValueType::I32])
                .expect_stack(vec![Value::I32(42), Value::I32(42)]);
        }

        #[test]
        fn local_get_out_of_bounds() {
            ExecutorTest::new()
                .args(vec![Value::I32(42)])
                .inst(InstructionKind::LocalGet { local_idx: 1 })
                .expect_error("local variable index out of bounds: 1");
        }

        #[test]
        fn local_get_with_drop() {
            // Test interaction with drop
            ExecutorTest::new()
                .args(vec![Value::I32(42), Value::I64(84)])
                .inst(InstructionKind::LocalGet { local_idx: 0 })
                .inst(InstructionKind::LocalGet { local_idx: 1 })
                .inst(InstructionKind::Drop)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn local_set_basic() {
            // Set a local and read it back
            ExecutorTest::new()
                .args(vec![Value::I32(0)])
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::LocalSet { local_idx: 0 })
                .inst(InstructionKind::LocalGet { local_idx: 0 })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn local_set_multiple() {
            // Set multiple locals
            ExecutorTest::new()
                .args(vec![Value::I32(1), Value::I64(2), Value::F32(3.0)])
                .inst(InstructionKind::I32Const { value: 10 })
                .inst(InstructionKind::LocalSet { local_idx: 0 })
                .inst(InstructionKind::I64Const { value: 20 })
                .inst(InstructionKind::LocalSet { local_idx: 1 })
                .inst(InstructionKind::F32Const { value: 30.0 })
                .inst(InstructionKind::LocalSet { local_idx: 2 })
                .inst(InstructionKind::LocalGet { local_idx: 0 })
                .inst(InstructionKind::LocalGet { local_idx: 1 })
                .inst(InstructionKind::LocalGet { local_idx: 2 })
                .returns(vec![ValueType::I32, ValueType::I64, ValueType::F32])
                .expect_stack(vec![Value::I32(10), Value::I64(20), Value::F32(30.0)]);
        }

        #[test]
        fn local_set_out_of_bounds() {
            ExecutorTest::new()
                .args(vec![Value::I32(42)])
                .inst(InstructionKind::I32Const { value: 100 })
                .inst(InstructionKind::LocalSet { local_idx: 1 })
                .expect_error("local variable index out of bounds: 1");
        }

        #[test]
        fn local_set_empty_stack() {
            ExecutorTest::new()
                .args(vec![Value::I32(42)])
                .inst(InstructionKind::LocalSet { local_idx: 0 })
                .expect_error("stack underflow");
        }

        #[test]
        fn local_set_get_sequence() {
            // Complex sequence of sets and gets
            ExecutorTest::new()
                .args(vec![Value::I32(1), Value::I32(2)])
                .inst(InstructionKind::LocalGet { local_idx: 0 }) // stack: [1]
                .inst(InstructionKind::LocalGet { local_idx: 1 }) // stack: [1, 2]
                .inst(InstructionKind::LocalSet { local_idx: 0 }) // stack: [1], local[0] = 2
                .inst(InstructionKind::LocalSet { local_idx: 1 }) // stack: [], local[1] = 1
                .inst(InstructionKind::LocalGet { local_idx: 0 }) // stack: [2]
                .inst(InstructionKind::LocalGet { local_idx: 1 }) // stack: [2, 1]
                .returns(vec![ValueType::I32, ValueType::I32])
                .expect_stack(vec![Value::I32(2), Value::I32(1)]);
        }

        #[test]
        fn local_tee_basic() {
            // Tee sets local but leaves value on stack
            ExecutorTest::new()
                .args(vec![Value::I32(0)])
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::LocalTee { local_idx: 0 })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn local_tee_verify_stored() {
            // Verify that tee actually stores the value
            ExecutorTest::new()
                .args(vec![Value::I32(0)])
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::LocalTee { local_idx: 0 })
                .inst(InstructionKind::Drop)
                .inst(InstructionKind::LocalGet { local_idx: 0 })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn local_tee_multiple() {
            // Use tee with multiple locals
            ExecutorTest::new()
                .args(vec![Value::I32(1), Value::I64(2), Value::F32(3.0)])
                .inst(InstructionKind::I32Const { value: 10 })
                .inst(InstructionKind::LocalTee { local_idx: 0 })
                .inst(InstructionKind::I64Const { value: 20 })
                .inst(InstructionKind::LocalTee { local_idx: 1 })
                .inst(InstructionKind::F32Const { value: 30.0 })
                .inst(InstructionKind::LocalTee { local_idx: 2 })
                .returns(vec![ValueType::I32, ValueType::I64, ValueType::F32])
                .expect_stack(vec![Value::I32(10), Value::I64(20), Value::F32(30.0)]);
        }

        #[test]
        fn local_tee_out_of_bounds() {
            ExecutorTest::new()
                .args(vec![Value::I32(42)])
                .inst(InstructionKind::I32Const { value: 100 })
                .inst(InstructionKind::LocalTee { local_idx: 1 })
                .expect_error("local variable index out of bounds: 1");
        }

        #[test]
        fn local_tee_empty_stack() {
            ExecutorTest::new()
                .args(vec![Value::I32(42)])
                .inst(InstructionKind::LocalTee { local_idx: 0 })
                .expect_error("stack underflow");
        }

        #[test]
        fn local_tee_chain() {
            // Chain multiple tees
            ExecutorTest::new()
                .args(vec![Value::I32(0), Value::I32(0)])
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::LocalTee { local_idx: 0 })
                .inst(InstructionKind::LocalTee { local_idx: 1 })
                .inst(InstructionKind::Drop)
                .inst(InstructionKind::LocalGet { local_idx: 0 })
                .inst(InstructionKind::LocalGet { local_idx: 1 })
                .returns(vec![ValueType::I32, ValueType::I32])
                .expect_stack(vec![Value::I32(42), Value::I32(42)]);
        }

        // ============================================================================
        // Global Variable Tests
        // ============================================================================

        #[test]
        fn global_get_basic() {
            // Test basic global.get with actual global
            ExecutorTest::new()
                .global(ValueType::I32, Value::I32(42), false)
                .inst(InstructionKind::GlobalGet { global_idx: 0 })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn global_get_multiple() {
            // Test multiple globals with different types
            ExecutorTest::new()
                .global(ValueType::I32, Value::I32(10), false)
                .global(ValueType::F64, Value::F64(3.14), false)
                .global(ValueType::I64, Value::I64(100), false)
                .inst(InstructionKind::GlobalGet { global_idx: 1 })
                .inst(InstructionKind::GlobalGet { global_idx: 0 })
                .inst(InstructionKind::GlobalGet { global_idx: 2 })
                .returns(vec![ValueType::F64, ValueType::I32, ValueType::I64])
                .expect_stack(vec![Value::F64(3.14), Value::I32(10), Value::I64(100)]);
        }

        #[test]
        fn global_get_out_of_bounds() {
            ExecutorTest::new()
                .global(ValueType::I32, Value::I32(42), false)
                .inst(InstructionKind::GlobalGet { global_idx: 1 })
                .expect_error("global variable index out of bounds: 1");
        }

        #[test]
        fn global_set_basic() {
            // Test basic global.set with mutable global
            ExecutorTest::new()
                .global(ValueType::I32, Value::I32(0), true) // mutable
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::GlobalSet { global_idx: 0 })
                .inst(InstructionKind::GlobalGet { global_idx: 0 })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn global_set_multiple_types() {
            // Test setting different value types
            ExecutorTest::new()
                .global(ValueType::I32, Value::I32(0), true)
                .global(ValueType::F32, Value::F32(0.0), true)
                .global(ValueType::I64, Value::I64(0), true)
                .inst(InstructionKind::I32Const { value: 100 })
                .inst(InstructionKind::GlobalSet { global_idx: 0 })
                .inst(InstructionKind::F32Const { value: 2.5 })
                .inst(InstructionKind::GlobalSet { global_idx: 1 })
                .inst(InstructionKind::I64Const { value: 999 })
                .inst(InstructionKind::GlobalSet { global_idx: 2 })
                .inst(InstructionKind::GlobalGet { global_idx: 0 })
                .inst(InstructionKind::GlobalGet { global_idx: 1 })
                .inst(InstructionKind::GlobalGet { global_idx: 2 })
                .returns(vec![ValueType::I32, ValueType::F32, ValueType::I64])
                .expect_stack(vec![Value::I32(100), Value::F32(2.5), Value::I64(999)]);
        }

        #[test]
        fn global_set_out_of_bounds() {
            ExecutorTest::new()
                .global(ValueType::I32, Value::I32(42), true)
                .inst(InstructionKind::I32Const { value: 100 })
                .inst(InstructionKind::GlobalSet { global_idx: 1 })
                .expect_error("global variable index out of bounds: 1");
        }

        #[test]
        fn global_set_empty_stack() {
            ExecutorTest::new()
                .global(ValueType::I32, Value::I32(42), true)
                .inst(InstructionKind::GlobalSet { global_idx: 0 })
                .expect_error("stack underflow");
        }

        #[test]
        fn global_set_immutable() {
            // Test that setting immutable global fails
            ExecutorTest::new()
                .global(ValueType::I32, Value::I32(42), false) // immutable
                .inst(InstructionKind::I32Const { value: 100 })
                .inst(InstructionKind::GlobalSet { global_idx: 0 })
                .expect_error("cannot set immutable global");
        }

        #[test]
        fn global_mixed_mutability() {
            // Test mix of mutable and immutable globals
            ExecutorTest::new()
                .global(ValueType::I32, Value::I32(10), false) // immutable
                .global(ValueType::I32, Value::I32(20), true) // mutable
                .inst(InstructionKind::I32Const { value: 100 })
                .inst(InstructionKind::GlobalSet { global_idx: 1 }) // Should work (mutable)
                .inst(InstructionKind::GlobalGet { global_idx: 0 }) // Should get immutable value
                .inst(InstructionKind::GlobalGet { global_idx: 1 }) // Should get new value
                .returns(vec![ValueType::I32, ValueType::I32])
                .expect_stack(vec![Value::I32(10), Value::I32(100)]);
        }
    }
}
