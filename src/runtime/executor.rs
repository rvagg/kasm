//! WebAssembly instruction executor

use super::{
    control::{Label, LabelStack, LabelType},
    frame::CallFrame,
    memory::Memory,
    ops,
    stack::Stack,
    RuntimeError, Value,
};
use crate::parser::instruction::{BlockType, Instruction, InstructionKind};
use crate::parser::module::{DataMode, ExternalKind, FunctionType, Locals, Module, ValueType};
use crate::parser::structured::{BlockEnd, StructuredFunction, StructuredInstruction};

/// Maximum call stack depth to prevent stack overflow
const MAX_CALL_DEPTH: usize = 1000;

/// Executes WebAssembly instructions
pub struct Executor<'a> {
    module: &'a Module,
    stack: Stack,
    /// Call stack for managing function calls (always has at least one frame during execution)
    call_stack: Vec<CallFrame>,
    /// Memory instances (WebAssembly 1.0 supports only 1)
    memories: Vec<Memory>,
    /// Global variable values
    globals: Vec<Value>,
    /// Function bodies for quick access
    functions: Vec<Option<StructuredFunction>>,
    /// Function types for quick access
    function_types: Vec<FunctionType>,
}

impl<'a> Executor<'a> {
    /// Create a new executor for a module
    ///
    /// # Errors
    /// - If the module has more than one memory (WebAssembly 1.0 limitation)
    /// - If memory initialisation fails
    pub fn new(module: &'a Module) -> Result<Self, RuntimeError> {
        // Initialise memories from module definition
        let memories = if module.memory.memory.is_empty() {
            vec![]
        } else {
            // WebAssembly 1.0: Only one memory allowed per module
            // This restriction is relaxed in the multi-memory proposal, but we don't support that yet
            if module.memory.memory.len() > 1 {
                return Err(RuntimeError::MemoryError(format!(
                    "WebAssembly 1.0 only supports one memory per module, found {}",
                    module.memory.memory.len()
                )));
            }

            let mem_def = &module.memory.memory[0];
            match Memory::new(mem_def.limits.min, mem_def.limits.max) {
                Ok(memory) => vec![memory],
                Err(e) => return Err(e),
            }
        };

        // Initialise globals from module

        let mut globals = Vec::new();

        // Reserve space for imported globals with default values
        // In a real implementation, these would be provided by the import object
        for import in &module.imports.imports {
            if let ExternalKind::Global(global_type) = &import.external_kind {
                let initial_value = match global_type.value_type {
                    ValueType::I32 => Value::I32(0),
                    ValueType::I64 => Value::I64(0),
                    ValueType::F32 => Value::F32(0.0),
                    ValueType::F64 => Value::F64(0.0),
                    ValueType::FuncRef => Value::FuncRef(None),
                    ValueType::ExternRef => Value::ExternRef(None),
                    ValueType::V128 => {
                        return Err(RuntimeError::UnimplementedInstruction(
                            "V128 type not yet supported".to_string(),
                        ));
                    }
                };
                globals.push(initial_value);
            }
        }

        // We can't evaluate init expressions yet because the executor isn't created
        // Store the module's globals info for later initialization
        let module_globals = module.globals.globals.clone();

        // Build function registry - collect all function bodies
        // First, count imported functions (they come first in the index space)
        let num_imported_functions = module
            .imports
            .imports
            .iter()
            .filter(|import| matches!(import.external_kind, ExternalKind::Function(_)))
            .count();

        let mut functions = Vec::new();

        // Imported functions have no bodies
        for _ in 0..num_imported_functions {
            functions.push(None);
        }

        // Defined functions have bodies in the code section
        for body in &module.code.code {
            functions.push(Some(body.body.clone()));
        }

        // Collect function types for quick access
        let function_types = module.types.types.clone();

        // Create executor instance
        let mut executor = Executor {
            module,
            stack: Stack::new(),
            call_stack: Vec::new(),
            memories,
            globals,
            functions,
            function_types,
        };

        // Now initialise module's own globals with their init expressions
        for global in &module_globals {
            let initial_value = if global.init.is_empty() {
                // No init expression, use default
                match global.global_type.value_type {
                    ValueType::I32 => Value::I32(0),
                    ValueType::I64 => Value::I64(0),
                    ValueType::F32 => Value::F32(0.0),
                    ValueType::F64 => Value::F64(0.0),
                    ValueType::FuncRef => Value::FuncRef(None),
                    ValueType::ExternRef => Value::ExternRef(None),
                    ValueType::V128 => {
                        return Err(RuntimeError::UnimplementedInstruction(
                            "V128 type not yet supported".to_string(),
                        ));
                    }
                }
            } else {
                // Evaluate the init expression
                executor.evaluate_const_expr(&global.init)?
            };
            executor.globals.push(initial_value);
        }

        // Initialise memory with data sections
        executor.initialise_data_sections()?;

        Ok(executor)
    }

    /// Initialise memory with data from data sections
    fn initialise_data_sections(&mut self) -> Result<(), RuntimeError> {
        // Process each data segment
        for data_segment in &self.module.data.data {
            match &data_segment.mode {
                DataMode::Active { memory_index, offset } => {
                    // Check memory index is valid (WebAssembly 1.0 only supports one memory)
                    if *memory_index != 0 {
                        return Err(RuntimeError::MemoryError(format!(
                            "Invalid memory index {} in data segment",
                            memory_index
                        )));
                    }

                    // Check we have a memory
                    if self.memories.is_empty() {
                        return Err(RuntimeError::MemoryError(
                            "Data segment requires memory but none exists".to_string(),
                        ));
                    }

                    // Evaluate the offset expression to get the starting address
                    // The offset expression should be a constant expression
                    let offset_value = self.evaluate_const_expr(offset)?;

                    // Extract the offset as u32
                    let offset_addr = match offset_value {
                        Value::I32(v) => v as u32,
                        _ => {
                            return Err(RuntimeError::MemoryError(
                                "Data segment offset must be an i32".to_string(),
                            ))
                        }
                    };

                    // Write the data to memory
                    let memory = &mut self.memories[0];
                    let data = &data_segment.init;

                    // Check if the data fits in memory
                    let end_addr = offset_addr as usize + data.len();
                    let memory_size_bytes = (memory.size() as usize) * 65536; // Convert pages to bytes
                    if end_addr > memory_size_bytes {
                        return Err(RuntimeError::MemoryError(format!(
                            "Data segment at offset {} with length {} exceeds memory size {} bytes",
                            offset_addr,
                            data.len(),
                            memory_size_bytes
                        )));
                    }

                    // Copy the data into memory using the shared helper
                    ops::memory::copy_to_memory(memory, offset_addr, data)?;
                }
                DataMode::Passive => {
                    // Passive data segments are not automatically initialised
                    // They're used with memory.init instruction
                    // For now, we skip them
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
            return Err(RuntimeError::InvalidConstExpr("Empty constant expression".to_string()));
        }

        // Check that the last instruction is End
        match instructions.last() {
            Some(inst) if matches!(inst.kind, InstructionKind::End) => {}
            _ => {
                return Err(RuntimeError::InvalidConstExpr(
                    "Constant expression must end with End instruction".to_string(),
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
                    // Get the global value
                    if (*global_idx as usize) >= self.globals.len() {
                        return Err(RuntimeError::GlobalIndexOutOfBounds(*global_idx));
                    }
                    Ok(self.globals[*global_idx as usize].clone())
                }
                InstructionKind::RefNull { ref_type } => match ref_type {
                    ValueType::FuncRef => Ok(Value::FuncRef(None)),
                    ValueType::ExternRef => Ok(Value::ExternRef(None)),
                    _ => Err(RuntimeError::InvalidConstExpr(format!(
                        "Invalid reference type for ref.null: {:?}",
                        ref_type
                    ))),
                },
                _ => Err(RuntimeError::InvalidConstExpr(format!(
                    "Unsupported instruction in constant expression: {:?}",
                    instructions[0].kind
                ))),
            }
        } else if instructions.len() == 1 && matches!(instructions[0].kind, InstructionKind::End) {
            // Just an End instruction - this shouldn't happen in valid WebAssembly
            Err(RuntimeError::InvalidConstExpr(
                "Constant expression cannot be just End".to_string(),
            ))
        } else {
            // TODO: Support more complex constant expressions (e.g., i32.add with two consts)
            Err(RuntimeError::InvalidConstExpr(format!(
                "Unsupported constant expression with {} instructions",
                instructions.len()
            )))
        }
    }

    /// Push a new call frame for a function call
    fn push_call_frame(&mut self, func_idx: u32, args: Vec<Value>, return_arity: usize) -> Result<(), RuntimeError> {
        // Check call stack depth
        if self.call_stack.len() >= MAX_CALL_DEPTH {
            return Err(RuntimeError::Trap("call stack exhausted".to_string()));
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
            .get(func_decl_idx)
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
            .get(func_decl_idx)
            .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))?;

        // Initialise locals: parameters first, then local declarations
        let mut locals = args;
        for (count, local_type) in body.locals.iter() {
            for _ in 0..*count {
                // Initialise locals to zero
                let zero_value = match local_type {
                    ValueType::I32 => Value::I32(0),
                    ValueType::I64 => Value::I64(0),
                    ValueType::F32 => Value::F32(0.0),
                    ValueType::F64 => Value::F64(0.0),
                    _ => {
                        return Err(RuntimeError::UnimplementedInstruction(format!(
                            "Local type {local_type:?} not supported"
                        )))
                    }
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

    /// Pop the current call frame and return to the caller
    fn pop_call_frame(&mut self) -> Option<CallFrame> {
        self.call_stack.pop()
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

    /// Handle a function call instruction
    fn handle_call(&mut self, func_idx: u32) -> Result<(), RuntimeError> {
        // Calculate the number of imported functions
        let num_imported_functions = self.module.imports.function_count();

        // Check if this is an imported function (which we can't execute)
        if (func_idx as usize) < num_imported_functions {
            return Err(RuntimeError::UnimplementedInstruction(format!(
                "Cannot execute imported function at index {func_idx}"
            )));
        }

        // Get the function declaration from the functions section
        // Note: functions section only contains non-imported functions,
        // so we need to subtract the number of imports
        let func_decl_idx = func_idx as usize - num_imported_functions;
        let func = self
            .module
            .functions
            .get(func_decl_idx)
            .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))?;

        let func_type = self
            .function_types
            .get(func.ftype_index as usize)
            .ok_or(RuntimeError::InvalidFunctionType)?
            .clone();

        // Pop arguments from the operand stack (in reverse order)
        let mut args = Vec::new();
        for param_type in func_type.parameters.iter().rev() {
            let value = self.stack.pop_typed(*param_type)?;
            args.push(value);
        }
        args.reverse();

        // Push new call frame
        self.push_call_frame(func_idx, args, func_type.return_types.len())?;

        // Get the function body
        let function_opt = self
            .functions
            .get(func_idx as usize)
            .ok_or(RuntimeError::FunctionIndexOutOfBounds(func_idx))?;

        // Check if this is an imported function (no body)
        let body = match function_opt {
            Some(body) => body.clone(),
            None => {
                // This is an imported function - we can't execute it
                return Err(RuntimeError::UnimplementedInstruction(format!(
                    "Cannot execute imported function at index {func_idx}"
                )));
            }
        };

        // Push implicit function block label
        let func_label = self.create_function_label(&func_type.return_types);
        self.current_label_stack_mut()?.push(func_label);

        // Execute the called function
        let result = self.execute_instructions(&body.body);

        // Pop the function label
        self.current_label_stack_mut()?.pop();

        // Pop the call frame
        let _frame = self.pop_call_frame().unwrap();

        // Handle the result
        match result {
            Ok(BlockEnd::Normal) | Ok(BlockEnd::Return) | Ok(BlockEnd::Branch(0)) => {
                // Function completed normally, return values are on the stack
                // Branch(0) means branch to the function's implicit block, which is like normal completion
                // The caller is responsible for verifying the correct number and types
                // of return values when popping them from the stack
                Ok(())
            }
            Ok(BlockEnd::Branch(depth)) => {
                // Uncaught branch in function (depth > 0)
                Err(RuntimeError::Trap(format!(
                    "uncaught branch with depth {depth} in function"
                )))
            }
            Err(e) => Err(e),
        }
    }

    /// Set a global value for testing purposes
    #[cfg(test)]
    pub fn set_global_for_test(&mut self, global_idx: u32, value: Value) -> Result<(), RuntimeError> {
        if global_idx as usize >= self.globals.len() {
            return Err(RuntimeError::GlobalIndexOutOfBounds(global_idx));
        }
        self.globals[global_idx as usize] = value;
        Ok(())
    }

    /// Execute a function
    pub fn execute_function(
        &mut self,
        func: &StructuredFunction,
        args: Vec<Value>,
        return_types: &[ValueType],
    ) -> Result<Vec<Value>, RuntimeError> {
        self.execute_function_with_locals(func, args, return_types, None)
    }

    /// Execute a function with explicit locals information
    pub fn execute_function_with_locals(
        &mut self,
        func: &StructuredFunction,
        args: Vec<Value>,
        return_types: &[ValueType],
        locals_info: Option<&Locals>,
    ) -> Result<Vec<Value>, RuntimeError> {
        // Initialise locals: parameters first, then local declarations
        let mut locals = args;

        // Add declared locals if provided
        if let Some(locals_info) = locals_info {
            for (count, local_type) in locals_info.iter() {
                for _ in 0..*count {
                    // Initialise locals to zero
                    let zero_value = match local_type {
                        ValueType::I32 => Value::I32(0),
                        ValueType::I64 => Value::I64(0),
                        ValueType::F32 => Value::F32(0.0),
                        ValueType::F64 => Value::F64(0.0),
                        _ => {
                            return Err(RuntimeError::UnimplementedInstruction(format!(
                                "Local type {local_type:?} not supported"
                            )))
                        }
                    };
                    locals.push(zero_value);
                }
            }
        }

        // Create initial call frame
        let initial_frame = CallFrame {
            function_idx: 0, // Top-level function
            ip: 0,
            locals,
            label_stack: Vec::new(),
            return_arity: return_types.len(),
        };
        self.call_stack.push(initial_frame);

        // Push implicit function block label
        let func_label = self.create_function_label(return_types);
        self.current_label_stack_mut()?.push(func_label);

        // Execute the function body
        let result = self.execute_instructions(&func.body);

        // Pop the function label
        self.current_label_stack_mut()?.pop();

        // Pop the initial frame
        self.call_stack.pop();

        // Handle the result
        match result {
            Ok(BlockEnd::Normal) | Ok(BlockEnd::Branch(0)) => {
                // Normal completion or branch to function block - collect return values
                // Branch(0) means branch to the function's implicit block, which is like normal completion
                let mut results = Vec::new();
                for return_type in return_types.iter().rev() {
                    let value = self.stack.pop_typed(*return_type)?;
                    results.push(value);
                }
                results.reverse();
                Ok(results)
            }
            Ok(BlockEnd::Return) => {
                // Return instruction was executed - collect return values
                let mut results = Vec::new();
                for return_type in return_types.iter().rev() {
                    let value = self.stack.pop_typed(*return_type)?;
                    results.push(value);
                }
                results.reverse();
                Ok(results)
            }
            Ok(BlockEnd::Branch(depth)) => {
                // Uncaught branch (depth > 0)
                Err(RuntimeError::InvalidLabel(depth))
            }
            Err(e) => Err(e),
        }
    }

    /// Execute a sequence of instructions
    fn execute_instructions(&mut self, instructions: &[StructuredInstruction]) -> Result<BlockEnd, RuntimeError> {
        for instruction in instructions {
            match self.execute_instruction(instruction)? {
                BlockEnd::Normal => continue,
                other => return Ok(other),
            }
        }
        Ok(BlockEnd::Normal)
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

    /// Execute a single instruction
    fn execute_instruction(&mut self, instruction: &StructuredInstruction) -> Result<BlockEnd, RuntimeError> {
        match instruction {
            StructuredInstruction::Plain(inst) => self.execute_plain_instruction(inst),

            StructuredInstruction::Block { block_type, body, .. } => {
                // Setup block structure (handle parameters and label)
                self.setup_block_structure(LabelType::Block, *block_type)?;

                // Execute the block body
                let result = self.execute_instructions(body);

                // Pop the label
                self.current_label_stack_mut()?.pop();

                // Handle the result
                match result {
                    Ok(BlockEnd::Normal) => Ok(BlockEnd::Normal),
                    Ok(BlockEnd::Return) => Ok(BlockEnd::Return),
                    Ok(BlockEnd::Branch(0)) => {
                        // Branch to this block (depth 0) - just exit normally
                        // The br/br_if instruction has already handled the stack properly
                        Ok(BlockEnd::Normal)
                    }
                    Ok(BlockEnd::Branch(depth)) => {
                        // Branch to outer block
                        Ok(BlockEnd::Branch(depth - 1))
                    }
                    Err(e) => Err(e),
                }
            }

            StructuredInstruction::Loop { block_type, body, .. } => {
                // Setup loop structure (handle parameters and label)
                self.setup_block_structure(LabelType::Loop, *block_type)?;

                // Execute the loop body
                loop {
                    let result = self.execute_instructions(body);

                    match result {
                        Ok(BlockEnd::Normal) => {
                            // Normal completion - exit the loop
                            self.current_label_stack_mut()?.pop();
                            return Ok(BlockEnd::Normal);
                        }
                        Ok(BlockEnd::Return) => {
                            // Return from function
                            self.current_label_stack_mut()?.pop();
                            return Ok(BlockEnd::Return);
                        }
                        Ok(BlockEnd::Branch(0)) => {
                            // Branch to this loop (depth 0) - continue the loop
                            // Stack has been restored by br instruction with loop parameters
                            continue;
                        }
                        Ok(BlockEnd::Branch(depth)) => {
                            // Branch to outer block
                            self.current_label_stack_mut()?.pop();
                            return Ok(BlockEnd::Branch(depth - 1));
                        }
                        Err(e) => {
                            self.current_label_stack_mut()?.pop();
                            return Err(e);
                        }
                    }
                }
            }

            StructuredInstruction::If {
                block_type,
                then_branch,
                else_branch,
                ..
            } => {
                // Pop condition
                let condition = self.stack.pop_i32()?;

                // Setup if structure (handle parameters and label)
                self.setup_block_structure(LabelType::If, *block_type)?;

                // Execute appropriate branch
                let result = if condition != 0 {
                    self.execute_instructions(then_branch)
                } else if let Some(else_body) = else_branch {
                    self.execute_instructions(else_body)
                } else {
                    Ok(BlockEnd::Normal)
                };

                // Pop the label
                self.current_label_stack_mut()?.pop();

                // Handle the result
                match result {
                    Ok(BlockEnd::Normal) => Ok(BlockEnd::Normal),
                    Ok(BlockEnd::Return) => Ok(BlockEnd::Return),
                    Ok(BlockEnd::Branch(0)) => {
                        // Branch to this if (depth 0) - just exit normally
                        Ok(BlockEnd::Normal)
                    }
                    Ok(BlockEnd::Branch(depth)) => {
                        // Branch to outer block
                        Ok(BlockEnd::Branch(depth - 1))
                    }
                    Err(e) => Err(e),
                }
            }
        }
    }

    /// Execute a plain (non-control-flow) instruction
    fn execute_plain_instruction(&mut self, inst: &Instruction) -> Result<BlockEnd, RuntimeError> {
        use InstructionKind::*;

        // Helper macro for memory operations
        macro_rules! with_memory {
            (load $op:ident($memarg:expr)) => {{
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError(
                        "No memory instance available".to_string(),
                    ));
                }
                ops::memory::$op(&mut self.stack, &self.memories[0], $memarg)?;
                Ok(BlockEnd::Normal)
            }};
            (store $op:ident($memarg:expr)) => {{
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError(
                        "No memory instance available".to_string(),
                    ));
                }
                ops::memory::$op(&mut self.stack, &mut self.memories[0], $memarg)?;
                Ok(BlockEnd::Normal)
            }};
            (size $op:ident()) => {{
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError(
                        "No memory instance available".to_string(),
                    ));
                }
                ops::memory::$op(&mut self.stack, &self.memories[0])?;
                Ok(BlockEnd::Normal)
            }};
            (grow $op:ident()) => {{
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError(
                        "No memory instance available".to_string(),
                    ));
                }
                ops::memory::$op(&mut self.stack, &mut self.memories[0])?;
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
                            "Invalid reference type for ref.null: {:?}",
                            ref_type
                        )))
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
                        })
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
                self.stack.push(Value::FuncRef(Some(*func_idx)));
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
                let value = locals
                    .get(*local_idx as usize)
                    .ok_or(RuntimeError::LocalIndexOutOfBounds(*local_idx))?
                    .clone();
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
                let value = self.stack.peek().ok_or(RuntimeError::StackUnderflow)?.clone();
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
                let value = self
                    .globals
                    .get(*global_idx as usize)
                    .ok_or(RuntimeError::GlobalIndexOutOfBounds(*global_idx))?
                    .clone();
                self.stack.push(value);
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
                if let Some(global_def) = self.module.globals.get(*global_idx) {
                    if !global_def.global_type.mutable {
                        return Err(RuntimeError::InvalidConversion(
                            "Cannot set immutable global".to_string(),
                        ));
                    }
                }

                self.globals[*global_idx as usize] = value;
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
            Call { func_idx } => {
                self.handle_call(*func_idx)?;
                Ok(BlockEnd::Normal)
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

            // memory.init x - Initialize memory from data segment
            // [i32 i32 i32] → []
            // Stack: [dest_addr, src_offset, length]
            MemoryInit { data_idx } => {
                if let Some(memory) = &mut self.memories.first_mut() {
                    ops::memory::memory_init(&mut self.stack, memory, *data_idx, &self.module.data.data)?;
                } else {
                    return Err(RuntimeError::MemoryError("No memory available".to_string()));
                }
                Ok(BlockEnd::Normal)
            }

            // memory.copy - Copy memory within the same memory
            // [i32 i32 i32] → []
            // Stack: [dest_addr, src_addr, length]
            MemoryCopy => {
                if let Some(memory) = &mut self.memories.first_mut() {
                    ops::memory::memory_copy(&mut self.stack, memory)?;
                } else {
                    return Err(RuntimeError::MemoryError("No memory available".to_string()));
                }
                Ok(BlockEnd::Normal)
            }

            // memory.fill - Fill memory with a byte value
            // [i32 i32 i32] → []
            // Stack: [dest_addr, value, length]
            MemoryFill => {
                if let Some(memory) = &mut self.memories.first_mut() {
                    ops::memory::memory_fill(&mut self.stack, memory)?;
                } else {
                    return Err(RuntimeError::MemoryError("No memory available".to_string()));
                }
                Ok(BlockEnd::Normal)
            }

            // data.drop x - Drop a data segment
            // [] → []
            // Prevents further use of data segment x
            // Note: This is a no-op in our implementation as we don't
            // track passive data segments separately after initialization
            DataDrop { data_idx: _ } => Ok(BlockEnd::Normal),

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
    use crate::runtime::test_utils::test::{make_instruction, ExecutorTest};
    use crate::runtime::Value;

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
                .expect_error("Type mismatch");
        }

        #[test]
        fn missing_return_value() {
            ExecutorTest::new()
                .returns(vec![ValueType::I32])
                .expect_error("Stack underflow");
        }

        #[test]
        fn too_few_return_values() {
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 42 })
                .returns(vec![ValueType::I32, ValueType::I32])
                .expect_error("Stack underflow");
        }
    }

    // ============================================================================
    // Structured Execution Tests
    // ============================================================================
    mod structured_execution {
        use super::*;

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
            let result = executor
                .execute_function(&func, vec![], &[ValueType::I32])
                .expect("Execution should succeed");

            assert_eq!(result, vec![Value::I32(42)]);
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
            let result = executor
                .execute_function(&func, vec![], &[ValueType::I32])
                .expect("Execution should succeed");

            assert_eq!(result, vec![Value::I32(10)]); // Should take then branch
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
        use crate::runtime::Instance;

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

            // Create instance and test
            let mut instance = Instance::new(&module).expect("Instance creation should succeed");
            let result = instance.invoke("main", vec![]).expect("Function call should succeed");

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

            let mut instance = Instance::new(&module).expect("Instance creation should succeed");

            // Test factorial(5) = 120
            let result = instance
                .invoke("factorial", vec![Value::I32(5)])
                .expect("Factorial should succeed");
            assert_eq!(result, vec![Value::I32(120)]);

            // Test factorial(0) = 1
            let result = instance
                .invoke("factorial", vec![Value::I32(0)])
                .expect("Factorial should succeed");
            assert_eq!(result, vec![Value::I32(1)]);
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
                .expect_error("Local variable index out of bounds: 1");
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
                .expect_error("Local variable index out of bounds: 1");
        }

        #[test]
        fn local_set_empty_stack() {
            ExecutorTest::new()
                .args(vec![Value::I32(42)])
                .inst(InstructionKind::LocalSet { local_idx: 0 })
                .expect_error("Stack underflow");
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
                .expect_error("Local variable index out of bounds: 1");
        }

        #[test]
        fn local_tee_empty_stack() {
            ExecutorTest::new()
                .args(vec![Value::I32(42)])
                .inst(InstructionKind::LocalTee { local_idx: 0 })
                .expect_error("Stack underflow");
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
                .expect_error("Global variable index out of bounds: 1");
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
                .expect_error("Global variable index out of bounds: 1");
        }

        #[test]
        fn global_set_empty_stack() {
            ExecutorTest::new()
                .global(ValueType::I32, Value::I32(42), true)
                .inst(InstructionKind::GlobalSet { global_idx: 0 })
                .expect_error("Stack underflow");
        }

        #[test]
        fn global_set_immutable() {
            // Test that setting immutable global fails
            ExecutorTest::new()
                .global(ValueType::I32, Value::I32(42), false) // immutable
                .inst(InstructionKind::I32Const { value: 100 })
                .inst(InstructionKind::GlobalSet { global_idx: 0 })
                .expect_error("Cannot set immutable global");
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
