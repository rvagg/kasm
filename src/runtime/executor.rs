//! WebAssembly instruction executor

use super::{
    control::{Label, LabelStack, LabelType},
    frame::Frame,
    memory::Memory,
    stack::Stack,
    RuntimeError, Value,
};
use crate::parser::instruction::{BlockType, Instruction, InstructionKind};
use crate::parser::module::{Module, ValueType};
use crate::parser::structured::{BlockEnd, StructuredFunction, StructuredInstruction};

/// Executes WebAssembly instructions
pub struct Executor<'a> {
    #[allow(dead_code)] // Will be used in the future
    module: &'a Module,
    stack: Stack,
    /// Current execution frame (contains locals)
    frame: Option<Frame>,
    /// Label stack for control flow
    label_stack: LabelStack,
    /// Memory instances (WebAssembly 1.0 supports only 1)
    memories: Vec<Memory>,
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

        Ok(Executor {
            module,
            stack: Stack::new(),
            frame: None,
            label_stack: LabelStack::new(),
            memories,
        })
    }

    /// Execute a function
    pub fn execute_function(
        &mut self,
        func: &StructuredFunction,
        args: Vec<Value>,
        return_types: &[ValueType],
    ) -> Result<Vec<Value>, RuntimeError> {
        // Initialise frame with arguments as locals
        let frame = Frame::new(args);
        self.frame = Some(frame);

        // Reset label stack for new function
        self.label_stack = LabelStack::new();

        // Execute the function body
        match self.execute_instructions(&func.body) {
            Ok(BlockEnd::Normal) => {
                // Normal completion - collect return values
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
                // Uncaught branch
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

    /// Execute a single instruction
    fn execute_instruction(&mut self, instruction: &StructuredInstruction) -> Result<BlockEnd, RuntimeError> {
        match instruction {
            StructuredInstruction::Plain(inst) => self.execute_plain_instruction(inst),

            StructuredInstruction::Block { block_type, body, .. } => {
                // Push label for the block
                let label = Label {
                    label_type: LabelType::Block,
                    block_type: *block_type,
                    stack_height: self.stack.len(),
                    unreachable: false,
                };
                self.label_stack.push(label);

                // Execute the block body
                let result = self.execute_instructions(body);

                // Pop the label
                self.label_stack.pop();

                // Handle the result
                match result {
                    Ok(BlockEnd::Normal) => Ok(BlockEnd::Normal),
                    Ok(BlockEnd::Return) => Ok(BlockEnd::Return),
                    Ok(BlockEnd::Branch(0)) => {
                        // Branch to this block (depth 0) - just exit normally
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
                // Push label for the loop
                let label = Label {
                    label_type: LabelType::Loop,
                    block_type: *block_type,
                    stack_height: self.stack.len(),
                    unreachable: false,
                };
                self.label_stack.push(label);

                // Execute the loop body
                loop {
                    let result = self.execute_instructions(body);

                    match result {
                        Ok(BlockEnd::Normal) => {
                            // Normal completion - exit the loop
                            self.label_stack.pop();
                            return Ok(BlockEnd::Normal);
                        }
                        Ok(BlockEnd::Return) => {
                            // Return from function
                            self.label_stack.pop();
                            return Ok(BlockEnd::Return);
                        }
                        Ok(BlockEnd::Branch(0)) => {
                            // Branch to this loop (depth 0) - continue the loop
                            // The label is already on the stack, just continue
                            continue;
                        }
                        Ok(BlockEnd::Branch(depth)) => {
                            // Branch to outer block
                            self.label_stack.pop();
                            return Ok(BlockEnd::Branch(depth - 1));
                        }
                        Err(e) => {
                            self.label_stack.pop();
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

                // Push label for the if
                let label = Label {
                    label_type: LabelType::If,
                    block_type: *block_type,
                    stack_height: self.stack.len(),
                    unreachable: false,
                };
                self.label_stack.push(label);

                // Execute appropriate branch
                let result = if condition != 0 {
                    self.execute_instructions(then_branch)
                } else if let Some(else_body) = else_branch {
                    self.execute_instructions(else_body)
                } else {
                    Ok(BlockEnd::Normal)
                };

                // Pop the label
                self.label_stack.pop();

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

        match &inst.kind {
            // ----------------------------------------------------------------
            // 4.4.1 Numeric Instructions
            //
            // 𝑡.const 𝑐
            // 1. Push the value 𝑡.const 𝑐 to the stack.
            I32Const { value } => {
                self.stack.push(Value::I32(*value));
                Ok(BlockEnd::Normal)
            }
            I64Const { value } => {
                self.stack.push(Value::I64(*value));
                Ok(BlockEnd::Normal)
            }
            F32Const { value } => {
                self.stack.push(Value::F32(*value));
                Ok(BlockEnd::Normal)
            }
            F64Const { value } => {
                self.stack.push(Value::F64(*value));
                Ok(BlockEnd::Normal)
            }

            // ----------------------------------------------------------------
            // 4.4.8 Control Instructions
            //
            // nop
            // 1. Do nothing.
            Nop => Ok(BlockEnd::Normal),

            // ----------------------------------------------------------------
            // 4.4.4 Parametric Instructions
            //
            // drop
            // 1. Assert: due to validation, a value is on the top of the stack.
            // 2. Pop the value val from the stack.
            Drop => {
                self.stack.pop()?;
                Ok(BlockEnd::Normal)
            }

            // ----------------------------------------------------------------
            // 4.4.5 Variable Instructions
            //
            // local.get 𝑥
            // 1. Let 𝐹 be the current frame.
            // 2. Assert: due to validation, 𝐹.locals[𝑥] exists.
            // 3. Let val be the value 𝐹.locals[𝑥].
            // 4. Push the value val to the stack.
            LocalGet { local_idx } => {
                let frame = self.frame.as_ref().ok_or(RuntimeError::InvalidFunctionType)?;
                let value = frame
                    .locals
                    .get(*local_idx as usize)
                    .ok_or(RuntimeError::LocalIndexOutOfBounds(*local_idx))?
                    .clone();
                self.stack.push(value);
                Ok(BlockEnd::Normal)
            }

            // local.set 𝑥
            // 1. Let 𝐹 be the current frame.
            // 2. Assert: due to validation, 𝐹.locals[𝑥] exists.
            // 3. Assert: due to validation, a value is on the top of the stack.
            // 4. Pop the value val from the stack.
            // 5. Replace 𝐹.locals[𝑥] with the value val.
            LocalSet { local_idx } => {
                let frame = self.frame.as_mut().ok_or(RuntimeError::InvalidFunctionType)?;
                let value = self.stack.pop()?;

                // Bounds check
                if *local_idx as usize >= frame.locals.len() {
                    return Err(RuntimeError::LocalIndexOutOfBounds(*local_idx));
                }

                frame.locals[*local_idx as usize] = value;
                Ok(BlockEnd::Normal)
            }

            // local.tee 𝑥
            // 1. Assert: due to validation, a value is on the top of the stack.
            // 2. Pop the value val from the stack.
            // 3. Push the value val to the stack.
            // 4. Push the value val to the stack.
            // 5. Execute the instruction local.set 𝑥.
            LocalTee { local_idx } => {
                let frame = self.frame.as_mut().ok_or(RuntimeError::InvalidFunctionType)?;
                let value = self.stack.pop()?;

                // Bounds check
                if *local_idx as usize >= frame.locals.len() {
                    return Err(RuntimeError::LocalIndexOutOfBounds(*local_idx));
                }

                // Push value back to stack (tee leaves it on the stack)
                self.stack.push(value.clone());

                // Store in local
                frame.locals[*local_idx as usize] = value;
                Ok(BlockEnd::Normal)
            }

            // ----------------------------------------------------------------
            // 4.4.8 Branch Instructions
            //
            // br l
            // From the spec:
            // 1. Assert: due to validation, the stack contains at least l + 1 labels.
            // 2. Let L be the l-th label appearing on the stack, starting from the top and counting from zero.
            // 3. Let n be the arity of L.
            // 4. Assert: due to validation, there are at least n values on the top of the stack.
            // 5. Pop the values val^n from the stack.
            // 6. Repeat l + 1 times: pop a label from the stack.
            // 7. Push the values val^n to the stack.
            // 8. Jump to the continuation of L.
            Br { label_idx } => {
                // Get the target label to determine arity
                let label = self
                    .label_stack
                    .get(*label_idx)
                    .ok_or(RuntimeError::InvalidLabel(*label_idx))?;

                let arity = match &label.block_type {
                    BlockType::Empty => 0,
                    BlockType::Value(_) => 1,
                    BlockType::FuncType(idx) => {
                        // For now, we don't have access to function types here
                        // This would need to be looked up from the module
                        return Err(RuntimeError::UnimplementedInstruction(format!(
                            "Branch with function type {idx} not yet supported"
                        )));
                    }
                };

                // Pop arity values from stack (these will be the block results)
                let mut values = Vec::with_capacity(arity);
                for _ in 0..arity {
                    values.push(self.stack.pop()?);
                }
                values.reverse(); // Restore original order

                // Push values back
                for value in values {
                    self.stack.push(value);
                }

                Ok(BlockEnd::Branch(*label_idx))
            }

            // br_if l
            // From the spec:
            // 1. Assert: due to validation, a value of type i32 is on the top of the stack.
            // 2. Pop the value c from the stack.
            // 3. If c is non-zero, then execute the instruction br l.
            // 4. Else, do nothing.
            BrIf { label_idx } => {
                let condition = self.stack.pop_i32()?;
                if condition != 0 {
                    // Get the target label to determine arity
                    let label = self
                        .label_stack
                        .get(*label_idx)
                        .ok_or(RuntimeError::InvalidLabel(*label_idx))?;

                    let arity = match &label.block_type {
                        BlockType::Empty => 0,
                        BlockType::Value(_) => 1,
                        BlockType::FuncType(idx) => {
                            return Err(RuntimeError::UnimplementedInstruction(format!(
                                "Branch with function type {idx} not yet supported"
                            )));
                        }
                    };

                    // Pop arity values from stack
                    let mut values = Vec::with_capacity(arity);
                    for _ in 0..arity {
                        values.push(self.stack.pop()?);
                    }
                    values.reverse();

                    // Push values back
                    for value in values {
                        self.stack.push(value);
                    }

                    Ok(BlockEnd::Branch(*label_idx))
                } else {
                    Ok(BlockEnd::Normal)
                }
            }

            // br_table l* lN
            // From the spec:
            // A br_table performs an indirect branch through an operand indexing into
            // a list of labels.
            // 1. Pop i32 index from stack
            // 2. If index < len(labels), branch to labels[index]
            // 3. Else branch to default
            BrTable { labels, default } => {
                // Pop index from stack
                let index = self.stack.pop_i32()?;

                // Choose the target label based on index
                let target = if index >= 0 && (index as usize) < labels.len() {
                    labels[index as usize]
                } else {
                    *default
                };

                // Now branch to the target (same as Br)
                let label = self.label_stack.get(target).ok_or(RuntimeError::InvalidLabel(target))?;

                let arity = match &label.block_type {
                    BlockType::Empty => 0,
                    BlockType::Value(_) => 1,
                    BlockType::FuncType(idx) => {
                        return Err(RuntimeError::UnimplementedInstruction(format!(
                            "Branch with function type {idx} not yet supported"
                        )));
                    }
                };

                // Pop arity values from stack
                let mut values = Vec::with_capacity(arity);
                for _ in 0..arity {
                    values.push(self.stack.pop()?);
                }
                values.reverse();

                // Push values back
                for value in values {
                    self.stack.push(value);
                }

                Ok(BlockEnd::Branch(target))
            }

            // return
            // From the spec:
            // The return instruction is a shortcut for an unconditional branch
            // to the outermost block, which implicitly is the body of the current function.
            //
            // Note: We handle this with BlockEnd::Return which propagates up through
            // all nested blocks to exit the function.
            Return => Ok(BlockEnd::Return),

            // ----------------------------------------------------------------
            // Memory Instructions
            //
            // memory.size
            // From the spec:
            // 1. Let F be the current frame.
            // 2. Assert: due to validation, F.module.memaddrs[0] exists.
            // 3. Let a be the memory address F.module.memaddrs[0].
            // 4. Assert: due to validation, S.mems[a] exists.
            // 5. Let mem be the memory instance S.mems[a].
            // 6. Let sz be the length of mem.data divided by the page size.
            // 7. Push the value i32.const sz to the stack.
            MemorySize => {
                // WebAssembly 1.0 only supports memory index 0
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }
                let size = self.memories[0].size();
                self.stack.push(Value::I32(size as i32));
                Ok(BlockEnd::Normal)
            }

            // memory.grow
            // From the spec:
            // 1. Let F be the current frame.
            // 2. Assert: due to validation, F.module.memaddrs[0] exists.
            // 3. Let a be the memory address F.module.memaddrs[0].
            // 4. Assert: due to validation, S.mems[a] exists.
            // 5. Let mem be the memory instance S.mems[a].
            // 6. Let sz be the length of mem.data divided by the page size.
            // 7. Assert: due to validation, a value of type i32 is on the top of the stack.
            // 8. Pop the value i32.const n from the stack.
            // 9. Let err be the i32 value -1.
            // 10. If n > max_mem_pages or growing fails, push i32.const err to the stack.
            // 11. Otherwise, push the value i32.const sz to the stack.
            MemoryGrow => {
                // WebAssembly 1.0 only supports memory index 0
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }
                let delta = self.stack.pop_i32()?;
                if delta < 0 {
                    // Negative delta always fails
                    self.stack.push(Value::I32(-1));
                } else {
                    let result = self.memories[0].grow(delta as u32);
                    self.stack.push(Value::I32(result));
                }
                Ok(BlockEnd::Normal)
            }

            // ----------------------------------------------------------------
            // 4.4.7 Memory Instructions
            //
            // i32.load
            // From the spec:
            // 1. Let F be the current frame.
            // 2. Assert: due to validation, F.module.memaddrs[0] exists.
            // 3. Let a be the memory address F.module.memaddrs[0].
            // 4. Assert: due to validation, S.mems[a] exists.
            // 5. Let mem be the memory instance S.mems[a].
            // 6. Assert: due to validation, a value of type i32 is on the top of the stack.
            // 7. Pop the value i32.const i from the stack.
            // 8. Let ea be the integer i + offset.
            // 9. If ea + 4 is larger than the length of mem.data, then trap.
            // 10. Let b* be the byte sequence mem.data[ea:ea+4].
            // 11. Let c be the integer for which bytes_i32(c) = b*.
            // 12. Push the value i32.const c to the stack.
            I32Load { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                // Pop address from stack
                let addr = self.stack.pop_i32()?;

                // Calculate effective address (checking for overflow)
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                // Check if address fits in u32 (WebAssembly 1.0 uses 32-bit addressing)
                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                // Load the value
                let value = self.memories[0].read_i32(ea as u32)?;
                self.stack.push(Value::I32(value));
                Ok(BlockEnd::Normal)
            }

            // i32.store
            // From the spec:
            // 1. Let F be the current frame.
            // 2. Assert: due to validation, F.module.memaddrs[0] exists.
            // 3. Let a be the memory address F.module.memaddrs[0].
            // 4. Assert: due to validation, S.mems[a] exists.
            // 5. Let mem be the memory instance S.mems[a].
            // 6. Assert: due to validation, a value of type i32 is on the top of the stack.
            // 7. Pop the value i32.const c from the stack.
            // 8. Assert: due to validation, a value of type i32 is on the top of the stack.
            // 9. Pop the value i32.const i from the stack.
            // 10. Let ea be the integer i + offset.
            // 11. If ea + 4 is larger than the length of mem.data, then trap.
            // 12. Let b* be the byte sequence bytes_i32(c).
            // 13. Replace the bytes mem.data[ea:ea+4] with b*.
            I32Store { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                // Pop value to store
                let value = self.stack.pop_i32()?;

                // Pop address
                let addr = self.stack.pop_i32()?;

                // Calculate effective address (checking for overflow)
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                // Check if address fits in u32
                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                // Store the value
                self.memories[0].write_i32(ea as u32, value)?;
                Ok(BlockEnd::Normal)
            }

            // i64.load
            // From the spec (4.4.7.1):
            // 1. Let F be the current frame.
            // 2. Assert: due to validation, F.module.memaddrs[0] exists.
            // 3. Let a be the memory address F.module.memaddrs[0].
            // 4. Assert: due to validation, S.mems[a] exists.
            // 5. Let mem be the memory instance S.mems[a].
            // 6. Assert: due to validation, a value of type i32 is on the top of the stack.
            // 7. Pop the value i32.const i from the stack.
            // 8. Let ea be the integer i + offset.
            // 9. If ea + 8 is larger than the length of mem.data, then trap.
            // 10. Let b* be the byte sequence mem.data[ea:ea+8].
            // 11. Let c be the integer for which bytes_i64(c) = b*.
            // 12. Push the value i64.const c to the stack.
            I64Load { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                let value = self.memories[0].read_i64(ea as u32)?;
                self.stack.push(Value::I64(value));
                Ok(BlockEnd::Normal)
            }

            // i64.store
            // From the spec (4.4.7.1):
            // 1. Let F be the current frame.
            // 2. Assert: due to validation, F.module.memaddrs[0] exists.
            // 3. Let a be the memory address F.module.memaddrs[0].
            // 4. Assert: due to validation, S.mems[a] exists.
            // 5. Let mem be the memory instance S.mems[a].
            // 6. Assert: due to validation, a value of type i64 is on the top of the stack.
            // 7. Pop the value i64.const c from the stack.
            // 8. Assert: due to validation, a value of type i32 is on the top of the stack.
            // 9. Pop the value i32.const i from the stack.
            // 10. Let ea be the integer i + offset.
            // 11. If ea + 8 is larger than the length of mem.data, then trap.
            // 12. Let b* be the byte sequence bytes_i64(c).
            // 13. Replace the bytes mem.data[ea:ea+8] with b*.
            I64Store { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let value = self.stack.pop_i64()?;
                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                self.memories[0].write_i64(ea as u32, value)?;
                Ok(BlockEnd::Normal)
            }

            // f32.load
            // From the spec (4.4.7.1):
            // 1. Let F be the current frame.
            // 2. Assert: due to validation, F.module.memaddrs[0] exists.
            // 3. Let a be the memory address F.module.memaddrs[0].
            // 4. Assert: due to validation, S.mems[a] exists.
            // 5. Let mem be the memory instance S.mems[a].
            // 6. Assert: due to validation, a value of type i32 is on the top of the stack.
            // 7. Pop the value i32.const i from the stack.
            // 8. Let ea be the integer i + offset.
            // 9. If ea + 4 is larger than the length of mem.data, then trap.
            // 10. Let b* be the byte sequence mem.data[ea:ea+4].
            // 11. Let c be the float for which bytes_f32(c) = b*.
            // 12. Push the value f32.const c to the stack.
            F32Load { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                let value = self.memories[0].read_f32(ea as u32)?;
                self.stack.push(Value::F32(value));
                Ok(BlockEnd::Normal)
            }

            // f32.store
            // From the spec (4.4.7.1):
            // 1. Let F be the current frame.
            // 2. Assert: due to validation, F.module.memaddrs[0] exists.
            // 3. Let a be the memory address F.module.memaddrs[0].
            // 4. Assert: due to validation, S.mems[a] exists.
            // 5. Let mem be the memory instance S.mems[a].
            // 6. Assert: due to validation, a value of type f32 is on the top of the stack.
            // 7. Pop the value f32.const c from the stack.
            // 8. Assert: due to validation, a value of type i32 is on the top of the stack.
            // 9. Pop the value i32.const i from the stack.
            // 10. Let ea be the integer i + offset.
            // 11. If ea + 4 is larger than the length of mem.data, then trap.
            // 12. Let b* be the byte sequence bytes_f32(c).
            // 13. Replace the bytes mem.data[ea:ea+4] with b*.
            F32Store { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let value = self.stack.pop_f32()?;
                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                self.memories[0].write_f32(ea as u32, value)?;
                Ok(BlockEnd::Normal)
            }

            // f64.load
            // From the spec (4.4.7.1):
            // 1. Let F be the current frame.
            // 2. Assert: due to validation, F.module.memaddrs[0] exists.
            // 3. Let a be the memory address F.module.memaddrs[0].
            // 4. Assert: due to validation, S.mems[a] exists.
            // 5. Let mem be the memory instance S.mems[a].
            // 6. Assert: due to validation, a value of type i32 is on the top of the stack.
            // 7. Pop the value i32.const i from the stack.
            // 8. Let ea be the integer i + offset.
            // 9. If ea + 8 is larger than the length of mem.data, then trap.
            // 10. Let b* be the byte sequence mem.data[ea:ea+8].
            // 11. Let c be the float for which bytes_f64(c) = b*.
            // 12. Push the value f64.const c to the stack.
            F64Load { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                let value = self.memories[0].read_f64(ea as u32)?;
                self.stack.push(Value::F64(value));
                Ok(BlockEnd::Normal)
            }

            // f64.store
            // From the spec (4.4.7.1):
            // 1. Let F be the current frame.
            // 2. Assert: due to validation, F.module.memaddrs[0] exists.
            // 3. Let a be the memory address F.module.memaddrs[0].
            // 4. Assert: due to validation, S.mems[a] exists.
            // 5. Let mem be the memory instance S.mems[a].
            // 6. Assert: due to validation, a value of type f64 is on the top of the stack.
            // 7. Pop the value f64.const c from the stack.
            // 8. Assert: due to validation, a value of type i32 is on the top of the stack.
            // 9. Pop the value i32.const i from the stack.
            // 10. Let ea be the integer i + offset.
            // 11. If ea + 8 is larger than the length of mem.data, then trap.
            // 12. Let b* be the byte sequence bytes_f64(c).
            // 13. Replace the bytes mem.data[ea:ea+8] with b*.
            F64Store { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let value = self.stack.pop_f64()?;
                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                self.memories[0].write_f64(ea as u32, value)?;
                Ok(BlockEnd::Normal)
            }

            // ----------------------------------------------------------------
            // 4.4.7 Memory Instructions - Sized Loads

            // i32.load8_u
            // From the spec (4.4.7.1):
            // Load 1 byte and zero-extend to i32
            I32Load8U { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                let value = self.memories[0].read_u8(ea as u32)?;
                self.stack.push(Value::I32(value as i32));
                Ok(BlockEnd::Normal)
            }

            // i32.load8_s
            // Load 1 byte and sign-extend to i32
            I32Load8S { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                let value = self.memories[0].read_i8(ea as u32)?;
                self.stack.push(Value::I32(value as i32));
                Ok(BlockEnd::Normal)
            }

            // i32.load16_u
            // Load 2 bytes and zero-extend to i32
            I32Load16U { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                let value = self.memories[0].read_u16(ea as u32)?;
                self.stack.push(Value::I32(value as i32));
                Ok(BlockEnd::Normal)
            }

            // i32.load16_s
            // Load 2 bytes and sign-extend to i32
            I32Load16S { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                let value = self.memories[0].read_i16(ea as u32)?;
                self.stack.push(Value::I32(value as i32));
                Ok(BlockEnd::Normal)
            }

            // i64.load8_u
            // Load 1 byte and zero-extend to i64
            I64Load8U { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                let value = self.memories[0].read_u8(ea as u32)?;
                self.stack.push(Value::I64(value as i64));
                Ok(BlockEnd::Normal)
            }

            // i64.load8_s
            // Load 1 byte and sign-extend to i64
            I64Load8S { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                let value = self.memories[0].read_i8(ea as u32)?;
                self.stack.push(Value::I64(value as i64));
                Ok(BlockEnd::Normal)
            }

            // i64.load16_u
            // Load 2 bytes and zero-extend to i64
            I64Load16U { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                let value = self.memories[0].read_u16(ea as u32)?;
                self.stack.push(Value::I64(value as i64));
                Ok(BlockEnd::Normal)
            }

            // i64.load16_s
            // Load 2 bytes and sign-extend to i64
            I64Load16S { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                let value = self.memories[0].read_i16(ea as u32)?;
                self.stack.push(Value::I64(value as i64));
                Ok(BlockEnd::Normal)
            }

            // i64.load32_u
            // Load 4 bytes and zero-extend to i64
            I64Load32U { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                let value = self.memories[0].read_u32(ea as u32)?;
                self.stack.push(Value::I64(value as i64));
                Ok(BlockEnd::Normal)
            }

            // i64.load32_s
            // Load 4 bytes and sign-extend to i64
            I64Load32S { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                let value = self.memories[0].read_i32(ea as u32)?;
                self.stack.push(Value::I64(value as i64));
                Ok(BlockEnd::Normal)
            }

            // ----------------------------------------------------------------
            // 4.4.7 Memory Instructions - Sized Stores

            // i32.store8
            // Store lowest 8 bits of i32
            I32Store8 { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let value = self.stack.pop_i32()?;
                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                self.memories[0].write_u8(ea as u32, value as u8)?;
                Ok(BlockEnd::Normal)
            }

            // i32.store16
            // Store lowest 16 bits of i32
            I32Store16 { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let value = self.stack.pop_i32()?;
                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                self.memories[0].write_u16(ea as u32, value as u16)?;
                Ok(BlockEnd::Normal)
            }

            // i64.store8
            // Store lowest 8 bits of i64
            I64Store8 { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let value = self.stack.pop_i64()?;
                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                self.memories[0].write_u8(ea as u32, value as u8)?;
                Ok(BlockEnd::Normal)
            }

            // i64.store16
            // Store lowest 16 bits of i64
            I64Store16 { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let value = self.stack.pop_i64()?;
                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                self.memories[0].write_u16(ea as u32, value as u16)?;
                Ok(BlockEnd::Normal)
            }

            // i64.store32
            // Store lowest 32 bits of i64
            I64Store32 { memarg } => {
                if self.memories.is_empty() {
                    return Err(RuntimeError::MemoryError("No memory instance available".to_string()));
                }

                let value = self.stack.pop_i64()?;
                let addr = self.stack.pop_i32()?;
                let ea = (addr as u64)
                    .checked_add(memarg.offset as u64)
                    .ok_or_else(|| RuntimeError::MemoryError("Address overflow".to_string()))?;

                if ea > u32::MAX as u64 {
                    return Err(RuntimeError::MemoryError("Address exceeds 32-bit range".to_string()));
                }

                self.memories[0].write_u32(ea as u32, value as u32)?;
                Ok(BlockEnd::Normal)
            }

            // ----------------------------------------------------------------
            // 4.4.1 Numeric Instructions - Unary Operations

            // i32.clz - Count leading zeros
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute clz(c1) = count of leading zero bits
            // 3. Push result to stack
            I32Clz => {
                let value = self.stack.pop_i32()?;
                self.stack.push(Value::I32(value.leading_zeros() as i32));
                Ok(BlockEnd::Normal)
            }

            // i32.ctz - Count trailing zeros
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute ctz(c1) = count of trailing zero bits
            // 3. Push result to stack
            I32Ctz => {
                let value = self.stack.pop_i32()?;
                self.stack.push(Value::I32(value.trailing_zeros() as i32));
                Ok(BlockEnd::Normal)
            }

            // i32.popcnt - Population count
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute popcnt(c1) = count of non-zero bits
            // 3. Push result to stack
            I32Popcnt => {
                let value = self.stack.pop_i32()?;
                self.stack.push(Value::I32(value.count_ones() as i32));
                Ok(BlockEnd::Normal)
            }

            // i64.clz - Count leading zeros
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute clz(c1) = count of leading zero bits
            // 3. Push result to stack
            I64Clz => {
                let value = self.stack.pop_i64()?;
                self.stack.push(Value::I64(value.leading_zeros() as i64));
                Ok(BlockEnd::Normal)
            }

            // i64.ctz - Count trailing zeros
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute ctz(c1) = count of trailing zero bits
            // 3. Push result to stack
            I64Ctz => {
                let value = self.stack.pop_i64()?;
                self.stack.push(Value::I64(value.trailing_zeros() as i64));
                Ok(BlockEnd::Normal)
            }

            // i64.popcnt - Population count
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute popcnt(c1) = count of non-zero bits
            // 3. Push result to stack
            I64Popcnt => {
                let value = self.stack.pop_i64()?;
                self.stack.push(Value::I64(value.count_ones() as i64));
                Ok(BlockEnd::Normal)
            }

            // f32.abs - Absolute value
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute abs(c1) = absolute value of c1
            // 3. Push result to stack
            // Note: Preserves NaN payload and sign handling per IEEE 754
            F32Abs => {
                let value = self.stack.pop_f32()?;
                self.stack.push(Value::F32(value.abs()));
                Ok(BlockEnd::Normal)
            }

            // f32.neg - Negation
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute neg(c1) = negation of c1
            // 3. Push result to stack
            // Note: Flips sign bit, including for NaN and infinity
            F32Neg => {
                let value = self.stack.pop_f32()?;
                self.stack.push(Value::F32(-value));
                Ok(BlockEnd::Normal)
            }

            // f32.sqrt - Square root
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute sqrt(c1) per IEEE 754-2019
            // 3. Push result to stack
            // Note: Returns NaN for negative inputs
            F32Sqrt => {
                let value = self.stack.pop_f32()?;
                self.stack.push(Value::F32(value.sqrt()));
                Ok(BlockEnd::Normal)
            }

            // f32.ceil - Ceiling
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute ceil(c1) = smallest integer >= c1
            // 3. Push result to stack
            // Note: Preserves NaN, ±∞, and ±0
            F32Ceil => {
                let value = self.stack.pop_f32()?;
                self.stack.push(Value::F32(value.ceil()));
                Ok(BlockEnd::Normal)
            }

            // f32.floor - Floor
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute floor(c1) = largest integer <= c1
            // 3. Push result to stack
            // Note: Preserves NaN, ±∞, and ±0
            F32Floor => {
                let value = self.stack.pop_f32()?;
                self.stack.push(Value::F32(value.floor()));
                Ok(BlockEnd::Normal)
            }

            // f32.trunc - Truncate
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute trunc(c1) = round toward zero
            // 3. Push result to stack
            // Note: Discards fractional part, preserves sign
            F32Trunc => {
                let value = self.stack.pop_f32()?;
                self.stack.push(Value::F32(value.trunc()));
                Ok(BlockEnd::Normal)
            }

            // f32.nearest - Round to nearest even
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute nearest(c1) = round to nearest integer
            // 3. If tie, round to even (banker's rounding)
            // 4. Push result to stack
            // Note: Rust's round_ties_even() implements IEEE 754 roundTiesToEven
            F32Nearest => {
                let value = self.stack.pop_f32()?;
                self.stack.push(Value::F32(value.round_ties_even()));
                Ok(BlockEnd::Normal)
            }

            // f64.abs - Absolute value
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute abs(c1) = absolute value of c1
            // 3. Push result to stack
            // Note: Preserves NaN payload and sign handling per IEEE 754
            F64Abs => {
                let value = self.stack.pop_f64()?;
                self.stack.push(Value::F64(value.abs()));
                Ok(BlockEnd::Normal)
            }

            // f64.neg - Negation
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute neg(c1) = negation of c1
            // 3. Push result to stack
            // Note: Flips sign bit, including for NaN and infinity
            F64Neg => {
                let value = self.stack.pop_f64()?;
                self.stack.push(Value::F64(-value));
                Ok(BlockEnd::Normal)
            }

            // f64.sqrt - Square root
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute sqrt(c1) per IEEE 754-2019
            // 3. Push result to stack
            // Note: Returns NaN for negative inputs
            F64Sqrt => {
                let value = self.stack.pop_f64()?;
                self.stack.push(Value::F64(value.sqrt()));
                Ok(BlockEnd::Normal)
            }

            // f64.ceil - Ceiling
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute ceil(c1) = smallest integer >= c1
            // 3. Push result to stack
            // Note: Preserves NaN, ±∞, and ±0
            F64Ceil => {
                let value = self.stack.pop_f64()?;
                self.stack.push(Value::F64(value.ceil()));
                Ok(BlockEnd::Normal)
            }

            // f64.floor - Floor
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute floor(c1) = largest integer <= c1
            // 3. Push result to stack
            // Note: Preserves NaN, ±∞, and ±0
            F64Floor => {
                let value = self.stack.pop_f64()?;
                self.stack.push(Value::F64(value.floor()));
                Ok(BlockEnd::Normal)
            }

            // f64.trunc - Truncate
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute trunc(c1) = round toward zero
            // 3. Push result to stack
            // Note: Discards fractional part, preserves sign
            F64Trunc => {
                let value = self.stack.pop_f64()?;
                self.stack.push(Value::F64(value.trunc()));
                Ok(BlockEnd::Normal)
            }

            // f64.nearest - Round to nearest even
            // From the spec (4.4.1.2 - t.unop):
            // 1. Pop value c1 from stack
            // 2. Compute nearest(c1) = round to nearest integer
            // 3. If tie, round to even (banker's rounding)
            // 4. Push result to stack
            // Note: Rust's round_ties_even() implements IEEE 754 roundTiesToEven
            F64Nearest => {
                let value = self.stack.pop_f64()?;
                self.stack.push(Value::F64(value.round_ties_even()));
                Ok(BlockEnd::Normal)
            }

            // ----------------------------------------------------------------
            // 4.4.1.3 Numeric Instructions - Binary Operations

            // f32.add
            // From the spec (4.4.1.3 - t.binop):
            // 1. Pop value c2 from stack
            // 2. Pop value c1 from stack
            // 3. Compute fadd(c1, c2) per IEEE 754-2019
            // 4. Push result to stack
            // Note: NaN propagation is non-deterministic per spec
            F32Add => {
                let c2 = self.stack.pop_f32()?;
                let c1 = self.stack.pop_f32()?;
                self.stack.push(Value::F32(c1 + c2));
                Ok(BlockEnd::Normal)
            }

            // f32.sub
            // From the spec (4.4.1.3 - t.binop):
            // 1. Pop value c2 from stack
            // 2. Pop value c1 from stack
            // 3. Compute fsub(c1, c2) per IEEE 754-2019
            // 4. Push result to stack
            F32Sub => {
                let c2 = self.stack.pop_f32()?;
                let c1 = self.stack.pop_f32()?;
                self.stack.push(Value::F32(c1 - c2));
                Ok(BlockEnd::Normal)
            }

            // f32.mul
            // From the spec (4.4.1.3 - t.binop):
            // 1. Pop value c2 from stack
            // 2. Pop value c1 from stack
            // 3. Compute fmul(c1, c2) per IEEE 754-2019
            // 4. Push result to stack
            F32Mul => {
                let c2 = self.stack.pop_f32()?;
                let c1 = self.stack.pop_f32()?;
                self.stack.push(Value::F32(c1 * c2));
                Ok(BlockEnd::Normal)
            }

            // f32.div
            // From the spec (4.4.1.3 - t.binop):
            // 1. Pop value c2 from stack
            // 2. Pop value c1 from stack
            // 3. Compute fdiv(c1, c2) per IEEE 754-2019
            // 4. Push result to stack
            // Note: Division by zero returns ±∞ per IEEE 754
            F32Div => {
                let c2 = self.stack.pop_f32()?;
                let c1 = self.stack.pop_f32()?;
                self.stack.push(Value::F32(c1 / c2));
                Ok(BlockEnd::Normal)
            }

            // f32.min
            // From the spec (4.4.1.3 - t.binop):
            // Returns the minimum of two values
            // Special cases: -0.0 < +0.0, NaN propagation
            F32Min => {
                let c2 = self.stack.pop_f32()?;
                let c1 = self.stack.pop_f32()?;
                // WebAssembly spec requires special NaN and -0.0 handling
                let result = if c1.is_nan() || c2.is_nan() {
                    f32::NAN
                } else if c1 == 0.0 && c2 == 0.0 && c1.is_sign_negative() != c2.is_sign_negative() {
                    // -0.0 is less than +0.0
                    if c1.is_sign_negative() {
                        c1
                    } else {
                        c2
                    }
                } else {
                    c1.min(c2)
                };
                self.stack.push(Value::F32(result));
                Ok(BlockEnd::Normal)
            }

            // f32.max
            // From the spec (4.4.1.3 - t.binop):
            // Returns the maximum of two values
            // Special cases: +0.0 > -0.0, NaN propagation
            F32Max => {
                let c2 = self.stack.pop_f32()?;
                let c1 = self.stack.pop_f32()?;
                // WebAssembly spec requires special NaN and -0.0 handling
                let result = if c1.is_nan() || c2.is_nan() {
                    f32::NAN
                } else if c1 == 0.0 && c2 == 0.0 && c1.is_sign_negative() != c2.is_sign_negative() {
                    // +0.0 is greater than -0.0
                    if c1.is_sign_negative() {
                        c2
                    } else {
                        c1
                    }
                } else {
                    c1.max(c2)
                };
                self.stack.push(Value::F32(result));
                Ok(BlockEnd::Normal)
            }

            // f32.copysign
            // From the spec (4.4.1.3 - t.binop):
            // Returns c1 with the sign of c2
            F32Copysign => {
                let c2 = self.stack.pop_f32()?;
                let c1 = self.stack.pop_f32()?;
                self.stack.push(Value::F32(c1.copysign(c2)));
                Ok(BlockEnd::Normal)
            }

            // f64.add
            // From the spec (4.4.1.3 - t.binop):
            // 1. Pop value c2 from stack
            // 2. Pop value c1 from stack
            // 3. Compute fadd(c1, c2) per IEEE 754-2019
            // 4. Push result to stack
            // Note: NaN propagation is non-deterministic per spec
            F64Add => {
                let c2 = self.stack.pop_f64()?;
                let c1 = self.stack.pop_f64()?;
                self.stack.push(Value::F64(c1 + c2));
                Ok(BlockEnd::Normal)
            }

            // f64.sub
            // From the spec (4.4.1.3 - t.binop):
            // 1. Pop value c2 from stack
            // 2. Pop value c1 from stack
            // 3. Compute fsub(c1, c2) per IEEE 754-2019
            // 4. Push result to stack
            F64Sub => {
                let c2 = self.stack.pop_f64()?;
                let c1 = self.stack.pop_f64()?;
                self.stack.push(Value::F64(c1 - c2));
                Ok(BlockEnd::Normal)
            }

            // f64.mul
            // From the spec (4.4.1.3 - t.binop):
            // 1. Pop value c2 from stack
            // 2. Pop value c1 from stack
            // 3. Compute fmul(c1, c2) per IEEE 754-2019
            // 4. Push result to stack
            F64Mul => {
                let c2 = self.stack.pop_f64()?;
                let c1 = self.stack.pop_f64()?;
                self.stack.push(Value::F64(c1 * c2));
                Ok(BlockEnd::Normal)
            }

            // f64.div
            // From the spec (4.4.1.3 - t.binop):
            // 1. Pop value c2 from stack
            // 2. Pop value c1 from stack
            // 3. Compute fdiv(c1, c2) per IEEE 754-2019
            // 4. Push result to stack
            // Note: Division by zero returns ±∞ per IEEE 754
            F64Div => {
                let c2 = self.stack.pop_f64()?;
                let c1 = self.stack.pop_f64()?;
                self.stack.push(Value::F64(c1 / c2));
                Ok(BlockEnd::Normal)
            }

            // f64.min
            // From the spec (4.4.1.3 - t.binop):
            // Returns the minimum of two values
            // Special cases: -0.0 < +0.0, NaN propagation
            F64Min => {
                let c2 = self.stack.pop_f64()?;
                let c1 = self.stack.pop_f64()?;
                // WebAssembly spec requires special NaN and -0.0 handling
                let result = if c1.is_nan() || c2.is_nan() {
                    f64::NAN
                } else if c1 == 0.0 && c2 == 0.0 && c1.is_sign_negative() != c2.is_sign_negative() {
                    // -0.0 is less than +0.0
                    if c1.is_sign_negative() {
                        c1
                    } else {
                        c2
                    }
                } else {
                    c1.min(c2)
                };
                self.stack.push(Value::F64(result));
                Ok(BlockEnd::Normal)
            }

            // f64.max
            // From the spec (4.4.1.3 - t.binop):
            // Returns the maximum of two values
            // Special cases: +0.0 > -0.0, NaN propagation
            F64Max => {
                let c2 = self.stack.pop_f64()?;
                let c1 = self.stack.pop_f64()?;
                // WebAssembly spec requires special NaN and -0.0 handling
                let result = if c1.is_nan() || c2.is_nan() {
                    f64::NAN
                } else if c1 == 0.0 && c2 == 0.0 && c1.is_sign_negative() != c2.is_sign_negative() {
                    // +0.0 is greater than -0.0
                    if c1.is_sign_negative() {
                        c2
                    } else {
                        c1
                    }
                } else {
                    c1.max(c2)
                };
                self.stack.push(Value::F64(result));
                Ok(BlockEnd::Normal)
            }

            // f64.copysign
            // From the spec (4.4.1.3 - t.binop):
            // Returns c1 with the sign of c2
            F64Copysign => {
                let c2 = self.stack.pop_f64()?;
                let c1 = self.stack.pop_f64()?;
                self.stack.push(Value::F64(c1.copysign(c2)));
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
    use crate::parser::instruction::{ByteRange, Instruction, InstructionKind};
    use crate::parser::module::Module;
    use crate::parser::structure_builder::StructureBuilder;

    /// Test builder for creating executor tests fluently
    struct ExecutorTest {
        instructions: Vec<Instruction>,
        args: Vec<Value>,
        return_types: Vec<ValueType>,
        with_memory: bool,
    }

    impl ExecutorTest {
        fn new() -> Self {
            ExecutorTest {
                instructions: Vec::new(),
                args: Vec::new(),
                return_types: Vec::new(),
                with_memory: false,
            }
        }

        fn with_memory(mut self) -> Self {
            self.with_memory = true;
            self
        }

        fn inst(mut self, kind: InstructionKind) -> Self {
            self.instructions.push(make_instruction(kind));
            self
        }

        fn args(mut self, args: Vec<Value>) -> Self {
            self.args = args;
            self
        }

        fn returns(mut self, types: Vec<ValueType>) -> Self {
            self.return_types = types;
            self
        }

        fn expect_stack(mut self, expected: Vec<Value>) {
            self.instructions.push(make_instruction(InstructionKind::End));
            let mut module = Module::new("test");

            // Add memory if requested
            if self.with_memory {
                use crate::parser::module::{Limits, Memory, MemorySection, SectionPosition};
                let mem = Memory {
                    limits: Limits {
                        min: 1, // 1 page (64KB)
                        max: None,
                    },
                };
                module.memory = MemorySection {
                    memory: vec![mem],
                    position: SectionPosition { start: 0, end: 0 },
                };
            }

            // Build structured representation
            let structured_func = StructureBuilder::build_function(
                &self.instructions,
                0, // Most tests don't use locals
                self.return_types.clone(),
            )
            .expect("Structure building should succeed");

            let mut executor = Executor::new(&module).expect("Executor creation should succeed");
            let results = executor
                .execute_function(&structured_func, self.args, &self.return_types)
                .expect("Execution should succeed");
            assert_eq!(results, expected);
        }

        fn run(mut self) -> Result<Executor<'static>, RuntimeError> {
            self.instructions.push(make_instruction(InstructionKind::End));
            let module = Box::leak(Box::new(Module::new("test")));

            // Add memory if requested
            if self.with_memory {
                use crate::parser::module::{Limits, Memory, MemorySection, SectionPosition};
                let mem = Memory {
                    limits: Limits {
                        min: 1, // 1 page (64KB)
                        max: None,
                    },
                };
                module.memory = MemorySection {
                    memory: vec![mem],
                    position: SectionPosition { start: 0, end: 0 },
                };
            }

            // Build structured representation
            let structured_func = StructureBuilder::build_function(
                &self.instructions,
                0, // Most tests don't use locals
                self.return_types.clone(),
            )
            .expect("Structure building should succeed");

            let mut executor = Executor::new(module)?;
            executor.execute_function(&structured_func, self.args, &self.return_types)?;
            Ok(executor)
        }

        fn expect_error(mut self, error_contains: &str) {
            self.instructions.push(make_instruction(InstructionKind::End));
            let mut module = Module::new("test");

            // Add memory if requested
            if self.with_memory {
                use crate::parser::module::{Limits, Memory, MemorySection, SectionPosition};
                let mem = Memory {
                    limits: Limits {
                        min: 1, // 1 page (64KB)
                        max: None,
                    },
                };
                module.memory = MemorySection {
                    memory: vec![mem],
                    position: SectionPosition { start: 0, end: 0 },
                };
            }

            // Build structured representation
            let structured_func = StructureBuilder::build_function(
                &self.instructions,
                0, // Most tests don't use locals
                self.return_types.clone(),
            )
            .expect("Structure building should succeed");

            let mut executor = Executor::new(&module).expect("Executor creation should succeed");
            let result = executor.execute_function(&structured_func, self.args, &self.return_types);
            assert!(result.is_err(), "Expected error but execution succeeded");
            let actual_error = result.unwrap_err().to_string();
            assert!(
                actual_error.contains(error_contains),
                "Error should contain '{}', but got: '{}'",
                error_contains,
                actual_error
            );
        }
    }

    fn make_instruction(kind: InstructionKind) -> Instruction {
        Instruction {
            kind,
            position: ByteRange { offset: 0, length: 1 },
            original_bytes: vec![],
        }
    }

    // ============================================================================
    // Numeric Constant Tests
    // ============================================================================
    mod numeric_constants {
        use super::*;

        #[test]
        fn i32_const_positive() {
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 42 })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn i32_const_negative() {
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: -42 })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(-42)]);
        }

        #[test]
        fn i32_const_limits() {
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: i32::MAX })
                .inst(InstructionKind::I32Const { value: i32::MIN })
                .returns(vec![ValueType::I32, ValueType::I32])
                .expect_stack(vec![Value::I32(i32::MAX), Value::I32(i32::MIN)]);
        }

        #[test]
        fn i64_const() {
            ExecutorTest::new()
                .inst(InstructionKind::I64Const {
                    value: 0x123456789ABCDEF,
                })
                .returns(vec![ValueType::I64])
                .expect_stack(vec![Value::I64(0x123456789ABCDEF)]);
        }

        #[test]
        fn f32_const() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 3.14159f32 })
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(3.14159f32)]);
        }

        #[test]
        fn f64_const() {
            ExecutorTest::new()
                .inst(InstructionKind::F64Const {
                    value: std::f64::consts::E,
                })
                .returns(vec![ValueType::F64])
                .expect_stack(vec![Value::F64(std::f64::consts::E)]);
        }

        #[test]
        fn mixed_constants() {
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 1 })
                .inst(InstructionKind::I64Const { value: 2 })
                .inst(InstructionKind::F32Const { value: 3.0 })
                .inst(InstructionKind::F64Const { value: 4.0 })
                .returns(vec![ValueType::I32, ValueType::I64, ValueType::F32, ValueType::F64])
                .expect_stack(vec![Value::I32(1), Value::I64(2), Value::F32(3.0), Value::F64(4.0)]);
        }
    }

    // ============================================================================
    // Parametric Instruction Tests
    // ============================================================================
    mod unary_operations {
        use super::*;

        // i32 unary operations
        #[test]
        fn i32_clz() {
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 0x0F000000 })
                .inst(InstructionKind::I32Clz)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(4)]);

            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 0 })
                .inst(InstructionKind::I32Clz)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(32)]);

            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: -1 })
                .inst(InstructionKind::I32Clz)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(0)]);
        }

        #[test]
        fn i32_ctz() {
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 0x00008000 })
                .inst(InstructionKind::I32Ctz)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(15)]);

            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 0 })
                .inst(InstructionKind::I32Ctz)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(32)]);
        }

        #[test]
        fn i32_popcnt() {
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 0x55555555 })
                .inst(InstructionKind::I32Popcnt)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(16)]);

            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 0 })
                .inst(InstructionKind::I32Popcnt)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(0)]);

            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: -1 })
                .inst(InstructionKind::I32Popcnt)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(32)]);
        }

        // i64 unary operations
        #[test]
        fn i64_clz() {
            ExecutorTest::new()
                .inst(InstructionKind::I64Const {
                    value: 0x0F00000000000000,
                })
                .inst(InstructionKind::I64Clz)
                .returns(vec![ValueType::I64])
                .expect_stack(vec![Value::I64(4)]);

            ExecutorTest::new()
                .inst(InstructionKind::I64Const { value: 0 })
                .inst(InstructionKind::I64Clz)
                .returns(vec![ValueType::I64])
                .expect_stack(vec![Value::I64(64)]);
        }

        #[test]
        fn i64_ctz() {
            ExecutorTest::new()
                .inst(InstructionKind::I64Const {
                    value: 0x8000000000000000u64 as i64,
                })
                .inst(InstructionKind::I64Ctz)
                .returns(vec![ValueType::I64])
                .expect_stack(vec![Value::I64(63)]);
        }

        #[test]
        fn i64_popcnt() {
            ExecutorTest::new()
                .inst(InstructionKind::I64Const {
                    value: 0x5555555555555555,
                })
                .inst(InstructionKind::I64Popcnt)
                .returns(vec![ValueType::I64])
                .expect_stack(vec![Value::I64(32)]);
        }

        // f32 unary operations
        #[test]
        fn f32_abs() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: -3.14 })
                .inst(InstructionKind::F32Abs)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(3.14)]);

            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 3.14 })
                .inst(InstructionKind::F32Abs)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(3.14)]);
        }

        #[test]
        fn f32_neg() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 3.14 })
                .inst(InstructionKind::F32Neg)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(-3.14)]);

            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: -3.14 })
                .inst(InstructionKind::F32Neg)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(3.14)]);
        }

        #[test]
        fn f32_sqrt() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 4.0 })
                .inst(InstructionKind::F32Sqrt)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(2.0)]);
        }

        #[test]
        fn f32_ceil() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 3.3 })
                .inst(InstructionKind::F32Ceil)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(4.0)]);

            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: -3.3 })
                .inst(InstructionKind::F32Ceil)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(-3.0)]);
        }

        #[test]
        fn f32_floor() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 3.7 })
                .inst(InstructionKind::F32Floor)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(3.0)]);

            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: -3.7 })
                .inst(InstructionKind::F32Floor)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(-4.0)]);
        }

        #[test]
        fn f32_trunc() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 3.7 })
                .inst(InstructionKind::F32Trunc)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(3.0)]);

            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: -3.7 })
                .inst(InstructionKind::F32Trunc)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(-3.0)]);
        }

        #[test]
        fn f32_nearest() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 3.5 })
                .inst(InstructionKind::F32Nearest)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(4.0)]); // Round to even

            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 2.5 })
                .inst(InstructionKind::F32Nearest)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(2.0)]); // Round to even

            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 3.3 })
                .inst(InstructionKind::F32Nearest)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(3.0)]);
        }

        // f64 unary operations
        #[test]
        fn f64_abs() {
            ExecutorTest::new()
                .inst(InstructionKind::F64Const { value: -3.14159 })
                .inst(InstructionKind::F64Abs)
                .returns(vec![ValueType::F64])
                .expect_stack(vec![Value::F64(3.14159)]);
        }

        #[test]
        fn f64_neg() {
            ExecutorTest::new()
                .inst(InstructionKind::F64Const { value: 3.14159 })
                .inst(InstructionKind::F64Neg)
                .returns(vec![ValueType::F64])
                .expect_stack(vec![Value::F64(-3.14159)]);
        }

        #[test]
        fn f64_sqrt() {
            ExecutorTest::new()
                .inst(InstructionKind::F64Const { value: 9.0 })
                .inst(InstructionKind::F64Sqrt)
                .returns(vec![ValueType::F64])
                .expect_stack(vec![Value::F64(3.0)]);
        }

        #[test]
        fn f64_ceil() {
            ExecutorTest::new()
                .inst(InstructionKind::F64Const { value: 3.3 })
                .inst(InstructionKind::F64Ceil)
                .returns(vec![ValueType::F64])
                .expect_stack(vec![Value::F64(4.0)]);
        }

        #[test]
        fn f64_floor() {
            ExecutorTest::new()
                .inst(InstructionKind::F64Const { value: 3.7 })
                .inst(InstructionKind::F64Floor)
                .returns(vec![ValueType::F64])
                .expect_stack(vec![Value::F64(3.0)]);
        }

        #[test]
        fn f64_trunc() {
            ExecutorTest::new()
                .inst(InstructionKind::F64Const { value: -3.7 })
                .inst(InstructionKind::F64Trunc)
                .returns(vec![ValueType::F64])
                .expect_stack(vec![Value::F64(-3.0)]);
        }

        #[test]
        fn f64_nearest() {
            ExecutorTest::new()
                .inst(InstructionKind::F64Const { value: 4.5 })
                .inst(InstructionKind::F64Nearest)
                .returns(vec![ValueType::F64])
                .expect_stack(vec![Value::F64(4.0)]); // Round to even
        }
    }

    // ============================================================================
    mod parametric {
        use super::*;

        #[test]
        fn drop_single_value() {
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::Drop)
                .inst(InstructionKind::I32Const { value: 100 })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(100)]);
        }

        #[test]
        fn drop_multiple_values() {
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 1 })
                .inst(InstructionKind::I32Const { value: 2 })
                .inst(InstructionKind::I32Const { value: 3 })
                .inst(InstructionKind::Drop)
                .inst(InstructionKind::Drop)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(1)]);
        }

        #[test]
        fn drop_empty_stack() {
            ExecutorTest::new()
                .inst(InstructionKind::Drop)
                .expect_error("Stack underflow");
        }

        #[test]
        fn drop_different_types() {
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::Drop)
                .inst(InstructionKind::F64Const { value: 3.14 })
                .inst(InstructionKind::Drop)
                .inst(InstructionKind::I64Const { value: 100 })
                .returns(vec![ValueType::I64])
                .expect_stack(vec![Value::I64(100)]);
        }
    }

    // ============================================================================
    // Control Instruction Tests
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
    // Variable Instruction Tests
    // ============================================================================
    mod variable_instructions {
        use super::*;

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
    }

    // ============================================================================
    // Block and Control Flow Tests
    // ============================================================================
    mod block_tests {
        use super::*;

        #[test]
        fn block_empty() {
            // Empty block
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::End)
                .inst(InstructionKind::I32Const { value: 42 })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn block_with_value() {
            // Block that produces a value
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn nested_blocks() {
            // Nested blocks
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::End)
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn br_simple() {
            // Branch out of a block
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::Br { label_idx: 0 })
                .inst(InstructionKind::I32Const { value: 99 }) // Should be skipped
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn br_with_value() {
            // Branch with a value
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::Br { label_idx: 0 })
                .inst(InstructionKind::Drop) // Should be skipped
                .inst(InstructionKind::I32Const { value: 99 }) // Should be skipped
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn br_nested() {
            // Branch from inner to outer block
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::Br { label_idx: 1 }) // Branch to outer block
                .inst(InstructionKind::I32Const { value: 99 }) // Should be skipped
                .inst(InstructionKind::End)
                .inst(InstructionKind::I32Const { value: 88 }) // Should be skipped
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn br_if_true() {
            // Conditional branch taken
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::I32Const { value: 1 }) // True condition
                .inst(InstructionKind::BrIf { label_idx: 0 })
                .inst(InstructionKind::Drop) // Should be skipped
                .inst(InstructionKind::I32Const { value: 99 }) // Should be skipped
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn br_if_false() {
            // Conditional branch not taken
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::I32Const { value: 0 }) // False condition
                .inst(InstructionKind::BrIf { label_idx: 0 })
                .inst(InstructionKind::Drop) // Should execute
                .inst(InstructionKind::I32Const { value: 99 }) // Should execute
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(99)]);
        }

        #[test]
        fn br_if_with_value() {
            // Conditional branch with block value
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::I32Const { value: -1 }) // True condition (non-zero)
                .inst(InstructionKind::BrIf { label_idx: 0 })
                .inst(InstructionKind::Drop)
                .inst(InstructionKind::I32Const { value: 99 })
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn loop_simple() {
            // Simple loop that exits immediately
            ExecutorTest::new()
                .inst(InstructionKind::Loop {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn loop_with_counter() {
            // Loop with a counter (simplified - normally would use locals)
            ExecutorTest::new()
                .args(vec![Value::I32(3)]) // Counter in local 0
                .inst(InstructionKind::Loop {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::LocalGet { local_idx: 0 })
                .inst(InstructionKind::I32Const { value: 1 })
                .inst(InstructionKind::I32Const { value: 0 }) // Simulate i32.sub result of 0
                .inst(InstructionKind::LocalSet { local_idx: 0 })
                .inst(InstructionKind::LocalGet { local_idx: 0 })
                .inst(InstructionKind::BrIf { label_idx: 0 }) // Continue loop if non-zero
                .inst(InstructionKind::End)
                .inst(InstructionKind::I32Const { value: 42 })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        // ============================================================================
        // If/Else Tests
        // ============================================================================
        #[test]
        fn if_true_no_else() {
            // If with true condition, no else branch
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 1 }) // True condition
                .inst(InstructionKind::If {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::End)
                .inst(InstructionKind::I32Const { value: 99 })
                .returns(vec![ValueType::I32, ValueType::I32])
                .expect_stack(vec![Value::I32(42), Value::I32(99)]);
        }

        #[test]
        fn if_false_no_else() {
            // If with false condition, no else branch
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 0 }) // False condition
                .inst(InstructionKind::If {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 42 }) // Should be skipped
                .inst(InstructionKind::End)
                .inst(InstructionKind::I32Const { value: 99 })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(99)]);
        }

        #[test]
        fn if_true_with_else() {
            // If with true condition and else branch
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 1 }) // True condition
                .inst(InstructionKind::If {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::Else)
                .inst(InstructionKind::I32Const { value: 88 }) // Should be skipped
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn if_false_with_else() {
            // If with false condition and else branch
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 0 }) // False condition
                .inst(InstructionKind::If {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 42 }) // Should be skipped
                .inst(InstructionKind::Else)
                .inst(InstructionKind::I32Const { value: 88 })
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(88)]);
        }

        #[test]
        fn if_with_value() {
            // If that produces a value
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 1 })
                .inst(InstructionKind::If {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::Else)
                .inst(InstructionKind::I32Const { value: 88 })
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn nested_if() {
            // Nested if statements
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 1 })
                .inst(InstructionKind::If {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 0 })
                .inst(InstructionKind::If {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 11 }) // Should be skipped
                .inst(InstructionKind::Else)
                .inst(InstructionKind::I32Const { value: 22 })
                .inst(InstructionKind::End)
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(22)]);
        }

        #[test]
        fn if_br() {
            // Branch out of if
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 1 })
                .inst(InstructionKind::If {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::Br { label_idx: 0 })
                .inst(InstructionKind::Drop)
                .inst(InstructionKind::I32Const { value: 99 })
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        // ============================================================================
        // Return Tests
        // ============================================================================
        #[test]
        fn return_simple() {
            // Simple return with value
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::Return)
                .inst(InstructionKind::I32Const { value: 99 }) // Should not be executed
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn return_no_value() {
            // Return with no value
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::Drop)
                .inst(InstructionKind::Return)
                .inst(InstructionKind::I32Const { value: 99 }) // Should not be executed
                .returns(vec![])
                .expect_stack(vec![]);
        }

        #[test]
        fn return_multiple_values() {
            // Return with multiple values
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 1 })
                .inst(InstructionKind::I32Const { value: 2 })
                .inst(InstructionKind::Return)
                .inst(InstructionKind::I32Const { value: 99 }) // Should not be executed
                .returns(vec![ValueType::I32, ValueType::I32])
                .expect_stack(vec![Value::I32(1), Value::I32(2)]);
        }

        #[test]
        fn return_from_block() {
            // Return from inside a block
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::Return)
                .inst(InstructionKind::I32Const { value: 88 }) // Should not be executed
                .inst(InstructionKind::End)
                .inst(InstructionKind::I32Const { value: 99 }) // Should not be executed
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn return_from_nested_blocks() {
            // Return from deeply nested blocks
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::Return)
                .inst(InstructionKind::End)
                .inst(InstructionKind::End)
                .inst(InstructionKind::I32Const { value: 99 }) // Should not be executed
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn return_from_if() {
            // Return from inside if branch
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 1 })
                .inst(InstructionKind::If {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::Return)
                .inst(InstructionKind::Else)
                .inst(InstructionKind::I32Const { value: 88 })
                .inst(InstructionKind::End)
                .inst(InstructionKind::I32Const { value: 99 }) // Should not be executed
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn return_from_loop() {
            // Return from inside a loop
            ExecutorTest::new()
                .inst(InstructionKind::Loop {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::Return)
                .inst(InstructionKind::Br { label_idx: 0 }) // Should not be executed
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        // ============================================================================
        // BrTable Tests
        // ============================================================================
        #[test]
        fn br_table_index_0() {
            // Branch to first label in table (innermost block)
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::I32Const { value: 0 }) // index 0
                .inst(InstructionKind::BrTable {
                    labels: vec![0, 1],
                    default: 1,
                })
                .inst(InstructionKind::I32Const { value: 99 }) // Should be skipped
                .inst(InstructionKind::End)
                .inst(InstructionKind::I32Const { value: 88 })
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32, ValueType::I32])
                .expect_stack(vec![Value::I32(42), Value::I32(88)]);
        }

        #[test]
        fn br_table_index_1() {
            // Branch to second label in table (outer block)
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::I32Const { value: 1 }) // index 1 -> label 1
                .inst(InstructionKind::BrTable {
                    labels: vec![0, 1],
                    default: 0,
                })
                .inst(InstructionKind::I32Const { value: 99 }) // Should be skipped
                .inst(InstructionKind::End)
                .inst(InstructionKind::I32Const { value: 88 }) // Should be skipped
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn br_table_default() {
            // Index out of bounds, use default
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::I32Const { value: 5 }) // index out of bounds
                .inst(InstructionKind::BrTable {
                    labels: vec![0, 1],
                    default: 1,
                })
                .inst(InstructionKind::I32Const { value: 99 }) // Should be skipped
                .inst(InstructionKind::End)
                .inst(InstructionKind::I32Const { value: 88 }) // Should be skipped
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn br_table_negative_index() {
            // Negative index uses default
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::I32Const { value: -1 }) // negative index
                .inst(InstructionKind::BrTable {
                    labels: vec![0],
                    default: 0,
                })
                .inst(InstructionKind::I32Const { value: 99 }) // Should be skipped
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn br_table_single_label() {
            // Table with only one label plus default
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::I32Const { value: 0 })
                .inst(InstructionKind::BrTable {
                    labels: vec![0],
                    default: 0,
                })
                .inst(InstructionKind::Drop)
                .inst(InstructionKind::I32Const { value: 99 })
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn br_table_simple() {
            // Simple br_table in a single block
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::I32Const { value: 0 })
                .inst(InstructionKind::BrTable {
                    labels: vec![0],
                    default: 0,
                })
                .inst(InstructionKind::I32Const { value: 99 }) // Should be skipped
                .inst(InstructionKind::End)
                .inst(InstructionKind::I32Const { value: 88 })
                .returns(vec![ValueType::I32, ValueType::I32])
                .expect_stack(vec![Value::I32(42), Value::I32(88)]);
        }

        #[test]
        fn br_table_three_way_branch() {
            // Test 3-way branch with clear signal which path was taken
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    // Label 2 (outermost)
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::Block {
                    // Label 1 (middle)
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::Block {
                    // Label 0 (innermost)
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 1 }) // index 1 -> middle block
                .inst(InstructionKind::BrTable {
                    labels: vec![0, 1, 2],
                    default: 2,
                })
                .inst(InstructionKind::I32Const { value: 100 }) // innermost - skipped
                .inst(InstructionKind::End)
                .inst(InstructionKind::I32Const { value: 200 }) // middle - skipped
                .inst(InstructionKind::End)
                .inst(InstructionKind::I32Const { value: 300 }) // outermost - executed
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(300)]);
        }

        #[test]
        fn br_table_with_stack_values() {
            // Test that branch properly handles stack values according to label arity
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 100 })
                .inst(InstructionKind::I32Const { value: 200 })
                .inst(InstructionKind::I32Const { value: 0 }) // index 0 -> inner block (Empty)
                .inst(InstructionKind::BrTable {
                    labels: vec![0, 1],
                    default: 1,
                })
                .inst(InstructionKind::Drop) // Should be skipped
                .inst(InstructionKind::End)
                .inst(InstructionKind::I32Const { value: 300 }) // Should execute after inner branch
                .inst(InstructionKind::End)
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(300)]); // The 300 value pushed after the branch
        }
    }

    // ============================================================================
    // BrTable Error/Edge Case Tests
    // ============================================================================
    mod br_table_errors {
        use super::*;

        #[test]
        fn br_table_empty_stack_error() {
            // br_table should fail if there's no index on stack
            ExecutorTest::new()
                .inst(InstructionKind::BrTable {
                    labels: vec![0],
                    default: 0,
                })
                .expect_error("Stack underflow");
        }

        #[test]
        fn br_table_invalid_label_depth() {
            // br_table with label depth greater than available labels should fail
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 0 })
                .inst(InstructionKind::BrTable {
                    labels: vec![5], // Label 5 doesn't exist (no blocks)
                    default: 0,
                })
                .expect_error("Invalid label");
        }

        #[test]
        fn br_table_invalid_default_label() {
            // br_table with invalid default label
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 99 }) // Out of bounds index
                .inst(InstructionKind::BrTable {
                    labels: vec![0],
                    default: 5, // Invalid default label
                })
                .expect_error("Invalid label");
        }

        #[test]
        fn br_table_mixed_valid_invalid_labels() {
            // Some labels valid, some invalid - should catch during execution
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 1 }) // index 1 -> invalid label 5
                .inst(InstructionKind::BrTable {
                    labels: vec![0, 5], // label 5 doesn't exist
                    default: 0,
                })
                .expect_error("Invalid label");
        }

        #[test]
        fn br_table_very_large_index() {
            // Test with very large index (should use default)
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Value(ValueType::I32),
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::I32Const { value: 1000000 }) // Very large index
                .inst(InstructionKind::BrTable {
                    labels: vec![0],
                    default: 0,
                })
                .inst(InstructionKind::I32Const { value: 99 }) // Should be skipped
                .inst(InstructionKind::End)
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

        #[test]
        fn unimplemented_instruction() {
            ExecutorTest::new()
                .inst(InstructionKind::I32Add)
                .expect_error("Unimplemented instruction");
        }

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
    // Future Test Categories (to be populated as instructions are implemented)
    // ============================================================================

    // Local Variables Tests
    mod local_variables {
        // Tests for local.get, local.set, local.tee will go here
    }

    // Arithmetic Tests
    mod arithmetic {
        // Tests for i32.add, i64.mul, f32.div, etc. will go here
    }

    // Comparison Tests
    mod comparison {
        // Tests for i32.eq, f64.lt, etc. will go here
    }

    // Memory Tests
    mod memory {
        use super::*;
        use crate::parser::instruction::{InstructionKind::*, MemArg};
        use crate::parser::module::{Limits, Memory as MemoryDef};
        use crate::runtime::memory::PAGE_SIZE;

        /// Helper to execute instructions for memory tests
        fn execute_memory_test(
            module: &Module,
            instructions: Vec<Instruction>,
            args: Vec<Value>,
            return_types: &[ValueType],
        ) -> Result<Vec<Value>, RuntimeError> {
            // Build structured function
            let structured_func = StructureBuilder::build_function(&instructions, 0, return_types.to_vec())
                .expect("Structure building should succeed");

            let mut executor = Executor::new(module)?;
            executor.execute_function(&structured_func, args, return_types)
        }

        #[test]
        fn multiple_memories_error() {
            // WebAssembly 1.0 only supports one memory
            let mut module = Module::new("test");
            module.memory.memory.push(MemoryDef {
                limits: Limits { min: 1, max: None },
            });
            module.memory.memory.push(MemoryDef {
                limits: Limits { min: 1, max: None },
            });

            let result = Executor::new(&module);
            assert!(result.is_err());
            let error_msg = result.err().unwrap().to_string();
            assert!(error_msg.contains("only supports one memory"));
        }

        #[test]
        fn memory_size_no_memory() {
            // memory.size with no memory should error
            ExecutorTest::new()
                .inst(InstructionKind::MemorySize)
                .expect_error("No memory instance available");
        }

        #[test]
        fn memory_grow_no_memory() {
            // memory.grow with no memory should error
            ExecutorTest::new()
                .inst(InstructionKind::I32Const { value: 1 })
                .inst(InstructionKind::MemoryGrow)
                .expect_error("No memory instance available");
        }

        #[test]
        fn memory_size_initial() {
            // Create module with 1 page of memory
            let mut module = Module::new("test");
            module.memory.memory.push(MemoryDef {
                limits: Limits { min: 1, max: None },
            });

            let result = execute_memory_test(
                &module,
                vec![
                    make_instruction(InstructionKind::MemorySize),
                    make_instruction(InstructionKind::End),
                ],
                vec![],
                &[ValueType::I32],
            )
            .unwrap();

            assert_eq!(result, vec![Value::I32(1)]);
        }

        #[test]
        fn memory_grow_success() {
            // Create module with 1 page of memory, max 10
            let mut module = Module::new("test");
            module.memory.memory.push(MemoryDef {
                limits: Limits { min: 1, max: Some(10) },
            });

            let mut executor = Executor::new(&module).expect("Executor creation should succeed");
            let instructions = vec![
                make_instruction(InstructionKind::I32Const { value: 2 }),
                make_instruction(InstructionKind::MemoryGrow),
                make_instruction(InstructionKind::MemorySize),
                make_instruction(InstructionKind::End),
            ];
            let func = StructureBuilder::build_function(&instructions, 0, vec![ValueType::I32, ValueType::I32])
                .expect("Structure building should succeed");
            let result = executor
                .execute_function(&func, vec![], &[ValueType::I32, ValueType::I32])
                .unwrap();

            // Should return old size (1) and new size (3)
            assert_eq!(result, vec![Value::I32(1), Value::I32(3)]);
        }

        #[test]
        fn memory_grow_failure_max() {
            // Create module with 1 page of memory, max 2
            let mut module = Module::new("test");
            module.memory.memory.push(MemoryDef {
                limits: Limits { min: 1, max: Some(2) },
            });

            let mut executor = Executor::new(&module).expect("Executor creation should succeed");
            let instructions = vec![
                make_instruction(InstructionKind::I32Const { value: 5 }),
                make_instruction(InstructionKind::MemoryGrow),
                make_instruction(InstructionKind::End),
            ];
            let func = StructureBuilder::build_function(&instructions, 0, vec![ValueType::I32])
                .expect("Structure building should succeed");
            let result = executor.execute_function(&func, vec![], &[ValueType::I32]).unwrap();

            // Should return -1 (failure)
            assert_eq!(result, vec![Value::I32(-1)]);
        }

        #[test]
        fn memory_grow_negative() {
            // Create module with 1 page of memory
            let mut module = Module::new("test");
            module.memory.memory.push(MemoryDef {
                limits: Limits { min: 1, max: None },
            });

            let mut executor = Executor::new(&module).expect("Executor creation should succeed");
            let instructions = vec![
                make_instruction(InstructionKind::I32Const { value: -1 }),
                make_instruction(InstructionKind::MemoryGrow),
                make_instruction(InstructionKind::End),
            ];
            let func = StructureBuilder::build_function(&instructions, 0, vec![ValueType::I32])
                .expect("Structure building should succeed");
            let result = executor.execute_function(&func, vec![], &[ValueType::I32]).unwrap();

            // Should return -1 (failure)
            assert_eq!(result, vec![Value::I32(-1)]);
        }

        #[test]
        fn memory_grow_zero() {
            // Create module with 2 pages of memory
            let mut module = Module::new("test");
            module.memory.memory.push(MemoryDef {
                limits: Limits { min: 2, max: None },
            });

            let mut executor = Executor::new(&module).expect("Executor creation should succeed");
            let instructions = vec![
                make_instruction(InstructionKind::I32Const { value: 0 }),
                make_instruction(InstructionKind::MemoryGrow),
                make_instruction(InstructionKind::End),
            ];
            let func = StructureBuilder::build_function(&instructions, 0, vec![ValueType::I32])
                .expect("Structure building should succeed");
            let result = executor.execute_function(&func, vec![], &[ValueType::I32]).unwrap();

            // Should return current size (2)
            assert_eq!(result, vec![Value::I32(2)]);
        }

        #[test]
        fn memory_operations_sequence() {
            // Test a sequence of memory operations
            let mut module = Module::new("test");
            module.memory.memory.push(MemoryDef {
                limits: Limits { min: 1, max: Some(5) },
            });

            let mut executor = Executor::new(&module).expect("Executor creation should succeed");
            let instructions = vec![
                make_instruction(InstructionKind::MemorySize),
                make_instruction(InstructionKind::I32Const { value: 1 }),
                make_instruction(InstructionKind::MemoryGrow),
                make_instruction(InstructionKind::I32Const { value: 2 }),
                make_instruction(InstructionKind::MemoryGrow),
                make_instruction(InstructionKind::MemorySize),
                make_instruction(InstructionKind::I32Const { value: 2 }),
                make_instruction(InstructionKind::MemoryGrow),
                make_instruction(InstructionKind::End),
            ];
            let func = StructureBuilder::build_function(
                &instructions,
                0,
                vec![
                    ValueType::I32,
                    ValueType::I32,
                    ValueType::I32,
                    ValueType::I32,
                    ValueType::I32,
                ],
            )
            .expect("Structure building should succeed");
            let result = executor
                .execute_function(
                    &func,
                    vec![],
                    &[
                        ValueType::I32,
                        ValueType::I32,
                        ValueType::I32,
                        ValueType::I32,
                        ValueType::I32,
                    ],
                )
                .unwrap();

            // Results: initial size (1), grow result (1), grow result (2),
            // final size (4), failed grow (-1)
            assert_eq!(
                result,
                vec![
                    Value::I32(1),  // Initial size
                    Value::I32(1),  // Old size after first grow
                    Value::I32(2),  // Old size after second grow
                    Value::I32(4),  // Final size
                    Value::I32(-1), // Failed grow
                ]
            );
        }

        #[test]
        fn i32_load_basic() {
            // Create module with 1 page of memory
            let mut module = Module::new("test");
            module.memory.memory.push(MemoryDef {
                limits: Limits { min: 1, max: None },
            });

            let mut executor = Executor::new(&module).expect("Executor creation should succeed");
            let instructions = vec![
                make_instruction(InstructionKind::I32Const { value: 100 }), // address,
                make_instruction(InstructionKind::I32Const { value: 42 }),  // value,
                make_instruction(InstructionKind::I32Store {
                    memarg: MemArg { offset: 0, align: 2 },
                }),
                make_instruction(InstructionKind::I32Const { value: 100 }),
                make_instruction(InstructionKind::I32Load {
                    memarg: MemArg { offset: 0, align: 2 },
                }),
                make_instruction(InstructionKind::End),
            ];
            let func = StructureBuilder::build_function(&instructions, 0, vec![ValueType::I32])
                .expect("Structure building should succeed");
            let result = executor.execute_function(&func, vec![], &[ValueType::I32]).unwrap();

            assert_eq!(result, vec![Value::I32(42)]);
        }

        #[test]
        fn i32_load_with_offset() {
            // Create module with 1 page of memory
            let mut module = Module::new("test");
            module.memory.memory.push(MemoryDef {
                limits: Limits { min: 1, max: None },
            });

            let mut executor = Executor::new(&module).expect("Executor creation should succeed");
            let instructions = vec![
                make_instruction(InstructionKind::I32Const { value: 100 }),
                make_instruction(InstructionKind::I32Const { value: 0x12345678 }),
                make_instruction(InstructionKind::I32Store {
                    memarg: MemArg { offset: 0, align: 2 },
                }),
                make_instruction(InstructionKind::I32Const { value: 90 }),
                make_instruction(InstructionKind::I32Load {
                    memarg: MemArg { offset: 10, align: 2 },
                }),
                make_instruction(InstructionKind::End),
            ];
            let func = StructureBuilder::build_function(&instructions, 0, vec![ValueType::I32])
                .expect("Structure building should succeed");
            let result = executor.execute_function(&func, vec![], &[ValueType::I32]).unwrap();

            assert_eq!(result, vec![Value::I32(0x12345678)]);
        }

        #[test]
        fn i32_store_multiple() {
            // Test storing multiple values
            let mut module = Module::new("test");
            module.memory.memory.push(MemoryDef {
                limits: Limits { min: 1, max: None },
            });

            let mut executor = Executor::new(&module).expect("Executor creation should succeed");
            let instructions = vec![
                make_instruction(InstructionKind::I32Const { value: 0 }),
                make_instruction(InstructionKind::I32Const { value: 100 }),
                make_instruction(InstructionKind::I32Store {
                    memarg: MemArg { offset: 0, align: 2 },
                }),
                make_instruction(InstructionKind::I32Const { value: 4 }),
                make_instruction(InstructionKind::I32Const { value: 200 }),
                make_instruction(InstructionKind::I32Store {
                    memarg: MemArg { offset: 0, align: 2 },
                }),
                make_instruction(InstructionKind::I32Const { value: 8 }),
                make_instruction(InstructionKind::I32Const { value: 300 }),
                make_instruction(InstructionKind::I32Store {
                    memarg: MemArg { offset: 0, align: 2 },
                }),
                make_instruction(InstructionKind::I32Const { value: 0 }),
                make_instruction(InstructionKind::I32Load {
                    memarg: MemArg { offset: 0, align: 2 },
                }),
                make_instruction(InstructionKind::I32Const { value: 4 }),
                make_instruction(InstructionKind::I32Load {
                    memarg: MemArg { offset: 0, align: 2 },
                }),
                make_instruction(InstructionKind::I32Const { value: 8 }),
                make_instruction(InstructionKind::I32Load {
                    memarg: MemArg { offset: 0, align: 2 },
                }),
                make_instruction(InstructionKind::End),
            ];
            let func = StructureBuilder::build_function(
                &instructions,
                0,
                vec![ValueType::I32, ValueType::I32, ValueType::I32],
            )
            .expect("Structure building should succeed");
            let result = executor
                .execute_function(&func, vec![], &[ValueType::I32, ValueType::I32, ValueType::I32])
                .unwrap();

            assert_eq!(result, vec![Value::I32(100), Value::I32(200), Value::I32(300)]);
        }

        #[test]
        fn i32_load_bounds_check() {
            // Test bounds checking
            let mut module = Module::new("test");
            module.memory.memory.push(MemoryDef {
                limits: Limits { min: 1, max: None },
            });

            let mut executor = Executor::new(&module).expect("Executor creation should succeed");

            // Try to load from last valid address (PAGE_SIZE - 4)
            let instructions = vec![
                make_instruction(InstructionKind::I32Const {
                    value: (PAGE_SIZE - 4) as i32,
                }),
                make_instruction(InstructionKind::I32Load {
                    memarg: MemArg { offset: 0, align: 2 },
                }),
                make_instruction(InstructionKind::End),
            ];
            let func = StructureBuilder::build_function(&instructions, 0, vec![ValueType::I32])
                .expect("Structure building should succeed");
            let result = executor.execute_function(&func, vec![], &[ValueType::I32]).unwrap();
            assert_eq!(result, vec![Value::I32(0)]); // Memory is zero-initialised

            // Try to load from out of bounds address
            let instructions = vec![
                make_instruction(InstructionKind::I32Const {
                    value: (PAGE_SIZE - 3) as i32,
                }),
                make_instruction(InstructionKind::I32Load {
                    memarg: MemArg { offset: 0, align: 2 },
                }),
                make_instruction(InstructionKind::End),
            ];
            let func = StructureBuilder::build_function(&instructions, 0, vec![].to_vec())
                .expect("Structure building should succeed");
            let result = executor.execute_function(&func, vec![], &[]);
            assert!(result.is_err());
            assert!(result.unwrap_err().to_string().contains("Out of bounds"));
        }

        #[test]
        fn i32_store_bounds_check() {
            // Test bounds checking for store
            let mut module = Module::new("test");
            module.memory.memory.push(MemoryDef {
                limits: Limits { min: 1, max: None },
            });

            let mut executor = Executor::new(&module).expect("Executor creation should succeed");

            // Try to store at last valid address
            let instructions = vec![
                make_instruction(InstructionKind::I32Const {
                    value: (PAGE_SIZE - 4) as i32,
                }),
                make_instruction(InstructionKind::I32Const { value: 42 }),
                make_instruction(InstructionKind::I32Store {
                    memarg: MemArg { offset: 0, align: 2 },
                }),
                make_instruction(InstructionKind::I32Const { value: 1 }), // Return success indicator,
                make_instruction(InstructionKind::End),
            ];
            let func = StructureBuilder::build_function(&instructions, 0, vec![ValueType::I32])
                .expect("Structure building should succeed");
            let result = executor.execute_function(&func, vec![], &[ValueType::I32]).unwrap();
            assert_eq!(result, vec![Value::I32(1)]);

            // Try to store at out of bounds address
            let instructions = vec![
                make_instruction(InstructionKind::I32Const {
                    value: (PAGE_SIZE - 3) as i32,
                }),
                make_instruction(InstructionKind::I32Const { value: 42 }),
                make_instruction(InstructionKind::I32Store {
                    memarg: MemArg { offset: 0, align: 2 },
                }),
                make_instruction(InstructionKind::End),
            ];
            let func = StructureBuilder::build_function(&instructions, 0, vec![].to_vec())
                .expect("Structure building should succeed");
            let result = executor.execute_function(&func, vec![], &[]);
            assert!(result.is_err());
            assert!(result.unwrap_err().to_string().contains("Out of bounds"));
        }

        #[test]
        fn i32_load_offset_overflow() {
            // Test address calculation overflow
            let mut module = Module::new("test");
            module.memory.memory.push(MemoryDef {
                limits: Limits { min: 1, max: None },
            });

            let mut executor = Executor::new(&module).expect("Executor creation should succeed");
            let instructions = vec![
                make_instruction(InstructionKind::I32Const { value: i32::MAX }),
                make_instruction(InstructionKind::I32Load {
                    memarg: MemArg { offset: 10, align: 2 },
                }),
                make_instruction(InstructionKind::End),
            ];
            let func = StructureBuilder::build_function(&instructions, 0, vec![ValueType::I32])
                .expect("Structure building should succeed");
            let result = executor.execute_function(&func, vec![], &[ValueType::I32]);
            assert!(result.is_err());
            let error_msg = result.unwrap_err().to_string();
            assert!(
                error_msg.contains("overflow") || error_msg.contains("bounds"),
                "Expected overflow or bounds error, got: {}",
                error_msg
            );
        }

        #[test]
        fn i32_load_no_memory() {
            // Test load with no memory
            let module = Module::new("test");
            let result = execute_memory_test(
                &module,
                vec![
                    make_instruction(InstructionKind::I32Const { value: 0 }),
                    make_instruction(InstructionKind::I32Load {
                        memarg: MemArg { offset: 0, align: 2 },
                    }),
                    make_instruction(InstructionKind::End),
                ],
                vec![],
                &[ValueType::I32],
            );
            assert!(result.is_err());
            assert!(result.unwrap_err().to_string().contains("No memory instance available"));
        }

        #[test]
        fn i32_store_no_memory() {
            // Test store with no memory
            let module = Module::new("test");
            let result = execute_memory_test(
                &module,
                vec![
                    make_instruction(InstructionKind::I32Const { value: 0 }),
                    make_instruction(InstructionKind::I32Const { value: 42 }),
                    make_instruction(InstructionKind::I32Store {
                        memarg: MemArg { offset: 0, align: 2 },
                    }),
                    make_instruction(InstructionKind::End),
                ],
                vec![],
                &[],
            );
            assert!(result.is_err());
            assert!(result.unwrap_err().to_string().contains("No memory instance available"));
        }

        #[test]
        fn i32_memory_persistence() {
            // Test that memory persists across multiple function calls with same executor
            let mut module = Module::new("test");
            module.memory.memory.push(MemoryDef {
                limits: Limits { min: 1, max: None },
            });

            let mut executor = Executor::new(&module).expect("Executor creation should succeed");

            // First call: store a value
            let store_instructions = vec![
                make_instruction(InstructionKind::I32Const { value: 100 }),
                make_instruction(InstructionKind::I32Const { value: 42 }),
                make_instruction(InstructionKind::I32Store {
                    memarg: MemArg { offset: 0, align: 2 },
                }),
                make_instruction(InstructionKind::End),
            ];
            let store_func = StructureBuilder::build_function(&store_instructions, 0, vec![])
                .expect("Structure building should succeed");

            executor.execute_function(&store_func, vec![], &[]).unwrap();

            // Second call: load the value back
            let load_instructions = vec![
                make_instruction(InstructionKind::I32Const { value: 100 }),
                make_instruction(InstructionKind::I32Load {
                    memarg: MemArg { offset: 0, align: 2 },
                }),
                make_instruction(InstructionKind::End),
            ];
            let load_func = StructureBuilder::build_function(&load_instructions, 0, vec![ValueType::I32])
                .expect("Structure building should succeed");

            let result = executor
                .execute_function(&load_func, vec![], &[ValueType::I32])
                .unwrap();

            assert_eq!(result, vec![Value::I32(42)]);
        }

        #[test]
        fn i64_load_store() {
            ExecutorTest::new()
                .with_memory()
                .inst(I32Const { value: 8 }) // address
                .inst(I64Const {
                    value: 0x123456789ABCDEF0,
                }) // value
                .inst(I64Store {
                    memarg: MemArg { offset: 0, align: 3 },
                })
                .inst(I32Const { value: 8 }) // address
                .inst(I64Load {
                    memarg: MemArg { offset: 0, align: 3 },
                })
                .returns(vec![ValueType::I64])
                .expect_stack(vec![Value::I64(0x123456789ABCDEF0)]);
        }

        #[test]
        fn f32_load_store() {
            ExecutorTest::new()
                .with_memory()
                .inst(I32Const { value: 16 }) // address
                .inst(F32Const { value: 3.14159 }) // value
                .inst(F32Store {
                    memarg: MemArg { offset: 0, align: 2 },
                })
                .inst(I32Const { value: 16 }) // address
                .inst(F32Load {
                    memarg: MemArg { offset: 0, align: 2 },
                })
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(3.14159)]);
        }

        #[test]
        fn f64_load_store() {
            ExecutorTest::new()
                .with_memory()
                .inst(I32Const { value: 24 }) // address
                .inst(F64Const {
                    value: 2.718281828459045,
                }) // value
                .inst(F64Store {
                    memarg: MemArg { offset: 0, align: 3 },
                })
                .inst(I32Const { value: 24 }) // address
                .inst(F64Load {
                    memarg: MemArg { offset: 0, align: 3 },
                })
                .returns(vec![ValueType::F64])
                .expect_stack(vec![Value::F64(2.718281828459045)]);
        }

        #[test]
        fn i64_load_with_offset() {
            ExecutorTest::new()
                .with_memory()
                .inst(I32Const { value: 0 }) // address
                .inst(I64Const { value: i64::MAX }) // value
                .inst(I64Store {
                    memarg: MemArg { offset: 100, align: 3 },
                })
                .inst(I32Const { value: 0 }) // address
                .inst(I64Load {
                    memarg: MemArg { offset: 100, align: 3 },
                })
                .returns(vec![ValueType::I64])
                .expect_stack(vec![Value::I64(i64::MAX)]);
        }

        #[test]
        fn f32_store_multiple() {
            ExecutorTest::new()
                .with_memory()
                // Store multiple f32 values
                .inst(I32Const { value: 0 })
                .inst(F32Const { value: 1.0 })
                .inst(F32Store {
                    memarg: MemArg { offset: 0, align: 2 },
                })
                .inst(I32Const { value: 4 })
                .inst(F32Const { value: 2.0 })
                .inst(F32Store {
                    memarg: MemArg { offset: 0, align: 2 },
                })
                .inst(I32Const { value: 8 })
                .inst(F32Const { value: 3.0 })
                .inst(F32Store {
                    memarg: MemArg { offset: 0, align: 2 },
                })
                // Load them back
                .inst(I32Const { value: 0 })
                .inst(F32Load {
                    memarg: MemArg { offset: 0, align: 2 },
                })
                .inst(I32Const { value: 4 })
                .inst(F32Load {
                    memarg: MemArg { offset: 0, align: 2 },
                })
                .inst(I32Const { value: 8 })
                .inst(F32Load {
                    memarg: MemArg { offset: 0, align: 2 },
                })
                .returns(vec![ValueType::F32, ValueType::F32, ValueType::F32])
                .expect_stack(vec![Value::F32(1.0), Value::F32(2.0), Value::F32(3.0)]);
        }

        #[test]
        fn f64_bounds_check() {
            ExecutorTest::new()
                .with_memory()
                .inst(I32Const { value: 65536 - 4 }) // Near end of single page
                .inst(F64Const { value: 1.0 })
                .inst(F64Store {
                    memarg: MemArg { offset: 0, align: 3 },
                })
                .expect_error("Out of bounds");
        }

        #[test]
        fn i64_no_memory() {
            ExecutorTest::new()
                .inst(I32Const { value: 0 })
                .inst(I64Load {
                    memarg: MemArg { offset: 0, align: 3 },
                })
                .expect_error("No memory instance");
        }

        #[test]
        fn i32_load8_u() {
            ExecutorTest::new()
                .with_memory()
                .inst(I32Const { value: 0 })
                .inst(I32Const { value: 0xFF }) // 255
                .inst(I32Store8 {
                    memarg: MemArg { offset: 0, align: 0 },
                })
                .inst(I32Const { value: 0 })
                .inst(I32Load8U {
                    memarg: MemArg { offset: 0, align: 0 },
                })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(255)]);
        }

        #[test]
        fn i32_load8_s() {
            ExecutorTest::new()
                .with_memory()
                .inst(I32Const { value: 0 })
                .inst(I32Const { value: 0xFF }) // -1 when sign-extended
                .inst(I32Store8 {
                    memarg: MemArg { offset: 0, align: 0 },
                })
                .inst(I32Const { value: 0 })
                .inst(I32Load8S {
                    memarg: MemArg { offset: 0, align: 0 },
                })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(-1)]);
        }

        #[test]
        fn i32_load16_u() {
            ExecutorTest::new()
                .with_memory()
                .inst(I32Const { value: 0 })
                .inst(I32Const { value: 0xFFFF }) // 65535
                .inst(I32Store16 {
                    memarg: MemArg { offset: 0, align: 1 },
                })
                .inst(I32Const { value: 0 })
                .inst(I32Load16U {
                    memarg: MemArg { offset: 0, align: 1 },
                })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(65535)]);
        }

        #[test]
        fn i32_load16_s() {
            ExecutorTest::new()
                .with_memory()
                .inst(I32Const { value: 0 })
                .inst(I32Const { value: 0xFFFF }) // -1 when sign-extended
                .inst(I32Store16 {
                    memarg: MemArg { offset: 0, align: 1 },
                })
                .inst(I32Const { value: 0 })
                .inst(I32Load16S {
                    memarg: MemArg { offset: 0, align: 1 },
                })
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(-1)]);
        }

        #[test]
        fn i64_load8_u() {
            ExecutorTest::new()
                .with_memory()
                .inst(I32Const { value: 0 })
                .inst(I64Const { value: 0xFF }) // 255
                .inst(I64Store8 {
                    memarg: MemArg { offset: 0, align: 0 },
                })
                .inst(I32Const { value: 0 })
                .inst(I64Load8U {
                    memarg: MemArg { offset: 0, align: 0 },
                })
                .returns(vec![ValueType::I64])
                .expect_stack(vec![Value::I64(255)]);
        }

        #[test]
        fn i64_load32_s() {
            ExecutorTest::new()
                .with_memory()
                .inst(I32Const { value: 0 })
                .inst(I64Const { value: 0xFFFFFFFF }) // -1 when sign-extended from 32-bit
                .inst(I64Store32 {
                    memarg: MemArg { offset: 0, align: 2 },
                })
                .inst(I32Const { value: 0 })
                .inst(I64Load32S {
                    memarg: MemArg { offset: 0, align: 2 },
                })
                .returns(vec![ValueType::I64])
                .expect_stack(vec![Value::I64(-1)]);
        }
    }

    // Structured Execution Tests
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

    // Control Flow Tests
    mod control_flow {
        // Tests for block, loop, br, br_if, etc. will go here
    }

    // Binary Operation Tests
    mod binary_ops {
        use super::*;
        use crate::parser::structured::StructuredInstruction;

        // Helper to create a structured instruction from a regular one
        fn make_structured(kind: InstructionKind) -> StructuredInstruction {
            StructuredInstruction::Plain(make_instruction(kind))
        }

        // f32 addition tests
        #[test]
        fn f32_add_normal() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 1.5 })
                .inst(InstructionKind::F32Const { value: 2.5 })
                .inst(InstructionKind::F32Add)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(4.0)]);
        }

        #[test]
        fn f32_add_inf_inf() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: f32::INFINITY })
                .inst(InstructionKind::F32Const { value: f32::INFINITY })
                .inst(InstructionKind::F32Add)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(f32::INFINITY)]);
        }

        #[test]
        fn f32_add_inf_neg_inf() {
            // Create executor directly to have access to stack
            let module = Module::new("test");
            let mut exec = Executor::new(&module).unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: f32::INFINITY }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const {
                value: f32::NEG_INFINITY,
            }))
            .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Add))
                .unwrap();
            let result = exec.stack.pop_f32().unwrap();
            assert!(result.is_nan(), "inf + -inf should be NaN");
        }

        #[test]
        fn f32_add_zero_neg_zero() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 0.0 })
                .inst(InstructionKind::F32Const { value: -0.0 })
                .inst(InstructionKind::F32Add)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(0.0)]);
        }

        #[test]
        fn f32_add_max_overflow() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: f32::MAX })
                .inst(InstructionKind::F32Const { value: f32::MAX })
                .inst(InstructionKind::F32Add)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(f32::INFINITY)]);
        }

        // f32 subtraction tests
        #[test]
        fn f32_sub_normal() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 5.0 })
                .inst(InstructionKind::F32Const { value: 2.0 })
                .inst(InstructionKind::F32Sub)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(3.0)]);
        }

        #[test]
        fn f32_sub_inf_inf() {
            let module = Module::new("test");
            let mut exec = Executor::new(&module).unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: f32::INFINITY }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: f32::INFINITY }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Sub))
                .unwrap();
            let result = exec.stack.pop_f32().unwrap();
            assert!(result.is_nan(), "inf - inf should be NaN");
        }

        #[test]
        fn f32_sub_zero_zero() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 0.0 })
                .inst(InstructionKind::F32Const { value: 0.0 })
                .inst(InstructionKind::F32Sub)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(0.0)]);
        }

        // f32 multiplication tests
        #[test]
        fn f32_mul_normal() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 3.0 })
                .inst(InstructionKind::F32Const { value: 4.0 })
                .inst(InstructionKind::F32Mul)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(12.0)]);
        }

        #[test]
        fn f32_mul_inf_zero() {
            let module = Module::new("test");
            let mut exec = Executor::new(&module).unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: f32::INFINITY }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: 0.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Mul))
                .unwrap();
            let result = exec.stack.pop_f32().unwrap();
            assert!(result.is_nan(), "inf * 0 should be NaN");
        }

        #[test]
        fn f32_mul_neg_zero() {
            let module = Module::new("test");
            let mut exec = Executor::new(&module).unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: -1.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: 0.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Mul))
                .unwrap();
            let result = exec.stack.pop_f32().unwrap();
            assert_eq!(result, -0.0);
            assert!(result.is_sign_negative(), "Result should be -0.0");
        }

        // f32 division tests
        #[test]
        fn f32_div_normal() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 12.0 })
                .inst(InstructionKind::F32Const { value: 3.0 })
                .inst(InstructionKind::F32Div)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(4.0)]);
        }

        #[test]
        fn f32_div_by_zero() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 1.0 })
                .inst(InstructionKind::F32Const { value: 0.0 })
                .inst(InstructionKind::F32Div)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(f32::INFINITY)]);
        }

        #[test]
        fn f32_div_zero_by_zero() {
            let module = Module::new("test");
            let mut exec = Executor::new(&module).unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: 0.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: 0.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Div))
                .unwrap();
            let result = exec.stack.pop_f32().unwrap();
            assert!(result.is_nan(), "0 / 0 should be NaN");
        }

        #[test]
        fn f32_div_inf_by_inf() {
            let module = Module::new("test");
            let mut exec = Executor::new(&module).unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: f32::INFINITY }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: f32::INFINITY }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Div))
                .unwrap();
            let result = exec.stack.pop_f32().unwrap();
            assert!(result.is_nan(), "inf / inf should be NaN");
        }

        // f32 min tests
        #[test]
        fn f32_min_normal() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 3.0 })
                .inst(InstructionKind::F32Const { value: 5.0 })
                .inst(InstructionKind::F32Min)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(3.0)]);
        }

        #[test]
        fn f32_min_with_nan() {
            let module = Module::new("test");
            let mut exec = Executor::new(&module).unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: 3.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: f32::NAN }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Min))
                .unwrap();
            let result = exec.stack.pop_f32().unwrap();
            assert!(result.is_nan(), "min with NaN should propagate NaN");
        }

        #[test]
        fn f32_min_zeros() {
            let module = Module::new("test");
            let mut exec = Executor::new(&module).unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: 0.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: -0.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Min))
                .unwrap();
            let result = exec.stack.pop_f32().unwrap();
            assert_eq!(result, -0.0);
            assert!(result.is_sign_negative(), "min(0.0, -0.0) should be -0.0");
        }

        // f32 max tests
        #[test]
        fn f32_max_normal() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 3.0 })
                .inst(InstructionKind::F32Const { value: 5.0 })
                .inst(InstructionKind::F32Max)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(5.0)]);
        }

        #[test]
        fn f32_max_with_nan() {
            let module = Module::new("test");
            let mut exec = Executor::new(&module).unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: 3.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: f32::NAN }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Max))
                .unwrap();
            let result = exec.stack.pop_f32().unwrap();
            assert!(result.is_nan(), "max with NaN should propagate NaN");
        }

        #[test]
        fn f32_max_zeros() {
            let module = Module::new("test");
            let mut exec = Executor::new(&module).unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: 0.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: -0.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Max))
                .unwrap();
            let result = exec.stack.pop_f32().unwrap();
            assert_eq!(result, 0.0);
            assert!(!result.is_sign_negative(), "max(0.0, -0.0) should be 0.0");
        }

        // f32 copysign tests
        #[test]
        fn f32_copysign_positive() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: -5.0 })
                .inst(InstructionKind::F32Const { value: 1.0 })
                .inst(InstructionKind::F32Copysign)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(5.0)]);
        }

        #[test]
        fn f32_copysign_negative() {
            ExecutorTest::new()
                .inst(InstructionKind::F32Const { value: 5.0 })
                .inst(InstructionKind::F32Const { value: -1.0 })
                .inst(InstructionKind::F32Copysign)
                .returns(vec![ValueType::F32])
                .expect_stack(vec![Value::F32(-5.0)]);
        }

        #[test]
        fn f32_copysign_nan() {
            let module = Module::new("test");
            let mut exec = Executor::new(&module).unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: f32::NAN }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Const { value: -1.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F32Copysign))
                .unwrap();
            let result = exec.stack.pop_f32().unwrap();
            assert!(result.is_nan(), "Result should be NaN");
            assert!(result.is_sign_negative(), "NaN should have negative sign");
        }

        // f64 tests (selected edge cases)
        #[test]
        fn f64_add_inf_neg_inf() {
            let module = Module::new("test");
            let mut exec = Executor::new(&module).unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F64Const { value: f64::INFINITY }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F64Const {
                value: f64::NEG_INFINITY,
            }))
            .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F64Add))
                .unwrap();
            let result = exec.stack.pop_f64().unwrap();
            assert!(result.is_nan(), "inf + -inf should be NaN");
        }

        #[test]
        fn f64_mul_inf_zero() {
            let module = Module::new("test");
            let mut exec = Executor::new(&module).unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F64Const { value: f64::INFINITY }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F64Const { value: 0.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F64Mul))
                .unwrap();
            let result = exec.stack.pop_f64().unwrap();
            assert!(result.is_nan(), "inf * 0 should be NaN");
        }

        #[test]
        fn f64_div_zero_by_zero() {
            let module = Module::new("test");
            let mut exec = Executor::new(&module).unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F64Const { value: 0.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F64Const { value: 0.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F64Div))
                .unwrap();
            let result = exec.stack.pop_f64().unwrap();
            assert!(result.is_nan(), "0 / 0 should be NaN");
        }

        #[test]
        fn f64_min_zeros() {
            let module = Module::new("test");
            let mut exec = Executor::new(&module).unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F64Const { value: 0.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F64Const { value: -0.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F64Min))
                .unwrap();
            let result = exec.stack.pop_f64().unwrap();
            assert_eq!(result, -0.0);
            assert!(result.is_sign_negative(), "min(0.0, -0.0) should be -0.0");
        }

        #[test]
        fn f64_max_zeros() {
            let module = Module::new("test");
            let mut exec = Executor::new(&module).unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F64Const { value: 0.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F64Const { value: -0.0 }))
                .unwrap();
            exec.execute_instruction(&make_structured(InstructionKind::F64Max))
                .unwrap();
            let result = exec.stack.pop_f64().unwrap();
            assert_eq!(result, 0.0);
            assert!(!result.is_sign_negative(), "max(0.0, -0.0) should be 0.0");
        }

        #[test]
        fn f64_copysign_inf() {
            ExecutorTest::new()
                .inst(InstructionKind::F64Const { value: f64::INFINITY })
                .inst(InstructionKind::F64Const { value: -1.0 })
                .inst(InstructionKind::F64Copysign)
                .returns(vec![ValueType::F64])
                .expect_stack(vec![Value::F64(f64::NEG_INFINITY)]);
        }
    }
}
