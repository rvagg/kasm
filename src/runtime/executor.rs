//! WebAssembly instruction executor

use super::{
    control::{Label, LabelStack, LabelType},
    frame::Frame,
    stack::Stack,
    RuntimeError, Value,
};
use crate::parser::instruction::{Instruction, InstructionKind};
use crate::parser::module::{Module, ValueType};

/// Result of executing an instruction
///
/// This enum controls the program counter (pc) after instruction execution:
/// - `Continue`: Increment pc by 1 to execute the next instruction
/// - `Branch(depth)`: Jump to a different location (handled by `handle_branch`)
/// - `SkipToElseOrEnd`: Skip forward to else or end (used by if/else control flow)
/// - `Return`: Exit the current function
#[derive(Debug)]
enum ExecutionResult {
    /// Continue to next instruction (pc += 1)
    Continue,
    /// Branch to a label at given depth (pc = calculated branch target)
    Branch(u32),
    /// Skip to next else or end at current nesting level
    SkipToElseOrEnd,
    /// Return from the current function
    Return,
}

/// Executes WebAssembly instructions
pub struct Executor<'a> {
    #[allow(dead_code)] // Will be used in the future
    module: &'a Module,
    stack: Stack,
    /// Current execution frame (contains locals)
    frame: Option<Frame>,
    /// Label stack for control flow
    label_stack: LabelStack,
}

impl<'a> Executor<'a> {
    /// Create a new executor for a module
    pub fn new(module: &'a Module) -> Self {
        Executor {
            module,
            stack: Stack::new(),
            frame: None,
            label_stack: LabelStack::new(),
        }
    }

    /// Execute a function
    pub fn execute_function(
        &mut self,
        _func_idx: u32,
        instructions: &[Instruction],
        args: Vec<Value>,
        return_types: &[ValueType],
    ) -> Result<Vec<Value>, RuntimeError> {
        // Create frame with arguments as initial locals
        // Function parameters are the first locals
        let frame = Frame::new(args);
        self.frame = Some(frame);

        // Execute instructions
        self.execute_instructions(instructions)?;

        // Pop return values in reverse order
        let mut results = Vec::new();
        for return_type in return_types.iter().rev() {
            let value = self.stack.pop_typed(*return_type)?;
            results.push(value);
        }
        results.reverse();

        Ok(results)
    }

    /// Execute a sequence of instructions
    fn execute_instructions(&mut self, instructions: &[Instruction]) -> Result<(), RuntimeError> {
        let mut pc = 0; // Program counter

        while pc < instructions.len() {
            // Execute the instruction and determine how to update the program counter
            match self.execute_instruction(&instructions[pc])? {
                ExecutionResult::Continue => pc += 1, // Normal flow: move to next instruction
                ExecutionResult::Branch(depth) => {
                    // Branch flow: calculate and jump to branch target
                    pc = self.handle_branch(pc, instructions, depth)?;
                }
                ExecutionResult::SkipToElseOrEnd => {
                    // Skip to else/end for if control flow
                    pc = self.skip_to_else_or_end(pc, instructions)?;
                }
                ExecutionResult::Return => {
                    // Exit the function early
                    break;
                }
            }
        }
        Ok(())
    }

    /// Handle a branch instruction by calculating the target program counter
    ///
    /// This method implements the WebAssembly branch semantics:
    /// 1. Pops values from the stack based on the target label's arity
    /// 2. Pops labels from the label stack up to and including the target
    /// 3. Pushes the values back onto the stack
    /// 4. Returns the new program counter position
    ///
    /// We have deal with structured control flow without explicit jump
    /// addresses. When we encounter a branch instruction, we only know the
    /// label depth (e.g., "branch to the 2nd enclosing block"), not the
    /// actual instruction address to jump to.
    ///
    /// - **For blocks**: Branch to the end → scan forward to find matching `end`
    /// - **For loops**: Branch to the beginning → scan backward to find matching `loop`
    ///
    /// The scanning must track nesting levels to handle nested control
    /// structures correctly.
    ///
    /// # Returns
    /// The new program counter position to continue execution from
    fn handle_branch(
        &mut self,
        current_pc: usize,
        instructions: &[Instruction],
        depth: u32,
    ) -> Result<usize, RuntimeError> {
        // Get the target label
        // From the spec (4.4.8 br l):
        // "Let L be the l-th label appearing on the stack, starting from the top and counting from zero"
        let label = self
            .label_stack
            .get(depth)
            .ok_or_else(|| RuntimeError::UnimplementedInstruction(format!("invalid branch depth {depth}")))?;

        // Get the arity and check if it's a loop
        // From the spec: "Let n be the arity of L"
        let arity = label.arity()?;
        let is_loop = label.label_type == LabelType::Loop;

        // Pop arity values from stack (these will be the block results)
        // We collect them in a temporary vec because we need to:
        // 1. Pop them all before popping labels (to maintain stack discipline)
        // 2. Push them back in the same order after popping labels
        let mut values = Vec::with_capacity(arity);
        for _ in 0..arity {
            values.push(self.stack.pop()?);
        }
        values.reverse(); // Restore original order

        // Pop control frames up to and including the target
        for _ in 0..=depth {
            self.label_stack.pop();
        }

        // Push values back in their original order
        for value in values {
            self.stack.push(value);
        }

        // Find the target position based on the label type
        if is_loop {
            // LOOP BRANCH: Jump to the beginning of the loop
            //
            // Loops branch to their start, but we only know our current
            // position. We must scan backwards through the instruction
            // stream to find where this loop began.
            //
            // We might encounter other loops/blocks while scanning
            // backward so use nesting_level to ensure we match the correct loop.
            let mut scan_pc = current_pc;
            let mut nesting_level = 0;
            let mut target_depth = depth as i32;

            // Scan backward through instructions
            while scan_pc > 0 {
                scan_pc -= 1;
                match &instructions[scan_pc].kind {
                    InstructionKind::End => {
                        // Found an 'end' while going backward - we're entering a nested structure
                        nesting_level += 1;
                    }
                    InstructionKind::Block { .. } | InstructionKind::Loop { .. } | InstructionKind::If { .. } => {
                        if nesting_level == 0 {
                            // Not inside a nested structure
                            if target_depth == 0 {
                                // Found our target loop!
                                // Return the loop instruction position so it gets re-executed
                                // (which will re-push the label onto the label stack)
                                return Ok(scan_pc);
                            }
                            // This is a label, but not our target - keep searching
                            target_depth -= 1;
                        } else {
                            // Exiting a nested structure
                            nesting_level -= 1;
                        }
                    }
                    _ => {} // Other instructions don't affect our search
                }
            }

            // Should not reach here - indicates malformed bytecode that got
            // past the validator.
            Err(RuntimeError::UnimplementedInstruction(
                "could not find loop start".to_string(),
            ))
        } else {
            // BLOCK BRANCH: Jump to the end of the block
            //
            // Blocks branch to their end. We need to find the matching 'end'
            // instruction for our target block.
            //
            // We need to skip 'depth' labels PLUS the current block we're
            // branching out of.
            let mut block_depth = depth + 1;
            let mut new_pc = current_pc + 1;

            // Scan forward through instructions
            while new_pc < instructions.len() && block_depth > 0 {
                match &instructions[new_pc].kind {
                    InstructionKind::Block { .. } | InstructionKind::Loop { .. } | InstructionKind::If { .. } => {
                        // Entering a nested structure
                        block_depth += 1;
                    }
                    InstructionKind::End => {
                        // Exiting a structure
                        block_depth -= 1;
                        if block_depth == 0 {
                            // Found the target! new_pc is now just after the 'end'
                            break;
                        }
                    }
                    _ => {} // Other instructions don't affect our search
                }
                new_pc += 1;
            }

            Ok(new_pc)
        }
    }

    /// Skip to the next else or end at the current nesting level
    ///
    /// This is used when:
    /// 1. An if condition is false - skip to else (to execute else branch) or end
    /// 2. We hit else during then branch - skip to end
    ///
    /// In both cases, we just skip to whatever comes first (else or end) at our
    /// nesting level. The logic works because:
    /// - After if: we'll find else or end
    /// - After else: we'll only find end (no else after else)
    fn skip_to_else_or_end(&mut self, current_pc: usize, instructions: &[Instruction]) -> Result<usize, RuntimeError> {
        let mut pc = current_pc + 1;
        let mut nesting_level = 0;

        while pc < instructions.len() {
            match &instructions[pc].kind {
                // Entering nested structures increases nesting
                InstructionKind::Block { .. } | InstructionKind::Loop { .. } | InstructionKind::If { .. } => {
                    nesting_level += 1;
                }
                // Found else
                InstructionKind::Else => {
                    if nesting_level == 0 {
                        // This is at our level - stop here
                        return Ok(pc + 1); // Skip past the else instruction
                    }
                    // Else at a different nesting level, ignore it
                }
                // Found end
                InstructionKind::End => {
                    if nesting_level == 0 {
                        // This is our if's end - don't pop label here,
                        // the End instruction will handle it
                        return Ok(pc);
                    }
                    nesting_level -= 1;
                }
                _ => {}
            }
            pc += 1;
        }

        Err(RuntimeError::UnimplementedInstruction(
            "could not find matching else or end for if".to_string(),
        ))
    }

    /// Execute a single instruction
    fn execute_instruction(&mut self, instruction: &Instruction) -> Result<ExecutionResult, RuntimeError> {
        use InstructionKind::*;

        match &instruction.kind {
            // ----------------------------------------------------------------
            // 4.4.1 Numeric Instructions
            //
            // 𝑡.const 𝑐
            // 1. Push the value 𝑡.const 𝑐 to the stack.
            I32Const { value } => {
                self.stack.push(Value::I32(*value));
                Ok(ExecutionResult::Continue)
            }
            I64Const { value } => {
                self.stack.push(Value::I64(*value));
                Ok(ExecutionResult::Continue)
            }
            F32Const { value } => {
                self.stack.push(Value::F32(*value));
                Ok(ExecutionResult::Continue)
            }
            F64Const { value } => {
                self.stack.push(Value::F64(*value));
                Ok(ExecutionResult::Continue)
            }

            // ----------------------------------------------------------------
            // Control (4.4.8 Control Instructions)
            // nop
            // 1. Do nothing.
            Nop => Ok(ExecutionResult::Continue),

            // ----------------------------------------------------------------
            // Parametric (4.4.4 Parametric Instructions)
            // drop
            // 1. Assert: due to validation, a value is on the top of the stack.
            // 2. Pop the value val from the stack.
            Drop => {
                self.stack.pop()?;
                Ok(ExecutionResult::Continue)
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
                Ok(ExecutionResult::Continue)
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
                Ok(ExecutionResult::Continue)
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
                Ok(ExecutionResult::Continue)
            }

            // ----------------------------------------------------------------
            // 4.4.8 Control Instructions
            // See: https://webassembly.github.io/spec/core/exec/instructions.html#control-instructions
            //
            // block blocktype instr* end
            // From the spec (4.4.8.1):
            // 1. Let 𝐹 be the current frame.
            // 2. Assert: due to validation, expand𝐹(blocktype) is defined.
            // 3. Let [𝑡ᵐ₁] → [𝑡ⁿ₂] be the function type expand𝐹(blocktype).
            // 4. Let 𝐿 be the label whose arity is 𝑛 and whose continuation is the end of the block.
            // 5. Assert: due to validation, there are at least 𝑚 values on the top of the stack.
            // 6. Pop the values val^𝑚 from the stack.
            // 7. Enter the block instr* with label 𝐿 and values val^𝑚.
            Block { block_type } => {
                // Create label as per spec step 4
                // "Let L be the label whose arity is n and whose continuation is the end of the block"
                let label = Label {
                    label_type: LabelType::Block,
                    block_type: block_type.clone(),
                    stack_height: self.stack.len(),
                    unreachable: false,
                };
                // Extend label stack as per spec: "gets extended with new labels when entering structured control instructions"
                self.label_stack.push(label);
                Ok(ExecutionResult::Continue)
            }

            // loop blocktype instr* end
            // From the spec (4.4.8.2):
            // "Let L be the label whose arity is n and whose continuation is the start of the loop"
            // Note: Unlike blocks, "the label of a loop does not target the end, but the beginning of the loop"
            Loop { block_type } => {
                let label = Label {
                    label_type: LabelType::Loop,
                    block_type: block_type.clone(),
                    stack_height: self.stack.len(),
                    unreachable: false,
                };
                // Extend label stack as per spec: "gets extended with new labels when entering structured control instructions"
                self.label_stack.push(label);
                Ok(ExecutionResult::Continue)
            }

            // if blocktype instr* else instr* end
            // From the spec (4.4.8.3):
            // The if instruction is a conditional: it pops an i32 condition and executes
            // one of two instruction sequences based on its value.
            If { block_type } => {
                // Pop condition value
                let condition = self.stack.pop_i32()?;

                // Push label for the if construct
                let label = Label {
                    label_type: LabelType::If,
                    block_type: block_type.clone(),
                    stack_height: self.stack.len(),
                    unreachable: false,
                };
                self.label_stack.push(label);

                if condition != 0 {
                    // Non-zero: execute the 'then' branch
                    Ok(ExecutionResult::Continue)
                } else {
                    // Zero: skip to 'else' or 'end'
                    Ok(ExecutionResult::SkipToElseOrEnd)
                }
            }

            // else
            // From the spec: else marks the beginning of the else branch
            // This instruction can be reached in two ways:
            // 1. From skip_to_else_or_end when if condition was false
            // 2. During execution of the 'then' branch (need to skip to end)
            Else => {
                // Check if we're in an if block
                if let Some(label) = self.label_stack.get(0) {
                    if label.label_type == LabelType::If {
                        // We're executing the then branch and hit else, skip to end
                        Ok(ExecutionResult::SkipToElseOrEnd)
                    } else {
                        Err(RuntimeError::UnimplementedInstruction(
                            "else not inside if block".to_string(),
                        ))
                    }
                } else {
                    Err(RuntimeError::UnimplementedInstruction("else without if".to_string()))
                }
            }

            // end
            // Handled implicitly by block/loop/if execution
            End => {
                // Pop control frame if there is one
                if !self.label_stack.is_empty() {
                    self.label_stack.pop();
                }
                Ok(ExecutionResult::Continue)
            }

            // br l
            // From the spec (4.4.8 br l):
            // 1. Assert: due to validation, the stack contains at least l + 1 labels.
            // 2. Let L be the l-th label appearing on the stack, starting from the top and counting from zero.
            // 3. Let n be the arity of L.
            // 4. Assert: due to validation, there are at least n values on the top of the stack.
            // 5. Pop the values val^n from the stack.
            // 6. Repeat l + 1 times: pop a label from the stack.
            // 7. Push the values val^n to the stack.
            // 8. Jump to the continuation of L.
            Br { label_idx } => Ok(ExecutionResult::Branch(*label_idx)),

            // br_if l
            // From the spec (4.4.8 br_if l):
            // 1. Assert: due to validation, a value of type i32 is on the top of the stack.
            // 2. Pop the value c from the stack.
            // 3. If c is non-zero, then execute the instruction br l.
            // 4. Else, do nothing.
            BrIf { label_idx } => {
                let condition = self.stack.pop_i32()?;
                if condition != 0 {
                    Ok(ExecutionResult::Branch(*label_idx))
                } else {
                    Ok(ExecutionResult::Continue)
                }
            }

            // br_table l* lN
            // From the spec (4.4.8 br_table l* lN):
            // A br_table performs an indirect branch through an operand indexing into
            // a list of labels.
            // 1. Pop i32 index from stack
            // 2. If index < len(labels), branch to labels[index]
            // 3. Else branch to default
            BrTable { labels, default } => {
                let index = self.stack.pop_i32()?;

                // Choose the target label based on index
                let target = if index >= 0 && (index as usize) < labels.len() {
                    labels[index as usize]
                } else {
                    *default
                };

                Ok(ExecutionResult::Branch(target))
            }

            // return
            // From the spec (4.4.8 return):
            // The return instruction is a shortcut for an unconditional branch
            // to the outermost block, which implicitly is the body of the current function.
            //
            // Note: We handle this differently than a branch - we don't pop labels
            // or manipulate the stack. The function epilogue in execute_function
            // will handle popping the return values.
            Return => Ok(ExecutionResult::Return),

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

    /// Test builder for creating executor tests fluently
    struct ExecutorTest {
        instructions: Vec<Instruction>,
        args: Vec<Value>,
        return_types: Vec<ValueType>,
    }

    impl ExecutorTest {
        fn new() -> Self {
            ExecutorTest {
                instructions: Vec::new(),
                args: Vec::new(),
                return_types: Vec::new(),
            }
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
            let module = Module::new("test");
            let mut executor = Executor::new(&module);
            let results = executor
                .execute_function(0, &self.instructions, self.args, &self.return_types)
                .expect("Execution should succeed");
            assert_eq!(results, expected);
        }

        fn expect_error(mut self, error_contains: &str) {
            self.instructions.push(make_instruction(InstructionKind::End));
            let module = Module::new("test");
            let mut executor = Executor::new(&module);
            let result = executor.execute_function(0, &self.instructions, self.args, &self.return_types);
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
                    block_type: crate::parser::instruction::BlockType::Empty,
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
                    block_type: crate::parser::instruction::BlockType::Empty,
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
                    block_type: crate::parser::instruction::BlockType::Empty,
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
                    block_type: crate::parser::instruction::BlockType::Empty,
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
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Empty,
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
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Empty,
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
                    block_type: crate::parser::instruction::BlockType::Empty,
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
                    block_type: crate::parser::instruction::BlockType::Empty,
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
                .expect_error("invalid branch depth");
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
                .expect_error("invalid branch depth");
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
                .expect_error("invalid branch depth");
        }

        #[test]
        fn br_table_very_large_index() {
            // Test with very large index (should use default)
            ExecutorTest::new()
                .inst(InstructionKind::Block {
                    block_type: crate::parser::instruction::BlockType::Empty,
                })
                .inst(InstructionKind::I32Const { value: 42 })
                .inst(InstructionKind::I32Const { value: 1000000 }) // Very large index
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
        // Tests for load/store operations will go here
    }

    // Control Flow Tests
    mod control_flow {
        // Tests for block, loop, br, br_if, etc. will go here
    }
}
