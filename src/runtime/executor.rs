//! WebAssembly instruction executor

use super::{
    control::{Label, LabelStack, LabelType},
    frame::Frame,
    memory::Memory,
    ops,
    stack::Stack,
    RuntimeError, Value,
};
use crate::parser::instruction::{Instruction, InstructionKind};
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
            // nop
            // 1. Do nothing.
            Nop => Ok(BlockEnd::Normal),

            // ----------------------------------------------------------------
            // 4.4.4 Parametric Instructions
            //
            Drop => {
                ops::parametric::drop(&mut self.stack)?;
                Ok(BlockEnd::Normal)
            }

            // 4.4.5 Variable Instructions
            LocalGet { local_idx } => {
                let frame = self.frame.as_ref().ok_or(RuntimeError::InvalidFunctionType)?;
                ops::variable::local_get(&mut self.stack, frame, *local_idx)?;
                Ok(BlockEnd::Normal)
            }

            LocalSet { local_idx } => {
                let frame = self.frame.as_mut().ok_or(RuntimeError::InvalidFunctionType)?;
                ops::variable::local_set(&mut self.stack, frame, *local_idx)?;
                Ok(BlockEnd::Normal)
            }

            LocalTee { local_idx } => {
                let frame = self.frame.as_mut().ok_or(RuntimeError::InvalidFunctionType)?;
                ops::variable::local_tee(&mut self.stack, frame, *local_idx)?;
                Ok(BlockEnd::Normal)
            }

            // 4.4.8 Control Instructions
            Br { label_idx } => ops::control::br(&mut self.stack, &self.label_stack, *label_idx),

            BrIf { label_idx } => ops::control::br_if(&mut self.stack, &self.label_stack, *label_idx),

            BrTable { labels, default } => ops::control::br_table(&mut self.stack, &self.label_stack, labels, *default),

            Return => ops::control::return_op(),

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

        #[test]
        fn unimplemented_instruction() {
            ExecutorTest::new()
                .inst(InstructionKind::I32WrapI64) // Conversion operations not yet implemented
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
}
