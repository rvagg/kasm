//! WebAssembly instruction executor

use super::{stack::Stack, RuntimeError, Value};
use crate::parser::instruction::{Instruction, InstructionKind};
use crate::parser::module::{Module, ValueType};

/// Executes WebAssembly instructions
pub struct Executor<'a> {
    #[allow(dead_code)] // Will be used in the future
    module: &'a Module,
    stack: Stack,
}

impl<'a> Executor<'a> {
    /// Create a new executor for a module
    pub fn new(module: &'a Module) -> Self {
        Executor {
            module,
            stack: Stack::new(),
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
        // Push arguments onto the stack
        self.stack.push_all(args);

        // Execute instructions
        self.execute_instructions(instructions)?;

        // Pop return values in reverse order (WebAssembly stack convention)
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
        for instruction in instructions {
            self.execute_instruction(instruction)?;
        }
        Ok(())
    }

    /// Execute a single instruction
    fn execute_instruction(&mut self, instruction: &Instruction) -> Result<(), RuntimeError> {
        use InstructionKind::*;

        match &instruction.kind {
            // ----------------------------------------------------------------
            // 4.4.1 Numeric Instructions
            //
            // 𝑡.const 𝑐
            // 1. Push the value 𝑡.const 𝑐 to the stack.
            I32Const { value } => {
                self.stack.push(Value::I32(*value));
                Ok(())
            }
            I64Const { value } => {
                self.stack.push(Value::I64(*value));
                Ok(())
            }
            F32Const { value } => {
                self.stack.push(Value::F32(*value));
                Ok(())
            }
            F64Const { value } => {
                self.stack.push(Value::F64(*value));
                Ok(())
            }

            // ----------------------------------------------------------------
            // Control (4.4.8 Control Instructions)
            // nop
            // 1. Do nothing.
            Nop => Ok(()),

            // End of block/function - handled by validation so considered
            // implicit by blocks that require it and can be ignored here.
            End => Ok(()),

            // ----------------------------------------------------------------
            // Parametric (4.4.4 Parametric Instructions)
            // drop
            // 1. Assert: due to validation, a value is on the top of the stack.
            // 2. Pop the value val from the stack.
            Drop => {
                self.stack.pop()?;
                Ok(())
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
            assert!(
                result.unwrap_err().to_string().contains(error_contains),
                "Error should contain '{}'",
                error_contains
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
                .args(vec![Value::I32(1), Value::I32(2), Value::I32(3)])
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
            ExecutorTest::new()
                .args(vec![Value::I32(42)])
                .returns(vec![ValueType::I32])
                .expect_stack(vec![Value::I32(42)]);
        }

        #[test]
        fn multiple_args_passthrough() {
            ExecutorTest::new()
                .args(vec![Value::I32(1), Value::I64(2), Value::F32(3.0)])
                .returns(vec![ValueType::I32, ValueType::I64, ValueType::F32])
                .expect_stack(vec![Value::I32(1), Value::I64(2), Value::F32(3.0)]);
        }

        #[test]
        fn args_with_operations() {
            ExecutorTest::new()
                .args(vec![Value::I32(42)])
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
                .inst(InstructionKind::LocalGet { local_idx: 0 })
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
