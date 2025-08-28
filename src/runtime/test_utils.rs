//! Test utilities for runtime testing
//!
//! This module provides test helpers that can be used by any runtime test
//! without creating circular dependencies.

#[cfg(test)]
pub mod test {
    use crate::parser::instruction::{ByteRange, Instruction, InstructionKind};
    use crate::parser::module::{Module, ValueType};
    use crate::parser::structure_builder::StructureBuilder;
    use crate::runtime::executor::Executor;
    use crate::runtime::Value;

    /// Test builder for creating executor tests fluently
    pub struct ExecutorTest {
        instructions: Vec<Instruction>,
        args: Vec<Value>,
        return_types: Vec<ValueType>,
        with_memory: bool,
    }

    impl ExecutorTest {
        pub fn new() -> Self {
            ExecutorTest {
                instructions: Vec::new(),
                args: Vec::new(),
                return_types: Vec::new(),
                with_memory: false,
            }
        }

        pub fn with_memory(mut self) -> Self {
            self.with_memory = true;
            self
        }

        pub fn inst(mut self, kind: InstructionKind) -> Self {
            self.instructions.push(make_instruction(kind));
            self
        }

        pub fn arg(mut self, value: Value) -> Self {
            self.args.push(value);
            self
        }

        pub fn args(mut self, args: Vec<Value>) -> Self {
            self.args = args;
            self
        }

        pub fn returns(mut self, types: Vec<ValueType>) -> Self {
            self.return_types = types;
            self
        }

        pub fn expect_stack(mut self, expected: Vec<Value>) {
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

        pub fn expect_error(mut self, error_contains: &str) {
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
            let structured_func = StructureBuilder::build_function(&self.instructions, 0, self.return_types.clone())
                .expect("Structure building should succeed");

            let mut executor = Executor::new(&module).expect("Executor creation should succeed");
            let result = executor.execute_function(&structured_func, self.args, &self.return_types);

            match result {
                Err(e) => {
                    let error_string = e.to_string();
                    assert!(
                        error_string.contains(error_contains),
                        "Expected error containing '{}', got: '{}'",
                        error_contains,
                        error_string
                    );
                }
                Ok(_) => panic!(
                    "Expected error containing '{}', but execution succeeded",
                    error_contains
                ),
            }
        }
    }

    pub fn make_instruction(kind: InstructionKind) -> Instruction {
        Instruction {
            kind,
            position: ByteRange { offset: 0, length: 1 },
            original_bytes: vec![],
        }
    }
}
