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
//! │  │ Function Space (FuncAddr → FunctionInstance)           │ │
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
//! The call stack enables arbitrarily deep cross-module chains (A → B → C → ...)
//! where each module can perform computation before/after its external calls.

use super::{ExecutionOutcome, Instance, RuntimeError, Value};
use crate::parser::module::{ExternalKind, FunctionType};

/// Type alias for host function implementations
type HostFunc = Box<dyn Fn(Vec<Value>) -> Result<Vec<Value>, RuntimeError>>;

/// Global function address - index into the Store's function space
///
/// FuncAddr provides stable, globally-unique identifiers for functions that work
/// across module boundaries, enabling proper funcref semantics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncAddr(pub usize);

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
}

impl<'a> Store<'a> {
    /// Create a new empty Store
    pub fn new() -> Self {
        Store {
            functions: Vec::new(),
            instances: Vec::new(),
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

    /// Create and register a new instance in the Store
    ///
    /// This handles the full lifecycle:
    /// 1. Creates the Instance
    /// 2. Allocates FuncAddr for all its functions
    /// 3. Adds it to the Store
    ///
    /// Returns the instance ID
    pub fn create_instance(
        &mut self,
        module: &'a crate::parser::module::Module,
        imports: Option<&super::ImportObject>,
    ) -> Result<usize, RuntimeError> {
        let instance_id = self.instances.len();

        // Create instance (without function addresses yet)
        let mut instance = Instance::new_unlinked(module, imports)?;

        // Allocate function addresses
        let num_imported_funcs = module.imports.function_count();
        let mut function_addresses = Vec::new();

        // Imported functions - get FuncAddr from imports and validate types
        for import in &module.imports.imports {
            if let ExternalKind::Function(type_idx) = &import.external_kind {
                if let Some(import_obj) = imports {
                    // Get the imported function address
                    let addr = import_obj.get_function(&import.module, &import.name)?;

                    // Get the expected type from the module's type section
                    let expected_type = module.types.get(*type_idx).ok_or(RuntimeError::InvalidFunctionType)?;

                    // Get the actual type from the imported function
                    let actual_type = self.get_function_type(addr)?;

                    // Validate that the types match
                    if expected_type != actual_type {
                        return Err(RuntimeError::ImportTypeMismatch {
                            module: import.module.clone(),
                            name: import.name.clone(),
                            expected: format!("{:?}", expected_type),
                            actual: format!("{:?}", actual_type),
                        });
                    }

                    function_addresses.push(addr);
                } else {
                    return Err(RuntimeError::UnknownFunction(format!(
                        "Import {}.{} not found",
                        import.module, import.name
                    )));
                }
            }
        }

        // Local functions - allocate new FuncAddr
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
            function_addresses.push(addr);
        }

        // Link the instance with its function addresses
        // This also initialises element segments now that addresses are available
        instance.link_functions(function_addresses)?;

        // Execute start function if present
        instance.execute_start()?;

        // Add instance to store
        self.instances.push(instance);

        Ok(instance_id)
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

    /// Execute a function by its address
    ///
    /// This is the main entry point for calling any function in the Store,
    /// whether it's a WebAssembly function or a host function.
    /// Handles cross-module calls automatically using a call stack.
    ///
    /// # Errors
    /// - Returns error if FuncAddr is invalid
    /// - Returns any errors from function execution
    pub fn execute(&mut self, addr: FuncAddr, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
        // Call stack tracks instances waiting for results from external calls
        let mut call_stack: Vec<usize> = Vec::new();

        // What we need to do next: either call a function or resume an instance
        let mut pending_call: Option<(FuncAddr, Vec<Value>)> = Some((addr, args));
        let mut pending_resume: Option<(usize, Vec<Value>)> = None;

        loop {
            // Get the outcome from either a new call or a resume
            let (outcome, source_instance) = if let Some((addr, args)) = pending_call.take() {
                self.execute_one(addr, args)?
            } else if let Some((instance_id, results)) = pending_resume.take() {
                let instance = self
                    .get_instance_mut(instance_id)
                    .ok_or(RuntimeError::FunctionIndexOutOfBounds(0))?;
                let outcome = instance.resume_with_results(results)?;
                (outcome, Some(instance_id))
            } else {
                unreachable!("Must have either a pending call or pending resume")
            };

            // Handle the outcome
            match outcome {
                ExecutionOutcome::Complete(results) => {
                    // Function or resume completed - pop caller from stack
                    if let Some(caller_id) = call_stack.pop() {
                        pending_resume = Some((caller_id, results));
                    } else {
                        // Stack empty - we're done
                        return Ok(results);
                    }
                }
                ExecutionOutcome::NeedsExternalCall(request) => {
                    // Push the current instance onto the call stack (if it's a wasm instance)
                    if let Some(instance_id) = source_instance {
                        call_stack.push(instance_id);
                    }
                    pending_call = Some((request.func_addr, request.args));
                }
            }
        }
    }

    /// Invoke an exported function by name on a specific instance
    ///
    /// This is the recommended way to invoke functions when cross-module calls may occur.
    pub fn invoke_export(
        &mut self,
        instance_id: usize,
        name: &str,
        args: Vec<Value>,
    ) -> Result<Vec<Value>, RuntimeError> {
        let func_addr = {
            let instance = self
                .get_instance(instance_id)
                .ok_or(RuntimeError::FunctionIndexOutOfBounds(0))?;
            instance.get_function_addr(name)?
        };
        self.execute(func_addr, args)
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
            .invoke_export(instance_id, "call_host", vec![])
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
            .invoke_export(instance_a, "call_b", vec![])
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

        // This is the critical test - A → B → C chain
        let result = store
            .invoke_export(instance_a, "call_chain", vec![])
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

        // A → B → C chain with computation at each level
        // Expected: 10 + 100 + 1000 = 1110
        let result = store
            .invoke_export(instance_a, "call_chain", vec![])
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
        // This tests: B (wasm) → Host → resume B
        // Then: A (wasm) → B (wasm) → Host → resume B → resume A
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

        // A → B → Host chain
        let result = store
            .invoke_export(instance_a, "start_chain", vec![])
            .expect("Wasm-Host-Wasm chain should succeed");

        assert_eq!(result, vec![Value::I32(777)]);
    }
}
