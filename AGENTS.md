# KASM - WebAssembly VM (Rust)

A WebAssembly virtual machine implementation in Rust, progressing from parser to runtime interpreter.

## Project Status
✅ **Complete**: Binary parser, instruction decoder, module representation, basic runtime
🚧 **In Progress**: Full instruction implementation, runtime optimisation
📋 **Planned**: JIT compilation, advanced optimisations

## Quick Start
```bash
cargo run -- file.wasm --dump-disassemble  # Parse and disassemble WASM binary
cargo test                                  # Run all tests (153 unit + 86 spec tests)
./check.sh                                  # Run ALL checks (fmt, clippy, tests)
```

## Code Quality Requirements (CRITICAL!)
**ALWAYS** run `./check.sh` after making changes. It ensures:
- ✅ Code formatting (rustfmt)
- ✅ No clippy warnings (treats warnings as errors)
- ✅ All tests pass (unit + integration + spec tests)
- ✅ No trailing whitespace

Individual checks:
```bash
cargo fmt                    # Format code (modifies files)
cargo fmt -- --check        # Check formatting without modifying
cargo clippy -- -D warnings  # Lint code (strict mode)
cargo test                   # Run all tests
cargo test <name>           # Run specific test by name pattern
cargo test -- --nocapture   # Show println! output during tests
```

## AI Agent Guidelines
**CRITICAL RULES**:
- ❌ NEVER run `git commit`, `git push`, or any destructive git commands
- ✅ Only use git for inspection: `git status`, `git diff`, `git log`
- ✅ ALWAYS run `./check.sh` after code changes
- ✅ Use Australian English spelling (initialise, analyse, behaviour, etc.)
- ✅ Document code thoroughly - this is an educational project
- ✅ Follow existing patterns and conventions in the codebase

## Project Architecture

### Parser (`src/parser/`)
```
├── mod.rs                        # Main parse() function, section readers
├── module.rs                     # Module struct, all section types
├── instruction/
│   ├── mod.rs                   # Instruction types, decode functions
│   ├── decode.rs                # Binary instruction decoder
│   ├── encode.rs                # Instruction encoder (for testing)
│   └── streaming_decode.rs     # Streaming instruction processor
├── reader.rs                    # Binary reader (LEB128, IEEE 754)
├── validate.rs                  # Validation (signatures, stack types)
├── structured.rs                # Structured control flow representation
├── structure_builder.rs         # Builds structured from flat instructions
└── streaming_structure_builder.rs # Memory-efficient streaming builder
```

### Runtime (`src/runtime/`)
```
├── mod.rs                       # Runtime module exports
├── instance.rs                  # Module instantiation
├── executor.rs                  # Instruction interpreter
├── memory.rs                    # Linear memory implementation
├── stack.rs                     # Operand stack
├── frame.rs                     # Call frames
├── value.rs                     # Value types (i32, i64, f32, f64)
└── control.rs                   # Control flow structures
```

## Key Concepts

### Parsing
- **Module**: Complete parsed WASM binary with all sections
- **Sections**: Parsed in order, handles datacount section reordering
- **Instructions**: Full instruction set (~200 opcodes) in `InstructionKind` enum
- **Validation**: Two-pass validation during parsing and execution
- **Position tracking**: All elements track byte positions for debugging

### Runtime
- **Instance**: Instantiated module with memories, tables, globals
- **Executor**: Stack-based interpreter with structured control flow
- **Structured Execution**: Pre-built control flow tree for O(1) branches
- **Memory**: Linear memory with bounds checking, page-based growth
- **Stack**: Typed operand stack with runtime type checking
- **Values**: i32, i64, f32, f64, funcref, externref support

## Control Flow Implementation
The runtime uses **structured execution** exclusively:
- Control flow is pre-processed into a tree structure
- Branches are O(1) lookups instead of O(n) instruction scanning
- Label stack maintained during execution
- BlockEnd enum controls flow: Normal, Branch(depth), Return

## Memory Management
- Page size: 64KB (65536 bytes)
- Maximum memory: 4GB (65536 pages)
- Zero-initialised on allocation
- Bounds checking on all operations
- Little-endian byte order

## Testing Strategy
1. **Unit Tests** (153): Test individual components
2. **Spec Tests** (86): Official WebAssembly test suite
   - Located in `tests/spec/*.json`
   - Base64-encoded binaries with expected behaviour
   - Test types: assert_return, assert_invalid, assert_trap, assert_malformed
3. **Integration Tests**: End-to-end module execution

## Common Development Tasks

### Adding a New Instruction
1. Add variant to `InstructionKind` enum in `src/parser/instruction/mod.rs`
2. Add decoding logic in `decode_instruction()` in `src/parser/instruction/decode.rs`
3. Add execution logic in `execute_instruction()` in `src/runtime/executor.rs`
4. Add Display implementation for disassembly
5. Add tests in appropriate test module
6. Run `./check.sh` to verify

### Debugging Tips
- Use `--dump-disassemble` to see parsed instructions
- Set `RUST_BACKTRACE=1` for stack traces
- Use `cargo test -- --nocapture` to see debug prints
- Check byte positions in error messages

### Performance Considerations
- Structured execution trades startup time for runtime performance
- Streaming parser available to reduce memory usage
- Pre-validated instructions skip redundant checks

## Important Implementation Details

### Error Handling
- Parser errors: `DecodeError` with byte positions
- Runtime errors: `RuntimeError` with execution context
- Validation errors: `ValidationError` with type information

### Type System
- Static validation during parsing
- Runtime type checking for safety
- Stack polymorphism for unreachable code

### Australian English Convention
The codebase uses Australian English spelling consistently:
- initialise (not initialize)
- analyse (not analyze)
- behaviour (not behavior)
- finalise (not finalize)
- optimise (not optimize)

## Dependencies
- `byteorder`: Binary data reading/writing
- `serde`: Test fixture deserialisation
- `thiserror`: Error type derivation
- `base64`: Test data encoding
- `fhex`: Float hexadecimal display

## Current Limitations
- No imports/exports execution (parsed but not linked)
- No tables implementation
- No globals implementation
- No multi-memory support
- No SIMD instructions
- No exception handling

## Files Not to Modify
- `tests/spec/*.json`: Official test suite (generated externally)
- `compile_test.mjs`: Test generation script (uses WABT tools)

## Performance Notes
- Debug builds are significantly slower than release builds
- Use `cargo build --release` for performance testing
- Structured execution is faster for loops/branches but slower to build
- Consider streaming parser for large modules to reduce memory usage

## Debugging WebAssembly
When debugging WASM issues:
1. Use `wasm-objdump` from WABT toolkit for reference
2. Compare disassembly output with expected
3. Check spec test JSON for expected behaviour
4. Verify stack state at each instruction
5. Ensure proper type validation

## Project Philosophy
This is an educational project that prioritises:
1. **Correctness**: Full spec compliance
2. **Readability**: Clear, well-documented code
3. **Learning**: Comprehensive comments explaining WebAssembly concepts
4. **Testing**: Thorough test coverage with spec tests

Remember: This codebase should help others understand WebAssembly internals!