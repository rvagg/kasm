# WebAssembly Runtime (Rust)

## Critical Rules
- Run `./check.sh` after changes (fmt, clippy, tests)
- Never run git commit/push
- Use Australian English (initialise, analyse, behaviour)
- Follow existing code patterns

## Architecture
```
src/parser/         # Binary parser, validation
  instruction/      # Opcode decoding (200+ instructions)
  module.rs         # Module representation
  structured.rs     # Control flow tree builder

src/runtime/        # Interpreter
  store.rs          # Store-based execution, cross-module function calls
  executor.rs       # State machine execution (no recursion)
  instance.rs       # Module instantiation
  imports.rs        # Import resolution (globals, functions, memories, tables)
  table.rs          # Table instances for indirect calls
  ops/              # Instruction implementations (numeric, memory, control, etc.)
  memory.rs         # Linear memory (64KB pages, 4GB max)
  stack.rs          # Operand stack
  value.rs          # i32/i64/f32/f64/funcref/externref
```

## Status
- 191+ instructions implemented (see src/runtime/implemented.rs)
- 86/86 core spec tests pass (21,303 assertions)
- All import types supported: functions, globals, memories, tables
- Tables and call_indirect: fully implemented
- Cross-module linking: supported via Store-based architecture
- Missing: SIMD (v128 type and vector operations)

## Development Workflow
```bash
cargo run --bin kasm -- file.wasm --dump-disassemble  # Parse and disassemble
cargo test                                             # Run all tests
cargo test <pattern>                                   # Run specific tests
cargo test -- --nocapture                              # Show println output
./check.sh                                             # Format, lint, test
./check.sh -f                                          # Also run fuzzer (60s)
./check.sh -f 300                                      # Fuzz for 5 minutes
./check.sh -h                                          # Show help
```

## Fuzzing
Fuzzing infrastructure is in `fuzz/` using cargo-fuzz (libFuzzer).

```bash
# Install cargo-fuzz (requires nightly)
cargo install cargo-fuzz

# Run parser fuzzer
cargo +nightly fuzz run parse_module

# Run with time limit
cargo +nightly fuzz run parse_module -- -max_total_time=60

# Minimise a crash
cargo fuzz tmin parse_module fuzz/artifacts/parse_module/<crash-file>
```

**Fuzz targets:**
- `parse_module` - Tests the binary parser with random/malformed input
- `execute_module` - Tests parser + execution

**Files to commit:** `fuzz/Cargo.toml`, `fuzz/fuzz_targets/*.rs`, `fuzz/.gitignore`
**Files to ignore:** `fuzz/target/`, `fuzz/corpus/`, `fuzz/artifacts/`

## Adding Instructions
1. Add to `InstructionKind` enum (src/parser/instruction/mod.rs)
2. Add decoding in `decode_instruction()` (src/parser/instruction/decode.rs)
3. Add execution in `execute_instruction()` (src/runtime/executor.rs) or ops module
4. Update src/runtime/implemented.rs
5. Run `./check.sh`

## Test Coverage System
1. **src/runtime/implemented.rs** - Central registry of implemented instructions
2. **tests/parser_tests.rs** - Spec test runner:
   - Checks if test's instructions are implemented
   - Skips tests with unimplemented instructions (keeps tests green)
   - Runs assert_return/assert_trap/assert_invalid assertions
   - Creates ImportObject with spectest globals (global_i32=666, etc.)
   - Uses Store for cross-module function execution
3. **src/bin/test_coverage.rs** - Shows which instructions block most tests

```bash
cargo run --bin test-coverage  # See which instructions enable most tests
```

## Key Files
- src/runtime/store.rs - Store, FuncAddr, MemoryAddr, TableAddr, cross-module execution
- src/runtime/executor.rs - State machine interpreter
- src/runtime/instance.rs - Module instantiation
- src/runtime/imports.rs - Import resolution (globals, functions, memories, tables)
- src/runtime/table.rs - Table instances
- src/runtime/ops/*.rs - Instruction implementations by category
- src/runtime/implemented.rs - Registry of implemented instructions
- tests/parser_tests.rs - Spec test harness with spectest imports
- src/bin/test_coverage.rs - Shows which instructions to prioritise

## Error Types
- `DecodeError` - Parser errors with byte positions
- `RuntimeError` - Execution errors
- `ValidationError` - Type validation errors

## Testing
- Unit tests: Individual component tests
- Spec tests: Official WebAssembly test suite
- Always check spec test JSON for expected behaviour
- Use `RUST_BACKTRACE=1` for debugging

## Performance Notes
- Structured execution: O(1) branches but slower startup
- Debug builds much slower than release
- State machine executor: unlimited call depth, no Rust stack overflow
