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
  executor.rs       # Main execution loop
  instance.rs       # Module instantiation
  ops/              # Instruction implementations (numeric, memory, control, etc.)
  memory.rs         # Linear memory (64KB pages, 4GB max)
  stack.rs          # Operand stack
  value.rs          # i32/i64/f32/f64/funcref/externref
```

## Status
- 188 instructions implemented (see src/runtime/implemented.rs)
- 86/90 core spec tests pass (tests/spec/*.json)
- Missing: imports/exports linking, tables, full globals, SIMD
- call instruction disabled in debug mode (stack overflow on deep recursion)

## Development Workflow
```bash
cargo run -- file.wasm --dump-disassemble  # Parse and disassemble
cargo test                                  # Run all tests
cargo test <pattern>                        # Run specific tests
cargo test -- --nocapture                   # Show println output
./check.sh                                  # Format, lint, test
```

## Adding Instructions
1. Add to `InstructionKind` enum (src/parser/instruction/mod.rs)
2. Add decoding in `decode_instruction()` (src/parser/instruction/decode.rs)
3. Add execution in `execute_instruction()` (src/runtime/executor.rs) or ops module
4. Update src/runtime/implemented.rs
5. Run `./check.sh`

## Test Coverage System
The project uses a coordinated test system:
1. **src/runtime/implemented.rs** - Central registry of implemented instructions (`get_implemented_instructions()`)
2. **tests/parser_tests.rs** - Spec test runner that:
   - Parses each test module and checks if all instructions are implemented
   - Skips tests with unimplemented instructions automatically
   - Runs assert_return/assert_trap/assert_invalid assertions
3. **src/bin/test_coverage.rs** - Development tool that analyses spec tests to show:
   - Which instructions block the most tests
   - Priority order for implementing missing instructions

```bash
cargo run --bin test_coverage  # See which instructions enable most tests
```

Test skipping logic: When parser_tests encounters an unimplemented instruction, it skips the entire test file rather than failing. This allows incremental development while maintaining green tests.

## Key Files
- src/runtime/executor.rs - Main interpreter loop with structured control flow
- src/runtime/ops/*.rs - Instruction implementations by category
- src/runtime/implemented.rs - Single source of truth for implemented instructions
- tests/parser_tests.rs - Spec test harness that checks implemented.rs before running
- tests/spec/*.json - WebAssembly spec tests (don't modify)
- src/bin/test_coverage.rs - Development tool to prioritise instruction implementation

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
- Use streaming parser for large modules