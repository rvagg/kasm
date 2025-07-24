# KASM - WebAssembly VM (Rust)

Goal: WASM VM. Current phase: Parser → AST → Interpreter → Optimized runtime.

## Quick Start
```bash
cargo run -- file.wasm --dump-disassemble  # Parse WASM binary
cargo test                                  # Run parser tests
./check.sh                                  # Run ALL checks (fmt, clippy, tests)
```

## Code Quality Checks (IMPORTANT!)
Always run these checks after making changes:
```bash
# Option 1: Run all checks at once
./check.sh

# Option 2: Run individual checks
cargo fmt                    # Format code (modifies files)
cargo fmt -- --check        # Check formatting without modifying
cargo clippy -- -D warnings  # Lint code (treats warnings as errors)
cargo test                   # Run all tests
```

**AI Agents**:
- NEVER run `git commit`, `git push`, or any destructive git commands
- Only use git for inspection: `git status`, `git diff`, `git log`, etc.
- Always run `./check.sh` after making code changes to ensure:
  - Code is properly formatted (rustfmt)
  - No clippy warnings
  - All tests pass
  - No trailing whitespace

## Other Useful Commands
```bash
cargo check              # Quick type check without building
cargo build              # Build debug version
cargo build --release    # Build optimized version
cargo doc --open         # Generate and open documentation
cargo test -- --nocapture # Run tests with println! output visible
cargo test <name>        # Run specific test by name pattern
```

## Core Architecture
```
src/parser/
├── mod.rs       # Main parse() function, section readers (read_section_*)
├── module.rs    # Module struct, section types (TypeSection, ImportSection, etc.)
├── ast.rs       # Instruction enum, decode logic for opcodes
├── reader.rs    # Binary reader (LEB128, read_vu32, etc.)
└── validate.rs  # Instruction validation (WIP)
```

## Key Concepts
- **Module**: Parsed WASM with all sections (types, imports, functions, code, memory, etc.)
- **Sections**: Parse in specific order, handle datacount reordering
- **Instructions**: AST with ~200 opcodes in InstructionType enum
- **Validation**: Import types checked, instruction sequences WIP

## Development Status
✅ Binary parsing (all sections)
✅ Import validation
✅ Module::to_string() - disassembly output
🚧 Full instruction validation
❌ Runtime/interpreter (next phase)

## Testing
- Fixtures: `tests/spec/*.json` - WASM spec tests as JSON
- Source: Official WebAssembly spec test suite
- Format: Base64 binaries + expected behavior
- Generation: `compile_test.mjs` uses WABT tools (not in repo)
- Test harness: `tests/parser_tests.rs` runs all spec tests
- Test types:
  - `assert_return`: Parse and validate module
  - `assert_invalid`: Should fail parsing/validation (caught)
  - `assert_trap`: Would trap at runtime (skipped for now)
  - `assert_malformed`: Binary format errors (caught)
  - `assert_unlinkable`: Parse OK but fail instantiation (skipped)
- All spec tests passing ✅

## Common Tasks
- New opcode: Add to `ast::InstructionType`, handle in `ast::Instruction::decode()`
- Section parsing: Edit `read_section_*` in mod.rs
- Display/dump: Implement Display traits in module.rs
- Validation: Extend validators in validate.rs

## Important Patterns
- Error type: `Result<T, ast::DecodeError>` 
- Position tracking: All sections track byte positions
- Module registry: For import resolution during parsing
- Test against spec: Always verify with JSON fixtures
- Import validation: Deferred to instantiation (not parse time)
- Cross-section references: Use Module methods for name lookups

## Dependencies
- byteorder: Binary reading
- serde: Test fixture parsing
- thiserror: Error types

## Project Documentation
- Prefer to document the code well; this project should also serve as an educational tool for readers and contributors.