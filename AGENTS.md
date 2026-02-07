# WebAssembly Runtime (Rust)

## Critical Rules
- Run `./check.sh` after changes (fmt, clippy, tests, builds AssemblyScript)
- Never run git commit/push
- Use Australian English (initialise, analyse, behaviour)
- Follow existing code patterns

## Architecture
```
src/parser/         # Binary parser, validation
  instruction/      # Opcode decoding (200+ instructions)
  module.rs         # Module representation
  structured.rs     # Control flow tree builder

src/wat/            # WAT (text format) parser
  mod.rs            # Public API: parse() function
  lexer.rs          # Tokeniser with iterator API
  token.rs          # Token types (keywords, integers, floats, strings, ids)
  cursor.rs         # Character-level cursor with position tracking
  sexpr.rs          # S-expression tree representation
  parser.rs         # Two-phase parser (S-expr → Module)
  error.rs          # LexError/ParseError with span information

src/encoder.rs      # Binary encoder (Module → .wasm)

src/parser/
  encoding.rs       # LEB128/float/vector encoding primitives (write_* and emit_* API)

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
  wasi/             # WASI preview1 implementation
    mod.rs          # WASI imports (fd_read, fd_write, args_*, environ_*, proc_exit)
    context.rs      # WasiContext with lazy memory binding
    types.rs        # WASI errno codes
    assemblyscript.rs  # AssemblyScript env.abort support

examples/assemblyscript/  # AssemblyScript WASI examples
  assembly/         # Source files (index.ts, echo.ts, grep.ts, env.ts, std.ts)
  build/            # Compiled .wasm output (gitignored, built by check.sh)

benches/                # Criterion benchmarks
  modules/          # WAT modules for benchmarking
```

## Status
- 191+ instructions implemented (see src/runtime/implemented.rs)
- 86/86 core spec tests pass (21,303 assertions)
- WASI preview1: fd_read/write, args_*, environ_*, proc_exit, fd_prestat_* (stub)
- AssemblyScript support: env.abort with UTF-16 string extraction
- WAT parser: complete WebAssembly 1.0 text format parser
- Binary encoder: Module → .wasm (all 13 sections, 80 tests including spec fixture round-trips)
- Missing: SIMD (v128), filesystem operations
- Future: encoder enables `kasm compile` CLI and replacing external `wat` dev-dependency

## CLI
```bash
kasm run <file.wasm> [-- args...]   # Execute WASI module
kasm dump <file.wasm>               # Show module details
kasm dump <file.wasm> --header      # Show only header
kasm dump <file.wasm> -d            # Disassemble
```

## Development Workflow
```bash
./check.sh                           # Format, lint, test (+ builds AssemblyScript)
./check.sh -f                        # Also run fuzzer (60s)
./check.sh -f 300                    # Fuzz for 5 minutes
cargo test                           # Run all tests
cargo test <pattern>                 # Run specific tests
cargo test -- --nocapture            # Show println output
```

## Fuzzing
Fuzzing infrastructure is in `fuzz/` using cargo-fuzz (libFuzzer).

```bash
# Install cargo-fuzz (requires nightly)
cargo install cargo-fuzz

# Seed corpus from spec tests and .wasm files
./fuzz/seed_corpus.sh

# Run parser fuzzer with dictionary
cargo +nightly fuzz run parse_module -- -dict=fuzz/wasm.dict

# Run with time limit
cargo +nightly fuzz run parse_module -- -max_total_time=60 -dict=fuzz/wasm.dict

# Run execution fuzzer (provides typed arguments)
cargo +nightly fuzz run execute_module -- -max_total_time=60 -dict=fuzz/wasm.dict

# Run structure-aware fuzzer (generates valid modules)
cargo +nightly fuzz run generate_module -- -max_total_time=60

# Run WAT lexer fuzzer
cargo +nightly fuzz run lex_wat -- -max_total_time=60 -dict=fuzz/wat.dict

# Run WAT parser fuzzer
cargo +nightly fuzz run parse_wat -- -max_total_time=60 -dict=fuzz/wat.dict

# Minimise a crash
cargo fuzz tmin parse_module fuzz/artifacts/parse_module/<crash-file>

# Via check.sh (runs after tests pass)
./check.sh -f                    # Default: parse_module for 60s
./check.sh -f 300                # 5 minutes
./check.sh -f 60 -t execute_module   # Different target
./check.sh -f 60 -t generate_module  # Structure-aware fuzzing
```

**Fuzz targets:**
- `parse_module` - Byte mutation fuzzing of the binary parser
- `execute_module` - Parser + execution with correctly typed function arguments
- `generate_module` - Structure-aware fuzzing: generates syntactically valid modules using `arbitrary` crate
- `lex_wat` - WAT lexer fuzzing with arbitrary byte sequences
- `parse_wat` - WAT parser fuzzing with arbitrary byte sequences

**Fuzzing resources:**
- `fuzz/wasm.dict` - Dictionary with WebAssembly byte sequences (opcodes, section IDs, etc.)
- `fuzz/wat.dict` - Dictionary with WAT text format tokens (keywords, numbers, strings, etc.)
- `fuzz/seed_corpus.sh` - Seeds corpus from .wasm files and spec test JSON

**Files to commit:** `fuzz/Cargo.toml`, `fuzz/fuzz_targets/*.rs`, `fuzz/.gitignore`, `fuzz/wasm.dict`, `fuzz/wat.dict`, `fuzz/seed_corpus.sh`
**Files to ignore:** `fuzz/target/`, `fuzz/corpus/`, `fuzz/artifacts/`

## Benchmarking
Criterion benchmarks in `benches/` with WAT modules in `benches/modules/`.

```bash
cargo bench --bench execution           # Full execution benchmarks
cargo bench --bench execution -- noop   # Filter by name
cargo bench --bench execution -- --test # Verify correctness only
cargo bench --bench validation          # Parser/validation benchmarks
```

**Benchmark modules:**
- `noop_loop.wat` - Pure dispatch overhead (loop + branch)
- `fib_iterative.wat` - CPU-bound compute (locals, arithmetic)
- `fib_recursive.wat` - Function call overhead (exponential calls)
- `memcpy.wat` - Memory load/store throughput
- `primes.wat` - Mixed compute + memory (Sieve of Eratosthenes)

**Baseline metrics (release build):**
| Metric | Value |
|--------|-------|
| Instruction dispatch | ~237 ns |
| Function call | ~0.34 µs |
| Memory load+store (per byte) | ~474 ns |

Results saved to `target/criterion/` with HTML reports.

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

## WAT Parser
The `src/wat/` module provides a complete WebAssembly 1.0 text format parser.

```rust
use kasm::wat::parse;

let source = r#"(module
    (func $add (param i32 i32) (result i32)
        local.get 0
        local.get 1
        i32.add)
)"#;
let module = parse(source).unwrap();
```

**Supported constructs:**
- Types: function types with params and results
- Functions: flat and folded (S-expression) instruction syntax
- Tables: with funcref/externref element types
- Memories: with limits (min, optional max)
- Globals: mutable and immutable, all value types
- Imports/Exports: functions, tables, memories, globals
- Start function
- Element segments: active, passive, declarative modes
- Data segments: active (with offset), passive modes
- All WASM 1.0 instructions including control flow, numeric, memory ops

**Parser architecture:**
1. **Lexer** (`lexer.rs`): Iterator-based tokenisation
2. **S-expression reader** (`sexpr.rs`): Builds tree from tokens
3. **Parser** (`parser.rs`): Transforms S-expr tree to Module

**Instruction syntax:**
- Flat syntax: `local.get 0` `i32.add`
- Folded syntax: `(i32.add (local.get 0) (local.get 1))`
- Mixed syntax supported within same function

**Design notes:**
- Two-phase parsing avoids instruction duplication via `ArgSource` abstraction
- Named identifiers (`$name`) resolved to indices during parsing
- Preserves source spans for error reporting
- Fuzz targets: `lex_wat`, `parse_wat`

## Binary Encoder
The `src/encoder.rs` module encodes a Module to WebAssembly binary format (.wasm).

```rust
use kasm::encoder;
use kasm::wat;

let module = wat::parse("(module (func))").unwrap();
let bytes = encoder::encode(&module).unwrap();
assert_eq!(&bytes[0..4], b"\0asm");
```

**Sections encoded** (in wire order): type, import, function, table, memory, global, export, start, element, data_count, code, data, custom.

**Encoding primitives** (`src/parser/encoding.rs`):
- `write_*(&mut Vec<u8>, value)` — zero-copy write into caller's buffer (primary API)
- `emit_*(value) -> Vec<u8>` — convenience wrappers returning allocated Vec

**Testing strategy:** Encode stability — encode a Module to bytes A, parse A back, re-encode to bytes B, assert A == B. This avoids needing PartialEq on Module types.

```bash
cargo test --test encoder_tests    # 80 encoder tests
```

## Key Files
- src/wat/mod.rs - WAT public API (parse function)
- src/wat/parser.rs - S-expression to Module transformation
- src/wat/sexpr.rs - S-expression tree with span tracking
- src/wat/lexer.rs - WAT tokeniser with iterator API
- src/wat/token.rs - Token, TokenKind, SignedValue, FloatLit, Span
- src/main.rs - CLI with `run` and `dump` subcommands
- src/runtime/store.rs - Store, FuncAddr, MemoryAddr, TableAddr, cross-module execution
- src/runtime/executor.rs - State machine interpreter
- src/runtime/wasi/mod.rs - WASI imports and create_wasi_instance helper
- src/runtime/wasi/context.rs - WasiContext (memory, fds, args, env)
- src/runtime/ops/*.rs - Instruction implementations by category
- src/runtime/implemented.rs - Registry of implemented instructions
- src/encoder.rs - Binary encoder (Module → .wasm bytes)
- src/parser/encoding.rs - Encoding primitives (LEB128, floats, vectors)
- tests/parser_tests.rs - Spec test harness with spectest imports
- tests/encoder_tests.rs - Encoder round-trip tests (WAT, binary, spec fixtures)
- tests/wasi_tests.rs - WASI integration tests (inline WAT)

## Error Types
- `DecodeError` - Binary parser errors with byte positions
- `RuntimeError` - Execution errors
- `ValidationError` - Type validation errors
- `LexError` - WAT lexer errors with line/column spans
- `ParseError` - WAT parser errors with source spans
- `EncodeError` - Binary encoder errors (invalid element flags, invalid state)

## Testing
- Unit tests: Individual component tests
- Spec tests: Official WebAssembly test suite
- Always check spec test JSON for expected behaviour
- Use `RUST_BACKTRACE=1` for debugging

## Performance Notes
- Interpreter is ~6,500x slower than native (see docs/PERFORMANCE.md for bottleneck analysis)
- Main bottleneck: Mutex lock per memory operation (~510 ns per load+store)
- Function calls use Rc-based body sharing (~0.5 µs/call)
- Debug builds much slower than release - always benchmark with `--release`
- State machine executor: unlimited call depth, no Rust stack overflow
- Run `cargo bench --bench execution` to measure current performance
