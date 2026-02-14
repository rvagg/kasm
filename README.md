# kasm

An experimental & educational WebAssembly runtime implementation in Rust that provides parsing, validation, and execution utilities for WebAssembly modules.

**"kasm"** is a placeholder name for now, it won't be published to crates.io until it matures further.

*If you want to join me in exploring WebAssembly by building, hit me up!*

## Features

- **Binary parser** with full section support and validation
- **WAT parser** for the WebAssembly text format
- **Binary encoder** producing spec-compliant `.wasm` from any parsed Module
- **State machine interpreter** supporting WebAssembly 1.0 + SIMD (427+ instructions)
- **Full SIMD (v128)** support: all 236 SIMD instructions across integer, float, bitwise, comparison, shuffle, conversion, and memory operations
- **WASI preview1** support: fd_read/write, args, environ, proc_exit, filesystem preopens
- **AssemblyScript** compatibility (env.abort with UTF-16 string extraction)
- Disassembler compatible with WABT wasm-objdump format
- Structured control flow execution following WebAssembly design intent
- Linear memory with bounds checking and page-based growth
- Tables with indirect function calls (call_indirect)
- Cross-module linking via Store-based architecture
- Passes 147/147 specification tests (90 core + 57 SIMD)

## CLI

```bash
# Run a WASI module (.wasm or .wat)
kasm run examples/hello.wat
kasm run module.wasm -- arg1 arg2
kasm run module.wasm --dir ./data -- arg1

# Compile WAT to binary
kasm compile examples/hello.wat              # produces examples/hello.wasm
kasm compile input.wat -o output.wasm

# Inspect a module
kasm dump module.wasm                        # detailed section info
kasm dump module.wasm --header               # magic and version only
kasm dump module.wasm -d                     # disassemble
```

Build and run via cargo:

```bash
cargo run --bin kasm -- run examples/hello.wat
cargo run --bin kasm -- compile examples/hello.wat
cargo run --bin kasm -- dump module.wasm -d
```

## Project Structure

```
src/parser/         Binary parser, validation, encoding primitives
src/wat/            WAT text format parser (lexer, S-expression, Module builder)
src/encoder.rs      Binary encoder (Module → .wasm)
src/runtime/        Interpreter, Store, WASI implementation
examples/           Example WAT modules
benches/            Criterion benchmarks with WAT modules
tests/              Unit tests, spec test suite, WASI integration tests
fuzz/               Fuzz targets (binary parser, executor, WAT lexer/parser)
```

## Development

```bash
./check.sh              # Format, lint, test (must pass before committing)
cargo test              # Run all tests
cargo bench             # Run benchmarks
```

## Current Limitations

- No filesystem WASI operations beyond preopened directories

## License

This project is licensed under the Apache 2.0 license. See the [LICENSE](LICENSE) file for details.
