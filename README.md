# kasm

An experimental & educational WebAssembly runtime implementation in Rust that provides parsing, validation, and execution utilities for WebAssembly modules.

**"kasm"** is a placeholder name for now, it won't be published to crates.io until it matures further.

*If you want to join me in exploring WebAssembly by building, hit me up!*

## Features

- Complete WebAssembly binary parser with full section support
- Disassembler compatible with WABT wasm-objdump format
- Interpreter supporting all WebAssembly 1.0 instructions (aside from SIMD, WIP)
- Structured control flow execution following WebAssembly design intent
- Linear memory with bounds checking and page-based growth
- Stack-based execution with runtime type checking
- Passes most core WebAssembly specification tests (WIP)

## Usage

```bash
# Parse and disassemble a WebAssembly module
cargo run -- file.wasm --dump-disassemble

# Run tests
cargo test

# Run all checks (formatting, linting, tests)
./check.sh
```

## Project Structure

- `src/parser/` - Binary parser and module representation
- `src/runtime/` - WebAssembly interpreter and execution engine
- `tests/` - Unit tests and WebAssembly specification test suite

## Current Limitations

- No import/export linking between modules
- No table operations
- Global variables not fully implemented
- No SIMD instructions
- Call instruction disabled in debug builds due to stack depth limitations

## License

This project is licensed under the Apache 2.0 license. See the [LICENSE](LICENSE) file for details.