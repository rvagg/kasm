# kasm

**"kasm"** is a placeholder name for a personal project; it is _not_ the "kasm" crate available on crates.io. If/when this matures enough to publish, I'll rename it.

This project is a collection of WASM utilities in Rust, that may evolve into something useful but currently implements a grab-bag of functionality that I expand as I have the time and inclination.

* `kasm::parser::reader::Reader` - a byte reader that supports various operations useful for consuming and decoding a WASM binary.
* `kasm::parser::Module` - a representation of a parsed WASM binary.
* `kasm::parser::parse(String, kasm::parser::reader::Reader)` - a function that parses a WASM binary into a `kasm::parser::Module` struct.
  - [x] Binaries are parsed and represented
  - [ ] WIP: A `Module` can be represented in string form, including disassembly, fully compatible with the WABT [`wasm-objdump`](https://webassembly.github.io/wabt/doc/wasm-objdump.1.html) utility.
  - [ ] WIP: Full validation of WASM instruction sequences to ensure binaries represent valid programs.

## License

This project is licensed under the Apache 2.0 license. See the [LICENSE](LICENSE) file for details.
