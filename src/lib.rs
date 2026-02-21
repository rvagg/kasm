//! A WebAssembly 2.0 runtime, parser, and toolkit written in Rust.
//!
//! kasm provides a complete pipeline for working with WebAssembly modules:
//! binary parsing, text format (WAT) parsing, validation, execution via a
//! stack-machine interpreter, binary encoding, and WASI preview1 support.
//!
//! # Modules
//!
//! - [`parser`] -- Binary format decoder. Reads `.wasm` bytes into a [`parser::module::Module`].
//! - [`wat`] -- Text format parser. Reads WAT source into the same `Module` representation.
//! - [`wast`] -- `.wast` script parser for the official specification test suite.
//! - [`encoder`] -- Binary encoder. Serialises a `Module` back to `.wasm` bytes.
//! - [`runtime`] -- Interpreter, memory, tables, and the WASI preview1 host.
//!
//! # Example
//!
//! Parse a WAT module, instantiate it, and call an exported function:
//!
//! ```
//! use kasm::wat;
//! use kasm::runtime::store::Store;
//! use kasm::runtime::Value;
//!
//! let module = wat::parse(r#"
//!     (module
//!         (func (export "add") (param i32 i32) (result i32)
//!             local.get 0
//!             local.get 1
//!             i32.add))
//! "#).unwrap();
//!
//! let mut store = Store::new();
//! let id = store.create_instance(&module, None).unwrap();
//! let results = store.invoke_export(id, "add", vec![Value::I32(2), Value::I32(3)], None).unwrap();
//! assert_eq!(results, vec![Value::I32(5)]);
//! ```
//!
//! # Specification
//!
//! Targets the [WebAssembly 2.0 specification](https://webassembly.github.io/spec/core/)
//! (W3C Working Group draft, `wg-2.0` tag), including bulk memory operations,
//! reference types, and the full SIMD instruction set.

pub mod encoder;
pub mod parser;
pub mod runtime;
pub mod wast;
pub mod wat;
