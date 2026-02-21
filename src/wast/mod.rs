//! WebAssembly Script Test (.wast) parser and execution support.
//!
//! This module parses `.wast` files, the test format used by the official
//! WebAssembly specification test suite. It provides the standard helpers
//! needed to execute them: value conversion, result comparison with NaN
//! tolerance, and the spectest host module.
//!
//! # Architecture
//!
//! The parser builds on the WAT lexer and S-expression reader. It does
//! NOT parse module bodies; instead, it extracts them as raw source text
//! (for WAT modules) or raw bytes (for binary modules). This allows the test
//! runner to decide when parsing should succeed or fail (e.g., `assert_malformed`
//! expects parsing to fail).
//!
//! # Example
//!
//! ```
//! use kasm::wast::parse_script;
//!
//! let source = r#"
//!     (module (func (export "f") (result i32) (i32.const 42)))
//!     (assert_return (invoke "f") (i32.const 42))
//! "#;
//! let script = parse_script(source).unwrap();
//! assert_eq!(script.commands.len(), 2);
//! ```

pub mod command;
mod parser;
pub mod spectest;
pub mod values;

pub use command::*;
pub use parser::{WastParseError, WastSyntaxError, parse_script};
pub use spectest::{create_spectest_imports, create_spectest_module};
pub use values::{convert_args, match_results};
