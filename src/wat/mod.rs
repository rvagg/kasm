//! WebAssembly Text Format (WAT) support.
//!
//! This module provides lexical analysis and parsing for WAT, the human-readable
//! text format for WebAssembly modules.
//!
//! # Parsing Example
//!
//! ```
//! use kasm::wat::parse;
//!
//! let wat = r#"
//!     (module
//!         (func $add (param i32 i32) (result i32)
//!             local.get 0
//!             local.get 1
//!             i32.add))
//! "#;
//!
//! let module = parse(wat).expect("valid WAT");
//! assert_eq!(module.functions.len(), 1);
//! ```
//!
//! # Lexer Example
//!
//! ```
//! use kasm::wat::{Lexer, TokenKind};
//!
//! let source = "(module (func $main (result i32) (i32.const 42)))";
//! let tokens: Vec<_> = Lexer::new(source).collect::<Result<_, _>>().unwrap();
//!
//! assert!(matches!(tokens[0].kind, TokenKind::LeftParen));
//! assert!(matches!(tokens[1].kind, TokenKind::Keyword(ref k) if k == "module"));
//! ```
//!
//! # Error Handling
//!
//! Both the lexer and parser return `Result` types with detailed error information
//! including source location:
//!
//! ```
//! use kasm::wat::Lexer;
//!
//! let source = "\"unterminated string";
//! let result: Result<Vec<_>, _> = Lexer::new(source).collect();
//! assert!(result.is_err());
//! ```

mod cursor;
mod error;
mod lexer;
mod parser;
pub mod sexpr;
mod token;

pub use error::LexError;
pub use lexer::Lexer;
pub use parser::{ParseError, parse};
pub use sexpr::{ReadError, SExpr, SExprList};
pub use token::{FloatLit, SignedValue, Span, Token, TokenKind};
