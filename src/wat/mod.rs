//! WebAssembly Text Format (WAT) support.
//!
//! This module provides lexical analysis for WAT, the human-readable text format
//! for WebAssembly modules.
//!
//! # Example
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
//! The lexer yields `Result<Token, LexError>` for each token. On malformed input,
//! iteration stops at the first error:
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
mod token;

pub use error::LexError;
pub use lexer::Lexer;
pub use token::{FloatLit, SignedValue, Span, Token, TokenKind};
