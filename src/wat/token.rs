//! Token types for the WAT lexer.
//!
//! This module defines the lexical tokens produced when tokenising WebAssembly
//! Text Format source code.

use fhex::FromHex;
use std::fmt;

/// A location in source text.
///
/// Spans track both byte offsets (for slicing) and line/column (for errors).
/// Columns count Unicode characters, not bytes, for accurate display.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// Byte offset where this span starts.
    pub start: usize,
    /// Byte offset just past the end of this span.
    pub end: usize,
    /// Line number (1-indexed).
    pub line: u32,
    /// Column number (1-indexed, counting characters not bytes).
    pub column: u32,
}

impl Span {
    /// A zero-length span at the start of source, for errors without position.
    pub const ZERO: Span = Span {
        start: 0,
        end: 0,
        line: 1,
        column: 1,
    };

    /// Create a new span.
    pub fn new(start: usize, end: usize, line: u32, column: u32) -> Self {
        Self {
            start,
            end,
            line,
            column,
        }
    }

    /// The length of this span in bytes.
    #[must_use]
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    /// Whether this span is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// A lexical token with its location in source.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// What kind of token this is.
    pub kind: TokenKind,
    /// Where in the source this token appears.
    pub span: Span,
}

impl Token {
    /// Create a new token.
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Get the original source text for this token.
    ///
    /// # Example
    ///
    /// ```
    /// use kasm::wat::{Lexer, TokenKind};
    ///
    /// let source = "(module)";
    /// let tokens: Vec<_> = Lexer::new(source).collect::<Result<_, _>>().unwrap();
    /// assert_eq!(tokens[1].text(source), "module");
    /// ```
    #[must_use]
    pub fn text<'a>(&self, source: &'a str) -> &'a str {
        &source[self.span.start..self.span.end]
    }
}

/// The kind of token, with associated data where relevant.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    /// Opening parenthesis `(`.
    LeftParen,

    /// Closing parenthesis `)`.
    RightParen,

    /// A keyword like `module`, `func`, `i32.add`, `memory.grow`.
    ///
    /// Keywords are bare identifiers that don't start with `$`.
    Keyword(String),

    /// An identifier like `$name`, `$0`, `$my_func`.
    ///
    /// The stored string excludes the leading `$`.
    Id(String),

    /// A string literal with escape sequences resolved.
    ///
    /// Stored as raw bytes since WAT strings can contain arbitrary bytes.
    String(Vec<u8>),

    /// An integer literal.
    ///
    /// Stored with magnitude and sign separate to correctly represent the full
    /// range of both signed (i64) and unsigned (u64) WebAssembly integers.
    Integer(SignedValue<u64>),

    /// A floating-point literal.
    ///
    /// Special values (inf, nan) are represented directly. NaN payloads are
    /// preserved in the optional payload field.
    Float(FloatLit),
}

/// A value with an explicit sign.
///
/// This representation preserves the distinction between `-0` and `0`, and
/// allows representing the full u64 range (which i64 cannot).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SignedValue<T> {
    /// The magnitude of the value.
    pub value: T,
    /// Whether a negative sign was present in the source.
    pub negative: bool,
    /// Whether any explicit sign (+ or -) was present in the source.
    pub has_sign: bool,
}

impl<T> SignedValue<T> {
    /// Create a value with an explicit sign (`+42` or `-42`).
    pub fn signed(value: T, negative: bool) -> Self {
        Self {
            value,
            negative,
            has_sign: true,
        }
    }

    /// Create an unsigned value (bare literal like `42`).
    pub fn unsigned(value: T) -> Self {
        Self {
            value,
            negative: false,
            has_sign: false,
        }
    }
}

impl SignedValue<u64> {
    /// Convert to i64, returning None if the value overflows.
    ///
    /// Note: `-0` converts to `0`.
    #[must_use]
    pub fn to_i64(self) -> Option<i64> {
        if self.negative {
            if self.value == 0 {
                Some(0)
            } else if self.value == i64::MAX as u64 + 1 {
                // Special case: this is exactly i64::MIN (negating would overflow)
                Some(i64::MIN)
            } else if self.value <= i64::MAX as u64 {
                Some(-(self.value as i64))
            } else {
                None
            }
        } else if self.value <= i64::MAX as u64 {
            Some(self.value as i64)
        } else {
            None
        }
    }

    /// Convert to u64, returning None if negative (except `-0`).
    #[must_use]
    pub fn to_u64(self) -> Option<u64> {
        if self.negative && self.value != 0 {
            None
        } else {
            Some(self.value)
        }
    }
}

/// A floating-point literal.
///
/// Stores the original source string for both decimal and hex floats so that
/// f32 and f64 conversions can each round independently, avoiding the
/// double-rounding problem of converting via an intermediate f64.
#[derive(Debug, Clone, PartialEq)]
pub enum FloatLit {
    /// A decimal floating-point literal. Stores the original string (e.g. "1.5e-3")
    /// so that f32 and f64 conversions use the standard library's correct rounding.
    Decimal { negative: bool, decimal_str: String },
    /// A hex floating-point value. Stores the original hex string (e.g. "0x1.8p+1")
    /// so that f32 and f64 conversions can each round independently via fhex.
    Hex { negative: bool, hex_str: String },
    /// Positive or negative infinity.
    Inf { negative: bool },
    /// Not a Number, with optional payload for the significand bits.
    Nan { negative: bool, payload: Option<u64> },
}

impl FloatLit {
    /// Convert to f64.
    ///
    /// NaN payloads are not preserved in the f64 representation.
    #[must_use]
    pub fn to_f64(&self) -> f64 {
        match self {
            FloatLit::Decimal { negative, decimal_str } => {
                let v: f64 = decimal_str.parse().unwrap_or(0.0);
                if *negative { -v } else { v }
            }
            FloatLit::Hex { negative, hex_str } => {
                let v = f64::from_hex(hex_str).unwrap_or(0.0);
                if *negative { -v } else { v }
            }
            FloatLit::Inf { negative: true } => f64::NEG_INFINITY,
            FloatLit::Inf { negative: false } => f64::INFINITY,
            FloatLit::Nan { .. } => f64::NAN,
        }
    }

    /// Convert to f32 with correct IEEE 754 rounding.
    ///
    /// For hex literals, calls `f32::from_hex()` directly. For decimal literals,
    /// calls `str::parse::<f32>()` directly. Both avoid intermediate f64 rounding.
    #[must_use]
    pub fn to_f32(&self) -> f32 {
        match self {
            FloatLit::Decimal { negative, decimal_str } => {
                let v: f32 = decimal_str.parse().unwrap_or(0.0);
                if *negative { -v } else { v }
            }
            FloatLit::Hex { negative, hex_str } => {
                let v = f32::from_hex(hex_str).unwrap_or(0.0);
                if *negative { -v } else { v }
            }
            FloatLit::Inf { negative: true } => f32::NEG_INFINITY,
            FloatLit::Inf { negative: false } => f32::INFINITY,
            FloatLit::Nan { .. } => f32::NAN,
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::Keyword(s) => write!(f, "{}", s),
            TokenKind::Id(s) => write!(f, "${}", s),
            TokenKind::String(bytes) => {
                write!(f, "\"")?;
                for &b in bytes {
                    if b.is_ascii_graphic() || b == b' ' {
                        write!(f, "{}", b as char)?;
                    } else {
                        write!(f, "\\{:02x}", b)?;
                    }
                }
                write!(f, "\"")
            }
            TokenKind::Integer(sv) => {
                if sv.negative {
                    write!(f, "-{}", sv.value)
                } else {
                    write!(f, "{}", sv.value)
                }
            }
            TokenKind::Float(fl) => match fl {
                FloatLit::Decimal { negative, decimal_str } => {
                    if *negative {
                        write!(f, "-{}", decimal_str)
                    } else {
                        write!(f, "{}", decimal_str)
                    }
                }
                FloatLit::Hex { negative, hex_str } => {
                    if *negative {
                        write!(f, "-{}", hex_str)
                    } else {
                        write!(f, "{}", hex_str)
                    }
                }
                FloatLit::Inf { negative: true } => write!(f, "-inf"),
                FloatLit::Inf { negative: false } => write!(f, "inf"),
                FloatLit::Nan {
                    negative,
                    payload: None,
                } => {
                    if *negative {
                        write!(f, "-nan")
                    } else {
                        write!(f, "nan")
                    }
                }
                FloatLit::Nan {
                    negative,
                    payload: Some(p),
                } => {
                    if *negative {
                        write!(f, "-nan:0x{:x}", p)
                    } else {
                        write!(f, "nan:0x{:x}", p)
                    }
                }
            },
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.kind, self.span)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_len() {
        let span = Span::new(10, 15, 1, 1);
        assert_eq!(span.len(), 5);
    }

    #[test]
    fn span_display() {
        let span = Span::new(0, 5, 3, 7);
        assert_eq!(format!("{}", span), "3:7");
    }

    #[test]
    fn signed_value_to_i64() {
        // Unsigned values
        assert_eq!(SignedValue::unsigned(42u64).to_i64(), Some(42));
        assert_eq!(SignedValue::unsigned(0u64).to_i64(), Some(0));
        assert_eq!(SignedValue::unsigned(i64::MAX as u64).to_i64(), Some(i64::MAX));

        // Negative values
        assert_eq!(SignedValue::signed(42u64, true).to_i64(), Some(-42));
        assert_eq!(SignedValue::signed(0u64, true).to_i64(), Some(0)); // -0 becomes 0
        assert_eq!(SignedValue::signed(i64::MAX as u64 + 1, true).to_i64(), Some(i64::MIN));

        // Overflow
        assert_eq!(SignedValue::unsigned(i64::MAX as u64 + 1).to_i64(), None);
        assert_eq!(SignedValue::signed(i64::MAX as u64 + 2, true).to_i64(), None);
    }

    #[test]
    fn signed_value_to_u64() {
        assert_eq!(SignedValue::unsigned(42u64).to_u64(), Some(42));
        assert_eq!(SignedValue::unsigned(u64::MAX).to_u64(), Some(u64::MAX));
        assert_eq!(SignedValue::signed(0u64, true).to_u64(), Some(0)); // -0 is 0
        assert_eq!(SignedValue::signed(1u64, true).to_u64(), None); // -1 invalid as u64
    }

    #[test]
    fn token_kind_display() {
        assert_eq!(format!("{}", TokenKind::LeftParen), "(");
        assert_eq!(format!("{}", TokenKind::RightParen), ")");
        assert_eq!(format!("{}", TokenKind::Keyword("func".into())), "func");
        assert_eq!(format!("{}", TokenKind::Id("name".into())), "$name");
        assert_eq!(format!("{}", TokenKind::Integer(SignedValue::unsigned(42))), "42");
        assert_eq!(format!("{}", TokenKind::Integer(SignedValue::signed(42, true))), "-42");
        assert_eq!(format!("{}", TokenKind::String(b"hi".to_vec())), "\"hi\"");
        assert_eq!(format!("{}", TokenKind::String(vec![0x00, 0x0a])), "\"\\00\\0a\"");
    }

    #[test]
    fn float_lit_display() {
        assert_eq!(
            format!("{}", TokenKind::Float(FloatLit::Inf { negative: false })),
            "inf"
        );
        assert_eq!(
            format!("{}", TokenKind::Float(FloatLit::Inf { negative: true })),
            "-inf"
        );
        assert_eq!(
            format!(
                "{}",
                TokenKind::Float(FloatLit::Nan {
                    negative: false,
                    payload: None
                })
            ),
            "nan"
        );
        assert_eq!(
            format!(
                "{}",
                TokenKind::Float(FloatLit::Nan {
                    negative: false,
                    payload: Some(0x1234)
                })
            ),
            "nan:0x1234"
        );
    }

    #[test]
    fn token_display() {
        let token = Token::new(TokenKind::Keyword("module".into()), Span::new(1, 7, 1, 2));
        assert_eq!(format!("{}", token), "module at 1:2");

        let token = Token::new(TokenKind::Id("foo".into()), Span::new(10, 14, 3, 5));
        assert_eq!(format!("{}", token), "$foo at 3:5");
    }

    #[test]
    fn token_text() {
        let source = "(module $test)";
        let token = Token::new(TokenKind::Keyword("module".into()), Span::new(1, 7, 1, 2));
        assert_eq!(token.text(source), "module");

        let token = Token::new(TokenKind::Id("test".into()), Span::new(8, 13, 1, 9));
        assert_eq!(token.text(source), "$test");
    }
}
