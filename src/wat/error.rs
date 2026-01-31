//! Error types for WAT parsing.

use super::token::Span;
use std::fmt;

/// An error encountered during lexical analysis.
#[derive(Debug, Clone)]
#[must_use]
pub struct LexError {
    /// Human-readable description of the error.
    pub message: String,
    /// Location in source where the error occurred.
    pub span: Span,
}

impl LexError {
    /// Create a new lexer error.
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.span, self.message)
    }
}

impl std::error::Error for LexError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn error_display() {
        let err = LexError::new("unexpected character", Span::new(10, 11, 3, 5));
        assert_eq!(format!("{}", err), "3:5: unexpected character");
    }
}
