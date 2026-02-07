//! S-Expression representation for WAT parsing.
//!
//! WAT (WebAssembly Text Format) is fundamentally an S-expression language.
//! This module provides a two-phase parsing approach:
//!
//! 1. **Lexer -> S-Expression Tree**: Parentheses are matched and tokens are
//!    organised into a tree structure. This phase handles syntax only.
//!
//! 2. **S-Expression Tree -> Module**: The tree is traversed to build the
//!    WebAssembly module. This phase handles semantics.
//!
//! This separation eliminates lookahead problems: when parsing a list, you can
//! see all its children without consuming tokens speculatively.
//!
//! # Example
//!
//! ```
//! use kasm::wat::sexpr::read;
//!
//! let sexpr = read("(module (func $add (param i32 i32) (result i32)))").unwrap();
//! let list = sexpr.as_list().unwrap();
//! assert_eq!(list.head_keyword(), Some("module"));
//! assert_eq!(list.len(), 2); // "module" and "(func ...)"
//! ```

use super::error::LexError;
use super::lexer::Lexer;
use super::token::{Span, Token, TokenKind};
use std::fmt;
use std::iter::Peekable;

// ============================================================================
// Error Type
// ============================================================================

/// An error encountered while reading S-expressions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReadError {
    pub message: String,
    pub span: Span,
}

impl ReadError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

impl fmt::Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at line {}, column {}",
            self.message, self.span.line, self.span.column
        )
    }
}

impl std::error::Error for ReadError {}

impl From<LexError> for ReadError {
    fn from(e: LexError) -> Self {
        Self {
            message: e.message,
            span: e.span,
        }
    }
}

// ============================================================================
// S-Expression Types
// ============================================================================

/// An S-expression: either an atom (single token) or a parenthesised list.
///
/// This is the fundamental unit of WAT syntax. Every WAT construct is either
/// an atomic value (number, string, identifier, keyword) or a list of
/// S-expressions enclosed in parentheses.
#[derive(Debug, Clone)]
pub enum SExpr {
    /// A single token: keyword, integer, float, string, or identifier.
    Atom(Token),

    /// A parenthesised list of S-expressions.
    ///
    /// The span covers the entire list including the parentheses.
    /// In WAT, the first element is typically a keyword identifying the
    /// construct (e.g., "module", "func", "param").
    List { span: Span, items: Vec<SExpr> },
}

impl SExpr {
    /// Returns the span of this S-expression.
    pub fn span(&self) -> Span {
        match self {
            SExpr::Atom(token) => token.span,
            SExpr::List { span, .. } => *span,
        }
    }

    /// Returns the token if this is an atom.
    pub fn as_atom(&self) -> Option<&Token> {
        match self {
            SExpr::Atom(token) => Some(token),
            SExpr::List { .. } => None,
        }
    }

    /// Returns the items if this is a list.
    pub fn as_list(&self) -> Option<SExprList<'_>> {
        match self {
            SExpr::Atom(_) => None,
            SExpr::List { span, items } => Some(SExprList { span: *span, items }),
        }
    }

    /// Returns the keyword string if this is an atom containing a keyword.
    pub fn as_keyword(&self) -> Option<&str> {
        match self {
            SExpr::Atom(Token {
                kind: TokenKind::Keyword(kw),
                ..
            }) => Some(kw),
            _ => None,
        }
    }

    /// Returns the identifier string if this is an atom containing an id.
    pub fn as_id(&self) -> Option<&str> {
        match self {
            SExpr::Atom(Token {
                kind: TokenKind::Id(id),
                ..
            }) => Some(id),
            _ => None,
        }
    }

    /// Returns true if this is a list starting with the given keyword.
    pub fn is_list_headed_by(&self, keyword: &str) -> bool {
        self.as_list()
            .map(|list| list.head_keyword() == Some(keyword))
            .unwrap_or(false)
    }

    /// Expects this to be a list, returning an error if not.
    pub fn expect_list(&self) -> Result<SExprList<'_>, ReadError> {
        self.as_list()
            .ok_or_else(|| ReadError::new("expected list", self.span()))
    }

    /// Expects this to be an atom, returning an error if not.
    pub fn expect_atom(&self) -> Result<&Token, ReadError> {
        self.as_atom()
            .ok_or_else(|| ReadError::new("expected atom", self.span()))
    }

    /// Expects this to be a keyword atom, returning the keyword string.
    pub fn expect_keyword(&self) -> Result<&str, ReadError> {
        self.as_keyword()
            .ok_or_else(|| ReadError::new("expected keyword", self.span()))
    }
}

// ============================================================================
// List View
// ============================================================================

/// A borrowed view of an S-expression list, providing convenient accessors.
///
/// This is the primary interface for parsing WAT constructs. Most WAT
/// constructs follow the pattern `(keyword arg1 arg2 ...)`, and this type
/// provides methods to extract and validate that pattern.
#[derive(Debug, Clone, Copy)]
pub struct SExprList<'a> {
    pub span: Span,
    pub items: &'a [SExpr],
}

impl<'a> SExprList<'a> {
    /// Returns the number of items in this list.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Returns true if this list is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Returns the first item (the "head") of this list.
    pub fn head(&self) -> Option<&'a SExpr> {
        self.items.first()
    }

    /// Returns all items after the first (the "tail") of this list.
    pub fn tail(&self) -> &'a [SExpr] {
        if self.items.is_empty() { &[] } else { &self.items[1..] }
    }

    /// Returns the item at the given index.
    pub fn get(&self, index: usize) -> Option<&'a SExpr> {
        self.items.get(index)
    }

    /// Returns the keyword if the head is a keyword atom.
    ///
    /// This is the most common pattern in WAT: `(keyword ...)`.
    pub fn head_keyword(&self) -> Option<&'a str> {
        self.head().and_then(|s| s.as_keyword())
    }

    /// Expects the head to be a specific keyword.
    pub fn expect_head(&self, expected: &str) -> Result<(), ReadError> {
        match self.head_keyword() {
            Some(kw) if kw == expected => Ok(()),
            Some(kw) => Err(ReadError::new(
                format!("expected '{}', found '{}'", expected, kw),
                self.head().unwrap().span(),
            )),
            None => Err(ReadError::new(format!("expected '{}' keyword", expected), self.span)),
        }
    }

    /// Iterates over items starting from the given index.
    pub fn iter_from(&self, start: usize) -> impl Iterator<Item = &'a SExpr> {
        self.items.iter().skip(start)
    }

    /// Finds the first list with the given head keyword, starting from index.
    pub fn find_list(&self, keyword: &str, from: usize) -> Option<SExprList<'a>> {
        self.iter_from(from)
            .find(|s| s.is_list_headed_by(keyword))
            .and_then(|s| s.as_list())
    }

    /// Returns all lists with the given head keyword, starting from index.
    pub fn filter_lists(&self, keyword: &str, from: usize) -> impl Iterator<Item = SExprList<'a>> {
        self.iter_from(from)
            .filter(move |s| s.is_list_headed_by(keyword))
            .filter_map(|s| s.as_list())
    }
}

// ============================================================================
// Reader
// ============================================================================

/// Reads an S-expression from WAT source text.
///
/// This is the entry point for parsing. The returned S-expression tree
/// represents the complete syntactic structure of the input, with all
/// parentheses matched.
///
/// # Example
///
/// ```
/// use kasm::wat::sexpr::read;
///
/// let sexpr = read("(module)").unwrap();
/// assert!(sexpr.as_list().is_some());
/// ```
///
/// # Errors
///
/// Returns an error if:
/// - The input contains lexical errors (e.g., unterminated string)
/// - Parentheses are unbalanced
/// - There are extra tokens after the expression
#[must_use = "parsing result should be checked"]
pub fn read(source: &str) -> Result<SExpr, ReadError> {
    let lexer = Lexer::new(source);
    let mut tokens = lexer.peekable();

    let sexpr = read_sexpr(&mut tokens)?;

    // Ensure we consumed all input
    match tokens.next() {
        Some(Ok(token)) => Err(ReadError::new("unexpected token after expression", token.span)),
        Some(Err(e)) => Err(ReadError::from(e)),
        None => Ok(sexpr),
    }
}

/// Reads multiple S-expressions from source (for files with multiple top-level forms).
pub fn read_all(source: &str) -> Result<Vec<SExpr>, ReadError> {
    let lexer = Lexer::new(source);
    let mut tokens = lexer.peekable();
    let mut results = Vec::new();

    while tokens.peek().is_some() {
        results.push(read_sexpr(&mut tokens)?);
    }

    Ok(results)
}

/// Internal: reads a single S-expression from the token stream.
fn read_sexpr<I>(tokens: &mut Peekable<I>) -> Result<SExpr, ReadError>
where
    I: Iterator<Item = Result<Token, LexError>>,
{
    let token = next_token(tokens)?;

    match token.kind {
        TokenKind::LeftParen => {
            let start_span = token.span;
            let mut items = Vec::new();

            loop {
                match peek_token(tokens)? {
                    Some(Token {
                        kind: TokenKind::RightParen,
                        ..
                    }) => {
                        let end_token = next_token(tokens)?;
                        let span = Span {
                            start: start_span.start,
                            end: end_token.span.end,
                            line: start_span.line,
                            column: start_span.column,
                        };
                        return Ok(SExpr::List { span, items });
                    }
                    Some(_) => {
                        items.push(read_sexpr(tokens)?);
                    }
                    None => {
                        return Err(ReadError::new("unclosed parenthesis", start_span));
                    }
                }
            }
        }
        TokenKind::RightParen => Err(ReadError::new("unexpected ')'", token.span)),
        _ => Ok(SExpr::Atom(token)),
    }
}

/// Helper: get next token or error on EOF.
fn next_token<I>(tokens: &mut Peekable<I>) -> Result<Token, ReadError>
where
    I: Iterator<Item = Result<Token, LexError>>,
{
    match tokens.next() {
        Some(Ok(token)) => Ok(token),
        Some(Err(e)) => Err(ReadError::from(e)),
        None => Err(ReadError::new(
            "unexpected end of input",
            Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            },
        )),
    }
}

/// Helper: peek at next token without consuming.
fn peek_token<I>(tokens: &mut Peekable<I>) -> Result<Option<&Token>, ReadError>
where
    I: Iterator<Item = Result<Token, LexError>>,
{
    match tokens.peek() {
        Some(Ok(token)) => Ok(Some(token)),
        Some(Err(e)) => Err(ReadError::new(e.message.clone(), e.span)),
        None => Ok(None),
    }
}

// ============================================================================
// Display
// ============================================================================

impl fmt::Display for SExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SExpr::Atom(token) => write!(f, "{}", token),
            SExpr::List { items, .. } => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // ------------------------------------------------------------------------
    // Basic Structure
    // ------------------------------------------------------------------------

    #[test]
    fn read_atom_keyword() {
        let sexpr = read("module").unwrap();
        assert_eq!(sexpr.as_keyword(), Some("module"));
    }

    #[test]
    fn read_atom_integer() {
        let sexpr = read("42").unwrap();
        let token = sexpr.as_atom().unwrap();
        assert!(matches!(token.kind, TokenKind::Integer(_)));
    }

    #[test]
    fn read_atom_id() {
        let sexpr = read("$main").unwrap();
        assert_eq!(sexpr.as_id(), Some("main"));
    }

    #[test]
    fn read_empty_list() {
        let sexpr = read("()").unwrap();
        let list = sexpr.as_list().unwrap();
        assert!(list.is_empty());
    }

    #[test]
    fn read_simple_list() {
        let sexpr = read("(module)").unwrap();
        let list = sexpr.as_list().unwrap();
        assert_eq!(list.len(), 1);
        assert_eq!(list.head_keyword(), Some("module"));
    }

    #[test]
    fn read_nested_list() {
        let sexpr = read("(module (func) (memory 1))").unwrap();
        let list = sexpr.as_list().unwrap();
        assert_eq!(list.len(), 3);
        assert_eq!(list.head_keyword(), Some("module"));

        let func = list.get(1).unwrap().as_list().unwrap();
        assert_eq!(func.head_keyword(), Some("func"));

        let memory = list.get(2).unwrap().as_list().unwrap();
        assert_eq!(memory.head_keyword(), Some("memory"));
    }

    #[test]
    fn read_deeply_nested() {
        let sexpr = read("(a (b (c (d))))").unwrap();
        let a = sexpr.as_list().unwrap();
        let b = a.get(1).unwrap().as_list().unwrap();
        let c = b.get(1).unwrap().as_list().unwrap();
        let d = c.get(1).unwrap().as_list().unwrap();
        assert_eq!(d.head_keyword(), Some("d"));
    }

    // ------------------------------------------------------------------------
    // List Operations
    // ------------------------------------------------------------------------

    #[test]
    fn list_head_and_tail() {
        let sexpr = read("(func $add (param i32) (result i32))").unwrap();
        let list = sexpr.as_list().unwrap();

        assert_eq!(list.head_keyword(), Some("func"));
        assert_eq!(list.tail().len(), 3); // $add, (param i32), (result i32)
    }

    #[test]
    fn list_find_and_filter() {
        let sexpr = read("(func (param i32) (param i64) (result i32))").unwrap();
        let list = sexpr.as_list().unwrap();

        // find_list returns first match
        let first_param = list.find_list("param", 1).unwrap();
        assert_eq!(first_param.len(), 2); // param, i32

        // filter_lists returns all matches
        let params: Vec<_> = list.filter_lists("param", 1).collect();
        assert_eq!(params.len(), 2);
    }

    #[test]
    fn is_list_headed_by() {
        let sexpr = read("(module (func))").unwrap();
        assert!(sexpr.is_list_headed_by("module"));
        assert!(!sexpr.is_list_headed_by("func"));

        let list = sexpr.as_list().unwrap();
        let func = list.get(1).unwrap();
        assert!(func.is_list_headed_by("func"));
    }

    // ------------------------------------------------------------------------
    // Real WAT Examples
    // ------------------------------------------------------------------------

    #[test]
    fn read_complete_function() {
        let wat = r#"(module
            (func $add (param $a i32) (param $b i32) (result i32)
                (i32.add (local.get $a) (local.get $b))))"#;

        let sexpr = read(wat).unwrap();
        let module = sexpr.as_list().unwrap();
        assert_eq!(module.head_keyword(), Some("module"));

        let func = module.get(1).unwrap().as_list().unwrap();
        assert_eq!(func.head_keyword(), Some("func"));

        // Function name
        assert_eq!(func.get(1).unwrap().as_id(), Some("add"));

        // Find all params
        let params: Vec<_> = func.filter_lists("param", 1).collect();
        assert_eq!(params.len(), 2);
    }

    #[test]
    fn read_import() {
        let wat = r#"(import "env" "memory" (memory 1))"#;
        let sexpr = read(wat).unwrap();
        let list = sexpr.as_list().unwrap();

        assert_eq!(list.head_keyword(), Some("import"));
        // Strings are atoms
        let module_name = list.get(1).unwrap().as_atom().unwrap();
        assert!(matches!(module_name.kind, TokenKind::String(_)));
    }

    #[test]
    fn read_control_flow() {
        let wat = "(block $outer (block $inner (br $outer)))";
        let sexpr = read(wat).unwrap();
        let outer = sexpr.as_list().unwrap();

        assert_eq!(outer.head_keyword(), Some("block"));
        assert_eq!(outer.get(1).unwrap().as_id(), Some("outer"));

        let inner = outer.get(2).unwrap().as_list().unwrap();
        assert_eq!(inner.head_keyword(), Some("block"));
    }

    // ------------------------------------------------------------------------
    // Errors
    // ------------------------------------------------------------------------

    #[test]
    fn error_unclosed_paren() {
        let result = read("(module (func)");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.message.contains("unclosed"));
    }

    #[test]
    fn error_unexpected_rparen() {
        let result = read(")");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.message.contains("unexpected ')'"));
    }

    #[test]
    fn error_extra_tokens() {
        let result = read("(module) extra");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.message.contains("unexpected token"));
    }

    #[test]
    fn error_lexer_propagated() {
        let result = read("\"unterminated");
        assert!(result.is_err());
    }

    // ------------------------------------------------------------------------
    // Expect Methods
    // ------------------------------------------------------------------------

    #[test]
    fn expect_list_on_atom_fails() {
        let sexpr = read("keyword").unwrap();
        assert!(sexpr.expect_list().is_err());
    }

    #[test]
    fn expect_atom_on_list_fails() {
        let sexpr = read("(list)").unwrap();
        assert!(sexpr.expect_atom().is_err());
    }

    #[test]
    fn expect_head_success() {
        let sexpr = read("(module)").unwrap();
        let list = sexpr.as_list().unwrap();
        assert!(list.expect_head("module").is_ok());
    }

    #[test]
    fn expect_head_wrong_keyword() {
        let sexpr = read("(module)").unwrap();
        let list = sexpr.as_list().unwrap();
        let err = list.expect_head("func").unwrap_err();
        assert!(err.message.contains("expected 'func'"));
    }

    // ------------------------------------------------------------------------
    // Display
    // ------------------------------------------------------------------------

    #[test]
    fn display_roundtrip() {
        let original = "(module (func $main (result i32) (i32.const 42)))";
        let sexpr = read(original).unwrap();
        let displayed = sexpr.to_string();
        // Structure preserved (whitespace may differ)
        assert!(displayed.starts_with("(module"));
        assert!(displayed.contains("func"));
        assert!(displayed.contains("$main"));
    }

    // ------------------------------------------------------------------------
    // Spans
    // ------------------------------------------------------------------------

    #[test]
    fn atom_span_correct() {
        let sexpr = read("keyword").unwrap();
        let span = sexpr.span();
        assert_eq!(span.start, 0);
        assert_eq!(span.end, 7);
    }

    #[test]
    fn list_span_covers_parens() {
        let sexpr = read("(module)").unwrap();
        let span = sexpr.span();
        assert_eq!(span.start, 0);
        assert_eq!(span.end, 8);
    }

    #[test]
    fn nested_spans_correct() {
        //           0123456789...
        let sexpr = read("(a (b c))").unwrap();
        let outer = sexpr.as_list().unwrap();
        assert_eq!(outer.span.start, 0);
        assert_eq!(outer.span.end, 9);

        let inner = outer.get(1).unwrap().as_list().unwrap();
        assert_eq!(inner.span.start, 3);
        assert_eq!(inner.span.end, 8);
    }

    // ------------------------------------------------------------------------
    // Edge Cases
    // ------------------------------------------------------------------------

    #[test]
    fn read_multiple() {
        let sexprs = read_all("(a) (b) (c)").unwrap();
        assert_eq!(sexprs.len(), 3);
        assert!(sexprs[0].is_list_headed_by("a"));
        assert!(sexprs[1].is_list_headed_by("b"));
        assert!(sexprs[2].is_list_headed_by("c"));
    }

    #[test]
    fn whitespace_preserved_in_strings() {
        let sexpr = read(r#""hello world""#).unwrap();
        let token = sexpr.as_atom().unwrap();
        if let TokenKind::String(bytes) = &token.kind {
            assert_eq!(std::str::from_utf8(bytes).unwrap(), "hello world");
        } else {
            panic!("expected string token");
        }
    }
}
