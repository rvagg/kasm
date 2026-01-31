//! Character-level cursor for navigating WAT source text.
//!
//! The cursor provides low-level character iteration while tracking position
//! information (byte offset, line, column) for accurate error reporting.

use super::token::Span;

/// A saved position in source text.
///
/// Used to mark a position before consuming tokens, allowing spans to be
/// created that cover the consumed input.
#[derive(Debug, Clone, Copy)]
pub struct Position {
    /// Byte offset from start of source.
    pub offset: usize,
    /// Line number (1-indexed).
    pub line: u32,
    /// Column number (1-indexed, counts characters).
    pub column: u32,
}

impl Position {
    /// Create a span from this position to another position.
    #[must_use]
    pub fn span_to(self, end: &Position) -> Span {
        Span::new(self.offset, end.offset, self.line, self.column)
    }

    /// Create a zero-length span at this position.
    #[must_use]
    pub fn span_here(self) -> Span {
        Span::new(self.offset, self.offset, self.line, self.column)
    }
}

/// A cursor for navigating through source text character by character.
///
/// Tracks byte position, line number, and column number. Columns count
/// Unicode characters (not bytes) for accurate display in error messages.
pub struct Cursor<'a> {
    /// The complete source text.
    source: &'a str,
    /// Remaining source text (slice starting at current position).
    remaining: &'a str,
    /// Current byte offset from start of source.
    offset: usize,
    /// Current line number (1-indexed).
    line: u32,
    /// Current column number (1-indexed, counts characters).
    column: u32,
}

impl<'a> Cursor<'a> {
    /// Create a new cursor at the start of the source text.
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            remaining: source,
            offset: 0,
            line: 1,
            column: 1,
        }
    }

    /// Get the current position.
    pub fn position(&self) -> Position {
        Position {
            offset: self.offset,
            line: self.line,
            column: self.column,
        }
    }

    /// Current byte offset in the source.
    pub fn offset(&self) -> usize {
        self.offset
    }

    /// Whether we've reached the end of input.
    pub fn is_eof(&self) -> bool {
        self.remaining.is_empty()
    }

    /// Peek at the next character without consuming it.
    pub fn peek(&self) -> Option<char> {
        self.remaining.chars().next()
    }

    /// Peek at the character after the next one.
    pub fn peek_second(&self) -> Option<char> {
        let mut chars = self.remaining.chars();
        chars.next();
        chars.next()
    }

    /// Consume and return the next character.
    ///
    /// Updates position, line, and column tracking.
    pub fn advance(&mut self) -> Option<char> {
        let c = self.remaining.chars().next()?;
        let char_len = c.len_utf8();

        self.remaining = &self.remaining[char_len..];
        self.offset += char_len;

        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        Some(c)
    }

    /// Consume characters while the predicate returns true.
    ///
    /// Returns the number of characters consumed.
    pub fn skip_while(&mut self, predicate: impl Fn(char) -> bool) -> usize {
        let mut count = 0;
        while let Some(c) = self.peek() {
            if !predicate(c) {
                break;
            }
            self.advance();
            count += 1;
        }
        count
    }

    /// Extract a slice of the source text by byte offsets.
    ///
    /// This is only available in tests. Production code should use `slice_from`
    /// which takes a `Position` for type safety and clarity.
    #[cfg(test)]
    pub fn slice(&self, start: usize, end: usize) -> &'a str {
        &self.source[start..end]
    }

    /// Extract a slice from a position to the current position.
    pub fn slice_from(&self, start: &Position) -> &'a str {
        &self.source[start.offset..self.offset]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_cursor_starts_at_beginning() {
        let cursor = Cursor::new("hello");
        let pos = cursor.position();
        assert_eq!(pos.offset, 0);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.column, 1);
        assert!(!cursor.is_eof());
    }

    #[test]
    fn empty_source() {
        let cursor = Cursor::new("");
        assert!(cursor.is_eof());
        assert_eq!(cursor.peek(), None);
    }

    #[test]
    fn peek_does_not_advance() {
        let cursor = Cursor::new("ab");
        assert_eq!(cursor.peek(), Some('a'));
        assert_eq!(cursor.peek(), Some('a'));
        assert_eq!(cursor.offset(), 0);
    }

    #[test]
    fn peek_second() {
        let cursor = Cursor::new("abc");
        assert_eq!(cursor.peek(), Some('a'));
        assert_eq!(cursor.peek_second(), Some('b'));
    }

    #[test]
    fn advance_moves_position() {
        let mut cursor = Cursor::new("abc");

        assert_eq!(cursor.advance(), Some('a'));
        assert_eq!(cursor.position().offset, 1);
        assert_eq!(cursor.position().column, 2);

        assert_eq!(cursor.advance(), Some('b'));
        assert_eq!(cursor.position().offset, 2);
        assert_eq!(cursor.position().column, 3);

        assert_eq!(cursor.advance(), Some('c'));
        assert_eq!(cursor.position().offset, 3);
        assert!(cursor.is_eof());

        assert_eq!(cursor.advance(), None);
    }

    #[test]
    fn newlines_update_line_and_column() {
        let mut cursor = Cursor::new("a\nb\nc");

        cursor.advance(); // 'a'
        assert_eq!(cursor.position().line, 1);
        assert_eq!(cursor.position().column, 2);

        cursor.advance(); // '\n'
        assert_eq!(cursor.position().line, 2);
        assert_eq!(cursor.position().column, 1);

        cursor.advance(); // 'b'
        assert_eq!(cursor.position().line, 2);
        assert_eq!(cursor.position().column, 2);

        cursor.advance(); // '\n'
        assert_eq!(cursor.position().line, 3);
        assert_eq!(cursor.position().column, 1);
    }

    #[test]
    fn unicode_characters() {
        let mut cursor = Cursor::new("a\u{1F600}b"); // a, emoji, b

        assert_eq!(cursor.advance(), Some('a'));
        assert_eq!(cursor.position().offset, 1);
        assert_eq!(cursor.position().column, 2);

        assert_eq!(cursor.advance(), Some('\u{1F600}'));
        assert_eq!(cursor.position().offset, 5); // emoji is 4 bytes
        assert_eq!(cursor.position().column, 3); // but 1 character

        assert_eq!(cursor.advance(), Some('b'));
        assert_eq!(cursor.position().offset, 6);
        assert_eq!(cursor.position().column, 4);
    }

    #[test]
    fn skip_while() {
        let mut cursor = Cursor::new("aaabbc");

        let count = cursor.skip_while(|c| c == 'a');
        assert_eq!(count, 3);
        assert_eq!(cursor.peek(), Some('b'));

        let count = cursor.skip_while(|c| c == 'b');
        assert_eq!(count, 2);
        assert_eq!(cursor.peek(), Some('c'));
    }

    #[test]
    fn position_span_to() {
        let mut cursor = Cursor::new("hello");
        let start = cursor.position();

        cursor.advance(); // h
        cursor.advance(); // e
        cursor.advance(); // l

        let span = start.span_to(&cursor.position());
        assert_eq!(span.start, 0);
        assert_eq!(span.end, 3);
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn slice_from_position() {
        let mut cursor = Cursor::new("hello world");
        let start = cursor.position();

        for _ in 0..5 {
            cursor.advance();
        }

        assert_eq!(cursor.slice_from(&start), "hello");
    }

    #[test]
    fn slice() {
        let cursor = Cursor::new("hello world");
        assert_eq!(cursor.slice(0, 5), "hello");
        assert_eq!(cursor.slice(6, 11), "world");
    }
}
