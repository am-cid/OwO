use crate::lexer::token::{Offset, Position};

/// width of the line column in error messages
pub const COL_WIDTH: usize = 3;

#[derive(Debug)]
pub struct LexerError {
    pub kind: LexerErrorKind,
    pub offset: Offset,
    pub line_offset: Offset,
    pub buffer_line: usize,
    pub buffer_col: usize,
}
impl LexerError {
    pub fn source_string(&self, source: &str) -> String {
        self.kind.source_string(
            self.source_str(source).trim(),
            self.buffer_line,
            self.buffer_col,
        )
    }
}
impl Position<'_> for LexerError {
    fn offset(&self) -> Offset {
        self.line_offset
    }
}
impl Default for LexerError {
    fn default() -> Self {
        Self {
            kind: LexerErrorKind::default(),
            offset: Offset::default(),
            line_offset: Offset::default(),
            buffer_line: 1,
            buffer_col: 1,
        }
    }
}

#[derive(Debug)]
pub enum LexerErrorKind {
    Unexpected(UnexpectedChar),
    Unknown(UnknownChar),
    UnclosedString(UnclosedString),
    InvalidEscapeChar(InvalidEscapeChar),
    UnclosedChar(UnclosedChar),
    EmptyChar(EmptyChar),
    // for trying another branch like += and +
    TryAgain,
}
impl Default for LexerErrorKind {
    fn default() -> Self {
        Self::TryAgain
    }
}
impl LexerErrorKind {
    fn source_string(&self, line_text: &str, buffer_line: usize, buffer_col: usize) -> String {
        match self {
            LexerErrorKind::Unexpected(err) => {
                err.source_string(line_text, buffer_line, buffer_col)
            }
            LexerErrorKind::Unknown(err) => err.source_string(line_text, buffer_line, buffer_col),
            LexerErrorKind::UnclosedString(err) => {
                err.source_string(line_text, buffer_line, buffer_col)
            }
            LexerErrorKind::InvalidEscapeChar(err) => {
                err.source_string(line_text, buffer_line, buffer_col)
            }
            LexerErrorKind::UnclosedChar(err) => {
                err.source_string(line_text, buffer_line, buffer_col)
            }
            LexerErrorKind::EmptyChar(err) => err.source_string(line_text, buffer_line, buffer_col),
            LexerErrorKind::TryAgain => "".into(),
        }
    }
}

#[derive(Debug)]
pub struct UnexpectedChar {
    pub actual: char,
    pub expected: Vec<char>,
}
impl UnexpectedChar {
    fn source_string(&self, line_text: &str, buffer_line: usize, buffer_col: usize) -> String {
        format!(
            r#"[UNEXPECTED TOKEN] at line {buffer_line}, col {buffer_col}
Expected any in: {expected}
Got: {got}
------{border}
{buffer_line:COL_WIDTH$} | {line_text}
{none:COL_WIDTH$} | {highlight}
------{border}"#,
            expected = self
                .expected
                .iter()
                .map(|e| format!("'{}'", e))
                .collect::<Vec<_>>()
                .join(", "),
            got = self.actual,
            border = "-".repeat(line_text.len()),
            highlight = " ".repeat(buffer_col - 1) + "^",
            none = " ",
        )
    }
}

#[derive(Debug)]
pub struct UnknownChar {
    pub ch: char,
}
impl UnknownChar {
    fn source_string(&self, line_text: &str, buffer_line: usize, buffer_col: usize) -> String {
        format!(
            r#"[UNKNOWN TOKEN {token:?}] at line {buffer_line}, col {buffer_col}
------{border}
{buffer_line:COL_WIDTH$} | {line_text}
{none:COL_WIDTH$} | {highlight}
------{border}"#,
            token = self.ch,
            border = "-".repeat(line_text.len()),
            highlight = " ".repeat(buffer_col - 1) + "^",
            none = " ",
        )
    }
}

#[derive(Debug)]
pub struct UnclosedString {
    pub actual: char,
    pub length: usize,
}
impl UnclosedString {
    fn source_string(&self, line_text: &str, buffer_line: usize, buffer_col: usize) -> String {
        format!(
            r#"[UNCLOSED STRING] at line {buffer_line}, col {col_start}-{col_end}
    Expected: '"'
    Got: {actual:?}
------{border}
{buffer_line:COL_WIDTH$} | {line_text}
{none:COL_WIDTH$} | {highlight}
------{border}"#,
            col_start = buffer_col + 1 - self.length,
            col_end = buffer_col,
            actual = self.actual,
            border = "-".repeat(line_text.len()),
            highlight = " ".repeat(buffer_col - self.length) + &"^".repeat(self.length),
            none = " ",
        )
    }
}

#[derive(Debug)]
pub struct UnclosedChar {
    pub actual: char,
    pub length: usize,
}
impl UnclosedChar {
    fn source_string(&self, line_text: &str, buffer_line: usize, buffer_col: usize) -> String {
        format!(
            r#"[UNCLOSED CHARACTER] at line {buffer_line}, col {col_start}-{col_end}
    Expected "'"
    Got: {actual:?}
------{border}
{buffer_line:COL_WIDTH$} | {line_text}
{none:COL_WIDTH$} | {highlight}
------{border}"#,
            col_start = buffer_col - self.length,
            col_end = buffer_col,
            actual = self.actual,
            border = "-".repeat(line_text.len()),
            highlight = " ".repeat(buffer_col - 1 - self.length) + &"^".repeat(self.length),
            none = " ",
        )
    }
}

#[derive(Debug)]
pub struct InvalidEscapeChar {
    pub actual: char,
}
impl InvalidEscapeChar {
    fn source_string(&self, line_text: &str, buffer_line: usize, buffer_col: usize) -> String {
        format!(
            r#"[INVALID ESCAPE] at line {buffer_line}, col {buffer_col}
Valid escapes: \n, \t, \r, \\, \', \", \0
Got: {got}
------{border}
{buffer_line:COL_WIDTH$} | {line_text}
{none:COL_WIDTH$} | {highlight}
------{border}"#,
            got = self.actual,
            border = "-".repeat(line_text.len()),
            highlight = " ".repeat(buffer_col - 1) + "^",
            none = " ",
        )
    }
}

#[derive(Debug)]
pub struct EmptyChar {}
impl EmptyChar {
    fn source_string(&self, line_text: &str, buffer_line: usize, buffer_col: usize) -> String {
        format!(
            r#"[EMPTY CHARACTER LITERAL] at line {buffer_line}, col {col_start}-{col_end}
    Character literals should not be empty
------{border}
{buffer_line:COL_WIDTH$} | {line_text}
{none:COL_WIDTH$} | {highlight}
------{border}"#,
            col_start = buffer_col - 1,
            col_end = buffer_col,
            border = "-".repeat(line_text.len()),
            highlight = " ".repeat(buffer_col - 2) + &"^^",
            line_text = line_text,
            none = " ",
        )
    }
}
