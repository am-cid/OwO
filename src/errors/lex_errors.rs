use std::fmt;

/// width of the line column in error messages
pub const COL_WIDTH: usize = 3;

#[derive(Debug)]
pub enum LexerError {
    Unexpected(UnexpectedChar),
    Unknown(UnknownChar),
    UnclosedString(UnclosedString),
    InvalidEscapeChar(InvalidEscapeChar),
    UnclosedChar(UnclosedChar),
    EmptyChar(EmptyChar),
    // for trying another branch like += and +
    TryAgain,
}
impl Default for LexerError {
    fn default() -> Self {
        Self::Unknown(UnknownChar {
            ch: '\0',
            line_text: "".into(),
            line: 0,
            col: 0,
        })
    }
}
impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Unexpected(res) => res.to_string(),
                Self::Unknown(res) => res.to_string(),
                Self::UnclosedString(res) => res.to_string(),
                Self::InvalidEscapeChar(res) => res.to_string(),
                Self::UnclosedChar(res) => res.to_string(),
                Self::EmptyChar(res) => res.to_string(),
                Self::TryAgain => "try again".into(),
            }
        )
    }
}

#[derive(Debug)]
pub struct UnexpectedChar {
    pub actual: char,
    pub expected: Vec<char>,
    pub line_text: String,
    pub line: usize,
    pub col: usize,
}
impl fmt::Display for UnexpectedChar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format!(
                r#"
[UNEXPECTED TOKEN] at line {line}, col {col}
Expected any in: {expected}
Got: {got}
------{border}
{line:COL_WIDTH$} | {line_text}
{none:COL_WIDTH$} | {highlight}
------{border}
"#,
                line = self.line + 1,
                col = self.col + 1,
                expected = self
                    .expected
                    .iter()
                    .map(|e| format!("'{}'", e))
                    .collect::<Vec<_>>()
                    .join(", "),
                got = self.actual,
                border = "-".repeat(self.line_text.len()),
                highlight = " ".repeat(self.col) + "^",
                line_text = self.line_text,
                none = " ",
            )
            .trim(),
        )
    }
}

#[derive(Debug)]
pub struct UnknownChar {
    pub ch: char,
    pub line_text: String,
    pub line: usize,
    pub col: usize,
}
impl fmt::Display for UnknownChar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format!(
                r#"
[UNKNOWN TOKEN {token:?}] at line {line}, col {col}
------{border}
{line:COL_WIDTH$} | {line_text}
{none:COL_WIDTH$} | {highlight}
------{border}
"#,
                line = self.line + 1,
                col = self.col + 1,
                token = self.ch,
                border = "-".repeat(self.line_text.len()),
                highlight = " ".repeat(self.col) + "^",
                line_text = self.line_text,
                none = " ",
            )
            .trim(),
        )
    }
}

#[derive(Debug)]
pub struct UnclosedString {
    pub actual: char,
    pub line_text: String,
    pub line: usize,
    pub col: usize,
    pub length: usize,
}
impl fmt::Display for UnclosedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            format!(
                r#"
[UNCLOSED STRING] at line {line}, col {col_start}-{col_end}
    Expected: '"'
    Got: {actual:?}
------{border}
{line:COL_WIDTH$} | {line_text}
{none:COL_WIDTH$} | {highlight}
------{border}
"#,
                line = self.line + 1,
                col_start = self.col + 1,
                col_end = self.col + self.length,
                actual = self.actual,
                border = "-".repeat(self.line_text.len()),
                highlight = " ".repeat(self.col) + &"^".repeat(self.length),
                line_text = self.line_text,
                none = " ",
            )
            .trim(),
        )
    }
}

#[derive(Debug)]
pub struct InvalidEscapeChar {
    pub actual: char,
    pub line_text: String,
    pub line: usize,
    pub col: usize,
}
impl fmt::Display for InvalidEscapeChar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format!(
                r#"
[INVALID ESCAPE] at line {line}, col {col}
Valid escapes: \n, \t, \r, \\, \', \", \0
Got: {got}
------{border}
{line:COL_WIDTH$} | {line_text}
{none:COL_WIDTH$} | {highlight}
------{border}
"#,
                line = self.line + 1,
                col = self.col + 1,
                got = self.actual,
                border = "-".repeat(self.line_text.len()),
                highlight = " ".repeat(self.col) + "^",
                line_text = self.line_text,
                none = " ",
            )
            .trim(),
        )
    }
}

#[derive(Debug)]
pub struct UnclosedChar {
    pub actual: char,
    pub line_text: String,
    pub line: usize,
    pub col: usize,
    pub length: usize,
}
impl fmt::Display for UnclosedChar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format!(
                r#"
[UNCLOSED CHARACTER] at line {line}, col {col_start}-{col_end}
    Expected "'"
    Got: {actual:?}
------{border}
{line:COL_WIDTH$} | {line_text}
{none:COL_WIDTH$} | {highlight}
------{border}
"#,
                line = self.line + 1,
                col_start = self.col + 1,
                col_end = self.col + self.length,
                actual = self.actual,
                border = "-".repeat(self.line_text.len()),
                highlight = " ".repeat(self.col) + &"^".repeat(self.length),
                line_text = self.line_text,
                none = " ",
            )
            .trim()
        )
    }
}

#[derive(Debug)]
pub struct EmptyChar {
    pub line_text: String,
    pub line: usize,
    pub col: usize,
}
impl fmt::Display for EmptyChar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            format!(
                r#"
[EMPTY CHARACTER LITERAL] at line {line}, col {col_start}-{col_end}
    Character literals should not be empty
------{border}
{line:COL_WIDTH$} | {line_text}
{none:COL_WIDTH$} | {highlight}
------{border}
"#,
                line = self.line + 1,
                col_start = self.col + 1,
                col_end = self.col + 2,
                border = "-".repeat(self.line_text.len()),
                highlight = " ".repeat(self.col) + &"^^",
                line_text = self.line_text,
                none = " ",
            )
            .trim(),
        )
    }
}
