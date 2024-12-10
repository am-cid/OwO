use std::fmt;

pub trait CompilerError {
    fn message(&self) -> String;
}

/// width of the line column in error messages
const COL_WIDTH: usize = 3;

#[derive(Debug, Clone, Copy)]
pub enum LexerError<'a> {
    UnknownToken(UnknownToken<'a>),
    UnclosedString(UnclosedString<'a>),
    UnclosedChar(UnclosedChar<'a>),
    EmptyChar(EmptyChar<'a>),
}
impl<'a> fmt::Display for LexerError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::UnknownToken(res) => res.to_string(),
                Self::UnclosedString(res) => res.to_string(),
                Self::UnclosedChar(res) => res.to_string(),
                Self::EmptyChar(res) => res.to_string(),
            }
        )
    }
}
#[derive(Debug, Clone, Copy)]
pub struct UnknownToken<'a> {
    pub token: char,
    pub line_text: &'a str,
    pub line: usize,
    pub col: usize,
}
impl<'a> fmt::Display for UnknownToken<'a> {
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
                token = self.token,
                border = "-".repeat(self.line_text.len()),
                highlight = " ".repeat(self.col) + "^",
                line_text = self.line_text,
                none = " ",
            )
            .trim(),
        )
    }
}
#[derive(Debug, Clone, Copy)]
pub struct UnclosedString<'a> {
    pub actual: char,
    pub line_text: &'a str,
    pub line: usize,
    pub col: usize,
    pub length: usize,
}
impl<'a> fmt::Display for UnclosedString<'a> {
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
#[derive(Debug, Clone, Copy)]
pub struct UnclosedChar<'a> {
    pub actual: char,
    pub line_text: &'a str,
    pub line: usize,
    pub col: usize,
    pub length: usize,
}
impl<'a> fmt::Display for UnclosedChar<'a> {
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
#[derive(Debug, Clone, Copy)]
pub struct EmptyChar<'a> {
    pub line_text: &'a str,
    pub line: usize,
    pub col: usize,
}
impl<'a> fmt::Display for EmptyChar<'a> {
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
