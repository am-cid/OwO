use crate::{
    errors::lex_errors::COL_WIDTH,
    lexer::token::{Position, Token, TokenKind},
    utils::string::StringExt,
};
use core::fmt;

#[derive(Debug)]
pub enum ParserError<'src> {
    EmptyBody(EmptyBodyError<'src>),
    NoMain(NoMainError<'src>),
    Unexpected(UnexpectedTokenError<'src>),
}
impl fmt::Display for ParserError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyBody(res) => res.fmt(f),
            Self::NoMain(res) => res.fmt(f),
            Self::Unexpected(res) => res.fmt(f),
        }
    }
}

#[derive(Debug)]
pub struct EmptyBodyError<'src> {
    opening_buffer_pos: (usize, usize),
    closing_str: &'src str,
    closing_buffer_pos: (usize, usize),
    expected: Vec<TokenKind>,
    body_type: BodyType,
    line_text: &'src str,
}
impl<'src> EmptyBodyError<'src> {
    pub fn new(
        opening_buffer_pos: (usize, usize),
        closing_str: &'src str,
        closing_buffer_pos: (usize, usize),
        expected: Vec<TokenKind>,
        body_type: BodyType,
        line_text: &'src str,
    ) -> Self {
        Self {
            opening_buffer_pos,
            closing_str,
            closing_buffer_pos,
            expected,
            body_type,
            line_text,
        }
    }
}
impl<'src> fmt::Display for EmptyBodyError<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let pipe = "|".blue();
        let none = " ";
        let line = self.closing_buffer_pos.0;
        let padding = " ".repeat(
            self.line_text
                .chars()
                .take_while(|c| c.is_ascii_whitespace())
                .count(),
        );
        let sample_statement = self.body_type.sample_statement();
        let suggestion = format!(
            ">_< try putting this statement before '{}'",
            self.closing_str,
        )
        .green()
        .italic();
        let line_text = self.line_text;

        write!(
            f,
            "{}",
            format!(
                r#"
{header}
{context}
{expected}
{none:COL_WIDTH$} {pipe}
{none:COL_WIDTH$} {pipe} {padding}{suggestion}
{suggestion_text}
"#,
                header = format!("[EMPTY {} BODY]", self.body_type).red(),
                context = "uwu block statements must not be empty".bold(),
                expected = format!(
                    "{} {}",
                    "Expected any in:".bold(),
                    self.expected
                        .iter()
                        .map(|e| format!("'{}'", e))
                        .collect::<Vec<_>>()
                        .join(", ")
                        .italic()
                ),
                suggestion_text = if self.opening_buffer_pos.0 == self.closing_buffer_pos.0 {
                    format!(
                        r#"{line:COL_WIDTH$} {pipe} {line_text_with_statement}
{none:COL_WIDTH$} {pipe} {error_msg}"#,
                        line_text_with_statement = format!(
                            "{} {} {}",
                            self.line_text
                                .get(0..self.opening_buffer_pos.1)
                                .unwrap_or_default(),
                            // "",
                            sample_statement.green(),
                            self.line_text
                                .get(self.closing_buffer_pos.1 - 1..)
                                .unwrap_or_default(),
                        ),
                        error_msg = format!(
                            "{}{} Expected statement, got '{}'",
                            " ".repeat(self.closing_buffer_pos.1 + sample_statement.len()),
                            "^".repeat(self.closing_str.len()),
                            self.closing_str
                        )
                        .red()
                        .italic()
                    )
                } else {
                    format!(
                        r#"{line_before:COL_WIDTH$} {pipe} {padding}{sample_statement}
{line:COL_WIDTH$} {pipe} {line_text}
{none:COL_WIDTH$} {pipe} {error_msg}"#,
                        line_before = line - 1,
                        sample_statement = sample_statement.green(),
                        error_msg = format!(
                            "{}{} Expected statement, got '{}'",
                            " ".repeat(self.closing_buffer_pos.1 - 1),
                            "^".repeat(self.closing_str.len()),
                            self.closing_str
                        )
                        .red()
                        .italic()
                    )
                }
            )
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BodyType {
    Fn,
    If,
    Mash,
    MashCase,
    For,
    Method,
    Group,
    Contract,
}
impl fmt::Display for BodyType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Fn => "FUNCTION",
                Self::If => "IF",
                Self::Mash => "MASH",
                Self::MashCase => "MASH CASE",
                Self::For => "FOR",
                Self::Method => "METHOD",
                Self::Group => "GROUP",
                Self::Contract => "CONTRACT",
            }
        )
    }
}
impl BodyType {
    pub fn sample_statement(&self) -> &'static str {
        match self {
            Self::Fn | Self::If | Self::MashCase | Self::For | Self::Method => {
                "pwint(\"hewwo world :3\")~"
            }
            Self::Mash => "default: pwint(\"hewwo world :3\")~",
            Self::Group => "field-chan~",
            Self::Contract => "method-chan(kun, senpai)~",
        }
    }
}

#[derive(Debug)]
pub struct NoMainError<'src> {
    line_text: &'src str,
    buffer_pos: (usize, usize),
}
impl<'src> NoMainError<'src> {
    pub fn new(line_text: &'src str, buffer_pos: (usize, usize)) -> Self {
        Self {
            line_text,
            buffer_pos,
        }
    }
}
impl<'src> fmt::Display for NoMainError<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format!(
                r#"
{header}
{context}
{none:COL_WIDTH$} {pipe}
{line:COL_WIDTH$} {pipe} {line_text}
{none:COL_WIDTH$} {pipe} {highlight} {error_msg}
{dots}
{dots}
{none:COL_WIDTH$} {pipe} {suggestion}
{line_1:COL_WIDTH$} {pipe} {main_func_head}
{line_2:COL_WIDTH$} {pipe} {main_func_body}
{line_3:COL_WIDTH$} {pipe} {main_func_tail}
"#,
                header = "[NO MAIN FUNCTION]".red(),
                context = "All .uwu programs must have a main function".bold(),
                pipe = "|".blue(),
                line = self.buffer_pos.0,
                none = " ",
                line_text = self.line_text,
                highlight = format!("{}{}", " ".repeat(self.buffer_pos.1), "^".red(),),
                error_msg = "expected main function, found EOF".red(),
                dots = ".".repeat(self.buffer_pos.1.to_string().len() + 1).blue(),
                suggestion = ">_< Define a main function".green().italic(),
                line_1 = self.buffer_pos.0 + 1,
                line_2 = self.buffer_pos.0 + 2,
                line_3 = self.buffer_pos.0 + 3,
                main_func_head = "fun main-san() {".green(),
                main_func_body = "    pwint(\"hewwo world :3\")~".green(),
                main_func_tail = "}".green(),
            )
        )
    }
}

#[derive(Debug)]
pub struct UnexpectedTokenError<'src> {
    actual_str: &'src str,
    actual_kind: TokenKind,
    actual_buffer_pos: (usize, usize),
    expected_kinds: Vec<TokenKind>,
    header: Option<&'src str>,
    context: Option<&'src str>,
    line_text: &'src str,
}
impl<'src> UnexpectedTokenError<'src> {
    pub fn new(
        source: &'src str,
        line_starts: &'src Vec<usize>,
        actual: Token,
        expected_kinds: Vec<TokenKind>,
        header: Option<&'src str>,
        context: Option<&'src str>,
    ) -> Self {
        let line = actual.line(line_starts);
        Self {
            actual_str: &source[actual.offset.range()],
            actual_kind: actual.kind,
            actual_buffer_pos: (
                actual.buffer_line(line_starts),
                actual.buffer_col(line_starts),
            ),
            expected_kinds,
            header,
            context,
            line_text: source.lines().nth(line).unwrap_or_default(),
        }
    }
}
impl<'src> fmt::Display for UnexpectedTokenError<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let pipe = "|".blue();
        write!(
            f,
            "{}",
            format!(
                r#"
{header} {line_col_info}
{context}{expected}
{got}
{none:COL_WIDTH$} {pipe}
{line:COL_WIDTH$} {pipe} {line_text}
{none:COL_WIDTH$} {pipe} {padding}{highlight} {error_msg}
"#,
                header = format!("[{}]", self.header.unwrap_or("UNEXPECTED TOKEN")).red(),
                line_col_info = if self.actual_str.len() == 1 {
                    format!(
                        "at line {} column {}",
                        self.actual_buffer_pos.0, self.actual_buffer_pos.1
                    )
                    .bold()
                } else {
                    format!(
                        "at line {} from column {} to {}",
                        self.actual_buffer_pos.0,
                        self.actual_buffer_pos.1,
                        self.actual_buffer_pos.1 + self.actual_str.len() - 1,
                    )
                    .bold()
                },
                context = match &self.context {
                    Some(ctx) => {
                        ctx.italic() + "\n"
                    }
                    None => "".to_string(),
                },
                expected = format!(
                    "{} {}",
                    "Expected any in:".bold(),
                    self.expected_kinds
                        .iter()
                        .map(|e| format!("'{}'", e))
                        .collect::<Vec<_>>()
                        .join(", ")
                        .italic()
                ),
                got = format!(
                    "{} '{}'",
                    "Got:".bold(),
                    self.actual_kind.to_string().italic()
                )
                .bold(),
                none = " ",
                line = self.actual_buffer_pos.0,
                line_text = self.line_text,
                padding = " ".repeat(self.actual_buffer_pos.1 - 1),
                highlight = "^".repeat(self.actual_str.len()).red().italic(),
                error_msg = "unexpected token".red().italic(),
            )
        )
    }
}
