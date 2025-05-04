use crate::{
    errors::lex_errors::COL_WIDTH,
    lexer::token::{Position, Token, TokenKind},
    utils::string::StringExt,
};
use core::fmt;

#[derive(Clone, Copy)]
pub enum EmptyBodyErrorType {
    Fn,
    If,
    Mash,
    MashCase,
    For,
    Method,
    Group,
    Contract,
}
impl fmt::Display for EmptyBodyErrorType {
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
impl EmptyBodyErrorType {
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
pub struct EmptyBodyError<'a> {
    opening_buffer_pos: (usize, usize),
    closing_str: &'a str,
    closing_buffer_pos: (usize, usize),
    expected: Vec<TokenKind>,
    body_type: EmptyBodyErrorType,
    line_text: &'a str,
}
impl<'a> EmptyBodyError<'a> {
    pub fn new(
        opening_buffer_pos: (usize, usize),
        closing_str: &'a str,
        closing_buffer_pos: (usize, usize),
        expected: Vec<TokenKind>,
        body_type: EmptyBodyErrorType,
        line_text: &'a str,
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
impl<'a> fmt::Display for EmptyBodyError<'a> {
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
            .trim()
        )
    }
}

pub struct NoMainError<'a> {
    line_text: &'a str,
    buffer_pos: (usize, usize),
}
impl<'a> NoMainError<'a> {
    pub fn new(line_text: &'a str, buffer_pos: (usize, usize)) -> Self {
        Self {
            line_text,
            buffer_pos,
        }
    }
}
impl<'a> fmt::Display for NoMainError<'a> {
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
            .trim()
        )
    }
}

//
pub struct NonCallableInPipelineError<'a> {
    pipeline_lines: Vec<&'a str>,
    pipeline_pos: (usize, usize),
    non_callable_str: &'a str,
    non_callable_formatted_str: &'a str,
    last_type: &'a str,
}
impl<'a> NonCallableInPipelineError<'a> {
    /// - `pipeline_lines`: text separated by newline of the entire pipeline
    /// including the error.
    /// - `pipeline_pos`: starting buffer position of the entire pipeline.
    /// - `non_callable_str`: unformatted non-callable in the pipeline that
    /// caused the error.
    /// - `non_callable_formatted_str`: the non-callable in pipeline formatted
    /// for the error message
    /// - `non_callable_line_end`: the last line of non_callable if it spans
    /// multiple lines.
    /// - `last_type`: the type of construct the non callable is for it to not
    /// be allowed in the pipeline expression.
    pub fn new(
        pipeline_lines: Vec<&'a str>,
        pipeline_pos: (usize, usize),
        non_callable_str: &'a str,
        non_callable_formatted_str: &'a str,
        last_type: &'a str,
    ) -> Self {
        Self {
            pipeline_lines,
            pipeline_pos,
            non_callable_str,
            non_callable_formatted_str,
            last_type,
        }
    }
}
impl<'a> fmt::Display for NonCallableInPipelineError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ok_pipeline_lines = self
            .pipeline_lines
            .iter()
            .take(self.pipeline_lines.len() - self.non_callable_str.lines().count() - 1)
            .collect::<Vec<_>>();
        println!("ok pipeline lines {ok_pipeline_lines:#?}");
        let error_lines = self
            .pipeline_lines
            .iter()
            .skip(ok_pipeline_lines.len())
            .zip(self.non_callable_str.lines())
            .collect::<Vec<_>>();
        println!("error_lines ({}): {error_lines:#?}", error_lines.len());
        write!(
            f,
            "{}",
            format!(
                r#"
{header} {line_col_info}
{context}
{none:COL_WIDTH$} {pipe}
{preview_ok}{none:COL_WIDTH$} {pipe} {padding}{error_msg}
{preview_error}
"#,
                header = "[NON CALLABLE IN PIPELINE]".red(),
                line_col_info = format!(
                    "starting at line {} column {}",
                    self.pipeline_pos.0 + 1,
                    self.pipeline_pos.1 + 1,
                )
                .bold(),
                context = format!(
                    "{} {}",
                    self.non_callable_formatted_str.bold().underline(),
                    "is not callable so it's not allowed in pipeline expressions".italic()
                ),
                none = " ",
                pipe = "|".blue(),
                preview_ok = match ok_pipeline_lines.len() {
                    0 => "".into(),
                    _ =>
                        ok_pipeline_lines
                            .iter()
                            .enumerate()
                            .map(|(i, line)| {
                                let line_no = (self.pipeline_pos.0 + 1 + i).to_string();
                                let side_border = format!("{: >COL_WIDTH$} | ", line_no).blue();
                                format!("{}{}", side_border, line,)
                            })
                            .collect::<Vec<_>>()
                            .join("\n")
                            + "\n",
                },
                padding = " ".repeat(
                    error_lines
                        .iter()
                        .map(|(&line, _)| line)
                        .collect::<Vec<_>>()
                        .first()
                        .unwrap_or(&&"")
                        .chars()
                        .take_while(|c| c.is_ascii_whitespace())
                        .collect::<Vec<_>>()
                        .len()
                ),
                error_msg = format!(">_< unexpected {}", self.last_type).red().italic(),
                preview_error = error_lines
                    .iter()
                    .enumerate()
                    .map(|(i, (line, err))| {
                        let line_no =
                            (self.pipeline_pos.0 + 1 + ok_pipeline_lines.len() + i).to_string();
                        let side_border = format!("{: >COL_WIDTH$} | ", line_no).blue();
                        println!(
                            "DEBUG: line {line} [{:?}] {} : {}",
                            line.find(err),
                            err,
                            err.red()
                        );
                        format!(
                            "{}{}",
                            side_border,
                            line.replace(err.trim(), err.red().as_str())
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n"),
            )
            .trim()
        )
    }
}

pub struct UnexpectedTokenError<'a> {
    actual_str: &'a str,
    actual_kind: TokenKind,
    actual_buffer_pos: (usize, usize),
    expected_kinds: Vec<TokenKind>,
    header: Option<&'a str>,
    context: Option<&'a str>,
    line_text: &'a str,
    line_before_text: &'a str,
}
impl<'a> UnexpectedTokenError<'a> {
    pub fn new(
        source: &'a str,
        line_starts: &'a Vec<usize>,
        actual: Token,
        expected_kinds: Vec<TokenKind>,
        header: Option<&'a str>,
        context: Option<&'a str>,
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
            line_before_text: match line {
                0 => "".into(),
                _ => source.lines().nth(line - 1).unwrap_or_default(),
            },
        }
    }
}
impl<'a> fmt::Display for UnexpectedTokenError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format!(
                r#"
{header} {line_col_info}
{context}
{expected}
{got}
{none:COL_WIDTH$} {pipe}
{line_before:COL_WIDTH$} {pipe} {line_before_text}
{line:COL_WIDTH$} {pipe} {line_text}
{none:COL_WIDTH$} {pipe} {padding}{highlight} {error_msg}
"#,
                header = self.header.unwrap_or("[UNEXPECTED TOKEN]").red(),
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
                        ctx.italic()
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
                pipe = "|".blue(),
                line_before = match self.actual_buffer_pos.0 - 1 {
                    0 => "".into(),
                    rest => rest.to_string(),
                },
                line_before_text = self.line_before_text,
                line = self.actual_buffer_pos.0,
                line_text = self.line_text,
                padding = " ".repeat(self.actual_buffer_pos.1 - 1),
                highlight = "^".repeat(self.actual_str.len()).red().italic(),
                error_msg = "unexpected token".red().italic(),
            )
            .trim()
        )
    }
}
