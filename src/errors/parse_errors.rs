use core::fmt;

use crate::{
    errors::lex_errors::CompilerError,
    lexer::token::{Token, TokenKind},
    parser::{
        identifiers::{Accessor, Identifier},
        productions::{AccessType, Param, Production, Range},
    },
    utils::string::StringExt,
};

#[derive(Clone, Copy)]
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
            Self::Fn | Self::If | Self::Mash | Self::MashCase | Self::For | Self::Method => {
                "pwint(\"hewwo world :3\")~"
            }
            Self::Group => "field-chan~",
            Self::Contract => "method-chan(kun, senpai)~",
        }
    }
}
pub struct EmptyBodyError {
    expected: Vec<TokenKind>,
    body_type: BodyType,
    line_text: &'static str,
    r_bracket_pos: (usize, usize),
}
impl EmptyBodyError {
    pub fn new(
        expected: Vec<TokenKind>,
        body_type: BodyType,
        line_text: &'static str,
        r_bracket_pos: (usize, usize),
    ) -> Self {
        Self {
            expected,
            body_type,
            line_text,
            r_bracket_pos: (r_bracket_pos.0 + 1, r_bracket_pos.1 + 1),
        }
    }
}
impl CompilerError for EmptyBodyError {
    fn message(&self) -> String {
        let mut msg = String::new();
        // header
        msg.push_str(format!("[EMPTY {} BODY]\n", self.body_type).red().as_str());
        msg.push_str("uwu block statements must not be empty\n".bold().as_str());
        msg.push_str(
            format!(
                "{} {}\n",
                "Expected any in:".bold(),
                self.expected
                    .iter()
                    .map(|e| format!("'{}'", e))
                    .collect::<Vec<_>>()
                    .join(", ")
                    .italic()
            )
            .as_str(),
        );
        // preview
        let line_no = self.r_bracket_pos.0.to_string();
        msg.push_str(
            format!("{: >width$} |\n", "", width = line_no.len())
                .blue()
                .as_str(),
        );
        msg.push_str(
            format!(
                "{}{}{}\n{}{}{} {}\n",
                format!(
                    "{: >width$} | ",
                    self.r_bracket_pos.0 - 1,
                    width = line_no.len()
                )
                .blue(),
                " ".repeat(
                    self.line_text
                        .chars()
                        .take_while(|c| c.is_ascii_whitespace())
                        .count()
                        + 4
                ),
                self.body_type.sample_statement().green(),
                format!("{: >width$} | ", "", width = line_no.len()).blue(),
                " ".repeat(
                    self.line_text
                        .chars()
                        .take_while(|c| c.is_ascii_whitespace())
                        .count()
                        + 4
                ),
                "-".repeat(self.body_type.sample_statement().len()).green(),
                "try putting this statement before the RBrace".green(),
            )
            .as_str(),
        );
        msg.push_str(
            format!("{: >width$} | ", line_no, width = line_no.len())
                .blue()
                .as_str(),
        );
        msg.push_str(format!("{}\n", self.line_text).as_str());
        msg.push_str(
            format!("{: >width$} | ", "", width = line_no.len())
                .blue()
                .as_str(),
        );
        msg.push_str(
            format!(
                "{}{} Expected statement, got RBrace\n",
                " ".repeat(self.r_bracket_pos.1 - 1),
                "^",
            )
            .red()
            .as_str(),
        );
        msg
    }
}

pub struct NoMainError {
    line_text: &'static str,
    end_pos: (usize, usize),
}
impl NoMainError {
    pub fn new(line_text: &'static str, end_pos: (usize, usize)) -> Self {
        Self {
            line_text,
            end_pos: (end_pos.0 + 1, end_pos.1 + 1),
        }
    }
}
impl CompilerError for NoMainError {
    fn message(&self) -> String {
        let mut msg = String::new();
        // header
        msg.push_str("[NO MAIN FUNCTION]\n".red().as_str());
        msg.push_str(
            "All .uwu programs must have a main function\n"
                .bold()
                .as_str(),
        );
        // preview
        let line_no = self.end_pos.0.to_string();
        msg.push_str(
            format!("{: >width$} |\n", "", width = line_no.len())
                .blue()
                .as_str(),
        );
        msg.push_str(
            format!("{: >width$} | ", line_no, width = line_no.len())
                .blue()
                .as_str(),
        );
        msg.push_str(format!("{}\n", self.line_text).as_str());
        msg.push_str(
            format!("{: >width$} | ", "", width = line_no.len())
                .blue()
                .as_str(),
        );
        msg.push_str(
            format!(
                "{}{} {}\n",
                " ".repeat(self.end_pos.1),
                "^".red(),
                "expected main function, found EOF".red(),
            )
            .as_str(),
        );
        msg.push_str((".".repeat(line_no.len() + 1) + "  \n").blue().as_str());
        msg.push_str((".".repeat(line_no.len() + 1) + "  ").blue().as_str());
        msg.push_str(format!("Define a main function\n",).green().as_str());
        msg.push_str(
            format!(
                "{}{}\n{}{}\n{}{}\n",
                format!("{: >width$} | ", self.end_pos.0 + 1, width = line_no.len()).blue(),
                "fun main-san() {".green(),
                format!("{: >width$} | ", self.end_pos.0 + 2, width = line_no.len()).blue(),
                "    pwint(\"hewwo world :3\")~".green(),
                format!("{: >width$} | ", self.end_pos.0 + 3, width = line_no.len()).blue(),
                "}".green(),
            )
            .as_str(),
        );
        msg
    }
}

pub struct UnexpectedTokenError {
    actual: Token,
    expected: Vec<TokenKind>,
    header: Option<&'static str>,
    context: Option<&'static str>,
    line_text: &'static str,
    pos: (usize, usize),
    end_pos: (usize, usize),
}
impl UnexpectedTokenError {
    pub fn new(
        actual: Token,
        expected: Vec<TokenKind>,
        header: Option<&'static str>,
        context: Option<&'static str>,
        line_text: &'static str,
        pos: (usize, usize),
        end_pos: (usize, usize),
    ) -> Self {
        Self {
            actual,
            expected,
            header,
            context,
            line_text,
            pos: (pos.0 + 1, pos.1 + 1),
            end_pos: (end_pos.0 + 1, end_pos.1 + 1),
        }
    }
}
impl CompilerError for UnexpectedTokenError {
    fn message(&self) -> String {
        let mut msg = String::new();
        // header
        msg.push_str(
            format!(
                "{} {}\n",
                format!("[{}]", self.header.unwrap_or("UNEXPECTED TOKEN")).red(),
                if self.pos.1 == self.end_pos.1 {
                    format!("at line {} column {}", self.pos.0, self.end_pos.1).bold()
                } else {
                    format!(
                        "at line {} from column {} to {}",
                        self.pos.0, self.pos.1, self.end_pos.1
                    )
                    .bold()
                }
            )
            .as_str(),
        );
        match &self.context {
            Some(ctx) => {
                msg.push_str((ctx.italic() + "\n").as_str());
            }
            None => (),
        }
        msg.push_str(
            format!(
                "{} {}\n",
                "Expected any in:".bold(),
                self.expected
                    .iter()
                    .map(|e| format!("'{}'", e))
                    .collect::<Vec<_>>()
                    .join(", ")
                    .italic()
            )
            .as_str(),
        );
        msg.push_str(
            format!(
                "{} '{}'\n",
                "Got:".bold(),
                self.actual.kind.to_string().italic()
            )
            .bold()
            .as_str(),
        );
        // preview
        let line_no = self.pos.0.to_string();
        let side_border = format!("{: >width$} | ", line_no, width = line_no.len()).blue();
        let side_border_empty = format!("{: >width$} | ", "", width = line_no.len()).blue();
        msg.push_str(format!("{}\n", side_border_empty).as_str());
        msg.push_str(side_border.as_str());
        msg.push_str(format!("{}\n", self.line_text).as_str());
        msg.push_str(side_border_empty.as_str());
        msg.push_str(
            format!(
                "{}{} {}\n",
                " ".repeat(self.pos.1 - 1),
                "^".repeat(self.end_pos.1 - self.pos.1 + 1).red(),
                "unexpected token".red(),
            )
            .as_str(),
        );
        msg
    }
}