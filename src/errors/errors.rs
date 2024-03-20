use std::collections::HashSet;

use crate::lexer::token::TokenType;

pub trait CompilerError {
    fn message(&self) -> String;
}

pub struct DelimError {
    token_type: TokenType,
    expected: HashSet<char>,
    actual: char,
    line_text: &'static str,
    pos: (usize, usize),
}
impl DelimError {
    pub fn new(
        token_type: TokenType,
        expected: HashSet<char>,
        actual: char,
        line_text: &'static str,
        pos: (usize, usize),
    ) -> Self {
        Self {
            token_type,
            expected,
            actual,
            line_text,
            pos,
        }
    }
}

impl CompilerError for DelimError {
    fn message(&self) -> String {
        let mut msg: String = "".to_string();
        let title = match self.token_type {
            TokenType::FloatLiteral => "FLOAT LITERAL",
            TokenType::IntLiteral => "INTEGER LITERAL",
            TokenType::StringLiteral => "STRING LITERAL",
            TokenType::Identifier => "IDENTIFIER",
            TokenType::ClassId => "CLASS ID",
            TokenType::StringPartStart => "STRING PART START",
            TokenType::StringPartMid => "STRING PART MID",
            TokenType::StringPartEnd => "STRING PART END",
            _ => self.token_type.to_str(),
        };
        msg.push_str(format!("[UNDELIMITED {}]", title).as_str());
        msg.push_str(format!(" at line {}, col {}\n", self.pos.0, self.pos.1).as_str());
        msg.push_str(
            format!(
                "    Expected any in: {}\n",
                self.expected
                    .iter()
                    .map(|&c| {
                        match c {
                            '\n' => "'NEWLINE'".to_string(),
                            '\t' => "'TAB'".to_string(),
                            ' ' => "'SPACE'".to_string(),
                            _ => format!("'{}'", c),
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            )
            .as_str(),
        );
        let actual = match self.actual {
            '\n' => "NEWLINE".to_string(),
            '\t' => "TAB".to_string(),
            ' ' => "SPACE".to_string(),
            _ => format!("{}", self.actual),
        };
        msg.push_str(format!("    Got: '{}'\n", actual).as_str());
        let line_length = self.line_text.len();
        let border = "-".repeat(line_length);
        let highlight = " ".repeat(self.pos.1) + "^";
        msg.push_str(format!("------{}\n", border).as_str());
        msg.push_str(format!("{:width$} | {}\n", self.pos.0, self.line_text, width = 3).as_str());
        msg.push_str(format!("    | {}\n", highlight).as_str());
        msg.push_str(format!("------{}\n", border).as_str());
        msg
    }
}

pub struct UnknownTokenError {
    line_text: &'static str,
    pos: (usize, usize),
}

impl UnknownTokenError {
    pub fn new(line_text: &'static str, pos: (usize, usize)) -> Self {
        Self { line_text, pos }
    }
}

impl CompilerError for UnknownTokenError {
    fn message(&self) -> String {
        let mut msg: String = "".to_string();
        msg.push_str(
            format!(
                "[UNKNOWN TOKEN] at line {}, col {}\n",
                self.pos.0, self.pos.1
            )
            .as_str(),
        );
        let line_length = self.line_text.len();
        let border = "-".repeat(line_length);
        let highlight = " ".repeat(self.pos.1) + "^";
        msg.push_str(format!("------{}\n", border).as_str());
        msg.push_str(format!("{:width$} | {}\n", self.pos.0, self.line_text, width = 3).as_str());
        msg.push_str(format!("    | {}\n", highlight).as_str());
        msg.push_str(format!("------{}\n", border).as_str());
        msg
    }
}
