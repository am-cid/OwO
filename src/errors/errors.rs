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
            TokenType::Identifier => "IDENTIFIER",
            TokenType::Type => "TYPE",
            TokenType::IntLiteral => "INTEGER LITERAL",
            TokenType::FloatLiteral => "FLOAT LITERAL",
            TokenType::StringLiteral => "STRING LITERAL",
            TokenType::StringPartStart => "STRING PART START",
            TokenType::StringPartMid => "STRING PART MID",
            TokenType::StringPartEnd => "STRING PART END",
            TokenType::CharLiteral => "CHAR LITERAL",
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
                            '\r' => "'CARRIAGE RETURN'".to_string(),
                            ' ' => "'SPACE'".to_string(),
                            _ => format!("'{}'", c),
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", "),
            )
            .as_str(),
        );
        let actual = match self.actual {
            '\n' => "NEWLINE".to_string(),
            '\t' => "TAB".to_string(),
            '\r' => "'CARRIAGE RETURN'".to_string(),
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

pub struct UnclosedStringError {
    actual: char,
    line_text: &'static str,
    start_pos: (usize, usize),
    length: usize,
}
impl UnclosedStringError {
    pub fn new(
        actual: char,
        line_text: &'static str,
        start_pos: (usize, usize),
        length: usize,
    ) -> Self {
        Self {
            actual,
            line_text,
            start_pos,
            length: length + 1,
        }
    }
}
impl CompilerError for UnclosedStringError {
    fn message(&self) -> String {
        let mut msg: String = "".to_string();
        msg.push_str(
            format!(
                "[UNCLOSED STRING] at line {}, col {}\n",
                self.start_pos.0, self.start_pos.1
            )
            .as_str(),
        );
        msg.push_str(format!("    Expected any in: '\"', '}}'\n").as_str());
        msg.push_str(
            format!(
                "    Got: '{}'\n",
                match self.actual {
                    '\n' => "NEWLINE",
                    '\r' => "CARRIAGE RETURN",
                    _ => unreachable!(),
                }
            )
            .as_str(),
        );
        let line_length = self.line_text.len();
        let border = "-".repeat(line_length);
        let highlight = " ".repeat(self.start_pos.1) + &"^".repeat(self.length);
        msg.push_str(format!("------{}\n", border).as_str());
        msg.push_str(
            format!(
                "{:width$} | {}\n",
                self.start_pos.0,
                self.line_text,
                width = 3
            )
            .as_str(),
        );
        msg.push_str(format!("    | {}\n", highlight).as_str());
        msg.push_str(format!("------{}\n", border).as_str());
        msg
    }
}

pub struct SingleBracketError {
    line_text: &'static str,
    pos: (usize, usize),
}
impl SingleBracketError {
    pub fn new(line_text: &'static str, pos: (usize, usize)) -> Self {
        Self { line_text, pos }
    }
}
impl CompilerError for SingleBracketError {
    fn message(&self) -> String {
        let mut msg: String = "".to_string();
        msg.push_str(
            format!(
                "[SINGLE BRACKET] at line {}, col {}\n",
                self.pos.0, self.pos.1
            )
            .as_str(),
        );
        msg.push_str("    Brackets in strings are escaped like: '{{' or '}}'\n");
        msg.push_str("    Got: '}'\n");
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

pub struct UnclosedCharError {
    actual: char,
    line_text: &'static str,
    start_pos: (usize, usize),
}
impl UnclosedCharError {
    pub fn new(actual: char, line_text: &'static str, start_pos: (usize, usize)) -> Self {
        Self {
            actual,
            line_text,
            start_pos,
        }
    }
}
impl CompilerError for UnclosedCharError {
    fn message(&self) -> String {
        let mut msg: String = "".to_string();
        msg.push_str(
            format!(
                "[UNCLOSED CHARACTER] at line {}, col {}\n",
                self.start_pos.0, self.start_pos.1
            )
            .as_str(),
        );
        msg.push_str(format!("    Expected \"'\"\n").as_str());
        let actual = self.actual.to_string();
        msg.push_str(
            format!(
                "    Got: \"{}\"\n",
                match self.actual {
                    ' ' => "WHITESPACE",
                    '\n' => "NEWLINE",
                    '\t' => "TAB",
                    '\r' => "CARRIAGE RETURN",
                    _ => actual.as_str(),
                }
            )
            .as_str(),
        );
        let line_length = self.line_text.len();
        let border = "-".repeat(line_length);
        let highlight = " ".repeat(self.start_pos.1 - 2) + &"^^^";
        msg.push_str(format!("------{}\n", border).as_str());
        msg.push_str(
            format!(
                "{:width$} | {}\n",
                self.start_pos.0,
                self.line_text,
                width = 3
            )
            .as_str(),
        );
        msg.push_str(format!("    | {}\n", highlight).as_str());
        msg.push_str(format!("------{}\n", border).as_str());
        msg
    }
}
