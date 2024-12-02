pub trait CompilerError {
    fn message(&self) -> String;
}

pub struct UnknownTokenError {
    token: char,
    line_text: &'static str,
    pos: (usize, usize),
}
impl UnknownTokenError {
    pub fn new(token: char, line_text: &'static str, pos: (usize, usize)) -> Self {
        Self {
            token,
            line_text,
            pos: (pos.0 + 1, pos.1 + 1),
        }
    }
}
impl CompilerError for UnknownTokenError {
    fn message(&self) -> String {
        let mut msg: String = "".to_string();
        msg.push_str(
            format!(
                "[UNKNOWN TOKEN '{}'] at line {}, col {}\n",
                self.token, self.pos.0, self.pos.1
            )
            .as_str(),
        );
        let line_length = self.line_text.len();
        let border = "-".repeat(line_length);
        let highlight = " ".repeat(self.pos.1 - 1) + "^";
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
            start_pos: (start_pos.0 + 1, start_pos.1 + 1),
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
                    '\n' => "NEWLINE".to_string(),
                    '\r' => "CARRIAGE RETURN".to_string(),
                    _ => unreachable!(),
                }
            )
            .as_str(),
        );
        let line_length = self.line_text.len();
        let border = "-".repeat(line_length);
        let highlight = " ".repeat(self.start_pos.1 - 1) + &"^".repeat(self.length);
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

pub struct UnescapedBracketInStringError {
    actual: char,
    line_text: &'static str,
    pos: (usize, usize),
}
impl UnescapedBracketInStringError {
    pub fn new(actual: char, line_text: &'static str, pos: (usize, usize)) -> Self {
        Self {
            actual,
            line_text,
            pos: (pos.0 + 1, pos.1 + 1),
        }
    }
}
impl CompilerError for UnescapedBracketInStringError {
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
        msg.push_str(format!("    Got: '{}'\n", self.actual).as_str());
        let line_length = self.line_text.len();
        let border = "-".repeat(line_length);
        let highlight = " ".repeat(self.pos.1 - 1) + "^";
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
            start_pos: (start_pos.0 + 1, start_pos.1 + 1),
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
