use regex_lite::Regex;
use std::{
    fmt::{self, Display},
    sync::OnceLock,
};

const INDENT_SIZE: usize = 4;
// to only compile the regex once
static ANSI_RE: OnceLock<Regex> = OnceLock::new();

#[derive(Default)]
pub enum AnsiCode {
    #[default]
    Reset,
    Bold,
    Italic,
    Underline,
    Strikethrough,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
}
impl AnsiCode {
    pub fn to_u8(&self) -> u8 {
        match self {
            Self::Reset => 0,
            Self::Bold => 1,
            Self::Italic => 3,
            Self::Underline => 4,
            Self::Strikethrough => 9,
            Self::Red => 31,
            Self::Green => 32,
            Self::Yellow => 33,
            Self::Blue => 94,
            Self::Magenta => 35,
            Self::Cyan => 36,
        }
    }
}
impl fmt::Display for AnsiCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\x1b[{}m",
            match self {
                Self::Reset => 0,
                Self::Bold => 1,
                Self::Italic => 3,
                Self::Underline => 4,
                Self::Strikethrough => 9,
                Self::Red => 31,
                Self::Green => 32,
                Self::Yellow => 33,
                Self::Blue => 94,
                Self::Magenta => 35,
                Self::Cyan => 36,
            }
        )
    }
}

pub trait StringExt: Display {
    // text space padding
    fn pad_right(&self, len: usize) -> String;
    fn pad_left(&self, len: usize) -> String;
    fn pad_mid(&self, len: usize) -> String;
    fn fill_right(&self, len: usize) -> String;
    fn fill_left(&self, len: usize) -> String;
    fn indent(&self, n: usize) -> String {
        self.fill_left(n * INDENT_SIZE)
    }

    // text formatting
    fn bold(&self) -> String {
        self.style(AnsiCode::Bold)
    }
    fn italic(&self) -> String {
        self.style(AnsiCode::Italic)
    }
    fn underline(&self) -> String {
        self.style(AnsiCode::Underline)
    }
    fn strikethrough(&self) -> String {
        self.style(AnsiCode::Strikethrough)
    }
    fn red(&self) -> String {
        self.style(AnsiCode::Red)
    }
    fn green(&self) -> String {
        self.style(AnsiCode::Green)
    }
    fn yellow(&self) -> String {
        self.style(AnsiCode::Yellow)
    }
    fn blue(&self) -> String {
        self.style(AnsiCode::Blue)
    }
    fn magenta(&self) -> String {
        self.style(AnsiCode::Magenta)
    }
    fn cyan(&self) -> String {
        self.style(AnsiCode::Cyan)
    }
    fn style(&self, ansi_code: AnsiCode) -> String;
    fn strip_style(&self) -> String;
}

impl StringExt for str {
    fn pad_right(&self, len: usize) -> String {
        let width = match len < self.len() {
            true => 0,
            false => len - self.len(),
        };
        format!("{}{}", self, " ".repeat(width))
    }
    fn pad_left(&self, len: usize) -> String {
        let width = match len < self.len() {
            true => 0,
            false => len - self.len(),
        };
        format!("{}{}", " ".repeat(width), self)
    }
    fn pad_mid(&self, len: usize) -> String {
        let width = match len < self.len() {
            true => 0,
            false => len - self.len(),
        };
        format!(
            "{}{}{}",
            " ".repeat(width / 2),
            self,
            " ".repeat(width - width / 2)
        )
    }
    fn fill_right(&self, len: usize) -> String {
        format!("{}{}", self, " ".repeat(len))
    }
    fn fill_left(&self, len: usize) -> String {
        format!("{}{}", " ".repeat(len), self)
    }
    fn style(&self, ansi_code: AnsiCode) -> String {
        let re = ANSI_RE
            .get_or_init(|| Regex::new(r"\x1b\[([0-9;]*)m").expect("Invalid regex for ANSI codes"));
        let (leading, string) = match self.find(|c: char| !c.is_whitespace()) {
            Some(idx) => self.split_at(idx),
            None => ("", self),
        };
        let (string, trailing) = match string.rfind(|c: char| !c.is_whitespace()) {
            Some(idx) => string.split_at(idx + 1),
            None => (string, ""),
        };

        let mut codes = Vec::new();
        let mut base_str = string;
        if let Some(captures) = re.captures(base_str) {
            if let Some(matched) = captures.get(0) {
                base_str = &base_str[matched.end()..];
            }
            if let Some(code_str) = captures.get(1) {
                if !code_str.as_str().is_empty() {
                    codes.extend(
                        code_str
                            .as_str()
                            .split(';')
                            .filter_map(|c| c.parse::<u8>().ok()),
                    );
                }
            }
        }
        if !codes.contains(&ansi_code.to_u8()) {
            codes.push(ansi_code.to_u8());
        }
        format!(
            "{}\x1b[{}m{}{}{}",
            leading,
            codes
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(";"),
            base_str,
            if self.ends_with("\x1b[0m") {
                ""
            } else {
                "\x1b[0m"
            },
            trailing,
        )
    }
    fn strip_style(&self) -> String {
        let re = ANSI_RE.get_or_init(|| Regex::new(r"\x1b\[[0-9;]*m").unwrap());
        re.replace_all(self, "").to_string()
    }
}

impl StringExt for String {
    fn pad_right(&self, len: usize) -> String {
        let width = match len < self.len() {
            true => 0,
            false => len - self.len(),
        };
        format!("{}{}", self, " ".repeat(width))
    }
    fn pad_left(&self, len: usize) -> String {
        let width = match len < self.len() {
            true => 0,
            false => len - self.len(),
        };
        format!("{}{}", " ".repeat(width), self)
    }
    fn pad_mid(&self, len: usize) -> String {
        let width = match len < self.len() {
            true => 0,
            false => len - self.len(),
        };
        format!(
            "{}{}{}",
            " ".repeat(width / 2),
            self,
            " ".repeat(width - width / 2)
        )
    }
    fn fill_right(&self, len: usize) -> String {
        format!("{}{}", self, " ".repeat(len))
    }
    fn fill_left(&self, len: usize) -> String {
        format!("{}{}", " ".repeat(len), self)
    }
    fn style(&self, ansi_code: AnsiCode) -> String {
        let re = ANSI_RE
            .get_or_init(|| Regex::new(r"\x1b\[([0-9;]*)m").expect("Invalid regex for ANSI codes"));
        let (leading, string) = match self.find(|c: char| !c.is_whitespace()) {
            Some(idx) => self.split_at(idx),
            None => ("", self.as_str()),
        };
        let (string, trailing) = match string.rfind(|c: char| !c.is_whitespace()) {
            Some(idx) => string.split_at(idx + 1),
            None => (string, ""),
        };

        let mut codes = Vec::new();
        let mut base_str = string;
        if let Some(captures) = re.captures(base_str) {
            if let Some(matched) = captures.get(0) {
                base_str = &base_str[matched.end()..];
            }
            if let Some(code_str) = captures.get(1) {
                if !code_str.as_str().is_empty() {
                    codes.extend(
                        code_str
                            .as_str()
                            .split(';')
                            .filter_map(|c| c.parse::<u8>().ok()),
                    );
                }
            }
        }
        if !codes.contains(&ansi_code.to_u8()) {
            codes.push(ansi_code.to_u8());
        }
        format!(
            "{}\x1b[{}m{}{}{}",
            leading,
            codes
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(";"),
            base_str,
            if self.ends_with("\x1b[0m") {
                ""
            } else {
                "\x1b[0m"
            },
            trailing,
        )
    }
    fn strip_style(&self) -> String {
        let re = ANSI_RE.get_or_init(|| Regex::new(r"\x1b\[[0-9;]*m").unwrap());
        re.replace_all(self, "").to_string()
    }
}
