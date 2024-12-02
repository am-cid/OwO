pub trait StringExt {
    // text space padding
    fn pad_right(&self, len: usize) -> String;
    fn pad_left(&self, len: usize) -> String;
    fn pad_mid(&self, len: usize) -> String;
    fn fill_right(&self, len: usize) -> String;
    fn fill_left(&self, len: usize) -> String;
    fn indent(&self, n: usize) -> String;

    // text formatting
    fn bold(&self) -> String;
    fn underline(&self) -> String;
    fn italic(&self) -> String;
    fn strikethrough(&self) -> String;
    fn red(&self) -> String;
    fn green(&self) -> String;
    fn yellow(&self) -> String;
    fn blue(&self) -> String;
    fn cyan(&self) -> String;
    fn magenta(&self) -> String;
    // \x1b[0m: reset
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
    fn indent(&self, n: usize) -> String {
        self.fill_left(n * 4)
    }
    fn bold(&self) -> String {
        let (leading, string) = match self.find(|c: char| !c.is_whitespace()) {
            Some(idx) => self.split_at(idx),
            None => ("", self),
        };
        let (string, trailing) = match string.rfind(|c: char| !c.is_whitespace()) {
            Some(idx) => string.split_at(idx + 1),
            None => (string, ""),
        };
        format!("{}\x1b[1m{}\x1b[0m{}", leading, string, trailing)
    }
    fn underline(&self) -> String {
        let (leading, string) = match self.find(|c: char| !c.is_whitespace()) {
            Some(idx) => self.split_at(idx),
            None => ("", self),
        };
        let (string, trailing) = match string.rfind(|c: char| !c.is_whitespace()) {
            Some(idx) => string.split_at(idx + 1),
            None => (string, ""),
        };
        format!("{}\x1b[4m{}\x1b[0m{}", leading, string, trailing)
    }
    fn italic(&self) -> String {
        let (leading, string) = match self.find(|c: char| !c.is_whitespace()) {
            Some(idx) => self.split_at(idx),
            None => ("", self),
        };
        let (string, trailing) = match string.rfind(|c: char| !c.is_whitespace()) {
            Some(idx) => string.split_at(idx + 1),
            None => (string, ""),
        };
        format!("{}\x1b[3m{}\x1b[0m{}", leading, string, trailing)
    }
    fn strikethrough(&self) -> String {
        let (leading, string) = match self.find(|c: char| !c.is_whitespace()) {
            Some(idx) => self.split_at(idx),
            None => ("", self),
        };
        let (string, trailing) = match string.rfind(|c: char| !c.is_whitespace()) {
            Some(idx) => string.split_at(idx + 1),
            None => (string, ""),
        };
        format!("{}\x1b[9m{}\x1b[0m{}", leading, string, trailing)
    }
    fn red(&self) -> String {
        format!("\x1b[31m{}\x1b[0m", self)
    }
    fn green(&self) -> String {
        format!("\x1b[32m{}\x1b[0m", self)
    }
    fn yellow(&self) -> String {
        format!("\x1b[33m{}\x1b[0m", self)
    }
    fn blue(&self) -> String {
        format!("\x1b[94m{}\x1b[0m", self)
    }
    fn magenta(&self) -> String {
        format!("\x1b[35m{}\x1b[0m", self)
    }
    fn cyan(&self) -> String {
        format!("\x1b[36m{}\x1b[0m", self)
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
    fn indent(&self, n: usize) -> String {
        self.fill_left(n * 4)
    }
    fn bold(&self) -> String {
        let (leading, string) = match self.find(|c: char| !c.is_whitespace()) {
            Some(idx) => self.split_at(idx),
            None => ("", self.as_str()),
        };
        let (string, trailing) = match string.rfind(|c: char| !c.is_whitespace()) {
            Some(idx) => string.split_at(idx + 1),
            None => (string, ""),
        };
        format!("{}\x1b[1m{}\x1b[0m{}", leading, string, trailing)
    }
    fn underline(&self) -> String {
        let (leading, string) = match self.find(|c: char| !c.is_whitespace()) {
            Some(idx) => self.split_at(idx),
            None => ("", self.as_str()),
        };
        let (string, trailing) = match string.rfind(|c: char| !c.is_whitespace()) {
            Some(idx) => string.split_at(idx + 1),
            None => (string, ""),
        };
        format!("{}\x1b[4m{}\x1b[0m{}", leading, string, trailing)
    }
    fn italic(&self) -> String {
        let (leading, string) = match self.find(|c: char| !c.is_whitespace()) {
            Some(idx) => self.split_at(idx),
            None => ("", self.as_str()),
        };
        let (string, trailing) = match string.rfind(|c: char| !c.is_whitespace()) {
            Some(idx) => string.split_at(idx + 1),
            None => (string, ""),
        };
        format!("{}\x1b[3m{}\x1b[0m{}", leading, string, trailing)
    }
    fn strikethrough(&self) -> String {
        let (leading, string) = match self.find(|c: char| !c.is_whitespace()) {
            Some(idx) => self.split_at(idx),
            None => ("", self.as_str()),
        };
        let (string, trailing) = match string.rfind(|c: char| !c.is_whitespace()) {
            Some(idx) => string.split_at(idx + 1),
            None => (string, ""),
        };
        format!("{}\x1b[9m{}\x1b[0m{}", leading, string, trailing)
    }
    fn red(&self) -> String {
        format!("\x1b[31m{}\x1b[0m", self)
    }
    fn green(&self) -> String {
        format!("\x1b[32m{}\x1b[0m", self)
    }
    fn yellow(&self) -> String {
        format!("\x1b[33m{}\x1b[0m", self)
    }
    fn blue(&self) -> String {
        format!("\x1b[94m{}\x1b[0m", self)
    }
    fn magenta(&self) -> String {
        format!("\x1b[35m{}\x1b[0m", self)
    }
    fn cyan(&self) -> String {
        format!("\x1b[36m{}\x1b[0m", self)
    }
}
