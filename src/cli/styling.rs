pub trait StringExt {
    // text space padding
    fn pad_right(&self, len: usize) -> String;
    fn pad_left(&self, len: usize) -> String;
    fn pad_mid(&self, len: usize) -> String;
    fn fill_right(&self, len: usize) -> String;
    fn fill_left(&self, len: usize) -> String;

    // text formatting
    fn bold(&self) -> String;
    fn underline(&self) -> String;
    fn italic(&self) -> String;
}

pub impl StringExt for String {
    fn pad_right(&self, len: usize) -> String {
        let width = match len < self.len() {
            true => 0,
            false => len - self.len(),
        };
        return format!("{}{}", self, " ".repeat(width));
    }
    fn pad_left(&self, len: usize) -> String {
        let width = match len < self.len() {
            true => 0,
            false => len - self.len(),
        };
        return format!("{}{}", " ".repeat(width), self);
    }
    fn pad_mid(&self, len: usize) -> String {
        let width = match len < self.len() {
            true => 0,
            false => len - self.len(),
        };
        return format!(
            "{}{}{}",
            " ".repeat(width / 2),
            self,
            " ".repeat(width - width / 2)
        );
    }
    fn fill_right(&self, len: usize) -> String {
        return format!("{}{}", self, " ".repeat(len));
    }
    fn fill_left(&self, len: usize) -> String {
        return format!("{}{}", " ".repeat(len), self);
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
}
