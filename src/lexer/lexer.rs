use crate::lexer::token::{atoms, to_token, Token, TokenType};
use std::fmt;

pub struct Lexer {
    pub source: &'static str,

    pub tokens: Vec<Token>,
    pub errors: Vec<String>,

    pub curr_char: char,
    pub peek_char: char,

    pub pos: usize,
    pub d_pos: (usize, usize),
}

impl fmt::Display for Lexer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "chars: ('{}','{}')\nat pos: {}(abs), ({}, {})(line, col)\n",
            self.curr_char, self.peek_char, self.pos, self.d_pos.0, self.d_pos.1
        )
    }
}

impl Lexer {
    pub fn new(source: &'static str) -> Lexer {
        let mut source_iter = source.chars();
        let first_char = source_iter.next().expect("source is empty");
        let second_char = source_iter.next().expect("source is empty");
        Lexer {
            source,
            tokens: vec![],
            errors: vec![],
            curr_char: first_char,
            peek_char: second_char,
            pos: 0,
            d_pos: (0, 0),
        }
    }
    pub fn tokenize(&mut self) -> () {
        while self.pos < self.source.len() {
            match self.curr_char {
                // TODO: remove unwrap or else:
                //       - do unknown token if anything else
                ' ' | '\t' | '\n' => self
                    .peek_reserved(TokenType::Whitespace)
                    .unwrap_or_else(|_| ()),
                'b' => self
                    .peek_reserved(TokenType::Bweak)
                    .unwrap_or(self.peek_ident()),
                'c' => self
                    .peek_reserved(TokenType::Chan)
                    .or_else(|_| self.peek_reserved(TokenType::Cap))
                    .or_else(|_| self.peek_reserved(TokenType::Cwass))
                    .unwrap_or_else(|_| self.peek_ident()),
                'd' => self
                    .peek_reserved(TokenType::Dono)
                    .or_else(|_| self.peek_reserved(TokenType::DoWhiwe))
                    .unwrap_or_else(|_| self.peek_ident()),
                'e' => self
                    .peek_reserved(TokenType::EwseIwf)
                    .or_else(|_| self.peek_reserved(TokenType::Ewse))
                    .unwrap_or_else(|_| self.peek_ident()),
                'f' => self
                    .peek_reserved(TokenType::Fow)
                    .or_else(|_| self.peek_reserved(TokenType::Fax))
                    .or_else(|_| self.peek_reserved(TokenType::Fwunc))
                    .unwrap_or_else(|_| self.peek_ident()),
                'g' => self
                    .peek_reserved(TokenType::Gwobaw)
                    .unwrap_or(self.peek_ident()),
                'i' => self
                    .peek_reserved(TokenType::Iwf)
                    .or_else(|_| self.peek_reserved(TokenType::Inpwt))
                    .unwrap_or_else(|_| self.peek_ident()),
                'm' => self
                    .peek_reserved(TokenType::Mainuwu)
                    .unwrap_or_else(|_| self.peek_ident()),
                'n' => self
                    .peek_reserved(TokenType::Nuww)
                    .unwrap_or_else(|_| self.peek_ident()),
                'p' => self
                    .peek_reserved(TokenType::Pwint)
                    .unwrap_or_else(|_| self.peek_ident()),
                's' => self
                    .peek_reserved(TokenType::San)
                    .or_else(|_| self.peek_reserved(TokenType::Senpai))
                    .or_else(|_| self.peek_reserved(TokenType::Sama))
                    .unwrap_or_else(|_| self.peek_ident()),
                'w' => self
                    .peek_reserved(TokenType::Whiwe)
                    .or_else(|_| self.peek_reserved(TokenType::Wetuwn))
                    .unwrap_or_else(|_| self.peek_ident()),
                'a'..='z' | 'A'..='Z' => self.peek_ident(),
                '0'..='9' => self.peek_num(),
                // TODO: peek string part mid/end instead of unit for pipe |
                '|' => self.peek_reserved(TokenType::Or).unwrap_or_else(|_| ()),
                '&' => self.peek_reserved(TokenType::And).unwrap_or_else(|_| ()),
                '=' => self
                    .peek_reserved(TokenType::Equal)
                    .or_else(|_| self.peek_reserved(TokenType::Assign))
                    .unwrap_or_else(|_| ()),
                '-' => self
                    .peek_reserved(TokenType::Decrement)
                    .or_else(|_| self.peek_reserved(TokenType::Dash))
                    .unwrap_or_else(|_| ()),
                '!' => self
                    .peek_reserved(TokenType::NotEqual)
                    .unwrap_or_else(|_| ()),
                '<' => self
                    .peek_reserved(TokenType::LessEqual)
                    .or_else(|_| self.peek_reserved(TokenType::LessThan))
                    .unwrap_or_else(|_| ()),
                '>' => self
                    .peek_multi_line_comment()
                    .or_else(|_| self.peek_single_line_comment())
                    .or_else(|_| self.peek_reserved(TokenType::GreaterEqual))
                    .or_else(|_| self.peek_reserved(TokenType::GreaterThan))
                    .unwrap_or_else(|_| ()),
                '*' => self
                    .peek_reserved(TokenType::Multiply)
                    .unwrap_or_else(|_| ()),
                '/' => self.peek_reserved(TokenType::Divide).unwrap_or_else(|_| ()),
                '%' => self.peek_reserved(TokenType::Modulo).unwrap_or_else(|_| ()),
                '{' => self.peek_reserved(TokenType::LBrace).unwrap_or_else(|_| ()),
                '}' => self.peek_reserved(TokenType::RBrace).unwrap_or_else(|_| ()),
                '(' => self.peek_reserved(TokenType::LParen).unwrap_or_else(|_| ()),
                ')' => self.peek_reserved(TokenType::RParen).unwrap_or_else(|_| ()),
                '[' => self
                    .peek_reserved(TokenType::DoubleLBracket)
                    .or_else(|_| self.peek_reserved(TokenType::LBracket))
                    .unwrap_or_else(|_| ()),
                ']' => self
                    .peek_reserved(TokenType::DoubleRBracket)
                    .or_else(|_| self.peek_reserved(TokenType::RBracket))
                    .unwrap_or_else(|_| ()),
                ',' => self.peek_reserved(TokenType::Comma).unwrap_or_else(|_| ()),
                '.' => self.peek_reserved(TokenType::Dot).unwrap_or_else(|_| ()),
                '~' => self
                    .peek_reserved(TokenType::Terminator)
                    .unwrap_or_else(|_| ()),
                // TODO: remove default case
                _ => break,
            }
        }
    }
    pub fn advance(&mut self, times: usize) -> () {
        for _ in 0..times {
            if self.pos > self.source.len() - 1 {
                self.curr_char = '\n';
                return;
            }
            self.pos += 1;
            self.d_pos.1 += 1;
            if self.curr_char == '\n' {
                self.d_pos.1 = 0;
                self.d_pos.0 += 1;
            }
            self.curr_char = self.peek_char;
            self.peek_char = self.source.chars().nth(self.pos + 1).unwrap_or('\n');
        }
    }
    fn expect_peek_char_is(&mut self, expected: char) -> bool {
        match expected == self.peek_char {
            true => {
                self.advance(1);
                true
            }
            false => false,
        }
    }
    pub fn reverse(&mut self, times: usize) -> () {
        for _ in 0..times {
            if self.pos == 0 {
                self.d_pos.1 = 0;
                self.curr_char = self.source.chars().nth(0).unwrap_or('\n');
                return;
            }
            self.pos -= 1;
            self.d_pos.1 = match self.d_pos.1 {
                0 => 0,
                _ => self.d_pos.1 - 1,
            };
            if self.curr_char == '\n' {
                self.d_pos.0 = match self.d_pos.0 {
                    0 => 0,
                    _ => self.d_pos.0 - 1,
                };
                self.d_pos.0 = self.source.lines().nth(self.d_pos.1).unwrap_or("").len();
            }
            self.peek_char = self.curr_char;
            self.curr_char = self.source.chars().nth(self.pos).unwrap_or('\n')
        }
    }
    fn peek_reserved(&mut self, expected: TokenType) -> Result<(), ()> {
        let expect_str = expected.to_str();
        for i in 0..expect_str.len() - 1 {
            if self.curr_char != expect_str.chars().nth(i).unwrap() {
                self.reverse(i);
                return Err(());
            }
            self.advance(1);
        }
        if !expected.delims().contains(&self.peek_char) {
            // TODO: check if identifier/class id, then error
            self.reverse(expect_str.len() - 1);
            return Err(());
        }
        self.tokens.push(
            to_token(
                expect_str,
                (self.d_pos.0, self.d_pos.1 + 1 - expect_str.len()),
                self.d_pos,
            )
            .map_err(|e| e.to_string())
            .unwrap(),
        );
        self.advance(1);
        Ok(())
    }
    fn peek_ident(&mut self) -> () {
        let delims = match self.curr_char.is_lowercase() {
            true => TokenType::Identifier.delims(),
            false => TokenType::ClassId.delims(),
        };
        let mut tmp: String = "".to_string();
        let mut times = 0;
        while !delims.contains(&self.curr_char) {
            if !atoms("alphanum").contains(&self.curr_char) {
                self.reverse(times);
                return;
            }
            tmp.push(self.curr_char);
            self.advance(1);
            times += 1;
        }
        let token: &'static str = Box::leak(tmp.into_boxed_str());
        self.tokens.push(
            to_token(
                token,
                (self.d_pos.0, self.d_pos.1 - token.len()),
                (self.d_pos.0, self.d_pos.1 - 1),
            )
            .unwrap(),
        );
    }
    fn peek_single_line_comment(&mut self) -> Result<(), ()> {
        let mut tmp: String = "".to_string();
        if !self.expect_peek_char_is('.') {
            return Err(());
        }
        if !self.expect_peek_char_is('<') {
            self.reverse(1); // TODO: add error here, not reverse
            return Err(());
        }
        self.advance(1); // consume the <

        tmp.push_str(">.<");
        while self.curr_char != '\n' {
            tmp.push(self.curr_char);
            self.advance(1);
        }
        let token: &'static str = Box::leak(tmp.into_boxed_str());
        self.tokens.push(
            to_token(
                token,
                (self.d_pos.0, self.d_pos.1 - token.len()),
                (self.d_pos.0, self.d_pos.1 - 1),
            )
            .map_err(|e| e.to_string())
            .unwrap(),
        );
        Ok(())
    }
    fn peek_multi_line_comment(&mut self) -> Result<(), ()> {
        let mut tmp: String = "".to_string();
        let start_pos = (self.d_pos.0, self.d_pos.1);
        if !self.expect_peek_char_is('/') {
            return Err(());
        }
        if !self.expect_peek_char_is('/') {
            self.reverse(1);
            return Err(());
        }
        if !self.expect_peek_char_is('<') {
            self.reverse(2);
            return Err(());
        }
        self.advance(1); // consume the <

        tmp.push_str(">//<");
        loop {
            // stop conditions
            // no closing >//<
            if self.pos >= self.source.len() - 1 {
                tmp.push(self.curr_char);
                break;
            }
            // has closing >//<
            if self.curr_char == '>' {
                if !self.expect_peek_char_is('/') {
                    tmp.push('>');
                    continue;
                }
                if !self.expect_peek_char_is('/') {
                    println!("{}", self.peek_char);
                    tmp.push_str(">/");
                    continue;
                }
                if !self.expect_peek_char_is('<') {
                    tmp.push_str(">//");
                    continue;
                }
                tmp.push_str(">//<");
                break;
            }
            tmp.push(self.curr_char);
            self.advance(1)
        }
        let token: &'static str = Box::leak(tmp.into_boxed_str());
        self.tokens.push(
            to_token(token, start_pos, self.d_pos)
                .map_err(|e| e.to_string())
                .unwrap(),
        );
        self.advance(1);
        Ok(())
    }
    fn peek_num(&self) -> () {
        todo!()
    }
}
