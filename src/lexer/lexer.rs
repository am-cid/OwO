use crate::errors::errors::{CompilerError, DelimError, UnknownTokenError};
use crate::lexer::token::{atoms, reserved_to_token_type, to_token, Token, TokenType};
use crate::lexer::token::{Atoms, Token, TokenType};
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
        let second_char = source_iter.next().unwrap_or('\0');
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
    pub fn pretty_print_tokens(&self, with_whitespace: bool) -> () {
        let width = 64;
        println!(" {}", "-".repeat(width));
        println!("|        kind       |         text         |       position      |");
        println!("|-------------------|----------------------|---------------------|");
        for token in &self.tokens {
            if [
                TokenType::Whitespace,
                TokenType::Tab,
                TokenType::Return,
                TokenType::Newline,
            ]
            .contains(&token.kind)
                && !with_whitespace
            {
                continue;
            }
            println!(
                "| {: ^17} | {: ^20} | ({: <2}, {: >2}) - ({: <2}, {: >2}) |",
                token.kind.to_string(),
                match token.text {
                    " " => "space".to_string(),
                    "\t" => "tab".to_string(),
                    "\r" => "return".to_string(),
                    "\n" => "newline".to_string(),
                    _ => {
                        let printed = token.text.to_string();
                        if printed.len() > 20 {
                            printed[0..17].to_string() + "..."
                        } else {
                            printed
                        }
                    }
                },
                token.pos.0,
                token.pos.1,
                token.end_pos.0,
                token.end_pos.1,
            );
        }
        println!(" {}", "-".repeat(width));
    }
    pub fn tokenize(&mut self) -> () {
        while self.pos < self.source.len() {
            match self.curr_char {
                ' ' => self
                    .peek_reserved(TokenType::Whitespace)
                    .unwrap_or_else(|_| ()),
                '\t' => self.peek_reserved(TokenType::Tab).unwrap_or_else(|_| ()),
                '\r' => self.peek_reserved(TokenType::Return).unwrap_or_else(|_| ()),
                '\n' => self
                    .peek_reserved(TokenType::Newline)
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
                '0'..='9' => self.peek_int(),
                '"' => self.peek_string(),
                // TODO: peek string part mid/end instead of unit for pipe |
                '|' => self
                    .peek_reserved(TokenType::Or)
                    .unwrap_or_else(|_| self.peek_string()),
                '&' => self.peek_reserved(TokenType::And).unwrap_or_else(|_| ()),
                '=' => self
                    .peek_reserved(TokenType::Equal)
                    .or_else(|_| self.peek_reserved(TokenType::Assign))
                    .unwrap_or_else(|_| ()),
                '-' => self
                    .peek_reserved(TokenType::Decrement)
                    .or_else(|_| self.peek_reserved(TokenType::Dash))
                    .unwrap_or_else(|_| ()),
                '!' => self.peek_reserved(TokenType::NotEqual).unwrap_or_else(|_| {
                    self.errors.push(
                        UnknownTokenError::new(
                            self.source.lines().nth(self.d_pos.0).unwrap(),
                            self.d_pos,
                        )
                        .message(),
                    );
                    self.advance(1);
                }),
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
                _ => {
                    self.errors.push(
                        UnknownTokenError::new(
                            self.source.lines().nth(self.d_pos.0).unwrap(),
                            self.d_pos,
                        )
                        .message(),
                    );
                    self.advance(1);
                }
            }
        }
    }
    // CURSOR MOVEMENT
    fn advance(&mut self, times: usize) -> () {
        for _ in 0..times {
            if self.pos > self.source.len() - 1 {
                self.curr_char = '\0';
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
    fn reverse(&mut self, times: usize) -> () {
        for _ in 0..times {
            if self.pos == 0 {
                self.d_pos.1 = 0;
                self.curr_char = self.source.chars().nth(0).unwrap_or('\0');
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
                self.d_pos.1 = self.source.lines().nth(self.d_pos.1).unwrap_or("").len() - 1;
            }
            self.peek_char = self.curr_char;
            self.curr_char = self.source.chars().nth(self.pos).unwrap_or('\n')
        }
    }
    fn peek_reserved(&mut self, expected: TokenType) -> Result<(), ()> {
        let expect_str = expected.to_str();
        let (line, start, end) = (self.d_pos.0, self.d_pos.1, self.d_pos.1);
        for i in 0..expect_str.len() {
            if self.curr_char != expect_str.chars().nth(i).unwrap_or('\n') {
                self.reverse(i);
                return Err(());
            }
            self.advance(1);
        }
        if !expected.delims().contains(&self.curr_char) {
            if atoms("alpha_num").contains(&self.curr_char)
                || !expect_str.chars().all(|c| atoms("alpha_num").contains(&c))
            {
                self.reverse(expect_str.len());
                return Err(());
            }
            self.errors.push(
                DelimError::new(
                    reserved_to_token_type(expect_str),
                    expected.delims(),
                    self.curr_char,
                    self.source.lines().nth(self.d_pos.0).unwrap(),
                    self.d_pos,
                )
                .message(),
            );
            return Err(());
        }
        let (line, start, end) = match expect_str {
            "\r" | "\n" => (line, start, end),
            _ => (
                self.d_pos.0,
                self.d_pos.1 - expect_str.len(),
                self.d_pos.1 - 1,
            ),
        };
        self.tokens.push(
            to_token(expect_str, (line, start), (line, end))
                .map_err(|e| e.to_string())
                .unwrap(),
        );
        Ok(())
    }
    fn peek_ident(&mut self) -> () {
        let mut tmp = String::new();
        while self.curr_char_can_be_ident() {
            tmp.push(self.curr_char);
            self.advance(1);
        }
        let token: &'static str = Box::leak(tmp.into_boxed_str());
        self.tokens.push(Token::from(
            token,
            (self.d_pos.0, self.d_pos.1 - token.len()),
            (self.d_pos.0, self.d_pos.1 - 1),
        ));
    }
    fn peek_single_line_comment(&mut self) -> Result<(), ()> {
        let mut tmp = ">.".to_string();
        self.expect_peek_char_is('.', 0)?;
        self.expect_peek_char_is('<', 1)?;
        while !['\n', '\r'].contains(&self.curr_char) {
            tmp.push(self.curr_char);
            self.advance(1);
        }
        let token: &'static str = Box::leak(tmp.into_boxed_str());
        self.tokens.push(Token::from(
            token,
            (self.d_pos.0, self.d_pos.1 - token.len()),
            (self.d_pos.0, self.d_pos.1 - 1),
        ));
        Ok(())
    }
    fn peek_int(&mut self) -> () {
        let mut tmp: String = "".to_string();
        while atoms("number").contains(&self.curr_char) {
            tmp.push(self.curr_char);
            self.advance(1);
        }
        if self.curr_char == '.' {
            return self.peek_float(tmp);
        }
        if !TokenType::IntLiteral.delims().contains(&self.curr_char) {
            self.errors.push(
                DelimError::new(
                    TokenType::IntLiteral,
                    TokenType::IntLiteral.delims(),
                    self.curr_char,
                    self.source.lines().nth(self.d_pos.0).unwrap(),
                    self.d_pos,
                )
                .message(),
            );
            return;
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
    }
    /// must be called after peek_int since it requires the digits before the .
    fn peek_float(&mut self, before: String) -> () {
        let mut tmp = before;
        tmp.push('.');
        self.advance(1); // consume the .
        if !atoms("number").contains(&self.curr_char) {
            self.errors.push(
                DelimError::new(
                    TokenType::FloatLiteral,
                    atoms("number"),
                    self.curr_char,
                    self.source.lines().nth(self.d_pos.0).unwrap(),
                    self.d_pos,
                )
                .message(),
            );
            return;
        }
        while atoms("number").contains(&self.curr_char) {
            tmp.push(self.curr_char);
            self.advance(1);
        }
        // wrong delimiter
        if !TokenType::FloatLiteral.delims().contains(&self.curr_char) {
            self.errors.push(
                DelimError::new(
                    TokenType::FloatLiteral,
                    TokenType::FloatLiteral.delims(),
                    self.curr_char,
                    self.source.lines().nth(self.d_pos.0).unwrap(),
                    self.d_pos,
                )
                .message(),
            );
            return;
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
    }
    fn peek_string(&mut self) -> () {
        let mut tmp: String = self.curr_char.to_string();
        self.advance(1); // consume the " or |
        let closing: Vec<char> = vec!['"', '|'];
        while !closing.contains(&self.curr_char) {
            if self.curr_char == '\r' || self.curr_char == '\n' {
                let token_type = match tmp.chars().nth(0).unwrap_or('\n') {
                    '"' => TokenType::StringLiteral,
                    '|' => TokenType::StringPartMid,
                    _ => unreachable!(),
                };
                let expected_delims = token_type.delims();
                self.errors.push(
                    DelimError::new(
                        token_type,
                        expected_delims,
                        self.curr_char,
                        self.source.lines().nth(self.d_pos.0).unwrap(),
                        self.d_pos,
                    )
                    .message(),
                );
                return;
            }
            tmp.push(self.curr_char);
            self.advance(1);
        }
        tmp.push(self.curr_char);
        let delims = match self.curr_char {
            '"' => TokenType::StringLiteral.delims(),
            '|' => TokenType::StringPartMid.delims(),
            _ => unreachable!(),
        };
        let token_type = match (
            tmp.chars().nth(0).unwrap_or('\n'),
            tmp.chars().last().unwrap_or('\n'),
        ) {
            ('"', '"') => TokenType::StringLiteral,
            ('"', '|') => TokenType::StringPartStart,
            ('|', '|') => TokenType::StringPartMid,
            ('|', '"') => TokenType::StringPartEnd,
            _ => unreachable!(),
        };
        self.advance(1); // consume the closing " or |
        if !delims.contains(&self.curr_char) {
            self.errors.push(
                DelimError::new(
                    token_type,
                    delims,
                    self.curr_char,
                    self.source.lines().nth(self.d_pos.0).unwrap(),
                    self.d_pos,
                )
                .message(),
            )
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
        )
    // HELPER METHODS
    fn expect_peek_char_is(&mut self, expected: char, reverse_count: usize) -> Result<(), ()> {
        match expected == self.peek_char {
            true => {
                self.advance(1);
                Ok(())
            }
            false => {
                self.reverse(reverse_count);
                Err(())
            }
        }
    }
    fn curr_char_can_be_ident(&self) -> bool {
        self.curr_char.is_ascii_alphanumeric() || self.curr_char == '_'
    }
}
