use crate::errors::errors::{
    CompilerError, DelimError, SingleBracketError, UnclosedCharError, UnclosedStringError,
    UnknownTokenError,
};
use crate::lexer::token::{Atoms, Token, TokenType};
use std::collections::HashSet;
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
                    .peek_symbol(TokenType::Whitespace)
                    .unwrap_or_else(|_| ()),
                '\t' => self.peek_symbol(TokenType::Tab).unwrap_or_else(|_| ()),
                '\r' => self.peek_symbol(TokenType::Return).unwrap_or_else(|_| ()),
                '\n' => self.peek_symbol(TokenType::Newline).unwrap_or_else(|_| ()),
                '+' => self
                    .peek_symbol(TokenType::PlusEqual)
                    .or_else(|_| self.peek_symbol(TokenType::Plus))
                    .unwrap_or_else(|_| ()),
                '-' => self
                    .peek_symbol(TokenType::DashEqual)
                    .or_else(|_| self.peek_symbol(TokenType::Dash))
                    .unwrap_or_else(|_| ()),
                '*' => self
                    .peek_symbol(TokenType::MultiplyEqual)
                    .or_else(|_| self.peek_symbol(TokenType::Multiply))
                    .unwrap_or_else(|_| ()),
                '/' => self
                    .peek_symbol(TokenType::DivideEqual)
                    .or_else(|_| self.peek_symbol(TokenType::Divide))
                    .unwrap_or_else(|_| ()),
                '%' => self
                    .peek_symbol(TokenType::ModuloEqual)
                    .or_else(|_| self.peek_symbol(TokenType::Modulo))
                    .unwrap_or_else(|_| ()),
                '^' => self
                    .peek_symbol(TokenType::ExponentEqual)
                    .or_else(|_| self.peek_symbol(TokenType::Exponent))
                    .unwrap_or_else(|_| ()),
                '<' => self
                    .peek_symbol(TokenType::LessEqual)
                    .or_else(|_| self.peek_symbol(TokenType::LessThan))
                    .unwrap_or_else(|_| ()),
                '>' => self
                    .peek_single_line_comment()
                    .or_else(|_| self.peek_symbol(TokenType::GreaterEqual))
                    .or_else(|_| self.peek_symbol(TokenType::GreaterThan))
                    .unwrap_or_else(|_| ()),
                '=' => self
                    .peek_symbol(TokenType::Equal)
                    .or_else(|_| self.peek_symbol(TokenType::Assign))
                    .unwrap_or_else(|_| ()),
                '!' => self
                    .peek_symbol(TokenType::NotEqual)
                    .or_else(|_| self.peek_symbol(TokenType::Bang))
                    .unwrap_or_else(|_| ()),
                '(' => self.peek_symbol(TokenType::LParen).unwrap_or_else(|_| ()),
                ')' => self.peek_symbol(TokenType::RParen).unwrap_or_else(|_| ()),
                '[' => self.peek_symbol(TokenType::LBracket).unwrap_or_else(|_| ()),
                ']' => self.peek_symbol(TokenType::RBracket).unwrap_or_else(|_| ()),
                '{' => self.peek_symbol(TokenType::LBrace).unwrap_or_else(|_| ()),
                '}' => self
                    .peek_string()
                    .or_else(|_| self.peek_symbol(TokenType::RBrace))
                    .unwrap_or_else(|_| ()),
                '.' => self.peek_symbol(TokenType::Dot).unwrap_or_else(|_| ()),
                '?' => self.peek_symbol(TokenType::Question).unwrap_or_else(|_| ()),
                ',' => self.peek_symbol(TokenType::Comma).unwrap_or_else(|_| ()),
                '|' => self.peek_symbol(TokenType::Pipe).unwrap_or_else(|_| ()),
                '~' => self
                    .peek_symbol(TokenType::Terminator)
                    .unwrap_or_else(|_| ()),
                'a'..='z' | 'A'..='Z' => self.peek_ident(),
                '0'..='9' => self.peek_int(),
                '"' => self.peek_string().unwrap_or_else(|_| ()),
                '\'' => self.peek_char_lit().unwrap_or_else(|_| ()),
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
    // TOKENIZERS
    fn peek_symbol(&mut self, expected: TokenType) -> Result<(), ()> {
        let expect_str = expected.to_str();
        let (line, start, end) = (self.d_pos.0, self.d_pos.1, self.d_pos.1);
        for i in 0..expect_str.len() {
            if self.curr_char != expect_str.chars().nth(i).unwrap_or('\n') {
                self.reverse(i);
                return Err(());
            }
            self.advance(1);
        }
        let end = match expect_str {
            "\r" | "\n" | "\t" | " " => end,
            _ => self.d_pos.1 - 1,
        };
        self.tokens
            .push(Token::from(expect_str, (line, start), (line, end)));
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
        let mut tmp: String = String::new();
        let delims = Atoms::combine(&[Atoms::Symbols, Atoms::Whitespace]);
        while self.curr_char.is_ascii_digit() || self.curr_char == '_' {
            tmp.push(self.curr_char);
            self.advance(1);
        }
        // ended with _
        if tmp.chars().last().unwrap_or_default() == '_' {
            self.reverse(1);
            self.errors.push(
                DelimError::new(
                    TokenType::IntLiteral,
                    delims,
                    self.curr_char,
                    self.source.lines().nth(self.d_pos.0).unwrap(),
                    self.d_pos,
                )
                .message(),
            );
            return;
        }
        if self.curr_char == '.' {
            return self.peek_float(tmp);
        }
        if !delims.contains(&self.curr_char) {
            self.errors.push(
                DelimError::new(
                    TokenType::IntLiteral,
                    delims,
                    self.curr_char,
                    self.source.lines().nth(self.d_pos.0).unwrap(),
                    self.d_pos,
                )
                .message(),
            );
            return;
        }
        let token: &'static str = Box::leak(tmp.into_boxed_str());
        self.tokens.push(Token::from(
            token,
            (self.d_pos.0, self.d_pos.1 - token.len()),
            (self.d_pos.0, self.d_pos.1 - 1),
        ));
    }
    /// must be called after peek_int since it requires the digits before the decimal point
    fn peek_float(&mut self, before: String) -> () {
        let mut tmp = before;
        self.advance(1); // consume the .
        tmp.push('.');
        let delims: HashSet<char> = Atoms::combine(&[Atoms::Symbols, Atoms::Whitespace])
            .into_iter()
            .filter(|&x| x != '.')
            .collect();
        while self.curr_char.is_ascii_digit() || self.curr_char == '_' {
            tmp.push(self.curr_char);
            self.advance(1);
        }
        if ['_', '.'].contains(&tmp.chars().last().unwrap_or_default()) {
            self.reverse(1);
        }
        if !delims.contains(&self.curr_char) {
            self.errors.push(
                DelimError::new(
                    TokenType::FloatLiteral,
                    delims,
                    self.curr_char,
                    self.source.lines().nth(self.d_pos.0).unwrap(),
                    self.d_pos,
                )
                .message(),
            );
            return;
        }
        let token: &'static str = Box::leak(tmp.into_boxed_str());
        self.tokens.push(Token::from(
            token,
            (self.d_pos.0, self.d_pos.1 - token.len()),
            (self.d_pos.0, self.d_pos.1 - 1),
        ));
    }
    fn peek_string(&mut self) -> Result<(), ()> {
        let starting = self.curr_char; // either " or }
        self.advance(1);
        let mut advance_count = 1;
        let mut tmp: String = starting.to_string();
        while !['"', '{'].contains(&self.curr_char) {
            // unclosed string or single bracket
            if self.curr_char == '\r' || self.curr_char == '\n' {
                match starting {
                    '"' => {
                        self.errors.push(
                            UnclosedStringError::new(
                                self.curr_char,
                                self.source.lines().nth(self.d_pos.0).unwrap(),
                                (self.d_pos.0, self.d_pos.1 - tmp.len()),
                                tmp.len(),
                            )
                            .message(),
                        );
                        return Err(());
                    }
                    '}' => {
                        self.reverse(advance_count);
                        return Err(());
                    }
                    _ => unreachable!(),
                }
            }
            // ensure double {{
            while self.curr_char == '{' && self.peek_char == '{' {
                tmp.extend("{{".chars());
                self.advance(2);
                advance_count += 2;
            }
            // ensure double }}
            while self.curr_char == '}' {
                if self.peek_char != '}' {
                    self.errors.push(
                        SingleBracketError::new(
                            self.source.lines().nth(self.d_pos.0).unwrap(),
                            self.d_pos,
                        )
                        .message(),
                    );
                    self.advance(1);
                    return Err(());
                }
                tmp.extend("}}".chars());
                self.advance(2);
                advance_count += 2;
            }
            tmp.push(self.curr_char);
            self.advance(1);
            advance_count += 1;
        }
        tmp.push(self.curr_char);
        let delims = match self.curr_char {
            '"' => Atoms::combine(&[Atoms::Symbols, Atoms::Whitespace]),
            '{' => Atoms::combine(&[Atoms::AlphaNum, Atoms::Symbols, Atoms::Whitespace]),
            _ => unreachable!(),
        };
        let token_type = match (
            tmp.chars().nth(0).unwrap_or('"'),
            tmp.chars().last().unwrap_or('"'),
        ) {
            ('"', '"') => TokenType::StringLiteral,
            ('"', '{') => TokenType::StringPartStart,
            ('}', '{') => TokenType::StringPartMid,
            ('}', '"') => TokenType::StringPartEnd,
            _ => unreachable!(),
        };
        self.advance(1); // consume the closing " or {
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
        self.tokens.push(Token::from(
            token,
            (self.d_pos.0, self.d_pos.1 - token.len()),
            (self.d_pos.0, self.d_pos.1 - 1),
        ));
        Ok(())
    }
    fn peek_char_lit(&mut self) -> Result<(), ()> {
        let mut tmp: String = self.curr_char.to_string();
        self.advance(1);
        tmp.push(self.curr_char);
        if self.curr_char != '\'' {
            self.advance(1);
            match self.curr_char {
                '\'' => {
                    tmp.push(self.curr_char);
                }
                _ => {
                    self.errors.push(
                        UnclosedCharError::new(
                            self.curr_char,
                            self.source.lines().nth(self.d_pos.0).unwrap(),
                            self.d_pos,
                        )
                        .message(),
                    );
                    self.advance(1);
                    return Err(());
                }
            }
        }
        self.advance(1);
        let delims = Atoms::combine(&[Atoms::Symbols, Atoms::Whitespace]);
        if !delims.contains(&self.curr_char) {
            self.errors.push(
                DelimError::new(
                    TokenType::CharLiteral,
                    delims,
                    self.curr_char,
                    self.source.lines().nth(self.d_pos.0).unwrap(),
                    self.d_pos,
                )
                .message(),
            );
            self.advance(1);
            return Err(());
        }
        let token: &'static str = Box::leak(tmp.into_boxed_str());
        self.tokens.push(Token::from(
            token,
            (self.d_pos.0, self.d_pos.1 - token.len()),
            (self.d_pos.0, self.d_pos.1 - 1),
        ));
        Ok(())
    }

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
