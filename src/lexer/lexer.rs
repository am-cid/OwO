use crate::lexer::token::{to_token, Token, TokenType};
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
            self.curr_char, self.peek_char, self.pos, self.d_pos.1, self.d_pos.0
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
                ' ' | '\t' | '\n' => self.advance(1),
                'c' => self.peek(TokenType::Chan),
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
            self.d_pos.0 += 1;
            if self.curr_char == '\n' {
                self.d_pos.0 = 0;
                self.d_pos.1 += 1;
            }
            self.curr_char = self.peek_char;
            self.peek_char = self.source.chars().nth(self.pos + 1).unwrap_or('\n');
        }
    }
    pub fn reverse(&mut self, times: usize) -> () {
        for _ in 0..times {
            if self.pos == 0 {
                self.d_pos.0 = 0;
                self.curr_char = self.source.chars().nth(0).unwrap_or('\n');
                return;
            }
            self.pos -= 1;
            self.d_pos.0 = match self.d_pos.0 {
                0 => 0,
                _ => self.d_pos.0 - 1,
            };
            if self.curr_char == '\n' {
                self.d_pos.1 = match self.d_pos.1 {
                    0 => 0,
                    _ => self.d_pos.1 - 1,
                };
                self.d_pos.0 = self.source.lines().nth(self.d_pos.1).unwrap_or("").len();
            }
            self.peek_char = self.curr_char;
            self.curr_char = self.source.chars().nth(self.pos).unwrap_or('\n')
        }
    }
    pub fn peek(&mut self, expected: TokenType) -> () {
        let expect_str = expected.to_str();
        for i in 0..expect_str.len() - 1 {
            if self.curr_char != expect_str.chars().nth(i).unwrap() {
                self.reverse(i);
                return;
            }
            self.advance(1);
        }
        if !expected.delims().contains(&self.peek_char) {
            // TODO: check if identifier/class id, then error
            self.reverse(expect_str.len() - 1);
            return;
        }
        self.tokens.push(
            to_token(
                expect_str,
                (self.d_pos.1, self.d_pos.0 + 1 - expect_str.len()),
                (self.d_pos.1, self.d_pos.0),
            )
            .map_err(|e| e.to_string())
            .unwrap(),
        );
        self.advance(1);
    }
}
