use crate::lexer::token::Token;
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
    pub fn advance(&mut self, times: usize) -> () {
        for _ in 0..times {
            if self.pos > self.source.len() - 1 {
                self.curr_char = '\0';
                return;
            }
            self.pos += 1;
            self.d_pos.0 += 1;
            if self.curr_char == '\n' {
                self.d_pos.0 = 0;
                self.d_pos.1 += 1;
            }
            self.curr_char = self.peek_char;
            self.peek_char = self.source.chars().nth(self.pos + 1).unwrap_or('\0');
        }
    }
    pub fn reverse(&mut self, times: usize) -> () {
        for _ in 0..times {
            if self.pos == 0 {
                self.d_pos.0 = 0;
                self.curr_char = self.source.chars().nth(0).unwrap_or('\0');
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
                self.d_pos.0 = self.source.lines().nth(self.d_pos.1).unwrap().len();
            }
            self.peek_char = self.curr_char;
            self.curr_char = self.source.chars().nth(self.pos).unwrap_or('\0')
        }
    }
    pub fn peek_str(&mut self, expected: &'static str) -> bool {
        for i in 0..expected.len() {
            if self.curr_char != expected.chars().nth(i).unwrap() {
                self.reverse(i);
                return false;
            }
            self.advance(1);
        }
        true
    }
}
