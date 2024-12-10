use core::fmt;
use std::fmt::Debug;

use crate::{
    debug_only,
    errors::lex_errors::{EmptyChar, LexerError, UnclosedChar, UnclosedString, UnknownToken},
    lexer::token::{Token, TokenKind, TokenPosition},
};

/***************
* LEXER
***************/
#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    pub source: String,
    pub tokens: Vec<Token>,
    pub errors: Vec<LexerError<'a>>,
    pub pos: LexerPosition,
}
impl<'a> Lexer<'a> {
    pub fn new(source: String) -> Self {
        let mut l = Lexer {
            source,
            tokens: vec![],
            errors: vec![],
            pos: LexerPosition::zero(),
        };
        l.tokenize();
        l
    }
    fn tokenize(&mut self) {
        while self.source.chars().nth(self.pos.abs).is_some() {
            self.next_token().ok();
        }
    }
    /// tokenizes the next token in the source code if any and stores it in [Lexer::tokens]
    fn next_token(&mut self) -> Result<(), ()> {
        match self.curr_char() {
            ' ' => self.lex(TokenKind::Whitespace),
            '\t' => self.lex(TokenKind::Tab),
            '\r' => self.lex(TokenKind::CarriageReturn),
            '\n' => self.lex(TokenKind::Newline),
            '+' => self
                .lex(TokenKind::PlusEqual)
                .or_else(|_| self.lex(TokenKind::Plus)),
            '-' => self
                .lex(TokenKind::DashEqual)
                .or_else(|_| self.lex(TokenKind::Dash)),
            '*' => self
                .lex(TokenKind::MultiplyEqual)
                .or_else(|_| self.lex(TokenKind::Multiply)),
            '/' => self
                .lex(TokenKind::DivideEqual)
                .or_else(|_| self.lex(TokenKind::Divide)),
            '%' => self
                .lex(TokenKind::ModuloEqual)
                .or_else(|_| self.lex(TokenKind::Modulo)),
            '^' => self
                .lex(TokenKind::ExponentEqual)
                .or_else(|_| self.lex(TokenKind::Exponent)),
            '<' => self
                .lex(TokenKind::LessEqual)
                .or_else(|_| self.lex(TokenKind::LessThan)),
            '>' => self
                .lex_comment()
                .or_else(|_| self.lex(TokenKind::GreaterEqual))
                .or_else(|_| self.lex(TokenKind::GreaterThan)),
            '=' => self
                .lex(TokenKind::Equal)
                .or_else(|_| self.lex(TokenKind::Assign)),
            '!' => self
                .lex(TokenKind::NotEqual)
                .or_else(|_| self.lex(TokenKind::Bang)),
            '(' => self.lex(TokenKind::LParen),
            ')' => self.lex(TokenKind::RParen),
            '[' => self.lex(TokenKind::LBracket),
            ']' => self.lex(TokenKind::RBracket),
            '{' => self.lex(TokenKind::LBrace),
            '}' => self.lex(TokenKind::RBrace),
            '.' => self
                .lex(TokenKind::Ellipsis)
                .or_else(|_| self.lex(TokenKind::Dot)),
            '?' => self.lex(TokenKind::Question),
            ',' => self.lex(TokenKind::Comma),
            ':' => self.lex(TokenKind::Colon),
            '#' => self.lex(TokenKind::Hash),
            '|' => self.lex(TokenKind::Pipe),
            '~' => self.lex(TokenKind::Terminator),
            'a'..='z' | 'A'..='Z' => self.lex_id(),
            '0'..='9' => self.lex_num(),
            '"' => self.lex_string(),
            '\'' => self.lex_char(),
            ch => {
                self.unknown_token_error(ch);
                Err(self.advance())
            }
        }
    }

    /****************
     * LEXERS
     ***************/
    /// starts with the beginning of a reserved word/symbol
    /// ends with the delimiter if [Ok]
    /// ends without moving cursor if [Err]
    fn lex(&mut self, expected: TokenKind) -> Result<(), ()> {
        let pos = self.pos;
        let expected_str = expected.to_str();
        let actual = self
            .source
            .get(pos.abs..pos.abs + expected_str.len())
            .unwrap_or_default();
        if expected_str != actual {
            return Err(());
        }
        for _ in 0..expected_str.len() {
            self.advance();
        }
        let res = Token {
            kind: expected,
            pos: TokenPosition::new((pos.abs, self.pos.abs), pos.line, pos.col),
        };
        Ok(self.tokens.push(res))
    }
    /// starts with the beginning of an identifier/Type
    /// ends with the delimiter if [Ok]
    /// never returns [Err]
    fn lex_id(&mut self) -> Result<(), ()> {
        let pos = self.pos;
        while self.curr_char().is_lexer_identifier() {
            self.advance();
        }
        let tok = self.token_from_str(pos);
        Ok(self.tokens.push(tok))
    }
    /// starts with the beginning of a number
    /// ends with the delimiter if [Ok]
    /// never returns [Err]
    fn lex_num(&mut self) -> Result<(), ()> {
        let pos = self.pos;
        while self.curr_char().is_lexer_digit() {
            self.advance();
        }
        match self.curr_char() {
            '.' => {
                self.advance();
                while self.curr_char().is_lexer_digit() {
                    self.advance();
                }
            }
            _ => (),
        }
        let tok = self.token_from_str(pos);
        Ok(self.tokens.push(tok))
    }
    /// starts with the beginning of a number
    /// ends with the delimiter if [Ok]
    /// ends with `\r`, `\n`, or `\0` that left string unclosed if [Err]
    fn lex_string(&mut self) -> Result<(), ()> {
        let pos = self.pos;
        loop {
            match self.next_char() {
                '"' => break,
                '\r' | '\n' | '\0' => break,
                '\\' => match self.next_char() {
                    // avoid escaping terminators
                    '\r' | '\n' | '\0' => break,
                    _ => (),
                },
                _ => (),
            }
        }
        match self.curr_char() {
            '"' => {
                self.advance(); // go to delimiter
                let tok = self.token_from_str(pos);
                Ok(self.tokens.push(tok))
            }
            unexpected => Err(self.unclosed_string_error(unexpected, pos, self.pos.abs - pos.abs)),
        }
    }
    /// starts with a single quote: `'`
    /// ends with the delimiter if [Ok]
    /// ends with char that left char unclosed if [Err]
    fn lex_char(&mut self) -> Result<(), ()> {
        let pos = self.pos;
        match self.next_char() {
            '\\' => match self.peek_char() {
                // avoid escaping terminators
                '\r' | '\n' | '\0' => (),
                _ => self.advance(),
            },
            '\'' => {
                return Err(self.empty_char_error(pos));
            }
            token @ ('\r' | '\n' | '\0') => {
                return Err(self.unclosed_char_error(token, pos, self.pos.abs - pos.abs));
            }
            _ => (),
        }
        match self.next_char() {
            '\'' => {
                self.advance(); // go to delimiter
                let tok = self.token_from_str(pos);
                Ok(self.tokens.push(tok))
            }
            unexpected => Err(self.unclosed_char_error(unexpected, pos, self.pos.abs - pos.abs)),
        }
    }
    /// starts with greater than: `>`
    /// ends with `\r`, `\n`, or `\0` if [Ok]
    /// ends without moving cursor if [Err]
    fn lex_comment(&mut self) -> Result<(), ()> {
        let pos = self.pos;
        let expected = ">_<";
        let actual = &self.source[pos.abs..pos.abs + 3];
        if expected == actual {
            for _ in 0..3 {
                self.advance();
            }
        } else {
            return Err(());
        }
        while !['\r', '\n', '\0'].contains(&self.curr_char()) {
            self.advance();
        }
        let tok = self.token_from_str(pos);
        Ok(self.tokens.push(tok))
    }

    /****************
     * HELPER METHODS
     ***************/
    /// Creates a token based on the str passed to it. This assumes that the passed in str is
    /// already tokenized beforehand by lexers from [Lexer] e.g. [Lexer::lex_id]
    ///
    /// [LexerPosition] passed in should be the start of the tokenized text.
    /// The current token must be at the delimiter of the tokenized text
    fn token_from_str(&self, pos: LexerPosition) -> Token {
        let text = &self.source[pos.abs..self.pos.abs];
        Token::from_str(
            text,
            TokenPosition::new((pos.abs, self.pos.abs), pos.line, pos.col),
        )
        .expect(
            format!(
                "{}: text was tokenized beforehand so that Token::from_str should not fail: {:?}",
                "Lexer::token_from_str", text,
            )
            .as_str(),
        )
    }
    /// Updates internal [Lexer] state, incrementing [Lexer::pos] by 1
    fn advance(&mut self) {
        match self.source.chars().nth(self.pos.abs) {
            None => (),
            Some(ch) => {
                self.pos.col += 1;
                if ch == '\n' {
                    self.pos.col = 0;
                    self.pos.line += 1;
                }
                self.pos.abs += 1;
            }
        }
    }
    /// Returns the current character
    ///
    /// Wrapper around [Iterator::nth] where this returns `\0` when it encounters [None]
    /// instead of returning [Option<char>]
    fn curr_char(&mut self) -> char {
        match self.source.chars().nth(self.pos.abs) {
            None => '\0',
            Some(ch) => ch,
        }
    }
    /// Returns the next character
    ///
    /// Wrapper around [Iterator::nth] where this returns `\0` when it encounters [None]
    /// instead of returning [Option<char>]
    fn peek_char(&mut self) -> char {
        match self.source.chars().nth(self.pos.abs + 1) {
            None => '\0',
            Some(ch) => ch,
        }
    }
    /// does [Lexer::advance] then [Lexer::curr_char]
    #[inline]
    fn next_char(&mut self) -> char {
        self.advance();
        self.curr_char()
    }

    /****************
     * ERROR METHODS
     ***************/
    fn unknown_token_error(&mut self, token: char) {
        self.errors.push(LexerError::UnknownToken(UnknownToken {
            token,
            line_text: Box::leak(
                self.source
                    .lines()
                    .nth(self.pos.line)
                    .unwrap_or_default()
                    .to_owned()
                    .into_boxed_str(),
            ),
            line: self.pos.line,
            col: self.pos.col,
        }));
    }
    fn unclosed_string_error(&mut self, token: char, pos: LexerPosition, length: usize) {
        self.errors.push(LexerError::UnclosedString(UnclosedString {
            actual: token,
            line_text: Box::leak(
                self.source
                    .lines()
                    .nth(pos.line)
                    .unwrap_or_default()
                    .to_owned()
                    .into_boxed_str(),
            ),
            line: pos.line,
            col: pos.col,
            length: length + 1,
        }));
    }
    fn unclosed_char_error(&mut self, token: char, pos: LexerPosition, length: usize) {
        self.errors.push(LexerError::UnclosedChar(UnclosedChar {
            actual: token,
            line_text: Box::leak(
                self.source
                    .lines()
                    .nth(pos.line)
                    .unwrap_or_default()
                    .to_owned()
                    .into_boxed_str(),
            ),
            line: pos.line,
            col: pos.col,
            length: length + 1,
        }));
    }
    fn empty_char_error(&mut self, pos: LexerPosition) {
        self.errors.push(LexerError::EmptyChar(EmptyChar {
            line_text: Box::leak(
                self.source
                    .lines()
                    .nth(pos.line)
                    .unwrap_or_default()
                    .to_owned()
                    .into_boxed_str(),
            ),
            line: pos.line,
            col: pos.col,
        }));
    }

    /****************
     * ETC
     ***************/
    /// pretty prints token information that the lexer tokenized
    /// does nothing on release builds
    pub fn debug_tokens(&self) {
        debug_only!(
            let border = "-".repeat(71);
            println!(
                r#"{border}
|       kind      |       text      |     position    |    abs pos    |
|-----------------|-----------------|-----------------|---------------|
{}
{border}"#,
                self.tokens
                    .iter()
                    .map(|token| format!(
                        "| {: <15} | {: <15} | {: <15} | {: <13} |",
                        token.kind.to_string(),
                        match token.str_from_source(&self.source) {
                            " " => "space".to_string(),
                            "\t" => "tab".to_string(),
                            "\r" => "carriage return".to_string(),
                            "\n" => "newline".to_string(),
                            _ => {
                                let printed = token.str_from_source(&self.source);
                                if printed.len() > 15 {
                                    printed[..6].to_string() + "..." + &printed[printed.len() - 6..]
                                } else {
                                    printed.to_string()
                                }
                            }
                        },
                        format!(
                            "{: >2}:{: >2}",
                            token.pos.buffer_line().to_string(),
                            match token.pos.buffer_col() == token.pos.buffer_col_end() {
                                true => token.pos.col.to_string(),
                                false => format!("{}-{}", token.pos.buffer_col(), token.pos.buffer_col_end()),
                            }
                        ),
                        format!(
                            "{: <2}-{: >2}",
                            token.pos.offset.start,
                            token.pos.offset.end - 1,
                        ),
                    ))
                    .collect::<Vec<String>>()
                    .join("\n"),
            )
        )
    }
}

/***************
* LEXER METADATA
***************/
/// position metadata for [Lexer] to track cursor state
#[derive(Debug, Clone, Copy, Default)]
pub struct LexerPosition {
    /// offset from the beginning of the buffer, aka absolute starting position
    pub abs: usize,
    /// current line in buffer
    pub line: usize,
    /// current column in buffer
    pub col: usize,
}
impl fmt::Display for LexerPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})[{}]", self.line, self.col, self.abs)
    }
}
impl LexerPosition {
    pub fn new(abs: usize, line: usize, col: usize) -> Self {
        Self { abs, line, col }
    }
    pub fn zero() -> Self {
        Self {
            abs: 0,
            line: 0,
            col: 0,
        }
    }
}

/***************
* LEXER HELPERS
***************/
/// helper trait to make tokenizing a bit easier
pub trait LexerCharExt {
    fn is_lexer_identifier(&self) -> bool;
    fn is_lexer_digit(&self) -> bool;
}
impl LexerCharExt for char {
    fn is_lexer_identifier(&self) -> bool {
        *self == '_' || self.is_ascii_alphanumeric()
    }
    fn is_lexer_digit(&self) -> bool {
        *self == '_' || self.is_ascii_digit()
    }
}
impl LexerCharExt for Option<&char> {
    fn is_lexer_identifier(&self) -> bool {
        match *self {
            None => false,
            Some(ch) => ch.is_lexer_identifier(),
        }
    }
    fn is_lexer_digit(&self) -> bool {
        match *self {
            None => false,
            Some(ch) => ch.is_lexer_digit(),
        }
    }
}
