use crate::errors::lex_errors::{
    CompilerError, UnclosedCharError, UnclosedStringError, UnescapedBracketInStringError,
    UnknownTokenError,
};
use crate::lexer::token::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Lexer {
    pub source: &'static str,
    pub tokens: Vec<Token>,
    pub errors: Vec<String>,
    pub pos: usize,
    pub d_pos: (usize, usize),
}

impl Lexer {
    pub fn new(source: &'static str) -> Lexer {
        Lexer {
            source,
            tokens: vec![],
            errors: vec![],
            pos: 0,
            d_pos: (0, 0),
        }
    }
    pub fn pretty_print_tokens(&self) -> () {
        let width = 69;
        println!(" {}", "-".repeat(width));
        println!("|       kind      |       text      |     position    |    abs pos    |");
        println!("|-----------------|-----------------|-----------------|---------------|");
        for token in &self.tokens {
            println!(
                "| {: <15} | {: <15} | {: <15} | {: <13} |",
                token.kind.to_string(),
                match token.text {
                    " " => "space".to_string(),
                    "\t" => "tab".to_string(),
                    "\r" => "return".to_string(),
                    "\n" => "newline".to_string(),
                    _ => {
                        let printed = token.text.to_string();
                        if printed.len() > 15 {
                            printed[0..12].to_string() + "..."
                        } else {
                            printed
                        }
                    }
                },
                format!(
                    "{: >2}:{: >2}",
                    match &token.pos.0 == &token.end_pos.0 {
                        true => token.pos.0.to_string(),
                        false => format!("{}-{}", token.pos.0, token.end_pos.0),
                    },
                    match &token.pos.1 == &token.end_pos.1 {
                        true => token.pos.1.to_string(),
                        false => format!("{}-{}", token.pos.1, token.end_pos.1),
                    }
                ),
                format!("{: <2}-{: >2}", token.range.0, token.range.1,),
            );
        }
        println!(" {}", "-".repeat(width));
    }
    pub fn tokenize(&mut self) {
        while self.pos < self.source.len() {
            match self.curr_char() {
                ' ' => self.peek_symbol(TokenKind::Whitespace).ok(),
                '\t' => self.peek_symbol(TokenKind::Tab).ok(),
                '\r' => self.peek_symbol(TokenKind::CarriageReturn).ok(),
                '\n' => self.peek_symbol(TokenKind::Newline).ok(),
                '+' => self
                    .peek_symbol(TokenKind::PlusEqual)
                    .or_else(|_| self.peek_symbol(TokenKind::Plus))
                    .ok(),
                '-' => self
                    .peek_symbol(TokenKind::DashEqual)
                    .or_else(|_| self.peek_symbol(TokenKind::Dash))
                    .ok(),
                '*' => self
                    .peek_symbol(TokenKind::MultiplyEqual)
                    .or_else(|_| self.peek_symbol(TokenKind::Multiply))
                    .ok(),
                '/' => self
                    .peek_symbol(TokenKind::DivideEqual)
                    .or_else(|_| self.peek_symbol(TokenKind::Divide))
                    .ok(),
                '%' => self
                    .peek_symbol(TokenKind::ModuloEqual)
                    .or_else(|_| self.peek_symbol(TokenKind::Modulo))
                    .ok(),
                '^' => self
                    .peek_symbol(TokenKind::ExponentEqual)
                    .or_else(|_| self.peek_symbol(TokenKind::Exponent))
                    .ok(),
                '<' => self
                    .peek_symbol(TokenKind::LessEqual)
                    .or_else(|_| self.peek_symbol(TokenKind::LessThan))
                    .ok(),
                '>' => self
                    .peek_comment()
                    .or_else(|_| self.peek_symbol(TokenKind::GreaterEqual))
                    .or_else(|_| self.peek_symbol(TokenKind::GreaterThan))
                    .ok(),
                '=' => self
                    .peek_symbol(TokenKind::Equal)
                    .or_else(|_| self.peek_symbol(TokenKind::Assign))
                    .ok(),
                '!' => self
                    .peek_symbol(TokenKind::NotEqual)
                    .or_else(|_| self.peek_symbol(TokenKind::Bang))
                    .ok(),
                '(' => self.peek_symbol(TokenKind::LParen).ok(),
                ')' => self.peek_symbol(TokenKind::RParen).ok(),
                '[' => self.peek_symbol(TokenKind::LBracket).ok(),
                ']' => self.peek_symbol(TokenKind::RBracket).ok(),
                '{' => self.peek_symbol(TokenKind::LBrace).ok(),
                '}' => self.peek_symbol(TokenKind::RBrace).ok(),
                '.' => self
                    .peek_symbol(TokenKind::Ellipsis)
                    .or_else(|_| self.peek_symbol(TokenKind::Dot))
                    .ok(),
                '?' => self.peek_symbol(TokenKind::Question).ok(),
                ',' => self.peek_symbol(TokenKind::Comma).ok(),
                ':' => self.peek_symbol(TokenKind::Colon).ok(),
                '#' => self.peek_symbol(TokenKind::Hash).ok(),
                '|' => self.peek_symbol(TokenKind::Pipe).ok(),
                '~' => self.peek_symbol(TokenKind::Terminator).ok(),
                'a'..='z' | 'A'..='Z' => Some(self.peek_ident()),
                '0'..='9' => Some(self.peek_int()),
                '"' => self.peek_string().ok(),
                '\'' => self.peek_char_lit().ok(),
                _ => Some(self.unknown_token_error()),
            };
        }
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
