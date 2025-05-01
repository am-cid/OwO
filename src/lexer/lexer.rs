use crate::{
    debug_only,
    errors::lex_errors::{
        EmptyChar, InvalidEscapeChar, LexerError, UnclosedChar, UnclosedString, UnexpectedChar,
        UnknownChar,
    },
    lexer::token::{Offset, Position, Token, TokenKind},
};
use multipeek::{multipeek, MultiPeek};
use std::{fmt::Debug, str::CharIndices};

#[derive(Debug)]
pub struct Lexer {
    pub source: String,
    pub tokens: Vec<Token>,
    pub errors: Vec<LexerError>,
    pub pos: usize,
    pub line_starts: Vec<usize>,
}
impl Lexer {
    pub fn new(source: String, start: usize) -> Self {
        let mut l = Lexer {
            source,
            tokens: vec![],
            errors: vec![],
            pos: start,
            line_starts: vec![],
        };
        let LexerResult {
            tokens,
            errors,
            line_starts,
        } = l.tokenize(&l.source, 0);
        l.tokens = tokens;
        l.errors = errors;
        l.line_starts = line_starts;
        l
    }

    // TOP-LEVEL LEXER {{{

    fn tokenize(&self, source: &str, start: usize) -> LexerResult {
        let mut chars = multipeek(source.char_indices());
        let mut tokens = vec![];
        let mut errors = vec![];
        let mut line_starts = match start {
            0 => vec![0],
            _ => vec![],
        };
        while let Some(&(i, ch)) = chars.peek() {
            match self.lex_general(ch, start, i, &mut chars, &mut line_starts) {
                Ok(tok) => tokens.push(tok),
                Err(err) => errors.push(err),
            }
        }
        LexerResult {
            tokens,
            errors,
            line_starts,
        }
    }
    /// tokenizes the next token in the source code if any and stores it in
    /// [Lexer::tokens]
    fn lex_general(
        &self,
        ch: char,
        base_start: usize,
        start: usize,
        chars: &mut MultiPeek<CharIndices<'_>>,
        line_starts: &mut Vec<usize>,
    ) -> Result<Token, LexerError> {
        match ch {
            c if c.is_ascii_alphabetic() => self.lex_id_or_word(base_start, chars),
            c if c.is_ascii_digit() => self.lex_num(base_start, chars),
            '"' => self.lex_string(base_start, chars),
            '\'' => self.lex_char(base_start, chars),
            '\n' => {
                line_starts.push(base_start + start);
                let res = self.lex_symbol(TokenKind::Newline, base_start, chars);
                res
            }
            '\r' => self.lex_symbol(TokenKind::CarriageReturn, base_start, chars),
            '\t' => self.lex_symbol(TokenKind::Tab, base_start, chars),
            ' ' => self.lex_symbol(TokenKind::Whitespace, base_start, chars),
            '(' => self.lex_symbol(TokenKind::LParen, base_start, chars),
            ')' => self.lex_symbol(TokenKind::RParen, base_start, chars),
            '[' => self.lex_symbol(TokenKind::LBracket, base_start, chars),
            ']' => self.lex_symbol(TokenKind::RBracket, base_start, chars),
            '{' => self.lex_symbol(TokenKind::LBrace, base_start, chars),
            '}' => self.lex_symbol(TokenKind::RBrace, base_start, chars),
            '?' => self.lex_symbol(TokenKind::Question, base_start, chars),
            ',' => self.lex_symbol(TokenKind::Comma, base_start, chars),
            ':' => self.lex_symbol(TokenKind::Colon, base_start, chars),
            '#' => self.lex_symbol(TokenKind::Hash, base_start, chars),
            '|' => self.lex_symbol(TokenKind::Pipe, base_start, chars),
            '~' => self.lex_symbol(TokenKind::Terminator, base_start, chars),
            '+' => self
                .lex_symbol(TokenKind::PlusEqual, base_start, chars)
                .or_else(|_| self.lex_symbol(TokenKind::Plus, base_start, chars)),
            '-' => self
                .lex_symbol(TokenKind::DashEqual, base_start, chars)
                .or_else(|_| self.lex_symbol(TokenKind::Dash, base_start, chars)),
            '*' => self
                .lex_symbol(TokenKind::MultiplyEqual, base_start, chars)
                .or_else(|_| self.lex_symbol(TokenKind::Multiply, base_start, chars)),
            '/' => self
                .lex_symbol(TokenKind::DivideEqual, base_start, chars)
                .or_else(|_| self.lex_symbol(TokenKind::Divide, base_start, chars)),
            '%' => self
                .lex_symbol(TokenKind::ModuloEqual, base_start, chars)
                .or_else(|_| self.lex_symbol(TokenKind::Modulo, base_start, chars)),
            '<' => self
                .lex_symbol(TokenKind::LessEqual, base_start, chars)
                .or_else(|_| self.lex_symbol(TokenKind::LessThan, base_start, chars)),
            '>' => self
                .lex_comment(base_start, chars)
                .or_else(|_| self.lex_symbol(TokenKind::GreaterEqual, base_start, chars))
                .or_else(|_| self.lex_symbol(TokenKind::GreaterThan, base_start, chars)),
            '=' => self
                .lex_symbol(TokenKind::Equal, base_start, chars)
                .or_else(|_| self.lex_symbol(TokenKind::Assign, base_start, chars)),
            '!' => self
                .lex_symbol(TokenKind::NotEqual, base_start, chars)
                .or_else(|_| self.lex_symbol(TokenKind::Bang, base_start, chars)),
            '.' => self
                .lex_symbol(TokenKind::Ellipsis, base_start, chars)
                .or_else(|_| self.lex_symbol(TokenKind::Dot, base_start, chars)),
            ch => {
                chars.next();
                Err(self.unknown_char_error(ch))
            }
        }
    }

    // }}}

    // LEXERS {{{

    /// starts with the beginning of a reserved word/symbol
    /// ends with the delimiter if [Ok]
    /// ends without moving cursor if [Err]
    fn lex_symbol(
        &self,
        expected: TokenKind,
        base_start: usize,
        chars: &mut MultiPeek<CharIndices<'_>>,
    ) -> Result<Token, LexerError> {
        let start = if let Some(&(i, ch)) = chars.peek() {
            base_start + i
        } else {
            return Err(self.unexpected_char_error(
                chars.peek().map(|&(_, ch)| ch).unwrap_or_default(),
                vec![
                    '\n', '\r', '\t', ' ', '(', ')', '[', ']', '{', '}', '?', ',', ':', '#', '|',
                    '~', '+', '-', '*', '/', '%', '<', '>', '=', '!', '.',
                ],
            ));
        };
        let expected_str = expected.to_str();
        let expected_len = expected_str.len();
        let actual = chars
            .clone()
            .take(expected_len)
            .map(|(_, ch)| ch)
            .collect::<String>();
        if expected_str != actual {
            return Err(LexerError::TryAgain);
        }
        let _ = chars.advance_by(expected_len);
        Ok(Token::new(
            expected,
            Offset::new(start, start + expected_len),
        ))
    }
    /// starts with the beginning of an identifier/Type in peek
    /// ends with the delimiter in peek if [Ok]
    /// never returns [Err]
    fn lex_id_or_word(
        &self,
        base_start: usize,
        chars: &mut MultiPeek<CharIndices<'_>>,
    ) -> Result<Token, LexerError> {
        let mut res = String::new();
        let (start, mut end) = if let Some(&(i, ch)) = chars.peek()
            && ch.is_ascii_alphabetic()
        {
            chars.next();
            res.push(ch);
            let start = base_start + i;
            (start, start + ch.len_utf8())
        } else {
            return Err(self.unexpected_char_error(
                chars.peek().map(|&(_, ch)| ch).unwrap_or_default(),
                ('a'..'z').chain('A'..'Z').collect(),
            ));
        };
        while let Some(&(i, ch)) = chars.peek()
            && ch.is_lexer_identifier()
        {
            chars.next();
            res.push(ch);
            end = base_start + i + ch.len_utf8();
        }
        Ok(Token::from_str(&res, Offset::new(start, end)).expect(
            format!(
                r#"lex_id_or_word: {:?} was tokenized beforehand and should not fail"#,
                &res,
            )
            .as_str(),
        ))
    }
    /// starts with the beginning of a number in peek
    /// ends with the delimiter in peek if [Ok]
    /// never returns [Err]
    fn lex_num(
        &self,
        base_start: usize,
        chars: &mut MultiPeek<CharIndices<'_>>,
    ) -> Result<Token, LexerError> {
        let (start, mut end) = if let Some(&(i, ch)) = chars.peek()
            && ch.is_ascii_digit()
        {
            chars.next();
            let start = base_start + i;
            (start, start + ch.len_utf8())
        } else {
            return Err(self.unexpected_char_error(
                chars.peek().map(|&(_, ch)| ch).unwrap_or_default(),
                ('0'..'9').collect(),
            ));
        };
        while let Some((i, ch)) = chars.peek()
            && ch.is_lexer_digit()
        {
            end = base_start + i + ch.len_utf8();
            chars.next();
        }
        let kind = if let Some(&(_, ch)) = chars.peek()
            && ch == '.'
        {
            if let Some(&(_, ch)) = chars.peek_nth(1)
                && ch.is_lexer_digit()
            {
                chars.next(); // consume .
                while let Some((_, ch)) = chars.peek()
                    && ch.is_lexer_digit()
                {
                    end = chars
                        .next()
                        .map(|(i, ch)| base_start + i + ch.len_utf8())
                        .expect("Peeked beforehand, should exist")
                }
                TokenKind::FloatLiteral
            } else {
                TokenKind::IntLiteral
            }
        } else {
            TokenKind::IntLiteral
        };
        Ok(Token::new(kind, Offset::new(start, end)))
    }
    /// starts with " in peek
    /// ends with the delimiter if [Ok]
    /// ends with `\r`, `\n`, or `\0` that left string unclosed if [Err]
    fn lex_string(
        &self,
        base_start: usize,
        chars: &mut MultiPeek<CharIndices<'_>>,
    ) -> Result<Token, LexerError> {
        let start = if let Some(&(i, ch)) = chars.peek()
            && ch == '"'
        {
            chars.next();
            base_start + i
        } else {
            return Err(self.unexpected_char_error(
                chars.peek().map(|&(_, ch)| ch).unwrap_or_default(),
                vec!['"'],
            ));
        };
        while let Some(&(i, ch)) = chars.peek()
            && ch != '"'
        {
            match ch {
                '\\' => {
                    let (i, ch) = chars.next().expect("Peeked beforehand, should exist");
                    match chars.peek() {
                        Some(&(i, ch)) => match ch {
                            ch if ['n', 'r', 't', '\\', '0', '"'].contains(&ch) => {
                                chars.next();
                            }
                            ch if ['\r', '\n', '\0'].contains(&ch) => {
                                return Err(self.unclosed_string_error(ch, i + ch.len_utf8()));
                            }
                            _ => {
                                return Err(self.invalid_escape_char(ch, i + ch.len_utf8()));
                            }
                        },
                        None => {
                            return Err(self.unclosed_string_error('\0', i + ch.len_utf8()));
                        }
                    }
                }
                ch if ['\r', '\n', '\0'].contains(&ch) => {
                    return Err(self.unclosed_string_error(ch, i + ch.len_utf8()));
                }
                _ => {
                    chars.next();
                }
            }
        }
        // consume closing "
        match chars.peek() {
            Some((_, '"')) => {
                let (closing_idx, closing) = chars.next().expect("Peeked beforehand, must exist");
                Ok(Token::new(
                    TokenKind::StringLiteral,
                    Offset::new(start, base_start + closing_idx + closing.len_utf8()),
                ))
            }
            None => Err(self.unclosed_string_error('\0', 0)),
            _ => unreachable!(
                r#"The only thing that can exit the string loop is a closing ", or never entering it in the first place, aka EOF."#
            ),
        }
    }
    /// starts with a single quote: `'`
    /// ends with the delimiter if [Ok]
    /// ends with char that left char unclosed if [Err]
    fn lex_char(
        &self,
        base_start: usize,
        chars: &mut MultiPeek<CharIndices<'_>>,
    ) -> Result<Token, LexerError> {
        let (start, end) = if let Some(&(i, ch)) = chars.peek()
            && ch == '\''
        {
            chars.next();
            let start = base_start + i;
            (start, start + ch.len_utf8())
        } else {
            return Err(self.unexpected_char_error(
                chars.peek().map(|&(_, ch)| ch).unwrap_or_default(),
                vec!['\''],
            ));
        };
        let end = match chars.next() {
            Some((i, ch @ '\\')) => match chars.peek() {
                Some((i, ch @ ('n' | 'r' | 't' | '\\' | '0'))) => start + i + ch.len_utf8(),
                Some(&(i, ch)) => {
                    return Err(self.invalid_escape_char(ch, i + ch.len_utf8()));
                }
                None => {
                    return Err(self.unclosed_char_error('\0', i + ch.len_utf8()));
                }
            },
            Some((i, ch @ ('\r' | '\n' | '\0'))) => {
                return Err(self.unclosed_char_error(ch, i + ch.len_utf8()));
            }
            Some((_, '\'')) => {
                return Err(self.empty_char_error());
            }
            Some((i, ch)) => start + i + ch.len_utf8(),
            None => {
                return Err(self.unclosed_char_error('\0', end - start));
            }
        };
        // consume closing '
        let (closing_idx, closing) = match chars.peek() {
            Some(&(i, ch @ '\'')) => {
                chars.next();
                (i, ch)
            }
            Some(&(i, ch)) => {
                return Err(self.unclosed_char_error(ch, i + ch.len_utf8()));
            }
            None => {
                return Err(self.unclosed_char_error('\0', end - start));
            }
        };
        Ok(Token::new(
            TokenKind::CharLiteral,
            Offset::new(start, base_start + closing_idx + closing.len_utf8()),
        ))
    }
    /// starts with greater than: `>`
    /// ends with `\r`, `\n`, or `\0` if [Ok]
    /// ends without moving cursor if [Err]
    fn lex_comment(
        &self,
        base_start: usize,
        chars: &mut MultiPeek<CharIndices<'_>>,
    ) -> Result<Token, LexerError> {
        let start = if let Some(&(i, ch)) = chars.peek()
            && ch == '>'
        {
            base_start + i
        } else {
            return Err(self.unexpected_char_error(
                chars.peek().map(|&(_, ch)| ch).unwrap_or_default(),
                vec!['>'],
            ));
        };
        let actual_str = chars.clone().take(3).map(|(_, ch)| ch).collect::<String>();
        if actual_str == ">_<" {
            let _ = chars
                .advance_by(3)
                .expect("Checked beforehand that there are 3 chars after (\">_<\")");
            let mut end = start + 3;
            while let Some((_, ch)) = chars.peek()
                && !['\r', '\n', '\0'].contains(ch)
            {
                let (i, ch) = chars.next().expect("Peeked beforehand, should exist");
                end = base_start + i + ch.len_utf8();
            }
            Ok(Token::new(TokenKind::Comment, Offset::new(start, end)))
        } else {
            Err(LexerError::TryAgain)
        }
    }

    // }}}

    // HELPER METHODS {{{

    /// Create position information (line, column) and gets current line text
    /// for error messages.
    ///
    /// Uses line texts as it's being built during lexing so this should only be
    /// called **DURING** lexing
    fn error_context(&self) -> (String, usize, usize) {
        let line = self.line_starts.len().saturating_sub(1);
        let line_text = self.source.lines().nth(line).unwrap_or_default().into();
        let col = self
            .pos
            .saturating_sub(*self.line_starts.iter().last().unwrap_or(&0))
            .saturating_sub(match line {
                0 => 0,
                _ => 1,
            });
        (line_text, line, col)
    }

    // }}}

    // ERROR METHODS {{{

    fn unexpected_char_error(&self, actual: char, expected: Vec<char>) -> LexerError {
        let (line_text, line, col) = self.error_context();
        LexerError::Unexpected(UnexpectedChar {
            actual,
            expected,
            line_text,
            line,
            col,
        })
    }

    fn unknown_char_error(&self, ch: char) -> LexerError {
        let (line_text, line, col) = self.error_context();
        LexerError::Unknown(UnknownChar {
            ch,
            line_text,
            line,
            col,
        })
    }

    fn unclosed_string_error(&self, actual: char, length: usize) -> LexerError {
        let (line_text, line, col) = self.error_context();
        LexerError::UnclosedString(UnclosedString {
            actual,
            line_text,
            line,
            col,
            length: length + 1,
        })
    }

    fn invalid_escape_char(&self, actual: char, length: usize) -> LexerError {
        let (line_text, line, col) = self.error_context();
        LexerError::InvalidEscapeChar(InvalidEscapeChar {
            actual,
            line_text,
            line,
            col,
        })
    }

    fn unclosed_char_error(&self, actual: char, length: usize) -> LexerError {
        let (line_text, line, col) = self.error_context();
        LexerError::UnclosedChar(UnclosedChar {
            actual,
            line_text,
            line,
            col,
            length: length + 1,
        })
    }

    fn empty_char_error(&self) -> LexerError {
        let (line_text, line, col) = self.error_context();
        LexerError::EmptyChar(EmptyChar {
            line_text,
            line,
            col,
        })
    }

    // }}}

    // ETC {{{

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
                        "| {: ^15} | {: ^15} | {: ^15} | {: ^13} |",
                        token.kind.to_string(),
                        format!("{:?}", {
                            let printed = token.source_str(&self.source);
                            if printed.len() > 13 {
                                printed[..5].to_string() + "..." + &printed[printed.len() - 5..]
                            } else {
                                printed.to_string()
                            }
                        }),
                        format!(
                            "{: <5}-{: >5}",
                            format!("{}:{}", token.buffer_line(&self.line_starts), token.buffer_col(&self.line_starts)),
                            format!("{}:{}", token.buffer_line_end(&self.line_starts), token.buffer_col_end(&self.line_starts)),
                        ),
                        format!(
                            "{: <2}-{: >2}",
                            token.offset.start,
                            token.offset.end - 1,
                        ),
                    ))
                    .collect::<Vec<String>>()
                    .join("\n"),
            )
        )
    }

    // }}}

        );
    }
    /// char that should've been a closing `'` should be in `self.curr_char()`
}

// LEXER METADATA {{{

/// Holds tokens, errors, and line starts of a lexer after tokenizing
#[derive(Debug)]
pub struct LexerResult {
    tokens: Vec<Token>,
    errors: Vec<LexerError>,
    line_starts: Vec<usize>,
}
impl LexerResult {
    pub fn new(tokens: Vec<Token>, errors: Vec<LexerError>, line_starts: Vec<usize>) -> Self {
        Self {
            tokens,
            errors,
            line_starts,
        }
    }
}

/// List of offsets containing the line starts of a source
pub struct LineStarts {}
impl LineStarts {
    pub fn new(source: &str) -> Vec<usize> {
        let mut res = vec![0];
        res.extend(
            source
                .char_indices()
                .filter(|&(_, ch)| ch == '\n')
                .map(|(i, _)| i),
        );
        res
    }
}

// }}}

// LEXER HELPERS {{{

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
// }}}
