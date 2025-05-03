use multipeek::{multipeek, MultiPeek};
use std::{fmt::Debug, str::CharIndices};

use crate::{
    debug_only,
    errors::lex_errors::{
        EmptyChar, InvalidEscapeChar, LexerError, LexerErrorKind, UnclosedChar, UnclosedString,
        UnexpectedChar, UnknownChar,
    },
    lexer::token::{Offset, Position, Token, TokenKind},
};

#[derive(Debug)]
pub struct Lexer {
    pub source: String,
    pub tokens: Vec<Token>,
    pub errors: Vec<LexerError>,
    pub line_starts: Vec<usize>,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        let mut l = Lexer {
            source,
            tokens: vec![],
            errors: vec![],
            line_starts: vec![],
        };
        let LexerResult {
            tokens,
            errors,
            line_starts,
        } = l.lex(&l.source, 0);
        l.tokens = tokens;
        l.errors = errors;
        l.line_starts = line_starts;
        l
    }

    // TOP-LEVEL LEXER {{{

    fn lex(&self, source: &str, start: usize) -> LexerResult {
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
            c if c.is_ascii_alphabetic() => self.lex_id_or_word(base_start, chars, line_starts),
            c if c.is_ascii_digit() => self.lex_num(base_start, chars, line_starts),
            '"' => self.lex_string(base_start, chars, line_starts),
            '\'' => self.lex_char(base_start, chars, line_starts),
            '\n' => {
                line_starts.push(base_start + start);
                self.lex_symbol(TokenKind::Newline, base_start, chars, line_starts)
            }
            '\r' => self.lex_symbol(TokenKind::CarriageReturn, base_start, chars, line_starts),
            '\t' => self.lex_symbol(TokenKind::Tab, base_start, chars, line_starts),
            ' ' => self.lex_symbol(TokenKind::Whitespace, base_start, chars, line_starts),
            '(' => self.lex_symbol(TokenKind::LParen, base_start, chars, line_starts),
            ')' => self.lex_symbol(TokenKind::RParen, base_start, chars, line_starts),
            '[' => self.lex_symbol(TokenKind::LBracket, base_start, chars, line_starts),
            ']' => self.lex_symbol(TokenKind::RBracket, base_start, chars, line_starts),
            '{' => self.lex_symbol(TokenKind::LBrace, base_start, chars, line_starts),
            '}' => self.lex_symbol(TokenKind::RBrace, base_start, chars, line_starts),
            '?' => self.lex_symbol(TokenKind::Question, base_start, chars, line_starts),
            ',' => self.lex_symbol(TokenKind::Comma, base_start, chars, line_starts),
            ':' => self.lex_symbol(TokenKind::Colon, base_start, chars, line_starts),
            '#' => self.lex_symbol(TokenKind::Hash, base_start, chars, line_starts),
            '|' => self.lex_symbol(TokenKind::Pipe, base_start, chars, line_starts),
            '~' => self.lex_symbol(TokenKind::Terminator, base_start, chars, line_starts),
            '+' => self
                .lex_symbol(TokenKind::PlusEqual, base_start, chars, line_starts)
                .or_else(|_| self.lex_symbol(TokenKind::Plus, base_start, chars, line_starts)),
            '-' => self
                .lex_symbol(TokenKind::DashEqual, base_start, chars, line_starts)
                .or_else(|_| self.lex_symbol(TokenKind::Dash, base_start, chars, line_starts)),
            '*' => self
                .lex_symbol(TokenKind::MultiplyEqual, base_start, chars, line_starts)
                .or_else(|_| self.lex_symbol(TokenKind::Multiply, base_start, chars, line_starts)),
            '/' => self
                .lex_symbol(TokenKind::DivideEqual, base_start, chars, line_starts)
                .or_else(|_| self.lex_symbol(TokenKind::Divide, base_start, chars, line_starts)),
            '%' => self
                .lex_symbol(TokenKind::ModuloEqual, base_start, chars, line_starts)
                .or_else(|_| self.lex_symbol(TokenKind::Modulo, base_start, chars, line_starts)),
            '<' => self
                .lex_symbol(TokenKind::LessEqual, base_start, chars, line_starts)
                .or_else(|_| self.lex_symbol(TokenKind::LessThan, base_start, chars, line_starts)),
            '>' => self
                .lex_comment(base_start, chars, line_starts)
                .or_else(|_| {
                    self.lex_symbol(TokenKind::GreaterEqual, base_start, chars, line_starts)
                })
                .or_else(|_| {
                    self.lex_symbol(TokenKind::GreaterThan, base_start, chars, line_starts)
                }),
            '=' => self
                .lex_symbol(TokenKind::Equal, base_start, chars, line_starts)
                .or_else(|_| self.lex_symbol(TokenKind::Assign, base_start, chars, line_starts)),
            '!' => self
                .lex_symbol(TokenKind::NotEqual, base_start, chars, line_starts)
                .or_else(|_| self.lex_symbol(TokenKind::Bang, base_start, chars, line_starts)),
            '.' => self
                .lex_symbol(TokenKind::Ellipsis, base_start, chars, line_starts)
                .or_else(|_| self.lex_symbol(TokenKind::Dot, base_start, chars, line_starts)),
            ch => {
                chars.next();
                let start = base_start + start;
                Err(self.unknown_char_error(ch, line_starts, start, start + ch.len_utf8()))
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
        line_starts: &Vec<usize>,
    ) -> Result<Token, LexerError> {
        let start = if let Some(&(i, _)) = chars.peek() {
            base_start + i
        } else {
            return Err(self.unexpected_char_error(
                chars.peek().map(|&(_, ch)| ch).unwrap_or_default(),
                vec![
                    '\n', '\r', '\t', ' ', '(', ')', '[', ']', '{', '}', '?', ',', ':', '#', '|',
                    '~', '+', '-', '*', '/', '%', '<', '>', '=', '!', '.',
                ],
                line_starts,
                base_start,
                base_start,
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
            return Err(LexerError {
                kind: LexerErrorKind::TryAgain,
                offset: Offset::zero(),
                line_offset: Offset::zero(),
                buffer_line: 0,
                buffer_col: 0,
            });
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
        line_starts: &Vec<usize>,
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
                line_starts,
                base_start,
                base_start,
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
        line_starts: &Vec<usize>,
    ) -> Result<Token, LexerError> {
        let (start, mut end) = if let Some(&(i, ch)) = chars.peek()
            && ch.is_ascii_digit()
        {
            chars.next();
            let start = base_start + i;
            (start, start + ch.len_utf8())
        } else {
            let ch = chars.peek().map(|&(_, ch)| ch).unwrap_or_default();
            return Err(self.unexpected_char_error(
                ch,
                ('0'..'9').collect(),
                line_starts,
                base_start,
                base_start + ch.len_utf8(),
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
        line_starts: &Vec<usize>,
    ) -> Result<Token, LexerError> {
        let start = if let Some(&(i, ch)) = chars.peek()
            && ch == '"'
        {
            chars.next();
            base_start + i
        } else {
            let ch = chars.peek().map(|&(_, ch)| ch).unwrap_or_default();
            return Err(self.unexpected_char_error(
                ch,
                vec!['"'],
                line_starts,
                base_start,
                base_start + ch.len_utf8(),
            ));
        };
        while let Some(&(i, ch)) = chars.peek()
            && ch != '"'
        {
            match ch {
                '\\' => {
                    let (i, _) = chars.next().expect("Peeked beforehand, should exist");
                    match chars.peek() {
                        Some(&(i, ch)) => match ch {
                            ch if ['n', 'r', 't', '\\', '0', '"'].contains(&ch) => {
                                chars.next();
                            }
                            ch if ['\r', '\n', '\0'].contains(&ch) => {
                                return Err(self.unclosed_string_error(
                                    ch,
                                    line_starts,
                                    start,
                                    base_start + i,
                                ));
                            }
                            _ => {
                                return Err(self.invalid_escape_char(
                                    ch,
                                    line_starts,
                                    start,
                                    base_start + i,
                                ));
                            }
                        },
                        None => {
                            return Err(self.unclosed_string_error(
                                '\0',
                                line_starts,
                                start,
                                base_start + i,
                            ));
                        }
                    }
                }
                ch if ['\r', '\n', '\0'].contains(&ch) => {
                    return Err(self.unclosed_string_error(ch, line_starts, start, base_start + i));
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
            None => Err(self.unclosed_string_error('\0', line_starts, start, self.source.len())),
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
        line_starts: &Vec<usize>,
    ) -> Result<Token, LexerError> {
        let start = if let Some(&(i, ch)) = chars.peek()
            && ch == '\''
        {
            chars.next();
            base_start + i
        } else {
            let ch = chars.peek().map(|&(_, ch)| ch).unwrap_or_default();
            return Err(self.unexpected_char_error(
                ch,
                vec!['\''],
                line_starts,
                base_start,
                base_start + ch.len_utf8(),
            ));
        };
        match chars.next() {
            Some((i, '\\')) => match chars.peek() {
                Some((_, 'n' | 'r' | 't' | '\\' | '0')) => {
                    let (i, ch) = chars.next().expect("Peeked beforehand, should exist");
                    start + i + ch.len_utf8()
                }
                Some(&(i, ch)) => {
                    return Err(self.invalid_escape_char(ch, line_starts, start, base_start + i));
                }
                None => {
                    return Err(self.unclosed_char_error('\0', line_starts, start, base_start + i));
                }
            },
            Some((i, ch @ ('\r' | '\n' | '\0'))) => {
                return Err(self.unclosed_char_error(
                    ch,
                    line_starts,
                    start,
                    base_start + i + ch.len_utf8(),
                ));
            }
            Some((i, '\'')) => {
                return Err(self.empty_char_error(line_starts, start, base_start + i));
            }
            Some((i, ch)) => start + i + ch.len_utf8(),
            None => {
                return Err(self.unclosed_char_error('\0', line_starts, start, self.source.len()));
            }
        };
        // consume closing '
        let (closing_idx, closing) = match chars.peek() {
            Some(&(i, ch @ '\'')) => {
                chars.next();
                (i, ch)
            }
            Some(&(i, ch)) => {
                return Err(self.unclosed_char_error(
                    ch,
                    line_starts,
                    start,
                    base_start + i + ch.len_utf8(),
                ));
            }
            None => {
                return Err(self.unclosed_char_error('\0', line_starts, start, self.source.len()));
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
        line_starts: &Vec<usize>,
    ) -> Result<Token, LexerError> {
        let start = if let Some(&(i, ch)) = chars.peek()
            && ch == '>'
        {
            base_start + i
        } else {
            let ch = chars.peek().map(|&(_, ch)| ch).unwrap_or_default();
            return Err(self.unexpected_char_error(
                ch,
                vec!['>'],
                line_starts,
                base_start,
                base_start + ch.len_utf8(),
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
            Err(LexerError {
                kind: LexerErrorKind::TryAgain,
                offset: Offset::zero(),
                line_offset: Offset::zero(),
                buffer_line: 0,
                buffer_col: 0,
            })
        }
    }

    // }}}

    // HELPER METHODS {{{

    /// Create position information (line, column) and gets current line offset
    /// for error messages.
    ///
    /// Uses line texts as it's being built during lexing so this should only be
    /// called **DURING** lexing
    fn error_context(&self, start: usize, line_starts: &Vec<usize>) -> (Offset, usize, usize) {
        let line = line_starts.len().saturating_sub(1);
        let line_range = self.source.line_range(line);
        let col = start
            .saturating_sub(*line_starts.iter().last().unwrap_or(&0))
            .saturating_sub(match line {
                0 => 0,
                _ => 1,
            });
        (Offset::new(line_range.0, line_range.1), line, col)
    }

    // }}}

    // ERROR METHODS {{{

    fn unexpected_char_error(
        &self,
        actual: char,
        expected: Vec<char>,
        line_starts: &Vec<usize>,
        start: usize,
        end: usize,
    ) -> LexerError {
        let (line_offset, line, col) = self.error_context(end, line_starts);
        LexerError {
            kind: LexerErrorKind::Unexpected(UnexpectedChar { actual, expected }),
            offset: Offset::new(start, end),
            line_offset,
            buffer_line: line + 1,
            buffer_col: col + 1,
        }
    }

    fn unknown_char_error(
        &self,
        ch: char,
        line_starts: &Vec<usize>,
        start: usize,
        end: usize,
    ) -> LexerError {
        let (line_offset, line, col) = self.error_context(end, line_starts);
        LexerError {
            kind: LexerErrorKind::Unknown(UnknownChar { ch }),
            offset: Offset::new(start, end),
            line_offset,
            buffer_line: line + 1,
            buffer_col: col,
        }
    }

    fn unclosed_string_error(
        &self,
        actual: char,
        line_starts: &Vec<usize>,
        start: usize,
        end: usize,
    ) -> LexerError {
        let (line_offset, line, col) = self.error_context(end, line_starts);
        LexerError {
            kind: LexerErrorKind::UnclosedString(UnclosedString {
                actual,
                length: end - start + 1,
            }),
            offset: Offset::new(start, end),
            line_offset,
            buffer_line: line + 1,
            buffer_col: col + 1,
        }
    }

    fn invalid_escape_char(
        &self,
        actual: char,
        line_starts: &Vec<usize>,
        start: usize,
        end: usize,
    ) -> LexerError {
        let (line_offset, line, col) = self.error_context(end, line_starts);
        LexerError {
            kind: LexerErrorKind::InvalidEscapeChar(InvalidEscapeChar { actual }),
            offset: Offset::new(start, end),
            line_offset,
            buffer_line: line + 1,
            buffer_col: col + 1,
        }
    }

    fn unclosed_char_error(
        &self,
        actual: char,
        line_starts: &Vec<usize>,
        start: usize,
        end: usize,
    ) -> LexerError {
        let (line_offset, line, col) = self.error_context(end, line_starts);
        LexerError {
            kind: LexerErrorKind::UnclosedChar(UnclosedChar {
                actual,
                length: end - start,
            }),
            offset: Offset::new(start, end),
            line_offset,
            buffer_line: line + 1,
            buffer_col: col + 1,
        }
    }

    fn empty_char_error(&self, line_starts: &Vec<usize>, start: usize, end: usize) -> LexerError {
        let (line_offset, line, col) = self.error_context(end, line_starts);
        LexerError {
            kind: LexerErrorKind::EmptyChar(EmptyChar {}),
            offset: Offset::new(start, end),
            line_offset,
            buffer_line: line + 1,
            buffer_col: col + 1,
        }
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
}

// LEXER METADATA {{{

/// Holds tokens, errors, and line starts of a lexer after tokenizing
#[derive(Debug)]
pub struct LexerResult {
    tokens: Vec<Token>,
    errors: Vec<LexerError>,
    line_starts: Vec<usize>,
}

// }}}

// LEXER HELPERS {{{

/// helper trait for allowing underscores to be part of an identifier/number
/// when lexing.
pub trait LexerCharExt {
    fn is_lexer_identifier(&self) -> bool;
    fn is_lexer_digit(&self) -> bool;
}
impl LexerCharExt for char {
    /// checks if char is alphanumeric or _
    fn is_lexer_identifier(&self) -> bool {
        *self == '_' || self.is_ascii_alphanumeric()
    }
    /// checks if char is digit or _
    fn is_lexer_digit(&self) -> bool {
        *self == '_' || self.is_ascii_digit()
    }
}

/// helper trait for extending the source of the lexer.
pub trait LexerSourceExt {
    fn line_range(&self, line_number: usize) -> (usize, usize);
}
impl LexerSourceExt for str {
    /// gets the range of the given line number (0-indexed)
    fn line_range(&self, line_number: usize) -> (usize, usize) {
        let mut start = 0;
        let mut line_count = 0;
        let mut chars = self.char_indices().peekable();
        while let Some((i, c)) = chars.next() {
            if c == '\n' {
                if line_count == line_number {
                    return (start, i + 1);
                }
                line_count += 1;
                start = i + 1;
            }
        }
        (0, self.len())
    }
}

// }}}
