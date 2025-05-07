use std::fmt;
use std::hash::Hash;
use std::ops::Range;

// TOKEN KIND {{{

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum TokenKind {
    #[default]
    EOF,

    // ids
    Identifier,
    Type,

    // dtypes
    Chan,   // int
    Kun,    // float
    Senpai, // string
    Kouhai, // char
    San,    // null
    Sama,   // bool
    Dono,   // any

    // arith ops
    Plus,
    Dash,
    Multiply,
    Divide,
    Modulo,

    // shorthand arith assign ops
    PlusEqual,
    DashEqual,
    MultiplyEqual,
    DivideEqual,
    ModuloEqual,

    // rel ops
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,

    // boolean ops
    And,
    Or,
    Not,

    // equality ops
    Equal,
    NotEqual,

    // assignment
    Assign,

    // enclosures
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    // etc
    Dot,      // method/property access
    Question, // optional
    Bang,     // const
    Ellipsis, // variadic parameter
    Comma,
    Colon,
    Hash,
    Pipe,
    Terminator,

    // whitespace
    Whitespace,
    Tab,
    Newline,
    CarriageReturn,

    /// keywords
    Hi, // declaration
    Main,     // main func keyword
    Fun,      // function
    Group,    // class
    Contract, // interface
    Wetuwn,   // return

    // control flow
    Iwf,
    Ewse,
    Ewif,
    Mash,
    Default,

    // loop constructs
    Fow,
    Bweak,
    Continue,
    In, // create iterator over collection types only in for loops

    // literals
    IntLiteral,
    FloatLiteral,
    Fax, // True
    Cap, // False
    StringLiteral,
    CharLiteral,
    Nuww, // None

    // comments
    Comment,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EOF => write!(f, "EOF"),
            Self::Identifier => write!(f, "Identifier"),
            Self::Type => write!(f, "Type"),
            Self::Chan => write!(f, "Chan"),
            Self::Kun => write!(f, "Kun"),
            Self::Senpai => write!(f, "Senpai"),
            Self::Kouhai => write!(f, "Kouhai"),
            Self::San => write!(f, "San"),
            Self::Sama => write!(f, "Sama"),
            Self::Dono => write!(f, "Dono"),
            Self::Plus => write!(f, "Plus"),
            Self::Dash => write!(f, "Dash"),
            Self::Multiply => write!(f, "Multiply"),
            Self::Divide => write!(f, "Divide"),
            Self::Modulo => write!(f, "Modulo"),
            Self::PlusEqual => write!(f, "PlusEqual"),
            Self::DashEqual => write!(f, "DashEqual"),
            Self::MultiplyEqual => write!(f, "MultiplyEqual"),
            Self::DivideEqual => write!(f, "DivideEqual"),
            Self::ModuloEqual => write!(f, "ModuloEqual"),
            Self::LessThan => write!(f, "LessThan"),
            Self::LessEqual => write!(f, "LessEqual"),
            Self::GreaterThan => write!(f, "GreaterThan"),
            Self::GreaterEqual => write!(f, "GreaterEqual"),
            Self::And => write!(f, "And"),
            Self::Or => write!(f, "Or"),
            Self::Not => write!(f, "Not"),
            Self::Equal => write!(f, "Equal"),
            Self::NotEqual => write!(f, "NotEqual"),
            Self::Assign => write!(f, "Assign"),
            Self::LParen => write!(f, "LParen"),
            Self::RParen => write!(f, "RParen"),
            Self::LBracket => write!(f, "LBracket"),
            Self::RBracket => write!(f, "RBracket"),
            Self::LBrace => write!(f, "LBrace"),
            Self::RBrace => write!(f, "RBrace"),
            Self::Dot => write!(f, "Dot"),
            Self::Question => write!(f, "Question"),
            Self::Bang => write!(f, "Bang"),
            Self::Ellipsis => write!(f, "Ellipsis"),
            Self::Comma => write!(f, "Comma"),
            Self::Colon => write!(f, "Colon"),
            Self::Hash => write!(f, "Hash"),
            Self::Pipe => write!(f, "Pipe"),
            Self::Terminator => write!(f, "Terminator"),
            Self::Whitespace => write!(f, "Whitespace"),
            Self::Tab => write!(f, "Tab"),
            Self::Newline => write!(f, "Newline"),
            Self::CarriageReturn => write!(f, "Return"),
            Self::Hi => write!(f, "Hi"),
            Self::Main => write!(f, "Main"),
            Self::Fun => write!(f, "Fun"),
            Self::Group => write!(f, "Group"),
            Self::Contract => write!(f, "Contract"),
            Self::Wetuwn => write!(f, "Wetuwn"),
            Self::Iwf => write!(f, "Iwf"),
            Self::Ewse => write!(f, "Ewse"),
            Self::Ewif => write!(f, "Ewif"),
            Self::Mash => write!(f, "Mash"),
            Self::Default => write!(f, "Default"),
            Self::Fow => write!(f, "Fow"),
            Self::Bweak => write!(f, "Bweak"),
            Self::Continue => write!(f, "Continue"),
            Self::In => write!(f, "In"),
            Self::IntLiteral => write!(f, "IntLiteral"),
            Self::FloatLiteral => write!(f, "FloatLiteral"),
            Self::Fax => write!(f, "Fax"),
            Self::Cap => write!(f, "Cap"),
            Self::StringLiteral => write!(f, "StringLiteral"),
            Self::CharLiteral => write!(f, "CharLiteral"),
            Self::Nuww => write!(f, "Nuww"),
            Self::Comment => write!(f, "Comment"),
        }
    }
}

impl TokenKind {
    pub fn to_str(&self) -> &str {
        match self {
            // non-reserved words/symbols
            Self::Comment
            | Self::IntLiteral
            | Self::FloatLiteral
            | Self::StringLiteral
            | Self::Identifier
            | Self::Type
            | Self::CharLiteral
            | Self::EOF => "\0",
            // reserved words/symbols
            Self::Chan => "chan",
            Self::Kun => "kun",
            Self::Senpai => "senpai",
            Self::Kouhai => "kouhai",
            Self::San => "san",
            Self::Sama => "sama",
            Self::Dono => "dono",
            Self::Plus => "+",
            Self::Dash => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::Modulo => "%",
            Self::PlusEqual => "+=",
            Self::DashEqual => "-=",
            Self::MultiplyEqual => "*=",
            Self::DivideEqual => "/=",
            Self::ModuloEqual => "%=",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::LessEqual => "<=",
            Self::GreaterEqual => ">=",
            Self::And => "and",
            Self::Or => "or",
            Self::Not => "not",
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::Assign => "=",
            Self::LParen => "(",
            Self::RParen => ")",
            Self::LBracket => "[",
            Self::RBracket => "]",
            Self::LBrace => "{",
            Self::RBrace => "}",
            Self::Dot => ".",
            Self::Question => "?",
            Self::Bang => "!",
            Self::Ellipsis => "...",
            Self::Comma => ",",
            Self::Colon => ":",
            Self::Hash => "#",
            Self::Pipe => "|",
            Self::Terminator => "~",
            Self::Whitespace => " ",
            Self::Tab => "\t",
            Self::Newline => "\n",
            Self::CarriageReturn => "\r",
            Self::Hi => "hi",
            Self::Main => "main",
            Self::Fun => "fun",
            Self::Group => "gwoup",
            Self::Contract => "contwact",
            Self::Wetuwn => "wetuwn",
            Self::Iwf => "iwf",
            Self::Ewse => "ewse",
            Self::Ewif => "ewif",
            Self::Mash => "mash",
            Self::Default => "default",
            Self::Fow => "fow",
            Self::Continue => "continue",
            Self::Bweak => "bweak",
            Self::In => "in",
            Self::Fax => "fax",
            Self::Cap => "cap",
            Self::Nuww => "nuww",
        }
    }
    pub fn data_types() -> Vec<Self> {
        vec![
            Self::Type,
            Self::Chan,
            Self::Kun,
            Self::Senpai,
            Self::Kouhai,
            Self::San,
            Self::Sama,
            Self::Dono,
        ]
    }
    pub fn assign_ops() -> Vec<Self> {
        vec![
            Self::Assign,
            Self::PlusEqual,
            Self::DashEqual,
            Self::MultiplyEqual,
            Self::DivideEqual,
            Self::ModuloEqual,
        ]
    }
    pub fn math_ops() -> Vec<Self> {
        vec![
            Self::Plus,
            Self::Dash,
            Self::Multiply,
            Self::Divide,
            Self::Modulo,
        ]
    }
    pub fn is_type(&self) -> bool {
        matches!(
            self,
            Self::Type
                | Self::Chan
                | Self::Kun
                | Self::Senpai
                | Self::Kouhai
                | Self::San
                | Self::Sama
                | Self::Dono
        )
    }
}

// }}}

// TOKEN {{{

#[derive(Debug, Clone, Copy, Default)]
pub struct Token {
    pub kind: TokenKind,
    pub offset: Offset,
}

// // TODO: cannot implement Hash. Figure out how to hash tokens with text
// // information for Identifiers, Groups, Contracts, string literals, etc.
// // Do I even need this? Figure it out during evaluation.
// impl Hash for Token {
//     fn hash<H: Hasher>(&self, state: &mut H) {
//         self.kind.hash(state);
//         self.text.hash(state);
//     }
// }

impl<'src> Token {
    pub fn default() -> Self {
        Self {
            kind: TokenKind::EOF,
            offset: Offset::zero(),
        }
    }
    pub fn new(kind: TokenKind, offset: Offset) -> Self {
        Self { kind, offset }
    }

    /// Equality check for Tokens. Since [Token] does not store text info,
    /// a source is needed to check equality. This equality does not check
    /// position (offset, line, and column).
    pub fn eq(&self, other: &Token, source: &'src str) -> bool {
        self.kind == other.kind
            && match (self.kind, other.kind) {
                (
                    TokenKind::Identifier | TokenKind::Type,
                    TokenKind::Identifier | TokenKind::Type,
                ) => self.source_str(source) == other.source_str(source),
                _ => true,
            }
    }

    /// Like [Token::eq] but also checks equality of position (offset, line,
    /// and column).
    pub fn eq_all(&self, other: &Token, source: &'src str) -> bool {
        self.eq(other, source) && self.offset == other.offset
    }

    pub fn eq_dtype(&self, other: &Token, source: &'src str) -> bool {
        match (&self.kind, &other.kind) {
            (TokenKind::Dono, TokenKind::Dono)
            | (
                TokenKind::Type
                | TokenKind::Chan
                | TokenKind::Kun
                | TokenKind::Senpai
                | TokenKind::Kouhai
                | TokenKind::San
                | TokenKind::Sama,
                TokenKind::Dono,
            )
            | (
                TokenKind::Dono,
                TokenKind::Type
                | TokenKind::Chan
                | TokenKind::Kun
                | TokenKind::Senpai
                | TokenKind::Kouhai
                | TokenKind::San
                | TokenKind::Sama,
            ) => true,
            _ => self.eq(other, source),
        }
    }

    /// Attempts to create a token based on the str passed to it and returns a [Token]
    /// Returns default token with kind [TokenKind::EOF] as error if it cannot recognize the str
    /// # Examples
    /// ```
    /// let tok = Token::from_str("chan", Offset::new(0, 4));
    /// assert_eq!(
    ///     tok,
    ///     Ok(Token::new(
    ///         TokenKind::Chan,
    ///         Offset::new(0, 4),
    ///     )),
    /// );
    ///
    /// let unknown = Token::from_str("@", Offset::new(0, 1));
    /// assert_eq!(tok, Err(Token::default()));
    /// ```
    pub fn from_str(text: &str, offset: Offset) -> Result<Self, Self> {
        let kind = match text {
            "chan" => TokenKind::Chan,
            "kun" => TokenKind::Kun,
            "senpai" => TokenKind::Senpai,
            "kouhai" => TokenKind::Kouhai,
            "san" => TokenKind::San,
            "sama" => TokenKind::Sama,
            "dono" => TokenKind::Dono,
            "+" => TokenKind::Plus,
            "-" => TokenKind::Dash,
            "*" => TokenKind::Multiply,
            "/" => TokenKind::Divide,
            "%" => TokenKind::Modulo,
            "+=" => TokenKind::PlusEqual,
            "-=" => TokenKind::DashEqual,
            "*=" => TokenKind::MultiplyEqual,
            "/=" => TokenKind::DivideEqual,
            "%=" => TokenKind::ModuloEqual,
            "<" => TokenKind::LessThan,
            ">" => TokenKind::GreaterThan,
            "<=" => TokenKind::LessEqual,
            ">=" => TokenKind::GreaterEqual,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "not" => TokenKind::Not,
            "==" => TokenKind::Equal,
            "!=" => TokenKind::NotEqual,
            "=" => TokenKind::Assign,
            "(" => TokenKind::LParen,
            ")" => TokenKind::RParen,
            "[" => TokenKind::LBracket,
            "]" => TokenKind::RBracket,
            "{" => TokenKind::LBrace,
            "}" => TokenKind::RBrace,
            "." => TokenKind::Dot,
            "?" => TokenKind::Question,
            "!" => TokenKind::Bang,
            "..." => TokenKind::Ellipsis,
            "," => TokenKind::Comma,
            ":" => TokenKind::Colon,
            "#" => TokenKind::Hash,
            "|" => TokenKind::Pipe,
            "~" => TokenKind::Terminator,
            " " => TokenKind::Whitespace,
            "\t" => TokenKind::Tab,
            "\r" => TokenKind::CarriageReturn,
            "\n" => TokenKind::Newline,
            "hi" => TokenKind::Hi,
            "main" => TokenKind::Main,
            "fun" => TokenKind::Fun,
            "gwoup" => TokenKind::Group,
            "contwact" => TokenKind::Contract,
            "wetuwn" => TokenKind::Wetuwn,
            "iwf" => TokenKind::Iwf,
            "ewse" => TokenKind::Ewse,
            "ewif" => TokenKind::Ewif,
            "mash" => TokenKind::Mash,
            "default" => TokenKind::Default,
            "fow" => TokenKind::Fow,
            "continue" => TokenKind::Continue,
            "bweak" => TokenKind::Bweak,
            "in" => TokenKind::In,
            "fax" => TokenKind::Fax,
            "cap" => TokenKind::Cap,
            "nuww" => TokenKind::Nuww,
            _ if text.chars().nth(0) == Some('"') => TokenKind::StringLiteral,
            _ if text.starts_with(|c: char| c.is_ascii_alphabetic() && c.is_lowercase()) => {
                TokenKind::Identifier
            }
            _ if text.starts_with(|c: char| c.is_ascii_alphabetic() && c.is_uppercase()) => {
                TokenKind::Type
            }
            _ if text.chars().nth(0) == Some('\'') => TokenKind::CharLiteral,
            _ if text.chars().all(|c| c.is_ascii_digit() || c == '_') => TokenKind::IntLiteral,
            _ if text.starts_with(|c: char| c.is_ascii_digit()) => TokenKind::FloatLiteral,
            _ if text.starts_with(">_<") => TokenKind::Comment,
            _ => return Err(Token::default()),
        };
        Ok(Token::new(kind, offset))
    }
}

impl<'a> Position<'a> for Token {
    fn offset(&self) -> Offset {
        self.offset
    }
}

// }}}

// OFFSET {{{

/// Represents the start and exclusive end range of an item within a buffer.
/// Used for tracking item positions within text or data.
/// # Examples
/// ```
/// let offset = Offset::new(0, 4);
/// assert_eq!(offset.len(), 4);
/// assert_eq!(offset.range(), 0..4);
///
/// let string = "aqua-chan".to_string();
/// assert_eq!(&string[offset.range()], "aqua");
/// ```
#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq)]
pub struct Offset {
    /// The start position (inclusive) of the token in the buffer.
    pub start: usize,
    /// The end position (exclusive) of the token in the buffer.
    pub end: usize,
}
impl fmt::Display for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}-{})", self.start, self.end)
    }
}
impl Offset {
    /// Creates a new `Offset` from the given start (inclusive) and end
    /// (exclusive) positions.
    /// # Example
    /// ```
    /// let offset = Offset::new(0, 3);
    /// assert_eq!(offset.start, 0);
    /// assert_eq!(offset.end, 3);
    /// assert_eq!(offset.len(), 3);
    /// assert_eq!(offset.range(), 0..3);
    /// ```
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
    /// Returns an `Offset` with both start and end set to 0.
    /// # Example
    /// ```
    /// let zero_offset = Offset::zero();
    /// assert_eq!(zero_offset.start, 0);
    /// assert_eq!(zero_offset.end, 0);
    /// assert_eq!(offset.len(), 0);
    /// assert_eq!(offset.range(), 0..0);
    /// ```
    pub fn zero() -> Self {
        Self { start: 0, end: 0 }
    }
    /// Creates an `Offset` given the 0-indexed starting and ending line and
    /// column. `start` and `end` [usize] tuple is (line, col)
    pub fn from_line_col(source: &str, start: (usize, usize), end: (usize, usize)) -> Self {
        Self {
            start: source
                .split_inclusive('\n')
                .take(start.0)
                .map(|line| line.len())
                .sum::<usize>()
                + start.1,
            end: source
                .split_inclusive('\n')
                .take(end.0)
                .map(|line| line.len())
                .sum::<usize>()
                + end.1,
        }
    }
    /// Returns the length of the offset range.
    /// Equivalent to `end - start`.
    /// # Example
    /// ```
    /// let offset = Offset::new(3, 7);
    /// assert_eq!(offset.len(), 4);
    /// ```
    pub fn len(&self) -> usize {
        self.end - self.start
    }
    /// Construct an end exclusive range from [Offset::start] to [Offset::end]
    /// # Examples
    /// ```
    /// let offset = Offset::new(0, 4);
    /// assert_eq!(offset.range(), 0..4);
    ///
    /// let string = "aqua-chan".to_string();
    /// assert_eq!(&string[offset.range()], "aqua");
    /// ```
    pub fn range(&self) -> Range<usize> {
        self.start..self.end
    }
    /// makes sure to include one token before and after
    pub fn overlaps(&self, other: &Self) -> bool {
        self.end >= other.start && self.start <= other.end
    }
    // /// makes sure to include one token before and after
    // pub fn overlaps(&self, other: &Self) -> bool {
    //     self.end >= other.start && self.start <= other.end
    // }
}

// }}}

// POSITION {{{

/// Position of a token or item within source text. Provides methods for
/// accessing offset-based and line/column-based location data in both
/// zero-based and one-based (buffer-friendly) forms.
pub trait Position<'a> {
    /// Returns the [Offset]
    /// of this position, which includes the
    /// starting and ending indices within the source text.
    fn offset(&self) -> Offset;
    /// Returns the 0-based line number.
    fn line(&self, line_starts: &Vec<usize>) -> usize {
        let res = match line_starts.binary_search(&self.offset().start) {
            Ok(exact_line) => exact_line,
            Err(near_line) => near_line,
        }
        .saturating_sub(1);
        res
    }
    /// Returns the 0-based column number.
    fn col(&self, line_starts: &Vec<usize>) -> usize {
        let line = self.line(line_starts);
        self.offset()
            .start
            .saturating_sub(*line_starts.iter().nth(line).unwrap_or(&0))
            .saturating_sub(match line {
                0 => 0,
                _ => 1,
            })
    }
    /// Returns the 0-based ending line index of this token, computed from the
    /// start line, and the number of newlines in the source text.
    ///
    /// # Example
    /// Source used is:
    /// ```
    /// fax
    /// or
    /// cap
    /// ```
    /// Example infix expression based off the source above:
    /// ```
    /// let line_starts = vec![0, 3, 6];
    /// let infix_expr = InfixExpression {
    ///     left: Box::new(Expression::Token(
    ///         Token::from_str("fax", Offset::new(0, 3))
    ///     )),
    ///     op: Token::from_str("or", Offset::new(4, 6)),
    ///     right: Box::new(Expression::Token(
    ///         Token::from_str("cap", Offset::new(7, 10))
    ///     )),
    ///     offset: Offset::new(0, 10),
    /// };
    /// assert_eq!(infix_expr.line(&line_starts), 0);
    /// assert_eq!(infix_expr.line_end(&line_starts), 2);
    /// ```
    fn line_end(&self, line_starts: &Vec<usize>) -> usize {
        // let low = s.partition_point(|x| x < &1);
        match line_starts.binary_search(&(self.offset().end - 1)) {
            Ok(exact_line) => exact_line,
            Err(near_line) => near_line,
        }
        .saturating_sub(1)
    }
    /// Returns the 0-based ending column index of this token, computed from
    /// the start column, the offset length, and the number of newlines in the
    /// source text.
    ///
    /// # Example
    /// Source used is:
    /// ```
    /// fax
    /// or
    /// cap
    /// ```
    /// Example infix expression based off the source above:
    /// ```
    /// let line_starts = vec![0, 3, 6];
    /// let infix_expr = InfixExpression {
    ///     left: Box::new(Expression::Token(
    ///         Token::from_str("fax", Offset::new(0, 3))
    ///     )),
    ///     op: Token::from_str("or", Offset::new(4, 6)),
    ///     right: Box::new(Expression::Token(
    ///         Token::from_str("cap", Offset::new(7, 10))
    ///     )),
    ///     offset: Offset::new(0, 10),
    /// };
    /// assert_eq!(infix_expr.col(&line_starts), 0);
    /// assert_eq!(infix_expr.col_end(&line_starts), 2);
    /// ```
    /// Line end is 2 because there are 3 chars in the last line and columns
    /// are 0-indexed.
    fn col_end(&self, line_starts: &Vec<usize>) -> usize {
        let line = self.line_end(line_starts);
        self.offset()
            .end
            .saturating_sub(*line_starts.iter().nth(line).unwrap_or(&0))
            .saturating_sub(match line {
                0 => 1,
                _ => 2,
            })
    }
    /// Returns the (line, column) tuple using 0-based indexing.
    /// # Example
    /// ```
    /// let line_starts = vec![0];
    /// let offset = Offset::new(0, 4);
    /// let tok = Token::from_str("chan", offset);
    /// assert_eq!(tok.pos(&line_starts), (0, 0));
    /// ```
    #[inline]
    fn pos(&self, line_starts: &Vec<usize>) -> (usize, usize) {
        (self.line(line_starts), self.col(line_starts))
    }
    /// Returns the (line, column) tuple of the last character of an item
    /// using 0-based indexing.
    /// # Example
    /// ```
    /// let line_starts = vec![0];
    /// let offset = Offset::new(0, 4);
    /// let tok = Token::from_str("chan", offset);
    /// assert_eq!(tok.pos_end(&line_starts), (0, 3));
    /// ```
    #[inline]
    fn pos_end(&self, line_starts: &Vec<usize>) -> (usize, usize) {
        (self.line_end(line_starts), self.col_end(line_starts))
    }
    /// Returns the (line, column) tuple using 1-based indexing
    /// # Example
    /// ```
    /// let line_starts = vec![0];
    /// let offset = Offset::new(0, 4);
    /// let tok = Token::from_str("chan", offset);
    /// assert_eq!(tok.buffer_pos(&line_starts), (1, 1));
    /// ```
    #[inline]
    fn buffer_pos(&self, line_starts: &Vec<usize>) -> (usize, usize) {
        (self.buffer_line(line_starts), self.buffer_col(line_starts))
    }
    /// Returns the (line, column) tuple of the last character of an item
    /// using 1-based indexing
    /// # Example
    /// ```
    /// let line_starts = vec![0];
    /// let offset = Offset::new(0, 4);
    /// let tok = Token::from_str("chan", offset);
    /// assert_eq!(tok.buffer_pos_end(&line_starts), (1, 4));
    /// ```
    #[inline]
    fn buffer_pos_end(&self, line_starts: &Vec<usize>) -> (usize, usize) {
        (
            self.buffer_line_end(line_starts),
            self.buffer_col_end(line_starts),
        )
    }
    /// Returns the 1-based line number.
    /// # Example
    /// ```
    /// let line_starts = vec![0];
    /// let tok = Token::from_str("1", Offset::new(0, 1));
    /// assert_eq!(tok.buffer_line(&line_starts), 1);
    /// ```
    fn buffer_line(&self, line_starts: &Vec<usize>) -> usize {
        self.line(line_starts) + 1
    }
    /// Returns the 1-based column number.
    /// # Example
    /// ```
    /// let line_starts = vec![0];
    /// let tok = Token::from_str("1", Offset::new(0, 1));
    /// assert_eq!(tok.col(&line_starts), 0);
    /// assert_eq!(tok.buffer_col(&line_starts), 1);
    /// ```
    fn buffer_col(&self, line_starts: &Vec<usize>) -> usize {
        self.col(line_starts) + 1
    }
    /// Returns the 1-based ending line index of this token, computed from the
    /// start column, the offset length, and the number of newlines in the
    /// source text.
    ///
    /// # Example
    /// Source used is:
    /// ```
    /// fax
    /// or
    /// cap
    /// ```
    /// Example infix expression based off the source above:
    /// ```
    /// let line_starts = vec![0, 3, 6];
    /// let infix_expr = InfixExpression {
    ///     left: Box::new(Expression::Token(
    ///         Token::from_str("fax", Offset::new(0, 3))
    ///     )),
    ///     op: Token::from_str("or", Offset::new(4, 6)),
    ///     right: Box::new(Expression::Token(
    ///         Token::from_str("cap", Offset::new(7, 10))
    ///     )),
    ///     offset: Offset::new(0, 10),
    /// };
    /// assert_eq!(infix_expr.line(&line_starts), 1);
    /// assert_eq!(infix_expr.line_end(&line_starts), 3);
    /// ```
    /// Line end is 2 because there are 3 chars in the last line and columns
    /// are 0-indexed.
    fn buffer_line_end(&self, line_starts: &Vec<usize>) -> usize {
        self.line_end(line_starts) + 1
    }
    /// Returns the 1-based ending column index (inclusive).
    /// # Example
    /// ```
    /// let line_starts = vec![0, 3, 6];
    /// let offset = Offset::new(0, 3);
    /// let tok = Token::from_str("fax", offset);
    /// assert_eq!(tok.len(), 3);
    /// assert_eq!(
    ///     (tok.col(), tok.col_end()),
    ///     (0, 2),
    /// );
    /// assert_eq!(
    ///     (tok.buffer_col(&line_starts), tok.buffer_col_end(&line_starts)),
    ///     (1, 3),
    /// );
    /// ```
    fn buffer_col_end(&self, line_starts: &Vec<usize>) -> usize {
        self.col_end(line_starts) + 1
    }
    /// Returns the substring of the source text that corresponds to the range
    /// of this position, as defined by the start and end positions of the
    /// `Offset`.
    /// # Example
    /// ```
    /// let source = "aqua-chan";
    /// let offset = Offset::new(0, 4);
    /// let token = Token::from_str("aqua", offset);
    /// assert_eq!(token.source_str(source), "aqua");
    /// ```
    fn source_str(&self, source: &'a str) -> &'a str {
        source.get(self.offset().range()).unwrap_or_default()
    }
}

// }}}
