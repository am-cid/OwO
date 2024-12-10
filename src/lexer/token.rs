use std::fmt;
use std::hash::Hash;
use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum TokenKind {
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
    Exponent,

    // shorthand arith assign ops
    PlusEqual,
    DashEqual,
    MultiplyEqual,
    DivideEqual,
    ModuloEqual,
    ExponentEqual,

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
    #[default]
    EOF,

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
    Assewt, // assertions

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
            Self::Exponent => write!(f, "Exponent"),
            Self::PlusEqual => write!(f, "PlusEqual"),
            Self::DashEqual => write!(f, "DashEqual"),
            Self::MultiplyEqual => write!(f, "MultiplyEqual"),
            Self::DivideEqual => write!(f, "DivideEqual"),
            Self::ModuloEqual => write!(f, "ModuloEqual"),
            Self::ExponentEqual => write!(f, "ExponentEqual"),
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
            Self::EOF => write!(f, "EOF"),
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
            Self::Assewt => write!(f, "Assewt"),
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
    pub fn to_str(&self) -> &'static str {
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
            Self::Exponent => "^",
            Self::PlusEqual => "+=",
            Self::DashEqual => "-=",
            Self::MultiplyEqual => "*=",
            Self::DivideEqual => "/=",
            Self::ModuloEqual => "%=",
            Self::ExponentEqual => "^=",
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
            Self::Assewt => "assewt",
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
            Self::ExponentEqual,
        ]
    }
    pub fn math_ops() -> Vec<Self> {
        vec![
            Self::Plus,
            Self::Dash,
            Self::Multiply,
            Self::Divide,
            Self::Modulo,
            Self::Exponent,
        ]
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: TokenPosition,
}

// impl Eq for Token {}
// impl PartialEq for Token {
//     fn eq(&self, other: &Self) -> bool {
//         self.kind == other.kind && self.text == other.text
//     }
// }
// impl Hash for Token {
//     fn hash<H: Hasher>(&self, state: &mut H) {
//         self.kind.hash(state);
//         self.text.hash(state);
//     }
// }
impl<'a> Token {
    fn default() -> Self {
        Self {
            kind: TokenKind::EOF,
            pos: TokenPosition::zero(),
        }
    }
    fn new(kind: TokenKind, pos: TokenPosition) -> Self {
        Self { kind, pos }
    }
    /// Gets the token text from a given source.
    /// A [Token] only contains its position (and kind) and does not store its representation
    /// # Example
    /// ```
    /// let source = "hello world";
    /// let tok = Token::new(
    ///     TokenKind::Identifier,
    ///     TokenPosition::new((0, 5), 0, 0),
    /// );
    /// assert_eq!("hello", tok.str_from_source(source));
    /// ```
    pub fn str_from_source(&self, source: &'a str) -> &'a str {
        &source[self.pos.offset.range()]
    }
    /// Attempts to create a token based on the str passed to it and returns a [Token]
    /// Returns default token with kind [TokenKind::EOF] as error if it cannot recognize the str
    /// # Examples
    /// ```
    /// let tok = Token::from_str("chan", TokenPosition::zero());
    /// assert_eq!(tok, Ok(Token::new(TokenKind::Chan, TokenPosition::zero()));
    ///
    /// let unknown = Token::from_str("@", TokenPosition::zero());
    /// assert_eq!(tok, Err(Token::default()));
    /// ```
    pub fn from_str(text: &str, pos: TokenPosition) -> Result<Self, Self> {
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
            "^" => TokenKind::Exponent,
            "+=" => TokenKind::PlusEqual,
            "-=" => TokenKind::DashEqual,
            "*=" => TokenKind::MultiplyEqual,
            "/=" => TokenKind::DivideEqual,
            "%=" => TokenKind::ModuloEqual,
            "^=" => TokenKind::ExponentEqual,
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
            "assewt" => TokenKind::Assewt,
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
        Ok(Token { pos, kind })
    }
}

#[derive(Debug, Clone, Copy, Default)]
/// like [LexerPosition] but has an [Offset] to track where the token is in the buffer
pub struct TokenPosition {
    pub offset: Offset,
    pub line: usize,
    pub col: usize,
}
impl fmt::Display for TokenPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})[{:?}]", self.line, self.col, self.offset)
    }
}
impl TokenPosition {
    pub fn new(abs: (usize, usize), line: usize, col: usize) -> Self {
        Self {
            offset: Offset::new(abs.0, abs.1),
            line,
            col,
        }
    }
    pub fn new_with_offset(offset: Offset, line: usize, col: usize) -> Self {
        Self { offset, line, col }
    }
    pub fn zero() -> Self {
        Self {
            offset: Offset::zero(),
            line: 0,
            col: 0,
        }
    }
    /// Gets the ending column based on [Offset::len]
    /// # Example
    /// ```
    /// let offset = Offset::new(0, 4);
    /// let pos =  TokenPosition::new_with_offset(offset, 0, 0);
    /// // or just TokenPosition::new((0, 4), 0, 0);
    ///
    /// assert_eq!(pos.col_end(), 3);
    /// ```
    pub fn col_end(&self) -> usize {
        self.col + self.offset.len() - 1
    }
    /// Returns the line information for buffers (1-indexed)
    /// # Example
    /// ```
    /// let pos =  TokenPosition::new((0, 0), 0, 0);
    /// assert_eq!(pos.line, 0);
    /// assert_eq!(pos.buffer_line(), 1);
    /// ```
    pub fn buffer_line(&self) -> usize {
        self.line + 1
    }
    /// Returns the column information for buffers (1-indexed)
    /// # Example
    /// ```
    /// let pos =  TokenPosition::new((0, 0), 0, 0);
    /// assert_eq!(pos.col, 0);
    /// assert_eq!(pos.buffer_col(), 1);
    /// ```
    pub fn buffer_col(&self) -> usize {
        self.col + 1
    }
    /// Returns the column end information for buffers (1-indexed)
    /// # Example
    /// ```
    /// let pos =  TokenPosition::new((0, 3), 0, 0);
    /// assert_eq!(pos.col_end(), 2);
    /// assert_eq!(pos.buffer_line(), 3);
    /// ```
    pub fn buffer_col_end(&self) -> usize {
        self.col_end() + 1
    }
}

/// Represents the start and exclusive end range of a token within a buffer.
/// Used for tracking token positions within text or data.
/// # Examples
/// ```
/// let offset = Offset::new(0, 4);
/// assert_eq!(offset.len(), 4);
/// assert_eq!(offset.range(), 0..4);
///
/// let string = "aqua-chan";
/// assert_eq!(&string[offset.range()], "aqua");
/// ```
#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq)]
pub struct Offset {
    pub start: usize,
    pub end: usize,
}
impl fmt::Display for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}-{})", self.start, self.end)
    }
}
impl Offset {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
    pub fn zero() -> Self {
        Self { start: 0, end: 0 }
    }
    /// length of the beginning of the offset to the end
    /// # Example
    /// ```
    /// let offset = Offset::new(0, 4);
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
    /// let string = "aqua-chan";
    /// assert_eq!(&string[offset.range()], "aqua");
    /// ```
    pub fn range(&self) -> Range<usize> {
        self.start..self.end
    }
}
