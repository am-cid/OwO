use std::collections::HashSet;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
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

    // shorthand arith ops
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
    Comma,
    Pipe,

    // delimiters
    Terminator,
    EOF,
    Whitespace,
    Tab,
    Newline,
    Return,

    /// keywords
    // general
    Hi,
    Main,
    Fun,
    Group,
    Contract,
    Wetuwn,
    In,
    Assewt,
    Uwu,

    // io
    Pwint,
    Inpwt,

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

    // literals
    IntLiteral,
    FloatLiteral,
    Fax, // True
    Cap, // False
    StringLiteral,
    StringPartStart,
    StringPartMid,
    StringPartEnd,
    CharLiteral,
    Nuww, // None

    // comments
    SingleLineComment,
}

impl fmt::Display for TokenType {
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
            Self::Comma => write!(f, "Comma"),
            Self::Pipe => write!(f, "Pipe"),
            Self::Terminator => write!(f, "Terminator"),
            Self::EOF => write!(f, "EOF"),
            Self::Whitespace => write!(f, "Whitespace"),
            Self::Tab => write!(f, "Tab"),
            Self::Newline => write!(f, "Newline"),
            Self::Return => write!(f, "Return"),
            Self::Hi => write!(f, "Hi"),
            Self::Main => write!(f, "Main"),
            Self::Fun => write!(f, "Fun"),
            Self::Group => write!(f, "Group"),
            Self::Contract => write!(f, "Contract"),
            Self::Wetuwn => write!(f, "Wetuwn"),
            Self::In => write!(f, "In"),
            Self::Assewt => write!(f, "Assewt"),
            Self::Uwu => write!(f, "Uwu"),
            Self::Pwint => write!(f, "Pwint"),
            Self::Inpwt => write!(f, "Inpwt"),
            Self::Iwf => write!(f, "Iwf"),
            Self::Ewse => write!(f, "Ewse"),
            Self::Ewif => write!(f, "Ewif"),
            Self::Mash => write!(f, "Mash"),
            Self::Default => write!(f, "Default"),
            Self::Fow => write!(f, "Fow"),
            Self::Bweak => write!(f, "Bweak"),
            Self::Continue => write!(f, "Continue"),
            Self::IntLiteral => write!(f, "IntLiteral"),
            Self::FloatLiteral => write!(f, "FloatLiteral"),
            Self::Fax => write!(f, "Fax"),
            Self::Cap => write!(f, "Cap"),
            Self::StringLiteral => write!(f, "StringLiteral"),
            Self::StringPartStart => write!(f, "StringPartStart"),
            Self::StringPartMid => write!(f, "StringPartMid"),
            Self::StringPartEnd => write!(f, "StringPartEnd"),
            Self::CharLiteral => write!(f, "CharLiteral"),
            Self::Nuww => write!(f, "Nuww"),
            Self::SingleLineComment => write!(f, "SingleLineComment"),
        }
    }
}

impl TokenType {
    pub fn to_str(&self) -> &'static str {
        match self {
            // non-reserved words/symbols
            Self::SingleLineComment
            | Self::IntLiteral
            | Self::FloatLiteral
            | Self::StringLiteral
            | Self::Identifier
            | Self::Type
            | Self::StringPartStart
            | Self::StringPartMid
            | Self::StringPartEnd
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
            Self::Comma => ",",
            Self::Pipe => "|",
            Self::Terminator => "~",
            Self::Whitespace => " ",
            Self::Tab => "\t",
            Self::Newline => "\n",
            Self::Return => "\r",
            Self::Hi => "hi",
            Self::Main => "main",
            Self::Fun => "fun",
            Self::Group => "gwoup",
            Self::Contract => "contwact",
            Self::Wetuwn => "wetuwn",
            Self::In => "in",
            Self::Assewt => "assewt",
            Self::Uwu => "uwu",
            Self::Pwint => "pwint",
            Self::Inpwt => "inpwt",
            Self::Iwf => "iwf",
            Self::Ewse => "ewse",
            Self::Ewif => "ewif",
            Self::Mash => "mash",
            Self::Default => "default",
            Self::Fow => "fow",
            Self::Continue => "continue",
            Self::Bweak => "bweak",
            Self::Fax => "fax",
            Self::Cap => "cap",
            Self::Nuww => "nuww",
        }
    }
}

pub enum Atoms {
    AlphaNum,
    Symbols,
    Whitespace,
}
impl Atoms {
    pub fn charset(&self) -> HashSet<char> {
        match self {
            Self::AlphaNum => ('a'..='z')
                .chain('A'..='Z')
                .chain('0'..='9')
                .chain(HashSet::from(['_']))
                .collect(),
            Self::Symbols => HashSet::from([
                '+', '-', '*', '/', '%', '^', '>', '<', '=', '|', '&', '!', '(', ')', '[', ']',
                '{', '}', '.', '?', ',', '~', '"',
            ]),
            Self::Whitespace => HashSet::from([' ', '\n', '\t', '\r']),
        }
    }
    pub fn combine(atoms: &[Atoms]) -> HashSet<char> {
        let mut res = HashSet::new();
        for atom in atoms {
            res.extend(atom.charset());
        }
        res
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenType,
    pub text: &'static str,
    pub pos: (usize, usize),
    pub end_pos: (usize, usize),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.text)
    }
}

impl Token {
    pub fn new(
        kind: TokenType,
        text: &'static str,
        pos: (usize, usize),
        end_pos: (usize, usize),
    ) -> Self {
        Self {
            kind,
            text,
            pos,
            end_pos,
        }
    }
    pub fn from(text: &'static str, pos: (usize, usize), end_pos: (usize, usize)) -> Self {
        let kind = match text {
            "chan" => TokenType::Chan,
            "kun" => TokenType::Kun,
            "senpai" => TokenType::Senpai,
            "kouhai" => TokenType::Kouhai,
            "san" => TokenType::San,
            "sama" => TokenType::Sama,
            "dono" => TokenType::Dono,
            "+" => TokenType::Plus,
            "-" => TokenType::Dash,
            "*" => TokenType::Multiply,
            "/" => TokenType::Divide,
            "%" => TokenType::Modulo,
            "^" => TokenType::Exponent,
            "+=" => TokenType::PlusEqual,
            "-=" => TokenType::DashEqual,
            "*=" => TokenType::MultiplyEqual,
            "/=" => TokenType::DivideEqual,
            "%=" => TokenType::ModuloEqual,
            "^=" => TokenType::ExponentEqual,
            "<" => TokenType::LessThan,
            ">" => TokenType::GreaterThan,
            "<=" => TokenType::LessEqual,
            ">=" => TokenType::GreaterEqual,
            "and" => TokenType::And,
            "or" => TokenType::Or,
            "not" => TokenType::Not,
            "==" => TokenType::Equal,
            "!=" => TokenType::NotEqual,
            "=" => TokenType::Assign,
            "(" => TokenType::LParen,
            ")" => TokenType::RParen,
            "[" => TokenType::LBracket,
            "]" => TokenType::RBracket,
            "{" => TokenType::LBrace,
            "}" => TokenType::RBrace,
            "." => TokenType::Dot,
            "?" => TokenType::Question,
            "!" => TokenType::Bang,
            "," => TokenType::Comma,
            "|" => TokenType::Pipe,
            "~" => TokenType::Terminator,
            " " => TokenType::Whitespace,
            "\t" => TokenType::Tab,
            "\r" => TokenType::Return,
            "\n" => TokenType::Newline,
            "hi" => TokenType::Hi,
            "main" => TokenType::Main,
            "fun" => TokenType::Fun,
            "gwoup" => TokenType::Group,
            "contwact" => TokenType::Contract,
            "wetuwn" => TokenType::Wetuwn,
            "in" => TokenType::In,
            "assewt" => TokenType::Assewt,
            "uwu" => TokenType::Uwu,
            "pwint" => TokenType::Pwint,
            "inpwt" => TokenType::Inpwt,
            "iwf" => TokenType::Iwf,
            "ewse" => TokenType::Ewse,
            "ewif" => TokenType::Ewif,
            "mash" => TokenType::Mash,
            "default" => TokenType::Default,
            "fow" => TokenType::Fow,
            "continue" => TokenType::Continue,
            "bweak" => TokenType::Bweak,
            "fax" => TokenType::Fax,
            "cap" => TokenType::Cap,
            "nuww" => TokenType::Nuww,
            _ if text.chars().nth(0) == Some('"') => match text.chars().last() {
                Some('"') => TokenType::StringLiteral,
                Some('{') => TokenType::StringPartStart,
                _ => unreachable!(),
            },
            _ if text.chars().nth(0) == Some('}') => match text.chars().last() {
                Some('"') => TokenType::StringPartEnd,
                Some('{') => TokenType::StringPartMid,
                _ => unreachable!(),
            },
            _ if text.starts_with(|c: char| c.is_alphabetic() && c.is_lowercase()) => {
                TokenType::Identifier
            }
            _ if text.starts_with(|c: char| c.is_alphanumeric() && c.is_uppercase()) => {
                TokenType::Type
            }
            _ if text.chars().nth(0) == Some('\'') && text.chars().last() == Some('\'') => {
                TokenType::CharLiteral
            }
            _ if text.chars().all(|c| c.is_ascii_digit() || c == '_') => TokenType::IntLiteral,
            _ if text.starts_with(|c: char| c.is_ascii_digit())
                && text
                    .chars()
                    .skip(1)
                    .all(|c| c.is_ascii_digit() || c == '.' || c == '_')
                && text.chars().last() != Some('.')
                && text.chars().filter(|c| *c == '.').count() == 1 =>
            {
                TokenType::FloatLiteral
            }
            _ if text.starts_with(">.<") => TokenType::SingleLineComment,
            _ => unreachable!("Unknown token: {}", text),
        };
        Token {
            kind,
            text: text.into(),
            pos,
            end_pos,
        }
    }
}
