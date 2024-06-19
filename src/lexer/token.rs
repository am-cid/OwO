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
        write!(f, "{}", self)
    }
}

impl TokenType {
    pub fn to_str(&self) -> &'static str {
        match self {
            // non-reserved words/symbols
            Self::SingleLineComment
            | Self::MultiLineComment
            | Self::IntLiteral
            | Self::FloatLiteral
            | Self::StringLiteral
            | Self::Identifier
            | Self::ClassId
            | Self::StringPartStart
            | Self::StringPartMid
            | Self::StringPartEnd
            | Self::EOF => "\0",
            // reserved words/symbols
            Self::Chan => "chan",
            Self::Kun => "kun",
            Self::Senpai => "senpai",
            Self::Sama => "sama",
            Self::San => "san",
            Self::Concat => "&",
            Self::Increment => "++",
            Self::Decrement => "--",
            Self::Plus => "+",
            Self::Dash => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::Modulo => "%",
            Self::LessThan => "<",
            Self::LessEqual => "<=",
            Self::GreaterThan => ">",
            Self::GreaterEqual => ">=",
            Self::And => "&&",
            Self::Or => "||",
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::Assign => "=",
            Self::LParen => "(",
            Self::RParen => ")",
            Self::LBracket => "[",
            Self::RBracket => "]",
            Self::DoubleLBracket => "[[",
            Self::DoubleRBracket => "]]",
            Self::LBrace => "{",
            Self::RBrace => "}",
            Self::Dot => ".",
            Self::Comma => ",",
            Self::Terminator => "~",
            Self::Gwobaw => "gwobaw",
            Self::Mainuwu => "mainuwu",
            Self::Fwunc => "fwunc",
            Self::Cwass => "cwass",
            Self::Wetuwn => "wetuwn",
            Self::Dono => "dono",
            Self::Pwint => "pwint",
            Self::Inpwt => "inpwt",
            Self::Iwf => "iwf",
            Self::Ewse => "ewse",
            Self::EwseIwf => "ewse iwf",
            Self::Whiwe => "whiwe",
            Self::DoWhiwe => "do whiwe",
            Self::Fow => "fow",
            Self::Bweak => "bweak",
            Self::Fax => "fax",
            Self::Cap => "cap",
            Self::Nuww => "nuww",
            Self::Whitespace => " ",
            Self::Tab => "\t",
            Self::Newline => "\n",
            Self::Return => "\r",
        }
    }
        match self {
            Self::SingleLineComment => delim_set,
            _ => {
                delim_set.extend(atoms("whitespace"));
                delim_set
            }
        }
    }
}

pub fn atoms(key: &str) -> HashSet<char> {
    match key {
        "num" => ('1'..='9').collect(),
        "number" => ('0'..='9').collect(),
        "alpha_small" => ('a'..='z').collect(),
        "alpha_big" => ('A'..='Z').collect(),
        "alpha" => ('a'..='z').chain('A'..='Z').collect(),
        "alpha_num" => ('a'..='z').chain('A'..='Z').chain('0'..='9').collect(),
        "arith_op" => HashSet::from(['+', '-', '*', '/', '%']),
        "gen_op" => HashSet::from(['+', '-', '*', '/', '%', '>', '<', '=']),
        "whitespace" => HashSet::from([' ', '\n', '\t', '\r']),
        // all ascii characters (non-extended)
        "all" => ('\x00'..='\x7F').collect(),
        _ => HashSet::new(),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    kind: TokenType,
    text: &'static str,
    pos: (usize, usize),
    end_pos: (usize, usize),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} of type {}", self.text, self.kind)
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
}
                Some('"') => TokenType::StringLiteral,
                Some('"') => TokenType::StringPartEnd,
            text: text.into(),
            pos,
            end_pos,
        }
    }
}
