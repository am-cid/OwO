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
