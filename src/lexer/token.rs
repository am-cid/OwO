use std::collections::HashSet;
use std::fmt;
use std::hash::Hash;

pub enum TokenType {
    // ids
    Identifier,
    ClassId,
    // dtypes
    Chan,
    Kun,
    Senpai,
    Sama,
    San,
    // string and array concat op
    Concat,
    // unary ops
    Increment,
    Decrement,
    // arith ops
    Plus,
    Dash,
    Multiply,
    Divide,
    Modulo,
    // rel ops
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    // boolean ops
    And,
    Or,
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
    DoubleLBracket,
    DoubleRBracket,
    LBrace,
    RBrace,
    // method/property access
    Dot,
    // general symbols
    Comma,

    // keywords
    // general
    Gwobaw,
    Mainuwu,
    Fwunc,
    Cwass,
    Wetuwn,
    Dono,
    // io
    Pwint,
    Inpwt,
    // control flow
    Iwf,
    Ewse,
    EwseIwf,
    // loop constructs
    Whiwe,
    DoWhiwe,
    Fow,
    Bweak,
    // literals
    StringLiteral,
    StringPartStart,
    StringPartMid,
    StringPartEnd,
    IntLiteral,
    FloatLiteral,
    Fax,
    Cap,
    Nuww,
    // end of file
    EOF,
    // comments
    SingleLineComment,
    MultiLineComment,
    // whitespace
    Whitespace,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl TokenType {
    pub fn delims(&self) -> HashSet<char> {
        let mut delim_set = match self {
            Self::Identifier => {
                let mut res =
                    HashSet::from(['~', ',', '(', ')', '[', ']', '}', '!', '&', '|', '.']);
                res.extend(atoms("gen_op"));
                res
            }
            Self::ClassId => HashSet::from(['(']),
            Self::Chan | Self::Kun | Self::Senpai | Self::Sama | Self::San => {
                HashSet::from(['[', ',', '(', ')', '~', '=', '-'])
            }
            Self::Concat => HashSet::from(['i', '"']),
            Self::Increment | Self::Decrement => {
                let mut res = HashSet::from(['|', '~', ')', '!']);
                res.extend(atoms("gen_op"));
                res
            }
            Self::Plus
            | Self::Dash
            | Self::Multiply
            | Self::Divide
            | Self::Modulo
            | Self::LessThan
            | Self::LessEqual
            | Self::GreaterThan
            | Self::GreaterEqual => {
                let mut res = HashSet::from(['-', '(', '{']);
                res.extend(atoms("alpha_num"));
                res
            }
            Self::And | Self::Or | Self::Equal | Self::NotEqual | Self::Assign => {
                let mut res = HashSet::from(['"', '-', '(', '{']);
                res.extend(atoms("alpha_num"));
                res
            }
            Self::LParen => {
                let mut res = HashSet::from(['{', '-', '>', '(', ')', '"']);
                res.extend(atoms("alpha_num"));
                res
            }
            Self::RParen => {
                let mut res =
                    HashSet::from(['!', '&', '|', '~', '>', '.', ',', ')', '(', '[', ']', '}']);
                res.extend(atoms("gen_op"));
                res
            }
            Self::LBracket => {
                let mut res = HashSet::from([']', '-', '(']);
                res.extend(atoms("alpha_num"));
                res
            }
            Self::RBracket => {
                let mut res =
                    HashSet::from(['(', '~', ',', ')', '[', ']', '}', '!', '&', '|', '.']);
                res.extend(atoms("gen_op"));
                res
            }
            Self::DoubleLBracket | Self::DoubleRBracket => {
                let mut res = HashSet::from(['>']);
                res.extend(atoms("alpha"));
                res
            }
            Self::LBrace => {
                let mut res = HashSet::from(['{', '}', '(', '"', '>', '-']);
                res.extend(atoms("alpha_num"));
                res
            }
            Self::RBrace => {
                let mut res = HashSet::from(['}', '~', ',', ')', '>', '&', '!', '|']);
                res.extend(atoms("gen_op"));
                res
            }
            Self::Dot => atoms("alpha"),
            Self::Comma => {
                let mut res = HashSet::from(['"', '-', '>', '{']);
                res.extend(atoms("alpha_num"));
                res
            }
            Self::Mainuwu | Self::Wetuwn | Self::Inpwt | Self::Pwint => HashSet::from(['-']),
            Self::Gwobaw | Self::Fwunc | Self::Cwass => HashSet::new(),
            Self::Iwf | Self::Ewse | Self::EwseIwf | Self::Whiwe | Self::DoWhiwe | Self::Fow => {
                HashSet::from(['(', '['])
            }
            Self::Bweak => HashSet::from(['~']),
            Self::StringLiteral | Self::StringPartEnd => {
                HashSet::from(['|', ')', ',', '&', '}', '[', ']', '~', '!', '='])
            }
            Self::StringPartStart | Self::StringPartMid => {
                let mut res = HashSet::from(['"', '-', '(', '|', '&']);
                res.extend(atoms("alpha_num"));
                res
            }
            Self::IntLiteral | Self::FloatLiteral => {
                let mut res =
                    HashSet::from([',', ')', '}', ']', '~', '!', '&', '|', '>', '<', '=']);
                res.extend(atoms("gen_op"));
                res
            }
            Self::Fax | Self::Cap => HashSet::from([',', '}', ')', '~']),
            Self::Nuww => HashSet::from(['~', ')', '}', ',', '=', '!', '|', '&']),
            Self::SingleLineComment => HashSet::from(['\n']),
            Self::MultiLineComment => {
                let mut res = HashSet::from([']']);
                res.extend(atoms("alpha"));
                res
            }
            _ => HashSet::new(),
        };
        match self {
            Self::SingleLineComment => delim_set,
            _ => {
                delim_set.extend(atoms("whitespace"));
                delim_set
            }
        }
    }
}

fn atoms(key: &str) -> HashSet<char> {
    match key {
        "num" => ('1'..='9').collect(),
        "number" => ('0'..='9').collect(),
        "alpha_small" => ('a'..='z').collect(),
        "alpha_big" => ('A'..='Z').collect(),
        "alpha" => ('a'..='z').chain('A'..='Z').collect(),
        "alphanum" => ('a'..='z').chain('A'..='Z').chain('0'..='9').collect(),
        "arith_op" => HashSet::from(['+', '-', '*', '/', '%']),
        "gen_op" => HashSet::from(['+', '-', '*', '/', '%', '>', '<', '=']),
        "whitespace" => HashSet::from([' ', '\t', '\n']),
        _ => HashSet::new(),
    }
}

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
