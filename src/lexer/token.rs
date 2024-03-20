use std::collections::HashSet;
use std::fmt;

#[derive(Debug)]
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
    Terminator,

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
        }
    }
    pub fn delims(&self) -> HashSet<char> {
        let mut delim_set = match self {
            Self::EOF | Self::Whitespace => atoms("all"),
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
            Self::Dono => HashSet::from([',', '(', ')', '=']),
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
            Self::Terminator => atoms("all"),
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

pub fn atoms(key: &str) -> HashSet<char> {
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
        // all ascii characters (non-extended)
        "all" => ('\x00'..='\x7F').collect(),
        _ => HashSet::new(),
    }
}

#[derive(Debug)]
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

pub fn to_token(
    text: &'static str,
    pos: (usize, usize),
    end_pos: (usize, usize),
) -> Result<Token, String> {
    match text {
        "chan" => Ok(Token {
            kind: TokenType::Chan,
            text: text.into(),
            pos,
            end_pos,
        }),
        "kun" => Ok(Token {
            kind: TokenType::Kun,
            text: text.into(),
            pos,
            end_pos,
        }),
        "senpai" => Ok(Token {
            kind: TokenType::Senpai,
            text: text.into(),
            pos,
            end_pos,
        }),
        "sama" => Ok(Token {
            kind: TokenType::Sama,
            text: text.into(),
            pos,
            end_pos,
        }),
        "san" => Ok(Token {
            kind: TokenType::San,
            text: text.into(),
            pos,
            end_pos,
        }),
        "&" => Ok(Token {
            kind: TokenType::Concat,
            text: text.into(),
            pos,
            end_pos,
        }),
        "++" => Ok(Token {
            kind: TokenType::Increment,
            text: text.into(),
            pos,
            end_pos,
        }),
        "--" => Ok(Token {
            kind: TokenType::Decrement,
            text: text.into(),
            pos,
            end_pos,
        }),
        "+" => Ok(Token {
            kind: TokenType::Plus,
            text: text.into(),
            pos,
            end_pos,
        }),
        "-" => Ok(Token {
            kind: TokenType::Dash,
            text: text.into(),
            pos,
            end_pos,
        }),
        "*" => Ok(Token {
            kind: TokenType::Multiply,
            text: text.into(),
            pos,
            end_pos,
        }),
        "/" => Ok(Token {
            kind: TokenType::Divide,
            text: text.into(),
            pos,
            end_pos,
        }),
        "%" => Ok(Token {
            kind: TokenType::Modulo,
            text: text.into(),
            pos,
            end_pos,
        }),
        "<" => Ok(Token {
            kind: TokenType::LessThan,
            text: text.into(),
            pos,
            end_pos,
        }),
        "<=" => Ok(Token {
            kind: TokenType::LessEqual,
            text: text.into(),
            pos,
            end_pos,
        }),
        "&&" => Ok(Token {
            kind: TokenType::And,
            text: text.into(),
            pos,
            end_pos,
        }),
        "||" => Ok(Token {
            kind: TokenType::Or,
            text: text.into(),
            pos,
            end_pos,
        }),
        "==" => Ok(Token {
            kind: TokenType::Equal,
            text: text.into(),
            pos,
            end_pos,
        }),
        "!=" => Ok(Token {
            kind: TokenType::NotEqual,
            text: text.into(),
            pos,
            end_pos,
        }),
        "=" => Ok(Token {
            kind: TokenType::Assign,
            text: text.into(),
            pos,
            end_pos,
        }),
        "(" => Ok(Token {
            kind: TokenType::LParen,
            text: text.into(),
            pos,
            end_pos,
        }),
        ")" => Ok(Token {
            kind: TokenType::RParen,
            text: text.into(),
            pos,
            end_pos,
        }),
        "[" => Ok(Token {
            kind: TokenType::LBracket,
            text: text.into(),
            pos,
            end_pos,
        }),
        "]" => Ok(Token {
            kind: TokenType::RBracket,
            text: text.into(),
            pos,
            end_pos,
        }),
        "[[" => Ok(Token {
            kind: TokenType::DoubleLBracket,
            text: text.into(),
            pos,
            end_pos,
        }),
        "]]" => Ok(Token {
            kind: TokenType::DoubleRBracket,
            text: text.into(),
            pos,
            end_pos,
        }),
        "{" => Ok(Token {
            kind: TokenType::LBrace,
            text: text.into(),
            pos,
            end_pos,
        }),
        "}" => Ok(Token {
            kind: TokenType::RBrace,
            text: text.into(),
            pos,
            end_pos,
        }),
        "." => Ok(Token {
            kind: TokenType::Dot,
            text: text.into(),
            pos,
            end_pos,
        }),
        "," => Ok(Token {
            kind: TokenType::Comma,
            text: text.into(),
            pos,
            end_pos,
        }),
        "gwobaw" => Ok(Token {
            kind: TokenType::Gwobaw,
            text: text.into(),
            pos,
            end_pos,
        }),
        "mainuwu" => Ok(Token {
            kind: TokenType::Mainuwu,
            text: text.into(),
            pos,
            end_pos,
        }),
        "fwunc" => Ok(Token {
            kind: TokenType::Fwunc,
            text: text.into(),
            pos,
            end_pos,
        }),
        "cwass" => Ok(Token {
            kind: TokenType::Cwass,
            text: text.into(),
            pos,
            end_pos,
        }),
        "wetuwn" => Ok(Token {
            kind: TokenType::Wetuwn,
            text: text.into(),
            pos,
            end_pos,
        }),
        "dono" => Ok(Token {
            kind: TokenType::Dono,
            text: text.into(),
            pos,
            end_pos,
        }),
        "pwint" => Ok(Token {
            kind: TokenType::Pwint,
            text: text.into(),
            pos,
            end_pos,
        }),
        "inpwt" => Ok(Token {
            kind: TokenType::Inpwt,
            text: text.into(),
            pos,
            end_pos,
        }),
        "iwf" => Ok(Token {
            kind: TokenType::Iwf,
            text: text.into(),
            pos,
            end_pos,
        }),
        "ewse" => Ok(Token {
            kind: TokenType::Ewse,
            text: text.into(),
            pos,
            end_pos,
        }),
        "ewse iwf" => Ok(Token {
            kind: TokenType::EwseIwf,
            text: text.into(),
            pos,
            end_pos,
        }),
        "whiwe" => Ok(Token {
            kind: TokenType::Whiwe,
            text: text.into(),
            pos,
            end_pos,
        }),
        "do whiwe" => Ok(Token {
            kind: TokenType::DoWhiwe,
            text: text.into(),
            pos,
            end_pos,
        }),
        "fow" => Ok(Token {
            kind: TokenType::Fow,
            text: text.into(),
            pos,
            end_pos,
        }),
        "fax" => Ok(Token {
            kind: TokenType::Fax,
            text: text.into(),
            pos,
            end_pos,
        }),
        "cap" => Ok(Token {
            kind: TokenType::Cap,
            text: text.into(),
            pos,
            end_pos,
        }),
        "nuww" => Ok(Token {
            kind: TokenType::Nuww,
            text: text.into(),
            pos,
            end_pos,
        }),
        ">.<" => Ok(Token {
            kind: TokenType::SingleLineComment,
            text: text.into(),
            pos,
            end_pos,
        }),
        r#">//<"# => Ok(Token {
            kind: TokenType::MultiLineComment,
            text: text.into(),
            pos,
            end_pos,
        }),
        " " | "\t" | "\n" => Ok(Token {
            kind: TokenType::Whitespace,
            text: text.into(),
            pos,
            end_pos,
        }),
        _ if text.starts_with(|c: char| c.is_alphabetic() && c.is_lowercase()) => Ok(Token {
            kind: TokenType::Identifier,
            text: text.into(),
            pos,
            end_pos,
        }),
        _ if text.starts_with(|c: char| c.is_alphanumeric() && c.is_uppercase()) => Ok(Token {
            kind: TokenType::ClassId,
            text: text.into(),
            pos,
            end_pos,
        }),
        _ if text.chars().all(|c| c.is_numeric()) => Ok(Token {
            kind: TokenType::IntLiteral,
            text: text.into(),
            pos,
            end_pos,
        }),
        _ if text.starts_with(|c: char| c.is_numeric()) &&
            text.chars().skip(1).all(|c| c.is_numeric() || c == '.') &&
            text.chars().last() != Some('.') &&
            // check if only one . exists
            text.chars().filter(|c| *c == '.').count() == 1 =>
        {
            Ok(Token {
                kind: TokenType::FloatLiteral,
                text: text.into(),
                pos,
                end_pos,
            })
        }
        _ if text.starts_with(">.<") => Ok(Token {
            kind: TokenType::SingleLineComment,
            text: text.into(),
            pos,
            end_pos,
        }),
        _ if text.starts_with(r#">//<"#) => Ok(Token {
            kind: TokenType::MultiLineComment,
            text: text.into(),
            pos,
            end_pos,
        }),
        _ => Err(format!("Unknown token: {}", text)),
    }
}
