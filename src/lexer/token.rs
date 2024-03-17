use std::fmt;

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
    DoubleQuote,
    Comma,
    Pipe,

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
