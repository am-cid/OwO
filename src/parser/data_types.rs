//! This module is for enums that group certain data type [Production]s and their custom
//! implementations of PartialEq
//!
//! [VecType]
//! - example: `aqua-chan[1]` where this signifies that `aqua` has a 1D array of `chan` values
//! - equality checks the data type, the dimension, and the fact that its a [VecType]
//! - note that the dimension is always a [TokenType::IntLiteral]
//!
//! [SetType]
//! - example: `aqua-chan{}` where this signifies that `aqua` has a hashset of `chan` values
//! - equality checks are the same as [Token] which only checks the id
//!     - in this case `chan` and the fact its a [SetType]
//!
//! [MapType]
//! - example: `aqua-senpai{chan}` where this signifies that `aqua` has a hashmap of `chan` values
//! mapped to `senpai` keys
//! - equality checks the data type, the inner data type, and the fact that its a [MapType]

use crate::lexer::token::{Token, TokenKind};
use crate::parser::productions::{Production, Range};

#[derive(Clone, Debug, Eq, Hash)]
pub enum DataType {
    Token(Token),
    Vec(VecType),
    Set(SetType),
    Map(MapType),
}
impl Production for DataType {
    fn range(&self) -> Range {
        match self {
            Self::Token(tok) => tok.range(),
            Self::Vec(vec) => vec.range(),
            Self::Set(set) => set.range(),
            Self::Map(map) => map.range(),
        }
    }
    fn string(&self, n: usize) -> String {
        match self {
            Self::Token(tok) => tok.string(n),
            Self::Vec(vec) => vec.string(n),
            Self::Set(set) => set.string(n),
            Self::Map(map) => map.string(n),
        }
    }
}
impl Default for DataType {
    fn default() -> Self {
        Self::Token(Token::default())
    }
}
impl PartialEq for DataType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Token(tok), Self::Token(other_tok)) => match (tok.kind, other_tok.kind) {
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
                _ => false,
            },
            (Self::Vec(vec), Self::Vec(other_vec)) => vec == other_vec,
            (Self::Set(set), Self::Set(other_set)) => set == other_set,
            (Self::Map(map), Self::Map(other_map)) => map == other_map,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub enum Vectorable {
    Token(Token),
    Set(SetType),
    Map(MapType),
}
impl Production for Vectorable {
    fn range(&self) -> Range {
        match self {
            Self::Token(tok) => tok.range(),
            Self::Set(set) => set.range(),
            Self::Map(map) => map.range(),
        }
    }
    fn string(&self, n: usize) -> String {
        match self {
            Self::Token(tok) => tok.string(n),
            Self::Set(set) => set.string(n),
            Self::Map(map) => map.string(n),
        }
    }
}
impl Default for Vectorable {
    fn default() -> Self {
        Self::Token(Token::default())
    }
}
impl PartialEq for Vectorable {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Token(tok), Self::Token(other_tok)) => match (tok.kind, other_tok.kind) {
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
                _ => false,
            },
            (Self::Set(set), Self::Set(other_set)) => set == other_set,
            (Self::Map(map), Self::Map(other_map)) => map == other_map,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct VecType {
    pub id: Vectorable,
    pub dim: Token,
    pub range: Range,
}
impl Production for VecType {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!("{}[{}]", self.id.string(n), self.dim)
    }
}
impl PartialEq for VecType {
    fn eq(&self, other: &Self) -> bool {
        match self.dim == other.dim {
            false => false,
            true => self == other,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct SetType {
    pub tok: Token,
    pub range: Range,
}
impl Production for SetType {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!("{}{{}}", self.tok.string(n))
    }
}
impl PartialEq for SetType {
    fn eq(&self, other: &Self) -> bool {
        match (self.tok.kind, other.tok.kind) {
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
            _ => false,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct MapType {
    pub tok: Token,
    pub inner: Box<DataType>,
    pub range: Range,
}
impl Production for MapType {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!("{}{{{}}}", self.tok.string(n), self.inner.string(0))
    }
}
impl PartialEq for MapType {
    fn eq(&self, other: &Self) -> bool {
        let first = match (self.tok.kind, other.tok.kind) {
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
            _ => false,
        };
        first && self.inner == other.inner
    }
}
