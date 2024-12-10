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
use crate::parser::productions::Production;

#[derive(Clone, Debug)]
pub enum DataType {
    Token(Token),
    Vec(VecType),
    Set(SetType),
    Map(MapType),
}
impl<'a> Production<'a> for DataType {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        match self {
            Self::Token(tok) => tok.to_string(source, n),
            Self::Vec(vec) => vec.to_string(source, n),
            Self::Set(set) => set.to_string(source, n),
            Self::Map(map) => map.to_string(source, n),
        }
    }
}
impl Default for DataType {
    fn default() -> Self {
        Self::Token(Token::default())
    }
}
// impl PartialEq for DataType {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (Self::Token(tok), Self::Token(other_tok)) => match (tok.kind, other_tok.kind) {
//                 (TokenKind::Dono, TokenKind::Dono)
//                 | (
//                     TokenKind::Type
//                     | TokenKind::Chan
//                     | TokenKind::Kun
//                     | TokenKind::Senpai
//                     | TokenKind::Kouhai
//                     | TokenKind::San
//                     | TokenKind::Sama,
//                     TokenKind::Dono,
//                 )
//                 | (
//                     TokenKind::Dono,
//                     TokenKind::Type
//                     | TokenKind::Chan
//                     | TokenKind::Kun
//                     | TokenKind::Senpai
//                     | TokenKind::Kouhai
//                     | TokenKind::San
//                     | TokenKind::Sama,
//                 ) => true,
//                 _ => false,
//             },
//             (Self::Vec(vec), Self::Vec(other_vec)) => vec == other_vec,
//             (Self::Set(set), Self::Set(other_set)) => set == other_set,
//             (Self::Map(map), Self::Map(other_map)) => map == other_map,
//             _ => false,
//         }
//     }
// }

#[derive(Clone, Debug)]
pub enum Vectorable {
    Token(Token),
    Set(SetType),
    Map(MapType),
}
impl<'a> Production<'a> for Vectorable {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        match self {
            Self::Token(tok) => tok.to_string(source, n),
            Self::Set(set) => set.to_string(source, n),
            Self::Map(map) => map.to_string(source, n),
        }
    }
}
impl Default for Vectorable {
    fn default() -> Self {
        Self::Token(Token::default())
    }
}
// impl PartialEq for Vectorable {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (Self::Token(tok), Self::Token(other_tok)) => match (tok.kind, other_tok.kind) {
//                 (TokenKind::Dono, TokenKind::Dono)
//                 | (
//                     TokenKind::Type
//                     | TokenKind::Chan
//                     | TokenKind::Kun
//                     | TokenKind::Senpai
//                     | TokenKind::Kouhai
//                     | TokenKind::San
//                     | TokenKind::Sama,
//                     TokenKind::Dono,
//                 )
//                 | (
//                     TokenKind::Dono,
//                     TokenKind::Type
//                     | TokenKind::Chan
//                     | TokenKind::Kun
//                     | TokenKind::Senpai
//                     | TokenKind::Kouhai
//                     | TokenKind::San
//                     | TokenKind::Sama,
//                 ) => true,
//                 _ => false,
//             },
//             (Self::Set(set), Self::Set(other_set)) => set == other_set,
//             (Self::Map(map), Self::Map(other_map)) => map == other_map,
//             _ => false,
//         }
//     }
// }

#[derive(Clone, Debug)]
pub struct VecType {
    pub id: Vectorable,
    pub dim: Token,
}
impl<'a> Production<'a> for VecType {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}[{}]",
            self.id.to_string(source, n),
            self.dim.str_from_source(source)
        )
    }
}

#[derive(Clone, Debug)]
pub struct SetType {
    pub tok: Token,
}
impl<'a> Production<'a> for SetType {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!("{}{{}}", self.tok.to_string(source, n))
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

#[derive(Clone, Debug)]
pub struct MapType {
    pub tok: Token,
    pub inner: Box<DataType>,
}
impl<'a> Production<'a> for MapType {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}{{{}}}",
            self.tok.to_string(source, n),
            self.inner.to_string(source, 0)
        )
    }
}
// impl PartialEq for MapType {
//     fn eq(&self, other: &Self) -> bool {
//         let first = match (self.tok.kind, other.tok.kind) {
//             (TokenKind::Dono, TokenKind::Dono)
//             | (
//                 TokenKind::Type
//                 | TokenKind::Chan
//                 | TokenKind::Kun
//                 | TokenKind::Senpai
//                 | TokenKind::Kouhai
//                 | TokenKind::San
//                 | TokenKind::Sama,
//                 TokenKind::Dono,
//             )
//             | (
//                 TokenKind::Dono,
//                 TokenKind::Type
//                 | TokenKind::Chan
//                 | TokenKind::Kun
//                 | TokenKind::Senpai
//                 | TokenKind::Kouhai
//                 | TokenKind::San
//                 | TokenKind::Sama,
//             ) => true,
//             _ => false,
//         };
//         first && self.inner == other.inner
//     }
// }
