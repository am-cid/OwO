//! This module is for enums that group certain [Production]s into different contexts
//! where identifiers are used
//!
//! [Identifier]
//! - general ident enum
//! - used in situations where it does not matter the context, all idents are allowed
//!
//! [Accessor]
//! - can be used to access fields/methods: `aqua.field`
//! - difference with [Identifier] is that [Accessor] cannot be [Accessor]
//!
//! [Assignable]
//! - can be assigned to: `aqua = 1~`
//! - does not include [FnCall]: `aqua() = 1~` is invalid
//!
//! [Indexable]
//! - can be indexed into: `aqua[0]~`
//! - [Indexable] cannot be [Indexable]
use crate::lexer::token::Token;
use crate::parser::productions::{
    AccessType, FieldAccess, FnCall, GroupAccess, IndexedId, Production,
};

/// General ident enum used in situations where it does not matter the context; all idents are allowed
#[derive(Clone, Debug)]
pub enum Identifier {
    Token(Token),
    FnCall(FnCall),
    Indexed(IndexedId),
    Access(AccessType),
}
impl<'a> Production<'a> for Identifier {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        match self {
            Self::Token(decl) => decl.to_string(source, n),
            Self::FnCall(fn_call) => fn_call.to_string(source, n),
            Self::Indexed(idx) => idx.to_string(source, n),
            Self::Access(access) => access.to_string(source, n),
        }
    }
}

/// Productions that can be identifiers for LHS assignment statements
/// - `aqua = 1~`
/// - `aqua[1] = "something"~`
/// - `aqua.age = 18~`
#[derive(Clone, Debug)]
pub enum Assignable {
    Token(Token),
    Indexed(IndexedId),
    Access(GroupAccess<FieldAccess>),
}
impl<'a> Production<'a> for Assignable {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        match self {
            Self::Token(id) => id.to_string(source, n),
            Self::Indexed(idx) => idx.to_string(source, n),
            Self::Access(access) => access.to_string(source, n),
        }
    }
}

/// Productions that can be indexed into
/// - `aqua[1]`
/// - `aqua()[1]`
#[derive(Clone, Debug)]
pub enum Indexable {
    Token(Token),
    FnCall(FnCall),
}
impl<'a> Production<'a> for Indexable {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        match self {
            Self::Token(tok) => tok.to_string(source, n),
            Self::FnCall(fn_call) => fn_call.to_string(source, n),
        }
    }
}

/// Productions that can access class fields/methods
/// - `aqua.arms[1].wind_up().punch()~`
#[derive(Clone, Debug)]
pub enum Accessor {
    Token(Token),
    FnCall(FnCall),
    IndexedId(IndexedId),
}
impl<'a> Production<'a> for Accessor {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        match self {
            Self::Token(tok) => tok.to_string(source, n),
            Self::FnCall(fn_call) => fn_call.to_string(source, n),
            Self::IndexedId(id) => id.to_string(source, n),
        }
    }
}
impl Default for Accessor {
    fn default() -> Self {
        Self::Token(Token::default())
    }
}
