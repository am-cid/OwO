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
    AccessType, FieldAccess, FnCall, GroupAccess, IndexedId, Production, Range,
};

/// General ident enum used in situations where it does not matter the context; all idents are allowed
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Identifier {
    Token(Token),
    FnCall(FnCall),
    Indexed(IndexedId),
    Access(AccessType),
}
impl Production for Identifier {
    fn range(&self) -> Range {
        match self {
            Self::Token(decl) => decl.range(),
            Self::FnCall(fn_call) => fn_call.range(),
            Self::Indexed(idx) => idx.range(),
            Self::Access(access) => access.range(),
        }
    }
    fn string(&self, n: usize) -> String {
        match self {
            Self::Token(decl) => decl.string(n),
            Self::FnCall(fn_call) => fn_call.string(n),
            Self::Indexed(idx) => idx.string(n),
            Self::Access(access) => access.string(n),
        }
    }
}

/// Productions that can be identifiers for LHS assignment statements
/// - `aqua = 1~`
/// - `aqua[1] = "something"~`
/// - `aqua.age = 18~`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Assignable {
    Token(Token),
    Indexed(IndexedId),
    Access(GroupAccess<FieldAccess>),
}
impl Production for Assignable {
    fn range(&self) -> Range {
        match self {
            Self::Token(id) => id.range(),
            Self::Indexed(idx) => idx.range(),
            Self::Access(access) => access.range(),
        }
    }
    fn string(&self, indent: usize) -> String {
        match self {
            Self::Token(id) => id.string(indent),
            Self::Indexed(idx) => idx.string(indent),
            Self::Access(access) => access.string(indent),
        }
    }
}

/// Productions that can be indexed into
/// - `aqua[1]`
/// - `aqua()[1]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Indexable {
    Token(Token),
    FnCall(FnCall),
}
impl Production for Indexable {
    fn range(&self) -> Range {
        match self {
            Self::Token(tok) => tok.range(),
            Self::FnCall(fn_call) => fn_call.range(),
        }
    }
    fn string(&self, indent: usize) -> String {
        match self {
            Self::Token(tok) => tok.string(indent),
            Self::FnCall(fn_call) => fn_call.string(indent),
        }
    }
}

/// Productions that can access class fields/methods
/// - `aqua.arms[1].wind_up().punch()~`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Accessor {
    Token(Token),
    FnCall(FnCall),
    IndexedId(IndexedId),
}
impl Production for Accessor {
    fn range(&self) -> Range {
        match self {
            Self::Token(tok) => tok.range(),
            Self::FnCall(fn_call) => fn_call.range(),
            Self::IndexedId(id) => id.range(),
        }
    }
    fn string(&self, indent: usize) -> String {
        match self {
            Self::Token(tok) => tok.string(indent),
            Self::FnCall(fn_call) => fn_call.string(indent),
            Self::IndexedId(id) => id.string(indent),
        }
    }
}
impl Default for Accessor {
    fn default() -> Self {
        Self::Token(Token::default())
    }
}
