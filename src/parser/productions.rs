use crate::lexer::token::Token;

pub trait Node {
    fn token(&self) -> String;
}
pub trait Statement {
    fn token(&self) -> String;
    fn statement(&self);
}
pub trait Expression {
    fn token(&self) -> String;
    fn expression(&self);
}
// helper macros so we don't have to repeat the same code
// since token() should be the same for all 3 traits above
macro_rules! impl_statement {
    ($T:ident { fn token(&self) -> String { $($body:tt)* } }) => {
        impl Node for $T {
            fn token(&self) -> String {
                stringify!({ $($body)* }).to_string()
            }
        }
        impl Statement for $T {
            fn token(&self) -> String {
                stringify!({ $($body)* }).to_string()
            }
            fn statement(&self) {}
        }
    };
}
macro_rules! impl_expression {
    ($T:ident { fn token(&self) -> String { $($body:tt)* } }) => {
        impl Node for $T {
            fn token(&self) -> String {
                stringify!({ $($body)* }).to_string()
            }
        }
        impl Expression for $T {
            fn token(&self) -> String {
                stringify!({ $($body)* }).to_string()
            }
            fn expression(&self) {}
        }
    };
}
pub struct Program {
    pub main: Function,
    pub globals: Vec<Declaration>,
    pub classes: Vec<Class>,
    pub functions: Vec<Function>,
}
pub struct Function {
    pub name: Token,
    pub params: Vec<Param>,
    pub body: Body,
}
pub struct Class {
    pub name: Token,
    pub params: Vec<Param>,
    pub fields: Vec<Token>,
    pub methods: Vec<Function>,
}
pub struct Param {
    pub name: Token,
    pub dtype: Token,
}
pub struct Body {
    pub statements: Vec<Box<dyn Statement>>,
}
pub struct Declaration {
    pub name: Token,
    pub dtype: Token,
    pub value: Token,
    pub is_const: bool,
}
pub struct ArrayDeclaration {
    pub name: Token,
    pub dtype: Token,
    pub values: Vec<Token>,
    pub is_const: bool,
}
pub struct Assignment {
    pub name: Token,
    pub value: Token,
}
pub struct If {
    pub condition: Token,
    pub body: Body,
}
pub struct While {
    pub condition: Token,
    pub body: Body,
}
pub struct DoWhile {
    pub condition: Token,
    pub body: Body,
}
pub struct For {
    pub init: Token,
    pub condition: Token,
    pub increment: Token,
    pub body: Body,
}
pub struct Return {
    pub value: Token,
}
pub struct Break {}
pub struct Print {
    pub values: Vec<Token>,
}
pub struct Input {
    pub value: Token,
}
pub struct Prefix {
    pub op: Token,
    pub value: dyn Expression,
}
pub struct Infix {
    pub left: Box<dyn Expression>,
    pub op: Token,
    pub right: Box<dyn Expression>,
}
pub struct Postfix {
    pub value: Box<dyn Expression>,
    pub op: Token,
}
