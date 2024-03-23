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
impl Node for Program {
    fn token(&self) -> String {
        format!(
            "{}\n{}\n{}\n{}",
            self.main.token(),
            self.globals
                .iter()
                .map(|x| Node::token(x))
                .collect::<Vec<String>>()
                .join("\n"),
            self.classes
                .iter()
                .map(|x| x.token())
                .collect::<Vec<String>>()
                .join("\n"),
            self.functions
                .iter()
                .map(|x| x.token())
                .collect::<Vec<String>>()
                .join("\n"),
        )
    }
}

pub struct Function {
    pub name: Token,
    pub params: Vec<Param>,
    pub body: Body,
}
impl Node for Function {
    fn token(&self) -> String {
        format!(
            "{}({})\n{}",
            self.name,
            self.params
                .iter()
                .map(|x| x.token())
                .collect::<Vec<String>>()
                .join(", "),
            self.body.token(),
        )
    }
}

pub struct Class {
    pub name: Token,
    pub params: Vec<Param>,
    pub fields: Vec<Token>,
    pub methods: Vec<Function>,
}
impl Node for Class {
    fn token(&self) -> String {
        format!(
            "{}({})\n{}\n{}",
            self.name,
            self.params
                .iter()
                .map(|x| x.token())
                .collect::<Vec<String>>()
                .join(", "),
            self.fields
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join("\n"),
            self.methods
                .iter()
                .map(|x| x.token())
                .collect::<Vec<String>>()
                .join("\n"),
        )
    }
}

pub struct Param {
    pub name: Token,
    pub dtype: Token,
}
impl Node for Param {
    fn token(&self) -> String {
        format!("{}-{}", self.name, self.dtype)
    }
}

pub struct Body {
    pub statements: Vec<Box<dyn Statement>>,
}
impl Node for Body {
    fn token(&self) -> String {
        self.statements
            .iter()
            .map(|x| x.token())
            .collect::<Vec<String>>()
            .join("\n")
    }
}

pub struct Declaration {
    pub name: Token,
    pub dtype: Token,
    pub value: Token,
    pub is_const: bool,
}
impl_statement!(Declaration {
    fn token(&self) -> String {
        format!("{}: {} = {}", self.name, self.dtype, self.value)
    }
});

pub struct ArrayDeclaration {
    pub name: Token,
    pub dtype: Token,
    pub values: Vec<Token>,
    pub is_const: bool,
}
impl_statement!(ArrayDeclaration {
    fn token(&self) -> String {
        format!(
            "{}: {} = [{}]",
            self.name,
            self.dtype,
            self.values
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
});

pub struct Assignment {
    pub name: Token,
    pub value: Token,
}
impl_statement!(Assignment {
    fn token(&self) -> String {
        format!("{} = {}", self.name, self.value)
    }
});

pub struct If {
    pub condition: Token,
    pub body: Body,
    pub else_ifs: Vec<ElseIf>,
    pub else_body: Option<Body>,
}
impl_statement!(If {
    fn token(&self) -> String {
        format!(
            "if {}:\n{}\n{}\n{}",
            self.condition,
            self.body.token(),
            self.else_ifs
                .iter()
                .map(|x| x.token())
                .collect::<Vec<String>>()
                .join("\n"),
            self.else
        )
    }
});

pub struct ElseIf {
    pub condition: Token,
    pub body: Body,
}
impl_statement!(ElseIf {
    fn token(&self) -> String {
        format!("else if {}:\n{}", self.condition, self.body.token())
    }
});

pub struct While {
    pub condition: Token,
    pub body: Body,
}
impl_statement!(While {
    fn token(&self) -> String {
        format!("while {}:\n{}", self.condition, self.body.token())
    }
});

pub struct DoWhile {
    pub condition: Token,
    pub body: Body,
}
impl_statement!(DoWhile {
    fn token(&self) -> String {
        format!("do while {}:\n{}", self.condition, self.body.token())
    }
});

pub struct For {
    pub init: Token,
    pub condition: Token,
    pub increment: Token,
    pub body: Body,
}
impl_statement!(For {
    fn token(&self) -> String {
        format!(
            "for {} in {} to {}:\n{}",
            self.init,
            self.condition,
            self.increment,
            self.body.token()
        )
    }
});

pub struct Return {
    pub value: Token,
}
impl_statement!(Return {
    fn token(&self) -> String {
        format!("return {}", self.value)
    }
});

pub struct Break {}
impl_statement!(Break {
    fn token(&self) -> String {
        "break".to_string()
    }
});

pub struct Print {
    pub values: Vec<Token>,
}
impl_statement!(Print {
    fn token(&self) -> String {
        format!("print {}", self.values.join(", "))
    }
});

pub struct Input {
    pub value: Token,
}
impl_expression!(Input {
    fn token(&self) -> String {
        format!("input {}", self.value)
    }
});

pub struct Prefix {
    pub op: Token,
    pub value: dyn Expression,
}
impl_expression!(Prefix {
    fn token(&self) -> String {
        format!("({}{})", self.op, self.value.token())
    }
});

pub struct Infix {
    pub left: Box<dyn Expression>,
    pub op: Token,
    pub right: Box<dyn Expression>,
}
impl_expression!(Infix {
    fn token(&self) -> String {
        format!("({}{}{})", self.left.token(), self.op, self.right.token())
    }
});

pub struct Postfix {
    pub value: Box<dyn Expression>,
    pub op: Token,
}
impl_expression!(Postfix {
    fn token(&self) -> String {
        format!("({}{})", self.value.token(), self.op)
    }
});
