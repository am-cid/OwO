use crate::lexer::token::{Offset, Token, TokenKind};
use crate::parser::data_types::DataType;
use crate::parser::identifiers::{Accessor, Assignable, Identifier, Indexable};
use crate::utils::string::StringExt;

/// Max width of the [FnCall] args before falling back to vertical formatting.
/// Applies as well to :
/// - [GroupAccess<Method>]
/// - [IndexedId]
/// - [Pipeline]
const MAX_LINE_LENGTH: usize = 60;

pub trait Production<'a> {
    fn to_string(&self, source: &'a str, n: usize) -> String;
    // fn transpile(&self, indent: usize) -> String;
}
impl<'a> Production<'a> for Token {
    fn to_string(&self, source: &'a str, _n: usize) -> String {
        self.str_from_source(source).to_string()
        // self.text.to_string()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Default, Copy)]
pub struct ProductionSpan {
    pub abs: Offset,
    pub line: usize,
    pub col: usize,
}

/// Statements define actions and logic
#[derive(Clone, Debug)]
pub enum Statement {
    Declaration(Declaration),
    Assignment(Assignment),
    If(IfStatement),
    ForLoop(ForLoop),
    ForEach(ForEach),
    Mash(MashStatement),
    Break(Token),
    Continue(Token),
    FnCall(FnCall),
    Method(GroupAccess<MethodAccess>),
    Pipeline(Pipeline),
    Return(ReturnStatement),
}
impl<'a> Production<'a> for Statement {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        match self {
            Self::Declaration(res) => res.to_string(source, n).indent(n),
            Self::Assignment(res) => res.to_string(source, n).indent(n),
            Self::If(res) => res.to_string(source, n).indent(n),
            Self::Mash(res) => res.to_string(source, n).indent(n),
            Self::ForLoop(res) => res.to_string(source, n).indent(n),
            Self::ForEach(res) => res.to_string(source, n).indent(n),
            Self::Break(res) => res.to_string(source, n).indent(n) + "~",
            Self::Continue(res) => res.to_string(source, n).indent(n) + "~",
            Self::FnCall(res) => res.to_string(source, n).indent(n) + "~",
            Self::Method(res) => res.to_string(source, n).indent(n) + "~",
            Self::Pipeline(res) => res.to_string(source, n).indent(n) + "~",
            Self::Return(res) => res.to_string(source, n).indent(n),
        }
    }
}

/// Expressions define computations and operations that generate values
#[derive(Clone, Debug)]
pub enum Expression {
    Ident(Identifier),
    GroupInit(GroupInit),
    Pipeline(Pipeline),
    Array(ArrayLiteral),
    Set(SetLiteral),
    Map(MapLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Grouped(GroupedExpression),
}
impl<'a> Production<'a> for Expression {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        match self {
            Self::Ident(res) => res.to_string(source, n),
            Self::GroupInit(res) => res.to_string(source, n),
            Self::Pipeline(res) => res.to_string(source, n),
            Self::Array(res) => res.to_string(source, n),
            Self::Set(res) => res.to_string(source, n),
            Self::Map(res) => res.to_string(source, n),
            Self::Prefix(res) => res.to_string(source, n),
            Self::Infix(res) => res.to_string(source, n),
            Self::Grouped(res) => res.to_string(source, n),
        }
    }
}

/// Root node of the AST
#[derive(Clone, Debug, Default)]
pub struct Program {
    pub main: Function,
    pub functions: Vec<Function>,
    pub groups: Vec<Group>,
    pub methods: Vec<GroupMethod>,
    pub contracts: Vec<Contract>,
    pub globals: Vec<Statement>,
}
impl<'a> Production<'a> for Program {
    fn to_string(&self, source: &'a str, _n: usize) -> String {
        format!(
            "{}{}{}{}{}{}",
            match &self.globals.len() {
                0 => "".to_string(),
                _ => self
                    .globals
                    .iter()
                    .map(|global| global.to_string(source, 0) + "\n")
                    .collect::<Vec<_>>()
                    .join(""),
            },
            self.main.to_string(source, 0) + "\n",
            match &self.functions.len() {
                0 => "".to_string(),
                _ => self
                    .functions
                    .iter()
                    .map(|func| func.to_string(source, 0) + "\n")
                    .collect::<Vec<_>>()
                    .join(""),
            },
            match &self.groups.len() {
                0 => "".to_string(),
                _ => self
                    .groups
                    .iter()
                    .map(|group| group.to_string(source, 0) + "\n")
                    .collect::<Vec<_>>()
                    .join(""),
            },
            match &self.methods.len() {
                0 => "".to_string(),
                _ => self
                    .methods
                    .iter()
                    .map(|group| group.to_string(source, 0) + "\n")
                    .collect::<Vec<_>>()
                    .join(""),
            },
            match &self.contracts.len() {
                0 => "".to_string(),
                _ => self
                    .contracts
                    .iter()
                    .map(|contract| contract.to_string(source, 0) + "\n")
                    .collect::<Vec<_>>()
                    .join(""),
            },
        )
        .trim()
        .to_string()
    }
}

/*
 * GLOBAL PRODUCTIONS
 */
#[derive(Clone, Debug, Default)]
pub struct Function {
    pub id: Token,
    pub dtype: DataType,
    pub params: Vec<Param>,
    pub body: Body,
}
impl Function {
    pub fn signature(&self) -> FnSignature {
        FnSignature {
            id: self.id,
            dtype: self.dtype.clone(),
            params: self
                .params
                .clone()
                .into_iter()
                .map(|param| param.dtype)
                .collect::<Vec<DataType>>(),
        }
    }
}
impl<'a> Production<'a> for Function {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "fun {}-{}({} {{\n{}\n}}",
            self.id.str_from_source(source),
            self.dtype.to_string(source, 0),
            match &self
                .params
                .iter()
                .map(|v| v.to_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => format!(
                    "{})",
                    self.params
                        .iter()
                        .map(|param| param.to_string(source, n))
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                _ => format!(
                    "{}{}",
                    format!(
                        "\n{}",
                        self.params
                            .iter()
                            .map(|param| param.to_string(source, n + 1).indent(n + 1) + ",")
                            .collect::<Vec<_>>()
                            .join("\n"),
                    ),
                    format!("\n{}", ")".indent(n),),
                ),
            },
            self.body.to_string(source, n + 1),
        )
    }
}

#[derive(Clone, Debug)]
pub struct Group {
    pub id: Token,
    pub contracts: Vec<Token>,
    pub fields: Vec<GroupField>,
}
impl<'a> Production<'a> for Group {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "gwoup {}{} {{\n{}\n}}",
            self.id.str_from_source(source),
            match &self
                .contracts
                .iter()
                .map(|v| v.to_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0 => "".to_string(),
                1..=MAX_LINE_LENGTH => format!(
                    " [{}]",
                    self.contracts
                        .iter()
                        .map(|contract| contract.to_string(source, n))
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                _ => format!(
                    " [{}{}",
                    format!(
                        "\n{}",
                        self.contracts
                            .iter()
                            .map(|contract| contract.to_string(source, n + 1).indent(n + 1) + ",")
                            .collect::<Vec<_>>()
                            .join("\n"),
                    ),
                    format!("\n{}", "]".indent(n),),
                ),
            },
            self.fields
                .iter()
                .map(|field| format!("{}", field.to_string(source, n + 1).indent(n + 1)))
                .collect::<Vec<_>>()
                .join("\n"),
        )
    }
}

#[derive(Clone, Debug, Default)]
pub struct GroupMethod {
    pub id: Token,
    pub group: Token,
    pub mutable: bool,
    pub dtype: DataType,
    pub params: Vec<Param>,
    pub body: Body,
}
impl<'a> Production<'a> for GroupMethod {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "fun {}{} {}-{}({} {{\n{}\n}}",
            self.group.str_from_source(source),
            if self.mutable { "!" } else { "" },
            self.id.str_from_source(source),
            self.dtype.to_string(source, 0),
            match &self
                .params
                .iter()
                .map(|v| v.to_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => format!(
                    "{})",
                    self.params
                        .iter()
                        .map(|param| param.to_string(source, n))
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                _ => format!(
                    "{}{}",
                    format!(
                        "\n{}",
                        self.params
                            .iter()
                            .map(|param| param.to_string(source, n + 1).indent(n + 1) + ",")
                            .collect::<Vec<_>>()
                            .join("\n"),
                    ),
                    format!("\n{}", ")".indent(n),),
                ),
            },
            self.body.to_string(source, n + 1),
        )
    }
}

#[derive(Clone, Debug)]
pub struct Contract {
    pub id: Token,
    pub signatures: Vec<FnSignature>,
}
impl<'a> Production<'a> for Contract {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "contwact {} {{\n{}\n}}",
            self.id.str_from_source(source),
            self.signatures
                .iter()
                .map(|signature| signature.to_string(source, n + 1))
                .collect::<Vec<_>>()
                .join("\n"),
        )
    }
}

#[derive(Clone, Debug, Default)]
pub struct FnSignature {
    pub id: Token,
    pub dtype: DataType,
    pub params: Vec<DataType>,
}
impl<'a> Production<'a> for FnSignature {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}-{}({}~",
            self.id.str_from_source(source),
            self.dtype.to_string(source, 0),
            match &self
                .params
                .iter()
                .map(|v| v.to_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => {
                    format!(
                        "{})",
                        self.params
                            .iter()
                            .map(|dtype| dtype.to_string(source, n))
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
                }
                _ => {
                    format!(
                        "{}{}",
                        format!(
                            "\n{}",
                            self.params
                                .iter()
                                .map(|dtype| dtype.to_string(source, n + 1).indent(n + 1) + ",")
                                .collect::<Vec<_>>()
                                .join("\n"),
                        ),
                        format!("\n{}", ")".indent(n),),
                    )
                }
            }
        )
        .indent(n)
    }
}
// impl PartialEq for FnSignature {
//     fn eq(&self, other: &Self) -> bool {
//         self.id == other.id && self.dtype == other.dtype && self.params == other.params
//     }
// }

#[derive(Clone, Debug)]
pub struct Param {
    pub id: Token,
    pub dtype: DataType,
    pub variadic: bool,
}
impl<'a> Production<'a> for Param {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}-{}{}",
            self.id.str_from_source(source),
            self.dtype.to_string(source, n),
            if self.variadic { "..." } else { "" }
        )
    }
}

#[derive(Clone, Debug)]
pub struct GroupField {
    pub id: Token,
    pub dtype: DataType,
}
impl<'a> Production<'a> for GroupField {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}-{}~",
            self.id.str_from_source(source),
            self.dtype.to_string(source, n),
        )
    }
}

#[derive(Clone, Debug, Default)]
pub struct Body {
    pub statements: Vec<Statement>,
}
impl<'a> Production<'a> for Body {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        self.statements
            .iter()
            .map(|stmt| stmt.to_string(source, n))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

/*
 * STATEMENT PRODUCTIONS
 */
#[derive(Clone, Debug)]
pub struct Declaration {
    pub id: Token,
    pub dtype: DataType,
    pub mutable: bool,
    pub optional: bool,
    pub expr: Expression,
}
impl<'a> Production<'a> for Declaration {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "hi {}-{}{}{} = {}~",
            self.id.str_from_source(source),
            self.dtype.to_string(source, n),
            if self.mutable { "!" } else { "" },
            if self.optional { "?" } else { "" },
            self.expr.to_string(source, n),
        )
    }
}

#[derive(Clone, Debug)]
pub struct Assignment {
    pub id: Assignable,
    pub dtype: Option<Token>,
    pub assign_op: Token,
    pub expr: Expression,
}
impl<'a> Production<'a> for Assignment {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{} {} {}~",
            self.id.to_string(source, n),
            self.assign_op.to_string(source, 0),
            self.expr.to_string(source, n + 1),
        )
    }
}

#[derive(Clone, Debug)]
pub struct IfStatement {
    pub condition: Expression,
    pub body: Body,
    pub elifs: Vec<ElifStatement>,
    pub else_block: Option<Body>,
}
impl<'a> Production<'a> for IfStatement {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "iwf {} {{\n{}\n{}{}\n{}",
            self.condition.to_string(source, n),
            self.body.to_string(source, n + 1),
            match (&self.elifs.len(), &self.else_block) {
                (0, None) => "}".indent(n),
                _ => "".to_string(),
            },
            self.elifs
                .iter()
                .map(|elif| elif.to_string(source, n))
                .collect::<Vec<_>>()
                .join("\n"),
            match &self.else_block {
                Some(block) => format!(
                    "{} {{\n{}\n{}",
                    "} ewse".indent(n),
                    block.to_string(source, n + 1),
                    "}".indent(n),
                ),
                None => "}".indent(n),
            },
        )
    }
}

#[derive(Clone, Debug)]
pub struct ElifStatement {
    pub condition: Expression,
    pub body: Body,
}
impl<'a> Production<'a> for ElifStatement {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{} {} {{\n{}",
            "} ewif".indent(n),
            self.condition.to_string(source, n),
            self.body.to_string(source, n + 1),
        )
    }
}

#[derive(Clone, Debug)]
pub struct ForLoop {
    pub init: Declaration,
    pub condition: Expression,
    pub update: Expression,
    pub body: Body,
}
impl<'a> Production<'a> for ForLoop {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            // TODO: format this so if too long, separate init cond and update by newlines
            "fow {} {}~ {} {{\n{}\n{}",
            self.init.to_string(source, n),
            self.condition.to_string(source, n),
            self.update.to_string(source, n),
            self.body.to_string(source, n + 1),
            "}".indent(n),
        )
    }
}

#[derive(Clone, Debug)]
pub struct ForEach {
    pub item_id: Token,
    pub collection: Expression,
    pub body: Body,
}
impl<'a> Production<'a> for ForEach {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "fow {} in {} {{\n{}\n{}",
            self.item_id.to_string(source, 0),
            self.collection.to_string(source, n),
            self.body.to_string(source, n + 1),
            "}".indent(n),
        )
    }
}

#[derive(Clone, Debug)]
pub struct MashStatement {
    pub expr: Expression,
    pub cases: Vec<Case>,
    pub default: Option<Body>,
}
impl<'a> Production<'a> for MashStatement {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "mash {} {{\n{}{}{}",
            self.expr.to_string(source, n),
            self.cases
                .iter()
                .map(|case| case.to_string(source, n))
                .collect::<Vec<_>>()
                .join(""),
            match &self.default {
                Some(default) => {
                    format!(
                        "{}:{}\n",
                        "default".indent(n),
                        match default.statements.len() {
                            0 => unreachable!("Empty bodies aren't allowed during parsing"),
                            1 => " ".to_string() + default.to_string(source, n + 1).trim(),
                            _ => format!("\n{}", default.to_string(source, n + 1)),
                        }
                    )
                }
                None => "".to_string(),
            },
            "}".indent(n)
        )
    }
}

#[derive(Clone, Debug)]
pub struct Case {
    pub case_type: DataType,
    pub body: Body,
}
impl<'a> Production<'a> for Case {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}:{}\n",
            self.case_type.to_string(source, 0).indent(n),
            match &self.body.statements.len() {
                0 => unreachable!("Empty bodies aren't allowed during parsing"),
                1 => " ".to_string() + self.body.to_string(source, n + 1).trim(),
                _ => format!("\n{}", self.body.to_string(source, n + 1)),
            }
        )
    }
}

#[derive(Clone, Debug)]
pub struct FnCall {
    pub id: Token,
    pub args: Vec<Expression>,
    pub signature: FnSignature,
}
impl<'a> Production<'a> for FnCall {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}({}",
            self.id.str_from_source(source),
            match &self
                .args
                .iter()
                .map(|v| v.to_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => {
                    format!(
                        "{})",
                        self.args
                            .iter()
                            .map(|arg| arg.to_string(source, n))
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
                }
                _ => {
                    format!(
                        "{}{}",
                        format!(
                            "\n{}",
                            self.args
                                .iter()
                                .map(|arg| arg.to_string(source, n + 1).indent(n + 1) + ",")
                                .collect::<Vec<_>>()
                                .join("\n"),
                        ),
                        format!("\n{}", ")".indent(n),),
                    )
                }
            }
        )
    }
}

#[derive(Clone, Debug)]
pub enum Callable {
    Fn(FnCall),
    Method(GroupAccess<MethodAccess>),
}
impl<'a> Production<'a> for Callable {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        match self {
            Self::Fn(call) => call.to_string(source, n),
            Self::Method(method) => method.to_string(source, n),
        }
    }
}

#[derive(Clone, Debug)]
pub struct GroupInit {
    pub id: Token,
    pub args: Vec<Expression>,
}
impl<'a> Production<'a> for GroupInit {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}({}",
            self.id.str_from_source(source),
            match &self
                .args
                .iter()
                .map(|v| v.to_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => {
                    format!(
                        "{})",
                        self.args
                            .iter()
                            .map(|arg| arg.to_string(source, n))
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
                }
                _ => {
                    format!(
                        "{}{}",
                        format!(
                            "\n{}",
                            self.args
                                .iter()
                                .map(|arg| arg.to_string(source, n + 1).indent(n + 1) + ",")
                                .collect::<Vec<_>>()
                                .join("\n"),
                        ),
                        format!("\n{}", ")".indent(n),),
                    )
                }
            }
        )
    }
}
#[derive(Clone, Debug)]
pub struct Pipeline {
    pub first: Box<Identifier>,
    pub rest: Vec<Callable>,
}
impl<'a> Production<'a> for Pipeline {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}{}",
            self.first.to_string(source, n),
            match &self
                .rest
                .iter()
                .map(|v| v.to_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0 =>
                    unreachable!("Pipelines must always have other callables parsed during parsing"),
                1..=MAX_LINE_LENGTH => format!(
                    " | {}",
                    self.rest
                        .iter()
                        .map(|v| v.to_string(source, n))
                        .collect::<Vec<_>>()
                        .join(" | "),
                ),
                _ => format!(
                    "\n{}",
                    self.rest
                        .iter()
                        .map(|v| format!("| {}", v.to_string(source, n)).indent(n))
                        .collect::<Vec<_>>()
                        .join("\n"),
                ),
            }
        )
    }
    // fn transpile(&self, n: usize) -> String {
    //     let mut res = self.first.indented_string(0);
    //     for call in self.rest.iter() {
    //         match call {
    //             Callable::Fn(call) => {
    //                 res = format!(
    //                     "{}({}, {})",
    //                     call.id,
    //                     res,
    //                     call.args
    //                         .iter()
    //                         .map(|arg| arg.indented_string(0))
    //                         .collect::<Vec<_>>()
    //                         .join(", "),
    //                 );
    //             }
    //             Callable::Method(method) => {
    //                 let last = method.last();
    //                 res = format!(
    //                     "{}.{}({}, {})",
    //                     method
    //                         .accessed
    //                         .iter()
    //                         .take(method.accessed.iter().count() - 1)
    //                         .map(|accessed| accessed.indented_string(0))
    //                         .collect::<Vec<_>>()
    //                         .join("."),
    //                     last.id,
    //                     res,
    //                     last.args
    //                         .iter()
    //                         .map(|arg| arg.indented_string(0))
    //                         .collect::<Vec<_>>()
    //                         .join(", "),
    //                 )
    //             }
    //         }
    //     }
    //     res.indent(n)
    // }
}

#[derive(Clone, Debug)]
pub struct ReturnStatement {
    pub expr: Expression,
}
impl<'a> Production<'a> for ReturnStatement {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!("wetuwn {}~", self.expr.to_string(source, n))
    }
}

/*
 * EXPRESSION UNITS
 */
#[derive(Clone, Debug)]
pub struct IndexedId {
    pub id: Indexable,
    pub indices: Vec<Expression>,
}
impl<'a> Production<'a> for IndexedId {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}[{}",
            self.id.to_string(source, n),
            match &self
                .indices
                .iter()
                .map(|v| v.to_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => format!(
                    "{}]",
                    self.indices
                        .iter()
                        .map(|idx| idx.to_string(source, n))
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                _ => {
                    format!(
                        "{}{}",
                        format!(
                            "\n{}",
                            self.indices
                                .iter()
                                .map(|arg| arg.to_string(source, n + 1).indent(n + 1) + ",")
                                .collect::<Vec<_>>()
                                .join("\n"),
                        ),
                        format!("\n{}", "]".indent(n),),
                    )
                }
            }
        )
    }
}

#[derive(Clone, Debug)]
pub enum AccessType {
    Field(GroupAccess<FieldAccess>),
    Method(GroupAccess<MethodAccess>),
}
impl<'a> Production<'a> for AccessType {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        match self {
            Self::Method(method) => method.to_string(source, n),
            Self::Field(field) => field.to_string(source, n),
        }
    }
}

/*
 * COLLETION LITERALS
 */
#[derive(Clone, Debug)]
pub struct ArrayLiteral {
    pub exprs: Vec<Expression>,
}
impl<'a> Production<'a> for ArrayLiteral {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "[{}",
            match &self
                .exprs
                .iter()
                .map(|v| v.to_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => {
                    format!(
                        "{}]",
                        self.exprs
                            .iter()
                            .map(|expr| expr.to_string(source, n))
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
                }
                _ => {
                    format!(
                        "{}{}",
                        format!(
                            "\n{}",
                            self.exprs
                                .iter()
                                .map(|expr| expr.to_string(source, n + 1).indent(n + 1) + ",")
                                .collect::<Vec<_>>()
                                .join("\n"),
                        ),
                        format!("\n{}", "]".indent(n),),
                    )
                }
            },
        )
    }
}

#[derive(Clone, Debug)]
pub struct SetLiteral {
    pub exprs: Vec<Expression>,
}
impl<'a> Production<'a> for SetLiteral {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "#[{}",
            match &self
                .exprs
                .iter()
                .map(|v| v.to_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => {
                    format!(
                        "{}]",
                        self.exprs
                            .iter()
                            .map(|expr| expr.to_string(source, n))
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
                }
                _ => {
                    format!(
                        "{}{}",
                        format!(
                            "\n{}",
                            self.exprs
                                .iter()
                                .map(|expr| expr.to_string(source, n + 1).indent(n + 1) + ",")
                                .collect::<Vec<_>>()
                                .join("\n"),
                        ),
                        format!("\n{}", "]".indent(n),),
                    )
                }
            },
        )
    }
}

#[derive(Clone, Debug)]
pub struct MapLiteral {
    pub exprs: Vec<(Expression, Expression)>,
}
impl<'a> Production<'a> for MapLiteral {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "#[{}",
            if self.exprs.len() == 0 {
                ":".to_string()
            } else {
                match &self
                    .exprs
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k.to_string(source, n), v.to_string(source, n)))
                    .collect::<Vec<_>>()
                    .join("")
                    .len()
                {
                    0..=MAX_LINE_LENGTH => {
                        format!(
                            "{}]",
                            self.exprs
                                .iter()
                                .map(|(k, v)| format!(
                                    "{}: {}",
                                    k.to_string(source, n),
                                    v.to_string(source, n)
                                ))
                                .collect::<Vec<_>>()
                                .join(", "),
                        )
                    }
                    _ => {
                        format!(
                            "{}{}",
                            format!(
                                "\n{}",
                                self.exprs
                                    .iter()
                                    .map(|(k, v)| format!(
                                        "{}: {}",
                                        k.to_string(source, n + 1),
                                        v.to_string(source, n + 1)
                                    )
                                    .indent(n + 1)
                                        + ",")
                                    .collect::<Vec<_>>()
                                    .join("\n"),
                            ),
                            format!("\n{}", "]".indent(n),),
                        )
                    }
                }
            }
        )
    }
}

/*
 * EXPRESSIONS WITH OPERATORS
 */
#[derive(Clone, Debug)]
pub struct PrefixExpression {
    pub op: Token,
    pub right: Box<Expression>,
}
impl<'a> Production<'a> for PrefixExpression {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        match &self.op.kind {
            TokenKind::Dash => format!(
                "{}{}",
                self.op.str_from_source(source),
                self.right.to_string(source, n)
            ),
            _ => format!(
                "{} {}",
                self.op.str_from_source(source),
                self.right.to_string(source, n)
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub op: Token,
    pub right: Box<Expression>,
}
impl<'a> Production<'a> for InfixExpression {
    /// TODO: use better logic than this rudimentary implementation
    /// it currenly indents way too much if combined LR exceeds [MAX_LINE_LENGTH] chars
    fn to_string(&self, source: &'a str, n: usize) -> String {
        match self.left.to_string(source, 0).len() + self.right.to_string(source, 0).len() {
            0..=MAX_LINE_LENGTH => {
                format!(
                    "({} {} {})",
                    self.left.to_string(source, n),
                    self.op.str_from_source(source),
                    self.right.to_string(source, n)
                )
            }
            _ => {
                format!(
                    "{}\n{} {}",
                    self.left.to_string(source, n),
                    self.op.to_string(source, n + 1).indent(n + 1),
                    self.right.to_string(source, n + 1)
                )
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct GroupedExpression {
    pub expr: Box<Expression>,
}
impl<'a> Production<'a> for GroupedExpression {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        match &self.expr.to_string(source, 0).len() {
            0..=MAX_LINE_LENGTH => {
                format!("{}", self.expr.to_string(source, n))
            }
            _ => {
                format!("\n{}\n", self.expr.to_string(source, n + 1).indent(n + 1))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct FieldAccess;
#[derive(Clone, Debug)]
pub struct MethodAccess;
#[derive(Clone, Debug)]
pub struct GroupAccess<AccessType> {
    pub accessed: Vec<Accessor>,
    pub access_type: std::marker::PhantomData<AccessType>,
}
impl<'a, T> Production<'a> for GroupAccess<T> {
    fn to_string(&self, source: &'a str, n: usize) -> String {
        match self.accessed.len() {
            0..=1 => unreachable!(),
            _ => {
                let first = self.accessed.first().unwrap();
                let rest = self.accessed.iter().skip(1).collect::<Vec<_>>();
                format!(
                    "{}{}",
                    first.to_string(source, n),
                    match &rest
                        .iter()
                        .map(|v| v.to_string(source, 0))
                        .collect::<Vec<_>>()
                        .join("")
                        .len()
                    {
                        0 => "".to_string(),
                        1..=MAX_LINE_LENGTH => format!(
                            ".{}",
                            rest.iter()
                                .map(|v| v.to_string(source, 0))
                                .collect::<Vec<_>>()
                                .join("."),
                        ),
                        _ => format!(
                            "\n{}",
                            rest.iter()
                                .map(|v| format!(".{}", v.to_string(source, n)).indent(n))
                                .collect::<Vec<_>>()
                                .join("\n"),
                        ),
                    }
                )
            }
        }
    }
}
impl GroupAccess<MethodAccess> {
    pub fn last(&self) -> FnCall {
        match self.accessed.last().cloned() {
            Some(Accessor::FnCall(call)) => call,
            _ => unreachable!("MethodAccess always has fn call as its last during parsing"),
        }
    }
}
