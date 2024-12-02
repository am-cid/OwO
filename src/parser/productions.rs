use crate::lexer::token::{Token, TokenKind};
use crate::utils::string::StringExt;

/// Max width of the [FnCall] args before falling back to vertical formatting.
/// Applies as well to :
/// - [GroupAccess<Method>]
/// - [IndexedId]
/// - [Pipeline]
const MAX_LINE_LENGTH: usize = 60;

pub trait Production {
    fn range(&self) -> Range;
    fn string(&self, n: usize) -> String;
    // fn transpile(&self, indent: usize) -> String;
}
impl Production for Token {
    fn range(&self) -> Range {
        Range::new(self.pos, self.end_pos)
    }
    fn string(&self, _: usize) -> String {
        self.text.to_string()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Default, Copy)]
pub struct Range {
    pub start: (usize, usize),
    pub end: (usize, usize),
}
impl Range {
    pub fn new(start: (usize, usize), end: (usize, usize)) -> Self {
        Self { start, end }
    }
}

/// Statements define actions and logic
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
impl Production for Statement {
    fn range(&self) -> Range {
        match self {
            Self::Declaration(res) => res.range(),
            Self::Assignment(res) => res.range(),
            Self::If(res) => res.range(),
            Self::Mash(res) => res.range(),
            Self::ForLoop(res) => res.range(),
            Self::ForEach(res) => res.range(),
            Self::Break(res) => res.range(),
            Self::Continue(res) => res.range(),
            Self::FnCall(res) => res.range(),
            Self::Method(res) => res.range(),
            Self::Pipeline(res) => res.range(),
            Self::Return(res) => res.range(),
        }
    }
    fn string(&self, n: usize) -> String {
        match self {
            Self::Declaration(res) => res.string(n).indent(n),
            Self::Assignment(res) => res.string(n).indent(n),
            Self::If(res) => res.string(n).indent(n),
            Self::Mash(res) => res.string(n).indent(n),
            Self::ForLoop(res) => res.string(n).indent(n),
            Self::ForEach(res) => res.string(n).indent(n),
            Self::Break(res) => res.string(n).indent(n) + "~",
            Self::Continue(res) => res.string(n).indent(n) + "~",
            Self::FnCall(res) => res.string(n).indent(n) + "~",
            Self::Method(res) => res.string(n).indent(n) + "~",
            Self::Pipeline(res) => res.string(n).indent(n) + "~",
            Self::Return(res) => res.string(n).indent(n),
        }
    }
}
/// Root node of the AST
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Program {
    pub main: Function,
    pub functions: Vec<Function>,
    pub groups: Vec<Group>,
    pub methods: Vec<GroupMethod>,
    pub contracts: Vec<Contract>,
    pub globals: Vec<Statement>,
    pub range: Range,
}
impl Production for Program {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, _: usize) -> String {
        format!(
            "{}{}{}{}{}{}",
            match &self.globals.len() {
                0 => "".to_string(),
                _ => self
                    .globals
                    .iter()
                    .map(|global| global.string(0) + "\n")
                    .collect::<Vec<_>>()
                    .join(""),
            },
            self.main.string(0) + "\n",
            match &self.functions.len() {
                0 => "".to_string(),
                _ => self
                    .functions
                    .iter()
                    .map(|func| func.string(0) + "\n")
                    .collect::<Vec<_>>()
                    .join(""),
            },
            match &self.groups.len() {
                0 => "".to_string(),
                _ => self
                    .groups
                    .iter()
                    .map(|group| group.string(0) + "\n")
                    .collect::<Vec<_>>()
                    .join(""),
            },
            match &self.methods.len() {
                0 => "".to_string(),
                _ => self
                    .methods
                    .iter()
                    .map(|group| group.string(0) + "\n")
                    .collect::<Vec<_>>()
                    .join(""),
            },
            match &self.contracts.len() {
                0 => "".to_string(),
                _ => self
                    .contracts
                    .iter()
                    .map(|contract| contract.string(0) + "\n")
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
#[derive(Clone, Debug, Default, Eq, Hash)]
pub struct Function {
    pub id: Token,
    pub dtype: DataType,
    pub params: Vec<Param>,
    pub body: Body,
    pub range: Range,
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
            range: self.range,
        }
    }
}
impl Production for Function {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "fun {}-{}({} {{\n{}\n}}",
            self.id,
            self.dtype.string(0),
            match &self
                .params
                .iter()
                .map(|v| v.string(0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => format!(
                    "{})",
                    self.params
                        .iter()
                        .map(|param| param.string(n))
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                _ => format!(
                    "{}{}",
                    format!(
                        "\n{}",
                        self.params
                            .iter()
                            .map(|param| param.string(n + 1).indent(n + 1) + ",")
                            .collect::<Vec<_>>()
                            .join("\n"),
                    ),
                    format!("\n{}", ")".indent(n),),
                ),
            },
            self.body.string(n + 1),
        )
    }
}
impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.signature() == other.signature()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Group {
    pub id: Token,
    pub contracts: Vec<Token>,
    pub fields: Vec<GroupField>,
    pub range: Range,
}
impl Production for Group {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "gwoup {}{} {{\n{}\n}}",
            self.id,
            match &self
                .contracts
                .iter()
                .map(|v| v.string(0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0 => "".to_string(),
                1..=MAX_LINE_LENGTH => format!(
                    " [{}]",
                    self.contracts
                        .iter()
                        .map(|contract| contract.string(n))
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                _ => format!(
                    " [{}{}",
                    format!(
                        "\n{}",
                        self.contracts
                            .iter()
                            .map(|contract| contract.string(n + 1).indent(n + 1) + ",")
                            .collect::<Vec<_>>()
                            .join("\n"),
                    ),
                    format!("\n{}", "]".indent(n),),
                ),
            },
            self.fields
                .iter()
                .map(|field| format!("{}", field.string(n + 1).indent(n + 1)))
                .collect::<Vec<_>>()
                .join("\n"),
        )
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct GroupMethod {
    pub id: Token,
    pub group: Token,
    pub mutable: bool,
    pub dtype: DataType,
    pub params: Vec<Param>,
    pub body: Body,
    pub range: Range,
}
impl Production for GroupMethod {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "fun {}{} {}-{}({} {{\n{}\n}}",
            self.group,
            if self.mutable { "!" } else { "" },
            self.id,
            self.dtype.string(0),
            match &self
                .params
                .iter()
                .map(|v| v.string(0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => format!(
                    "{})",
                    self.params
                        .iter()
                        .map(|param| param.string(n))
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                _ => format!(
                    "{}{}",
                    format!(
                        "\n{}",
                        self.params
                            .iter()
                            .map(|param| param.string(n + 1).indent(n + 1) + ",")
                            .collect::<Vec<_>>()
                            .join("\n"),
                    ),
                    format!("\n{}", ")".indent(n),),
                ),
            },
            self.body.string(n + 1),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Contract {
    pub id: Token,
    pub signatures: Vec<FnSignature>,
    pub range: Range,
}
impl Production for Contract {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "contwact {} {{\n{}\n}}",
            self.id,
            self.signatures
                .iter()
                .map(|signature| signature.string(n + 1))
                .collect::<Vec<_>>()
                .join("\n"),
        )
    }
}

#[derive(Clone, Debug, Default, Eq, Hash)]
pub struct FnSignature {
    pub id: Token,
    pub dtype: DataType,
    pub params: Vec<DataType>,
    pub range: Range,
}
impl Production for FnSignature {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "{}-{}({}~",
            self.id,
            self.dtype.string(0),
            match &self
                .params
                .iter()
                .map(|v| v.string(0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => {
                    format!(
                        "{})",
                        self.params
                            .iter()
                            .map(|dtype| dtype.string(n))
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
                                .map(|dtype| dtype.string(n + 1).indent(n + 1) + ",")
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
impl PartialEq for FnSignature {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.dtype == other.dtype && self.params == other.params
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Param {
    pub id: Token,
    pub dtype: DataType,
    pub variadic: bool,
    pub range: Range,
}
impl Production for Param {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "{}-{}{}",
            self.id,
            self.dtype.string(n),
            if self.variadic { "..." } else { "" }
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GroupField {
    pub id: Token,
    pub dtype: DataType,
    pub range: Range,
}
impl Production for GroupField {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!("{}-{}~", self.id, self.dtype.string(n),)
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Body {
    pub statements: Vec<Statement>,
    pub range: Range,
}
impl Production for Body {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        self.statements
            .iter()
            .map(|stmt| stmt.string(n))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

/*
 * STATEMENT PRODUCTIONS
 */
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Declaration {
    pub id: Token,
    pub dtype: DataType,
    pub mutable: bool,
    pub optional: bool,
    pub expr: Expression,
    pub range: Range,
}
impl Production for Declaration {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "hi {}-{}{}{} = {}~",
            self.id,
            self.dtype.string(n),
            if self.mutable { "!" } else { "" },
            if self.optional { "?" } else { "" },
            self.expr.string(n),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Assignment {
    pub id: Assignable,
    pub dtype: Option<Token>,
    pub assign_op: Token,
    pub expr: Expression,
    pub range: Range,
}
impl Production for Assignment {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "{} {} {}~",
            self.id.string(n),
            self.assign_op.string(0),
            self.expr.string(n + 1),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IfStatement {
    pub condition: Expression,
    pub body: Body,
    pub elifs: Vec<ElifStatement>,
    pub else_block: Option<Body>,
    pub range: Range,
}
impl Production for IfStatement {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "iwf {} {{\n{}\n{}{}\n{}",
            self.condition.string(n),
            self.body.string(n + 1),
            match (&self.elifs.len(), &self.else_block) {
                (0, None) => "}".indent(n),
                _ => "".to_string(),
            },
            self.elifs
                .iter()
                .map(|elif| elif.string(n))
                .collect::<Vec<_>>()
                .join("\n"),
            match &self.else_block {
                Some(block) => format!(
                    "{} {{\n{}\n{}",
                    "} ewse".indent(n),
                    block.string(n + 1),
                    "}".indent(n),
                ),
                None => "}".indent(n),
            },
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ElifStatement {
    pub condition: Expression,
    pub body: Body,
    pub range: Range,
}
impl Production for ElifStatement {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "{} {} {{\n{}",
            "} ewif".indent(n),
            self.condition.string(n),
            self.body.string(n + 1),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ForLoop {
    pub init: Declaration,
    pub condition: Expression,
    pub update: Expression,
    pub body: Body,
    pub range: Range,
}
impl Production for ForLoop {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            // TODO: format this so if too long, separate init cond and update by newlines
            "fow {} {}~ {} {{\n{}\n{}",
            self.init.string(n),
            self.condition.string(n),
            self.update.string(n),
            self.body.string(n + 1),
            "}".indent(n),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ForEach {
    pub item_id: Token,
    pub collection: Expression,
    pub body: Body,
    pub range: Range,
}
impl Production for ForEach {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "fow {} in {} {{\n{}\n{}",
            self.item_id.string(0),
            self.collection.string(n),
            self.body.string(n + 1),
            "}".indent(n),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MashStatement {
    pub expr: Expression,
    pub cases: Vec<Case>,
    pub default: Option<Body>,
    pub range: Range,
}
impl Production for MashStatement {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "mash {} {{\n{}{}{}",
            self.expr.string(n),
            self.cases
                .iter()
                .map(|case| case.string(n))
                .collect::<Vec<_>>()
                .join(""),
            match &self.default {
                Some(default) => {
                    format!(
                        "{}:{}\n",
                        "default".indent(n),
                        match default.statements.len() {
                            0 => unreachable!("Empty bodies aren't allowed during parsing"),
                            1 => " ".to_string() + default.string(n + 1).trim(),
                            _ => format!("\n{}", default.string(n + 1)),
                        }
                    )
                }
                None => "".to_string(),
            },
            "}".indent(n)
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Case {
    pub case_type: DataType,
    pub body: Body,
    pub range: Range,
}
impl Production for Case {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "{}:{}\n",
            self.case_type.string(0).indent(n),
            match &self.body.statements.len() {
                0 => unreachable!("Empty bodies aren't allowed during parsing"),
                1 => " ".to_string() + self.body.string(n + 1).trim(),
                _ => format!("\n{}", self.body.string(n + 1)),
            }
        )
    }
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Pipeline {
    pub first: Box<Identifier>,
    pub rest: Vec<Callable>,
    pub range: Range,
}
impl Production for Pipeline {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "{}{}",
            self.first.string(n),
            match &self
                .rest
                .iter()
                .map(|v| v.string(0))
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
                        .map(|v| v.string(n))
                        .collect::<Vec<_>>()
                        .join(" | "),
                ),
                _ => format!(
                    "\n{}",
                    self.rest
                        .iter()
                        .map(|v| format!("| {}", v.string(n)).indent(n))
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ReturnStatement {
    pub expr: Expression,
    pub range: Range,
}
impl Production for ReturnStatement {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!("wetuwn {}~", self.expr.string(n))
    }
}

