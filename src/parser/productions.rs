use crate::lexer::token::{Token, TokenKind};
use crate::parser::data_types::DataType;
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

/// Expressions define computations and operations that generate values
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
impl Production for Expression {
    fn range(&self) -> Range {
        match self {
            Self::Ident(res) => res.range(),
            Self::GroupInit(res) => res.range(),
            Self::Pipeline(res) => res.range(),
            Self::Array(res) => res.range(),
            Self::Set(res) => res.range(),
            Self::Map(res) => res.range(),
            Self::Prefix(res) => res.range(),
            Self::Infix(res) => res.range(),
            Self::Grouped(res) => res.range(),
        }
    }
    fn string(&self, n: usize) -> String {
        match self {
            Self::Ident(res) => res.string(n),
            Self::GroupInit(res) => res.string(n),
            Self::Pipeline(res) => res.string(n),
            Self::Array(res) => res.string(n),
            Self::Set(res) => res.string(n),
            Self::Map(res) => res.string(n),
            Self::Prefix(res) => res.string(n),
            Self::Infix(res) => res.string(n),
            Self::Grouped(res) => res.string(n),
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
pub struct FnCall {
    pub id: Token,
    pub args: Vec<Expression>,
    pub range: Range,
    pub signature: FnSignature,
}
impl Production for FnCall {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "{}({}",
            self.id,
            match &self
                .args
                .iter()
                .map(|v| v.string(0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => {
                    format!(
                        "{})",
                        self.args
                            .iter()
                            .map(|arg| arg.string(n))
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
                                .map(|arg| arg.string(n + 1).indent(n + 1) + ",")
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Callable {
    Fn(FnCall),
    Method(GroupAccess<MethodAccess>),
}
impl Production for Callable {
    fn range(&self) -> Range {
        match self {
            Self::Fn(call) => call.range(),
            Self::Method(method) => method.range(),
        }
    }
    fn string(&self, n: usize) -> String {
        match self {
            Self::Fn(call) => call.string(n),
            Self::Method(method) => method.string(n),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GroupInit {
    pub id: Token,
    pub args: Vec<Expression>,
    pub range: Range,
}
impl Production for GroupInit {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "{}({}",
            self.id,
            match &self
                .args
                .iter()
                .map(|v| v.string(0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => {
                    format!(
                        "{})",
                        self.args
                            .iter()
                            .map(|arg| arg.string(n))
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
                                .map(|arg| arg.string(n + 1).indent(n + 1) + ",")
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

/*
 * EXPRESSION UNITS
 */
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IndexedId {
    pub id: Indexable,
    pub indices: Vec<Expression>,
    pub range: Range,
}
impl Production for IndexedId {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "{}[{}",
            self.id.string(n),
            match &self
                .indices
                .iter()
                .map(|v| v.string(0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => format!(
                    "{}]",
                    self.indices
                        .iter()
                        .map(|idx| idx.string(n))
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
                                .map(|arg| arg.string(n + 1).indent(n + 1) + ",")
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum AccessType {
    Field(GroupAccess<FieldAccess>),
    Method(GroupAccess<MethodAccess>),
}
impl Production for AccessType {
    fn range(&self) -> Range {
        match self {
            Self::Method(method) => method.range(),
            Self::Field(field) => field.range(),
        }
    }
    fn string(&self, n: usize) -> String {
        match self {
            Self::Method(method) => method.string(n),
            Self::Field(field) => field.string(n),
        }
    }
}

/*
 * COLLETION LITERALS
 */
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ArrayLiteral {
    pub exprs: Vec<Expression>,
    pub range: Range,
}
impl Production for ArrayLiteral {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "[{}",
            match &self
                .exprs
                .iter()
                .map(|v| v.string(0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => {
                    format!(
                        "{}]",
                        self.exprs
                            .iter()
                            .map(|expr| expr.string(n))
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
                                .map(|expr| expr.string(n + 1).indent(n + 1) + ",")
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SetLiteral {
    pub exprs: Vec<Expression>,
    pub range: Range,
}
impl Production for SetLiteral {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "#[{}",
            match &self
                .exprs
                .iter()
                .map(|v| v.string(0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => {
                    format!(
                        "{}]",
                        self.exprs
                            .iter()
                            .map(|expr| expr.string(n))
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
                                .map(|expr| expr.string(n + 1).indent(n + 1) + ",")
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MapLiteral {
    pub exprs: Vec<(Expression, Expression)>,
    pub range: Range,
}
impl Production for MapLiteral {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        format!(
            "#[{}",
            if self.exprs.len() == 0 {
                ":".to_string()
            } else {
                match &self
                    .exprs
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k.string(n), v.string(n)))
                    .collect::<Vec<_>>()
                    .join("")
                    .len()
                {
                    0..=MAX_LINE_LENGTH => {
                        format!(
                            "{}]",
                            self.exprs
                                .iter()
                                .map(|(k, v)| format!("{}: {}", k.string(n), v.string(n)))
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
                                        k.string(n + 1),
                                        v.string(n + 1)
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
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PrefixExpression {
    pub op: Token,
    pub right: Box<Expression>,
    pub range: Range,
}
impl Production for PrefixExpression {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        match &self.op.kind {
            TokenKind::Dash => format!("{}{}", self.op, self.right.string(n)),
            _ => format!("{} {}", self.op, self.right.string(n)),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub op: Token,
    pub right: Box<Expression>,
    pub range: Range,
}
impl Production for InfixExpression {
    fn range(&self) -> Range {
        self.range
    }
    /// TODO: use better logic than this rudimentary implementation
    /// it currenly indents way too much if combined LR exceeds [MAX_LINE_LENGTH] chars
    fn string(&self, n: usize) -> String {
        match self.left.string(0).len() + self.right.string(0).len() {
            0..=MAX_LINE_LENGTH => {
                format!(
                    "({} {} {})",
                    self.left.string(n),
                    self.op,
                    self.right.string(n)
                )
            }
            _ => {
                format!(
                    "{}\n{} {}",
                    self.left.string(n),
                    self.op.string(n + 1).indent(n + 1),
                    self.right.string(n + 1)
                )
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GroupedExpression {
    pub expr: Box<Expression>,
    pub range: Range,
}
impl Production for GroupedExpression {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        match &self.expr.string(0).len() {
            0..=MAX_LINE_LENGTH => {
                format!("{}", self.expr.string(n))
            }
            _ => {
                format!("\n{}\n", self.expr.string(n + 1).indent(n + 1))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FieldAccess;
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MethodAccess;
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GroupAccess<AccessType> {
    pub accessed: Vec<Accessor>,
    pub access_type: std::marker::PhantomData<AccessType>,
    pub range: Range,
}
impl<T> Production for GroupAccess<T> {
    fn range(&self) -> Range {
        self.range
    }
    fn string(&self, n: usize) -> String {
        match self.accessed.len() {
            0..=1 => unreachable!(),
            _ => {
                let first = self.accessed.first().unwrap();
                let rest = self.accessed.iter().skip(1).collect::<Vec<_>>();
                format!(
                    "{}{}",
                    first.string(n),
                    match &rest
                        .iter()
                        .map(|v| v.string(0))
                        .collect::<Vec<_>>()
                        .join("")
                        .len()
                    {
                        0 => "".to_string(),
                        1..=MAX_LINE_LENGTH => format!(
                            ".{}",
                            rest.iter()
                                .map(|v| v.string(0))
                                .collect::<Vec<_>>()
                                .join("."),
                        ),
                        _ => format!(
                            "\n{}",
                            rest.iter()
                                .map(|v| format!(".{}", v.string(n)).indent(n))
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
