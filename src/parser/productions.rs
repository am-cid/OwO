use crate::errors::parse_errors::NoMainError;
use crate::lexer::token::{Offset, Position, Token, TokenKind};
use crate::utils::string::StringExt;

/// Max width of the [FnCall] args before falling back to vertical formatting.
/// Applies as well to :
/// - [GroupAccess<Method>]
/// - [IndexedId]
/// - [Pipeline]
const MAX_LINE_LENGTH: usize = 80;

pub trait Production<'a>: Position<'a> {
    /// Converts the production to string based off the given source.
    /// `n` is the indent amount
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String;
    // fn to_unformatted_string(&self, source: &'a str, n: usize) -> String;
    // fn transpile(&self, indent: usize) -> String;
}
impl<'a> Production<'a> for Token {
    fn to_formatted_string(&self, source: &'a str, _n: usize) -> String {
        self.source_str(source).to_string() // self.text.to_string()
    }
}

// TOP LEVEL PRODUCTIONS {{{

/// Root node of the AST
#[derive(Clone, Debug, Default)]
pub struct Program {
    pub main: Function,
    pub functions: Vec<Function>,
    pub groups: Vec<Group>,
    pub methods: Vec<GroupMethod>,
    pub contracts: Vec<Contract>,
    pub globals: Vec<Statement>,
    pos: Offset,
}
impl<'a> Program {
    pub fn new(
        main: Option<Function>,
        functions: Vec<Function>,
        groups: Vec<Group>,
        methods: Vec<GroupMethod>,
        contracts: Vec<Contract>,
        globals: Vec<Statement>,
        source: &'a str,
        end_pos: (usize, usize),
    ) -> Result<Self, NoMainError<'a>> {
        match main {
            Some(main) => Ok(Self {
                main,
                functions,
                groups,
                methods,
                contracts,
                globals,
                pos: Offset::new(0, source.len()),
            }),
            None => Err(NoMainError::new(
                source.lines().nth(end_pos.0).unwrap_or_default(),
                (end_pos.0 + 1, end_pos.1 + 1),
            )),
        }
    }
}
impl<'a> Position<'a> for Program {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for Program {
    fn to_formatted_string(&self, source: &'a str, _n: usize) -> String {
        format!(
            "{}{}{}{}{}{}",
            match &self.globals.len() {
                0 => "".to_string(),
                _ => self
                    .globals
                    .iter()
                    .map(|global| global.to_formatted_string(source, 0) + "\n")
                    .collect::<Vec<_>>()
                    .join(""),
            },
            self.main.to_formatted_string(source, 0) + "\n",
            match &self.functions.len() {
                0 => "".to_string(),
                _ => self
                    .functions
                    .iter()
                    .map(|func| func.to_formatted_string(source, 0) + "\n")
                    .collect::<Vec<_>>()
                    .join(""),
            },
            match &self.groups.len() {
                0 => "".to_string(),
                _ => self
                    .groups
                    .iter()
                    .map(|group| group.to_formatted_string(source, 0) + "\n")
                    .collect::<Vec<_>>()
                    .join(""),
            },
            match &self.methods.len() {
                0 => "".to_string(),
                _ => self
                    .methods
                    .iter()
                    .map(|group| group.to_formatted_string(source, 0) + "\n")
                    .collect::<Vec<_>>()
                    .join(""),
            },
            match &self.contracts.len() {
                0 => "".to_string(),
                _ => self
                    .contracts
                    .iter()
                    .map(|contract| contract.to_formatted_string(source, 0) + "\n")
                    .collect::<Vec<_>>()
                    .join(""),
            },
        )
        .trim()
        .to_string()
    }
}

#[derive(Clone, Debug, Default)]
pub struct Function {
    pub id: Token,
    pub dtype: DataType,
    pub params: Vec<Param>,
    pub body: Body,
    pos: Offset,
}
impl Function {
    pub fn new(
        id: Token,
        dtype: DataType,
        params: Vec<Param>,
        body: Body,
        offset: (usize, usize),
    ) -> Self {
        Self {
            id,
            dtype,
            params,
            body,
            pos: Offset::new(offset.0, offset.1),
        }
    }
    /// Gets the signature of the function. Only use for equality checks since
    /// the offset is inaccurate (because it isn't part of the source code).
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
            pos: Offset::default(),
        }
    }
}
impl<'a> Position<'a> for Function {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for Function {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "fun {}-{}({} {{\n{}\n}}",
            self.id.source_str(source),
            self.dtype.to_formatted_string(source, 0),
            match &self
                .params
                .iter()
                .map(|v| v.to_formatted_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => format!(
                    "{})",
                    self.params
                        .iter()
                        .map(|param| param.to_formatted_string(source, n))
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                _ => format!(
                    "{}{}",
                    format!(
                        "\n{}",
                        self.params
                            .iter()
                            .map(
                                |param| param.to_formatted_string(source, n + 1).indent(n + 1)
                                    + ","
                            )
                            .collect::<Vec<_>>()
                            .join("\n"),
                    ),
                    format!("\n{}", ")".indent(n),),
                ),
            },
            self.body.to_formatted_string(source, n + 1),
        )
    }
}

#[derive(Clone, Debug)]
pub struct Group {
    pub id: Token,
    pub fields: Vec<GroupField>,
    pos: Offset,
}
impl Group {
    pub fn new(id: Token, fields: Vec<GroupField>, offset: (usize, usize)) -> Self {
        Self {
            id,
            fields,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for Group {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for Group {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "gwoup {} {{\n{}\n}}",
            self.id.source_str(source),
            self.fields
                .iter()
                .map(|field| format!("{}", field.to_formatted_string(source, n + 1).indent(n + 1)))
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
    pos: Offset,
}
impl GroupMethod {
    pub fn new(
        id: Token,
        group: Token,
        mutable: bool,
        dtype: DataType,
        params: Vec<Param>,
        body: Body,
        offset: (usize, usize),
    ) -> GroupMethod {
        GroupMethod {
            id,
            group,
            mutable,
            dtype,
            params,
            body,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for GroupMethod {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for GroupMethod {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "fun {}{} {}-{}({} {{\n{}\n}}",
            self.group.source_str(source),
            if self.mutable { "!" } else { "" },
            self.id.source_str(source),
            self.dtype.to_formatted_string(source, 0),
            match &self
                .params
                .iter()
                .map(|v| v.to_formatted_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => format!(
                    "{})",
                    self.params
                        .iter()
                        .map(|param| param.to_formatted_string(source, n))
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                _ => format!(
                    "{}{}",
                    format!(
                        "\n{}",
                        self.params
                            .iter()
                            .map(
                                |param| param.to_formatted_string(source, n + 1).indent(n + 1)
                                    + ","
                            )
                            .collect::<Vec<_>>()
                            .join("\n"),
                    ),
                    format!("\n{}", ")".indent(n),),
                ),
            },
            self.body.to_formatted_string(source, n + 1),
        )
    }
}

#[derive(Clone, Debug)]
pub struct Contract {
    pub id: Token,
    pub signatures: Vec<FnSignature>,
    pos: Offset,
}
impl Contract {
    pub fn new(id: Token, signatures: Vec<FnSignature>, offset: (usize, usize)) -> Self {
        Self {
            id,
            signatures,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for Contract {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for Contract {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "contwact {} {{\n{}\n}}",
            self.id.source_str(source),
            self.signatures
                .iter()
                .map(|signature| signature.to_formatted_string(source, n + 1))
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
    pos: Offset,
}
impl FnSignature {
    pub fn new(id: Token, dtype: DataType, params: Vec<DataType>, offset: (usize, usize)) -> Self {
        Self {
            id,
            dtype,
            params,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for FnSignature {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for FnSignature {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}-{}({}~",
            self.id.source_str(source),
            self.dtype.to_formatted_string(source, 0),
            match &self
                .params
                .iter()
                .map(|v| v.to_formatted_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => {
                    format!(
                        "{})",
                        self.params
                            .iter()
                            .map(|dtype| dtype.to_formatted_string(source, n))
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
                                .map(
                                    |dtype| dtype.to_formatted_string(source, n + 1).indent(n + 1)
                                        + ","
                                )
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
//         self.id == other.id
//         && self.dtype == other.dtype
//         && self.params == other.params
//     }
// }

#[derive(Clone, Debug)]
pub struct Param {
    pub id: Token,
    pub dtype: DataType,
    pub variadic: bool,
    pos: Offset,
}
impl Param {
    pub fn new(id: Token, dtype: DataType, variadic: bool, offset: (usize, usize)) -> Self {
        Self {
            id,
            dtype,
            variadic,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for Param {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for Param {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}-{}{}",
            self.id.source_str(source),
            self.dtype.to_formatted_string(source, n),
            if self.variadic { "..." } else { "" }
        )
    }
}

#[derive(Clone, Debug)]
pub struct GroupField {
    pub id: Token,
    pub dtype: DataType,
    pos: Offset,
}
impl GroupField {
    pub fn new(id: Token, dtype: DataType, offset: (usize, usize)) -> Self {
        Self {
            id,
            dtype,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for GroupField {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for GroupField {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}-{}~",
            self.id.source_str(source),
            self.dtype.to_formatted_string(source, n),
        )
    }
}

#[derive(Clone, Debug, Default)]
pub struct Body {
    pub statements: Vec<Statement>,
    pos: Offset,
}
impl Body {
    pub fn new(statements: Vec<Statement>, offset: (usize, usize)) -> Self {
        Self {
            statements,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for Body {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for Body {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        self.statements
            .iter()
            .map(|stmt| stmt.to_formatted_string(source, n))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

// }}}

// STATEMENT PRODUCTIONS {{{

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
    Expression(Expression),
}
impl Statement {
    fn as_position(&self) -> &dyn Position {
        match self {
            Self::Declaration(res) => res,
            Self::Assignment(res) => res,
            Self::If(res) => res,
            Self::Mash(res) => res,
            Self::ForLoop(res) => res,
            Self::ForEach(res) => res,
            Self::Break(res) => res,
            Self::Continue(res) => res,
            Self::FnCall(res) => res,
            Self::Method(res) => res,
            Self::Pipeline(res) => res,
            Self::Return(res) => res,
            Self::Expression(res) => res,
        }
    }
}
impl<'a> Position<'a> for Statement {
    fn offset(&self) -> Offset {
        self.as_position().offset()
    }
}
impl<'a> Production<'a> for Statement {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        match self {
            Self::Declaration(res) => res.to_formatted_string(source, n).indent(n),
            Self::Assignment(res) => res.to_formatted_string(source, n).indent(n),
            Self::If(res) => res.to_formatted_string(source, n).indent(n),
            Self::Mash(res) => res.to_formatted_string(source, n).indent(n),
            Self::ForLoop(res) => res.to_formatted_string(source, n).indent(n),
            Self::ForEach(res) => res.to_formatted_string(source, n).indent(n),
            Self::Break(res) => res.to_formatted_string(source, n).indent(n) + "~",
            Self::Continue(res) => res.to_formatted_string(source, n).indent(n) + "~",
            Self::FnCall(res) => res.to_formatted_string(source, n).indent(n) + "~",
            Self::Method(res) => res.to_formatted_string(source, n).indent(n) + "~",
            Self::Pipeline(res) => res.to_formatted_string(source, n).indent(n) + "~",
            Self::Return(res) => res.to_formatted_string(source, n).indent(n),
            Self::Expression(res) => res.to_formatted_string(source, n).indent(n) + "~",
        }
    }
}

#[derive(Clone, Debug)]
pub struct Declaration {
    pub id: Token,
    pub dtype: Option<DataType>,
    pub mutable: bool,
    pub optional: bool,
    pub expr: Expression,
    pos: Offset,
}
impl Declaration {
    pub fn new(
        id: Token,
        dtype: Option<DataType>,
        mutable: bool,
        optional: bool,
        expr: Expression,
        offset: (usize, usize),
    ) -> Self {
        Self {
            id,
            dtype,
            optional,
            mutable,
            expr,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for Declaration {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for Declaration {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "hi {}{}{}{} = {}~",
            self.id.source_str(source),
            match &self.dtype {
                Some(dtype) => "-".to_string() + &dtype.to_formatted_string(source, n),
                None => "".into(),
            },
            if self.mutable { "!" } else { "" },
            if self.optional { "?" } else { "" },
            self.expr.to_formatted_string(source, n),
        )
    }
}

#[derive(Clone, Debug)]
pub struct Assignment {
    pub id: Assignable,
    pub assign_op: Token,
    pub expr: Expression,
    pos: Offset,
}
impl Assignment {
    pub fn new(id: Assignable, assign_op: Token, expr: Expression, offset: (usize, usize)) -> Self {
        Self {
            id,
            assign_op,
            expr,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for Assignment {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for Assignment {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{} {} {}~",
            self.id.to_formatted_string(source, n),
            self.assign_op.to_formatted_string(source, 0),
            self.expr.to_formatted_string(source, n + 1),
        )
    }
}

#[derive(Clone, Debug)]
pub struct IfStatement {
    pub condition: Expression,
    pub body: Body,
    pub elifs: Vec<ElifStatement>,
    pub else_block: Option<Body>,
    pos: Offset,
}
impl IfStatement {
    pub fn new(
        condition: Expression,
        body: Body,
        elifs: Vec<ElifStatement>,
        else_block: Option<Body>,
        offset: (usize, usize),
    ) -> Self {
        Self {
            condition,
            body,
            elifs,
            else_block,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for IfStatement {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for IfStatement {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "iwf {} {{\n{}\n{}{}\n{}",
            self.condition.to_formatted_string(source, n),
            self.body.to_formatted_string(source, n + 1),
            match (&self.elifs.len(), &self.else_block) {
                (0, None) => "}".indent(n),
                _ => "".to_string(),
            },
            self.elifs
                .iter()
                .map(|elif| elif.to_formatted_string(source, n))
                .collect::<Vec<_>>()
                .join("\n"),
            match &self.else_block {
                Some(block) => format!(
                    "{} {{\n{}\n{}",
                    "} ewse".indent(n),
                    block.to_formatted_string(source, n + 1),
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
    pos: Offset,
}
impl ElifStatement {
    pub fn new(condition: Expression, body: Body, offset: (usize, usize)) -> Self {
        Self {
            condition,
            body,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for ElifStatement {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for ElifStatement {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{} {} {{\n{}",
            "} ewif".indent(n),
            self.condition.to_formatted_string(source, n),
            self.body.to_formatted_string(source, n + 1),
        )
    }
}

#[derive(Clone, Debug)]
pub struct ForLoop {
    pub init: Declaration,
    pub condition: Expression,
    pub update: Expression,
    pub body: Body,
    pos: Offset,
}
impl ForLoop {
    pub fn new(
        init: Declaration,
        condition: Expression,
        update: Expression,
        body: Body,
        offset: (usize, usize),
    ) -> Self {
        Self {
            init,
            condition,
            update,
            body,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for ForLoop {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for ForLoop {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            // TODO: format this so if too long, separate init cond and update
            // by newlines
            "fow {} {}~ {} {{\n{}\n{}",
            self.init.to_formatted_string(source, n),
            self.condition.to_formatted_string(source, n),
            self.update.to_formatted_string(source, n),
            self.body.to_formatted_string(source, n + 1),
            "}".indent(n),
        )
    }
}

#[derive(Clone, Debug)]
pub struct ForEach {
    pub item_id: Token,
    pub collection: Expression,
    pub body: Body,
    pos: Offset,
}
impl ForEach {
    pub fn new(item_id: Token, collection: Expression, body: Body, offset: (usize, usize)) -> Self {
        Self {
            item_id,
            collection,
            body,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for ForEach {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for ForEach {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "fow {} in {} {{\n{}\n{}",
            self.item_id.to_formatted_string(source, 0),
            self.collection.to_formatted_string(source, n),
            self.body.to_formatted_string(source, n + 1),
            "}".indent(n),
        )
    }
}

#[derive(Clone, Debug)]
pub struct MashStatement {
    pub expr: Expression,
    pub cases: Vec<Case>,
    pub default: Option<Body>,
    pos: Offset,
}
impl MashStatement {
    pub fn new(
        expr: Expression,
        cases: Vec<Case>,
        default: Option<Body>,
        offset: (usize, usize),
    ) -> Self {
        Self {
            expr,
            cases,
            default,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for MashStatement {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for MashStatement {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "mash {} {{\n{}{}{}",
            self.expr.to_formatted_string(source, n),
            self.cases
                .iter()
                .map(|case| case.to_formatted_string(source, n))
                .collect::<Vec<_>>()
                .join(""),
            match &self.default {
                Some(default) => {
                    format!(
                        "{}:{}\n",
                        "default".indent(n),
                        match default.statements.len() {
                            0 => unreachable!("Empty bodies aren't allowed during parsing"),
                            1 =>
                                " ".to_string() + default.to_formatted_string(source, n + 1).trim(),
                            _ => format!("\n{}", default.to_formatted_string(source, n + 1)),
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
    pos: Offset,
}
impl Case {
    pub fn new(case_type: DataType, body: Body, offset: (usize, usize)) -> Self {
        Self {
            case_type,
            body,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for Case {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for Case {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}:{}\n",
            self.case_type.to_formatted_string(source, 0).indent(n),
            match &self.body.statements.len() {
                0 => unreachable!("Empty bodies aren't allowed during parsing"),
                1 => " ".to_string() + self.body.to_formatted_string(source, n + 1).trim(),
                _ => format!("\n{}", self.body.to_formatted_string(source, n + 1)),
            }
        )
    }
}

#[derive(Clone, Debug)]
pub struct ReturnStatement {
    pub expr: Expression,
    pos: Offset,
}
impl ReturnStatement {
    pub fn new(expr: Expression, offset: (usize, usize)) -> Self {
        Self {
            expr,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for ReturnStatement {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for ReturnStatement {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!("wetuwn {}~", self.expr.to_formatted_string(source, n))
    }
}

// }}}

// EXPRESSION UNIT PRODUCTIONS {{{

/// Expressions define computations and operations that generate values
#[derive(Clone, Debug)]
pub enum Expression {
    Token(Token),
    FnCall(FnCall),
    Indexed(IndexedId),
    Access(AccessType),
    GroupInit(GroupInit),
    Pipeline(Pipeline),
    Array(ArrayLiteral),
    Set(SetLiteral),
    Map(MapLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Grouped(GroupedExpression),
}
impl<'a> Expression {
    pub fn is_indexable(&self) -> bool {
        match self {
            Self::FnCall(_) | Self::Access(_) => true,
            Self::Indexed(_) => false,
            Self::Token(res) => match res.kind {
                TokenKind::Identifier | TokenKind::StringLiteral => true,
                _ => false,
            },
            Self::Pipeline(_) | Self::Array(_) | Self::Set(_) | Self::Map(_) => true,
            Self::GroupInit(_) | Self::Prefix(_) | Self::Infix(_) => false,
            Self::Grouped(res) => res.expr.is_indexable(),
        }
    }
    pub fn is_accessible(&self) -> bool {
        match self {
            Self::FnCall(_) | Self::Indexed(_) | Self::Access(_) => true,
            Self::Token(res) => match res.kind {
                TokenKind::Identifier
                | TokenKind::Type
                | TokenKind::IntLiteral
                | TokenKind::FloatLiteral
                | TokenKind::Fax
                | TokenKind::Cap
                | TokenKind::StringLiteral
                | TokenKind::CharLiteral
                | TokenKind::Nuww => true,
                _ => false,
            },
            Self::GroupInit(_)
            | Self::Pipeline(_)
            | Self::Array(_)
            | Self::Set(_)
            | Self::Map(_)
            | Self::Prefix(_)
            | Self::Infix(_) => true,
            Self::Grouped(res) => res.expr.is_accessible(),
        }
    }
    fn as_position(&self) -> &dyn Position {
        match self {
            Self::Token(res) => res,
            Self::FnCall(res) => res,
            Self::Indexed(res) => res,
            Self::Access(res) => res,
            Self::GroupInit(res) => res,
            Self::Pipeline(res) => res,
            Self::Array(res) => res,
            Self::Set(res) => res,
            Self::Map(res) => res,
            Self::Prefix(res) => res,
            Self::Infix(res) => res,
            Self::Grouped(res) => res,
        }
    }
    fn as_production(&self) -> &dyn Production {
        match self {
            Self::Token(res) => res,
            Self::FnCall(res) => res,
            Self::Indexed(res) => res,
            Self::Access(res) => res,
            Self::GroupInit(res) => res,
            Self::Pipeline(res) => res,
            Self::Array(res) => res,
            Self::Set(res) => res,
            Self::Map(res) => res,
            Self::Prefix(res) => res,
            Self::Infix(res) => res,
            Self::Grouped(res) => res,
        }
    }
}
impl<'a> Position<'a> for Expression {
    fn offset(&self) -> Offset {
        self.as_position().offset()
    }
}
impl<'a> Production<'a> for Expression {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        self.as_production().to_formatted_string(source, n)
    }
}
impl From<Accessed> for Expression {
    fn from(value: Accessed) -> Self {
        match value {
            Accessed::Token(res) => Self::Token(res),
            Accessed::FnCall(res) => Self::FnCall(res),
            Accessed::IndexedId(res) => Self::Indexed(res),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Callable {
    Fn(FnCall),
    Method(GroupAccess<MethodAccess>),
    GroupInit(GroupInit),
}
impl Callable {
    fn as_position(&self) -> &dyn Position {
        match self {
            Self::Fn(res) => res,
            Self::Method(res) => res,
            Self::GroupInit(res) => res,
        }
    }
    fn as_production(&self) -> &dyn Production {
        match self {
            Self::Fn(res) => res,
            Self::Method(res) => res,
            Self::GroupInit(res) => res,
        }
    }
}
impl Default for Callable {
    fn default() -> Self {
        Self::Fn(FnCall::new(
            Token::default(),
            vec![],
            FnSignature::default(),
            (0, 0),
        ))
    }
}
impl<'a> Position<'a> for Callable {
    fn offset(&self) -> Offset {
        self.as_position().offset()
    }
}
impl<'a> Production<'a> for Callable {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        self.as_production().to_formatted_string(source, n)
    }
}

#[derive(Clone, Debug)]
pub struct FnCall {
    pub id: Token,
    pub args: Vec<Expression>,
    pub signature: FnSignature,
    pos: Offset,
}
impl FnCall {
    pub fn new(
        id: Token,
        args: Vec<Expression>,
        signature: FnSignature,
        offset: (usize, usize),
    ) -> Self {
        Self {
            id,
            args,
            signature,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for FnCall {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for FnCall {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}({}",
            self.id.source_str(source),
            match &self
                .args
                .iter()
                .map(|v| v.to_formatted_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => {
                    format!(
                        "{})",
                        self.args
                            .iter()
                            .map(|arg| arg.to_formatted_string(source, n))
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
                                .map(|arg| arg.to_formatted_string(source, n + 1).indent(n + 1)
                                    + ",")
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
pub struct FieldAccess;
#[derive(Clone, Debug)]
pub struct MethodAccess;
#[derive(Clone, Debug)]
pub struct GroupAccess<T> {
    pub accessor: Box<Expression>,
    /// guaranteed to always be at least size 1
    pub accessed: Vec<Accessed>,
    pub access_type: std::marker::PhantomData<T>,
    pos: Offset,
}
impl<T> GroupAccess<T> {
    pub fn new(
        accessor: Box<Expression>,
        accessed: Vec<Accessed>,
        access_type: std::marker::PhantomData<T>,
    ) -> Self {
        let (start, end) = (
            accessor.offset().start,
            accessed.last().unwrap_or(&Accessed::default()).offset().end,
        );
        Self {
            accessor,
            accessed,
            access_type,
            pos: Offset::new(start, end),
        }
    }
}
impl GroupAccess<MethodAccess> {
    pub fn last(&self) -> &FnCall {
        match self.accessed.last() {
            Some(Accessed::FnCall(call)) => call,
            Some(Accessed::IndexedId(res)) => match &res.id {
                Indexable::FnCall(call) => call,
                _ => unreachable!("MethodAccess always has fn call as its last during parsing"),
            },
            _ => unreachable!("MethodAccess always has fn call as its last during parsing"),
        }
    }
}
impl From<GroupAccess<FieldAccess>> for GroupAccess<MethodAccess> {
    fn from(value: GroupAccess<FieldAccess>) -> Self {
        Self::new(value.accessor, value.accessed, std::marker::PhantomData)
    }
}
impl From<GroupAccess<MethodAccess>> for GroupAccess<FieldAccess> {
    fn from(value: GroupAccess<MethodAccess>) -> Self {
        Self::new(value.accessor, value.accessed, std::marker::PhantomData)
    }
}
impl<'a, T> Position<'a> for GroupAccess<T> {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a, T> Production<'a> for GroupAccess<T> {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        match self.accessed.len() {
            0 => unreachable!(),
            _ => {
                format!(
                    "{}{}",
                    self.accessor.to_formatted_string(source, n),
                    match &self
                        .accessed
                        .iter()
                        .map(|v| v.to_formatted_string(source, 0))
                        .collect::<Vec<_>>()
                        .join("")
                        .len()
                    {
                        0 => "".to_string(),
                        1..=MAX_LINE_LENGTH => format!(
                            ".{}",
                            self.accessed
                                .iter()
                                .map(|v| v.to_formatted_string(source, 0))
                                .collect::<Vec<_>>()
                                .join("."),
                        ),
                        _ => format!(
                            "\n{}",
                            self.accessed
                                .iter()
                                .map(|v| format!(".{}", v.to_formatted_string(source, n)).indent(n))
                                .collect::<Vec<_>>()
                                .join("\n"),
                        ),
                    }
                )
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum AccessType {
    Field(GroupAccess<FieldAccess>),
    Method(GroupAccess<MethodAccess>),
}
impl AccessType {
    fn as_position(&self) -> &dyn Position {
        match self {
            Self::Method(res) => res,
            Self::Field(res) => res,
        }
    }
    fn as_production(&self) -> &dyn Production {
        match self {
            Self::Method(res) => res,
            Self::Field(res) => res,
        }
    }
}
impl<'a> Position<'a> for AccessType {
    fn offset(&self) -> Offset {
        self.as_position().offset()
    }
}
impl<'a> Production<'a> for AccessType {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        self.as_production().to_formatted_string(source, n)
    }
}

#[derive(Clone, Debug)]
pub struct GroupInit {
    pub id: Token,
    pub args: Vec<Expression>,
    pos: Offset,
}
impl GroupInit {
    pub fn new(id: Token, args: Vec<Expression>, offset_end: usize) -> Self {
        Self {
            id,
            args,
            pos: Offset::new(id.offset.start, offset_end),
        }
    }
}
impl<'a> Position<'a> for GroupInit {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for GroupInit {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}({}",
            self.id.source_str(source),
            match &self
                .args
                .iter()
                .map(|v| v.to_formatted_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => {
                    format!(
                        "{})",
                        self.args
                            .iter()
                            .map(|arg| arg.to_formatted_string(source, n))
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
                                .map(|arg| arg.to_formatted_string(source, n + 1).indent(n + 1)
                                    + ",")
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
pub struct IndexedId {
    pub id: Indexable,
    pub indices: Vec<Expression>,
    pos: Offset,
}
impl IndexedId {
    pub fn new(id: Indexable, indices: Vec<Expression>, offset_end: usize) -> Self {
        let offset_start = id.offset().start;
        Self {
            id,
            indices,
            pos: Offset::new(offset_start, offset_end),
        }
    }
}
impl<'a> Position<'a> for IndexedId {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for IndexedId {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}[{}",
            self.id.to_formatted_string(source, n),
            match &self
                .indices
                .iter()
                .map(|v| v.to_formatted_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => format!(
                    "{}]",
                    self.indices
                        .iter()
                        .map(|idx| idx.to_formatted_string(source, n))
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
                                .map(|arg| arg.to_formatted_string(source, n + 1).indent(n + 1)
                                    + ",")
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

// }}}

// EXPRESSION GROUP PRODUCTIONS {{{

#[derive(Clone, Debug)]
pub struct PrefixExpression {
    pub op: Token,
    pub right: Box<Expression>,
    pos: Offset,
}
impl PrefixExpression {
    pub fn new(op: Token, right: Box<Expression>) -> Self {
        let offset = (op.offset.start, right.offset().end);
        Self {
            op,
            right,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for PrefixExpression {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for PrefixExpression {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        match &self.op.kind {
            TokenKind::Dash => format!(
                "{}{}",
                self.op.source_str(source),
                self.right.to_formatted_string(source, n)
            ),
            _ => format!(
                "{} {}",
                self.op.source_str(source),
                self.right.to_formatted_string(source, n)
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub op: Token,
    pub right: Box<Expression>,
    pos: Offset,
}
impl InfixExpression {
    pub fn new(left: Box<Expression>, op: Token, right: Box<Expression>) -> Self {
        let offset = (left.offset().start, right.offset().end);
        Self {
            left,
            op,
            right,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for InfixExpression {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for InfixExpression {
    /// TODO: use better logic than this rudimentary implementation
    /// it currenly indents way too much if combined LR exceeds [MAX_LINE_LENGTH] chars
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        match self.left.to_formatted_string(source, 0).len()
            + self.right.to_formatted_string(source, 0).len()
        {
            0..=MAX_LINE_LENGTH => {
                format!(
                    "{} {} {}",
                    self.left.to_formatted_string(source, n),
                    self.op.source_str(source),
                    self.right.to_formatted_string(source, n)
                )
            }
            _ => {
                format!(
                    "{}\n{} {}",
                    self.left.to_formatted_string(source, n),
                    self.op.to_formatted_string(source, n + 1).indent(n + 1),
                    self.right.to_formatted_string(source, n + 1)
                )
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct GroupedExpression {
    pub expr: Box<Expression>,
    pos: Offset,
}
impl GroupedExpression {
    pub fn new(expr: Box<Expression>, offset: (usize, usize)) -> Self {
        Self {
            expr,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for GroupedExpression {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for GroupedExpression {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        let indented_close_paren = ")".indent(n);
        match &self.expr.to_formatted_string(source, 0).len() {
            0..=MAX_LINE_LENGTH => {
                format!("({})", self.expr.to_formatted_string(source, n))
            }
            _ => {
                format!(
                    "(\n{}\n{indented_close_paren}",
                    self.expr.to_formatted_string(source, n + 1).indent(n + 1)
                )
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Pipeline {
    pub first: Box<Expression>,
    /// guaranteed to always be at least size 1
    pub rest: Vec<Callable>,
    pos: Offset,
}
impl Pipeline {
    pub fn new(first: Box<Expression>, rest: Vec<Callable>) -> Self {
        let offset = (
            first.offset().start,
            rest.last().unwrap_or(&Callable::default()).offset().end,
        );
        Self {
            first,
            rest,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for Pipeline {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for Pipeline {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}{}",
            self.first.to_formatted_string(source, n),
            match &self
                .rest
                .iter()
                .map(|v| v.to_formatted_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0 => unreachable!("Pipelines always have other callables parsed."),
                1..=MAX_LINE_LENGTH => format!(
                    " | {}",
                    self.rest
                        .iter()
                        .map(|v| v.to_formatted_string(source, n))
                        .collect::<Vec<_>>()
                        .join(" | "),
                ),
                _ => format!(
                    "\n{}",
                    self.rest
                        .iter()
                        .map(|v| format!("| {}", v.to_formatted_string(source, n)).indent(n))
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

// }}}

// COLLETION LITERAL PARSERS {{{

#[derive(Clone, Debug)]
pub struct ArrayLiteral {
    pub exprs: Vec<Expression>,
    pos: Offset,
}
impl ArrayLiteral {
    pub fn new(exprs: Vec<Expression>, offset: (usize, usize)) -> Self {
        Self {
            exprs,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for ArrayLiteral {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for ArrayLiteral {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "[{}",
            match &self
                .exprs
                .iter()
                .map(|v| v.to_formatted_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => {
                    format!(
                        "{}]",
                        self.exprs
                            .iter()
                            .map(|expr| expr.to_formatted_string(source, n))
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
                                .map(|expr| expr.to_formatted_string(source, n + 1).indent(n + 1)
                                    + ",")
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
    pos: Offset,
}
impl SetLiteral {
    pub fn new(exprs: Vec<Expression>, offset: (usize, usize)) -> Self {
        Self {
            exprs,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for SetLiteral {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for SetLiteral {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "#[{}",
            match &self
                .exprs
                .iter()
                .map(|v| v.to_formatted_string(source, 0))
                .collect::<Vec<_>>()
                .join("")
                .len()
            {
                0..=MAX_LINE_LENGTH => {
                    format!(
                        "{}]",
                        self.exprs
                            .iter()
                            .map(|expr| expr.to_formatted_string(source, n))
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
                                .map(|expr| expr.to_formatted_string(source, n + 1).indent(n + 1)
                                    + ",")
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
    pos: Offset,
}
impl MapLiteral {
    pub fn new(exprs: Vec<(Expression, Expression)>, offset: (usize, usize)) -> Self {
        Self {
            exprs,
            pos: Offset::new(offset.0, offset.1),
        }
    }
}
impl<'a> Position<'a> for MapLiteral {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for MapLiteral {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "#[{}",
            if self.exprs.len() == 0 {
                ":".to_string()
            } else {
                match &self
                    .exprs
                    .iter()
                    .map(|(k, v)| {
                        format!(
                            "{}: {}",
                            k.to_formatted_string(source, n),
                            v.to_formatted_string(source, n)
                        )
                    })
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
                                    k.to_formatted_string(source, n),
                                    v.to_formatted_string(source, n)
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
                                        k.to_formatted_string(source, n + 1),
                                        v.to_formatted_string(source, n + 1)
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

// }}}

// DATA TYPE PRODUCTIONS {{{

#[derive(Clone, Debug)]
pub enum DataType {
    Token(Token),
    Vec(VecType),
    Set(SetType),
    Map(MapType),
}
impl<'a> DataType {
    pub fn eq_dtype(&self, other: &DataType, source: &'a str) -> bool {
        match (self, other) {
            // this needs to be first so the next case will not consider
            // comparing token with token
            (Self::Token(first), Self::Token(second)) => first.eq_dtype(second, source),
            (Self::Token(res), _) | (_, Self::Token(res)) => res.kind == TokenKind::Dono,
            (Self::Vec(first), Self::Vec(second)) => first.eq_dtype(second, source),
            (Self::Set(first), Self::Set(second)) => first.eq_dtype(second, source),
            (Self::Map(first), Self::Map(second)) => first.eq_dtype(second, source),
            _ => false,
        }
    }
    fn as_position(&self) -> &dyn Position {
        match self {
            Self::Token(res) => res,
            Self::Vec(res) => res,
            Self::Set(res) => res,
            Self::Map(res) => res,
        }
    }
}
impl<'a> Position<'a> for DataType {
    fn offset(&self) -> Offset {
        self.as_position().offset()
    }
}
impl<'a> Production<'a> for DataType {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        match self {
            Self::Token(tok) => tok.to_formatted_string(source, n),
            Self::Vec(vec) => vec.to_formatted_string(source, n),
            Self::Set(set) => set.to_formatted_string(source, n),
            Self::Map(map) => map.to_formatted_string(source, n),
        }
    }
}
impl Default for DataType {
    fn default() -> Self {
        Self::Token(Token::default())
    }
}

#[derive(Clone, Debug)]
pub enum Vectorable {
    Token(Token),
    Set(SetType),
    Map(MapType),
}
impl<'a> Vectorable {
    pub fn eq_dtype(&self, other: &Vectorable, source: &'a str) -> bool {
        match (self, other) {
            // this needs to be first so the next case will not consider
            // comparing token with token
            (Self::Token(first), Self::Token(second)) => first.eq_dtype(second, source),
            (Self::Token(res), _) | (_, Self::Token(res)) => res.kind == TokenKind::Dono,
            (Self::Set(first), Self::Set(second)) => first.eq_dtype(second, source),
            (Self::Map(first), Self::Map(second)) => first.eq_dtype(second, source),
            _ => false,
        }
    }
    fn as_position(&self) -> &dyn Position {
        match self {
            Self::Token(res) => res,
            Self::Set(res) => res,
            Self::Map(res) => res,
        }
    }
}
impl<'a> Position<'a> for Vectorable {
    fn offset(&self) -> Offset {
        self.as_position().offset()
    }
}
impl<'a> Production<'a> for Vectorable {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        match self {
            Self::Token(tok) => tok.to_formatted_string(source, n),
            Self::Set(set) => set.to_formatted_string(source, n),
            Self::Map(map) => map.to_formatted_string(source, n),
        }
    }
}
impl Default for Vectorable {
    fn default() -> Self {
        Self::Token(Token::default())
    }
}

/// Example: `aqua-chan[1]` where this signifies that `aqua` has a 1D array of
/// `chan` values.
/// - equality checks the data type, the dimension, and the fact that its a
/// [VecType]
/// - note that the dimension is always a [TokenType::IntLiteral]
#[derive(Clone, Debug)]
pub struct VecType {
    pub id: Vectorable,
    pub dim: Token,
    pos: Offset,
}
impl<'a> VecType {
    pub fn new(id: Vectorable, dim: Token, offset: (usize, usize)) -> Self {
        Self {
            id,
            dim,
            pos: Offset::new(offset.0, offset.1),
        }
    }
    pub fn eq_dtype(&self, other: &VecType, source: &'a str) -> bool {
        self.id.eq_dtype(&other.id, source)
            && self.dim.source_str(source) == other.dim.source_str(source)
    }
}
impl<'a> Position<'a> for VecType {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for VecType {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}[{}]",
            self.id.to_formatted_string(source, n),
            self.dim.source_str(source)
        )
    }
}

/// Example: `aqua-chan{}` where this signifies that `aqua` has a hashset of
/// chan` values.
/// - equality checks are the same as [Token] which only checks the id
///     - in this case `chan` and the fact its a [SetType]
#[derive(Clone, Debug)]
pub struct SetType {
    pub tok: Token,
    pos: Offset,
}
impl<'a> SetType {
    pub fn new(tok: Token, offset: (usize, usize)) -> Self {
        Self {
            tok,
            pos: Offset::new(offset.0, offset.1),
        }
    }
    pub fn eq_dtype(&self, other: &SetType, source: &'a str) -> bool {
        self.tok.eq_dtype(&other.tok, source)
    }
}
impl<'a> Position<'a> for SetType {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for SetType {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!("{}{{}}", self.tok.to_formatted_string(source, n))
    }
}

/// Example: `aqua-senpai{chan}` where this signifies that `aqua` has a hashmap
/// of `chan` values mapped to `senpai` keys
/// - equality checks the data type, the inner data type, and the fact that its
/// a [MapType]
#[derive(Clone, Debug)]
pub struct MapType {
    pub tok: Token,
    pub inner: Box<DataType>,
    pos: Offset,
}
impl<'a> MapType {
    pub fn new(tok: Token, inner: Box<DataType>, offset: (usize, usize)) -> Self {
        Self {
            tok,
            inner,
            pos: Offset::new(offset.0, offset.1),
        }
    }
    pub fn eq_dtype(&self, other: &MapType, source: &'a str) -> bool {
        self.tok.eq_dtype(&other.tok, source) && self.inner.eq_dtype(&other.inner, source)
    }
}
impl<'a> Position<'a> for MapType {
    fn offset(&self) -> Offset {
        self.pos
    }
}
impl<'a> Production<'a> for MapType {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        format!(
            "{}{{{}}}",
            self.tok.to_formatted_string(source, n),
            self.inner.to_formatted_string(source, 0)
        )
    }
}
// }}}

// IDENTIFIER BASED ON CONTEXT PRODUCTIONS {{{

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
impl Assignable {
    fn as_position(&self) -> &dyn Position {
        match self {
            Self::Token(res) => res,
            Self::Indexed(res) => res,
            Self::Access(res) => res,
        }
    }
}
impl<'a> Position<'a> for Assignable {
    fn offset(&self) -> Offset {
        self.as_position().offset()
    }
}
impl<'a> Production<'a> for Assignable {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        match self {
            Self::Token(id) => id.to_formatted_string(source, n),
            Self::Indexed(idx) => idx.to_formatted_string(source, n),
            Self::Access(access) => access.to_formatted_string(source, n),
        }
    }
}
impl TryFrom<Expression> for Assignable {
    type Error = ();
    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        match value {
            Expression::Token(res) => Ok(Self::Token(res)),
            Expression::Indexed(res) => Ok(Self::Indexed(res)),
            Expression::Access(AccessType::Field(field)) => Ok(Assignable::Access(field)),
            Expression::Access(AccessType::Method(method)) => match method.accessed.last() {
                Some(Accessed::IndexedId(_)) => Ok(Assignable::Access(method.into())),
                Some(Accessed::FnCall(_)) | None => Err(()),
                Some(Accessed::Token(_)) => unreachable!(
                    r#"TryFrom<Expression> for Assignable: cannot access token when access type is method. Something wrong happened in parsing"#
                ),
            },
            Expression::FnCall(_)
            | Expression::GroupInit(_)
            | Expression::Pipeline(_)
            | Expression::Array(_)
            | Expression::Set(_)
            | Expression::Map(_)
            | Expression::Prefix(_)
            | Expression::Infix(_)
            | Expression::Grouped(_) => Err(()),
        }
    }
}

/// Productions that can be indexed into
/// - `aqua[1]`
/// - `aqua()[1]`
#[derive(Clone, Debug)]
pub enum Indexable {
    Token(Token),
    Array(ArrayLiteral),
    Set(SetLiteral),
    Map(MapLiteral),
    FnCall(FnCall),
    Access(AccessType),
    Pipeline(Pipeline),
}
impl Indexable {
    fn as_position(&self) -> &dyn Position {
        match self {
            Self::Token(res) => res,
            Self::Array(res) => res,
            Self::Set(res) => res,
            Self::Map(res) => res,
            Self::FnCall(res) => res,
            Self::Access(res) => res,
            Self::Pipeline(res) => res,
        }
    }
    fn as_production(&self) -> &dyn Production {
        match self {
            Self::Token(res) => res,
            Self::Array(res) => res,
            Self::Set(res) => res,
            Self::Map(res) => res,
            Self::FnCall(res) => res,
            Self::Access(res) => res,
            Self::Pipeline(res) => res,
        }
    }
}
impl<'a> Position<'a> for Indexable {
    fn offset(&self) -> Offset {
        self.as_position().offset()
    }
}
impl<'a> Production<'a> for Indexable {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        self.as_production().to_formatted_string(source, n)
    }
}
impl TryFrom<Expression> for Indexable {
    type Error = String;
    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        match value {
            Expression::FnCall(res) => Ok(Self::FnCall(res)),
            Expression::Access(res) => Ok(Self::Access(res)),
            Expression::Indexed(_) => Err(
                r#"Cannot convert Indexable to Indexable: you can only index an item once"#.into(),
            ),
            Expression::Token(res) => match res.kind {
                TokenKind::Identifier | TokenKind::StringLiteral => Ok(Self::Token(res)),
                _ => Err(format!(
                    r#"Cannot convert token kind '{:?}': only Identifiers and Strings are Indexable"#,
                    res.kind
                )),
            },
            Expression::Array(res) => Ok(Self::Array(res)),
            Expression::Set(res) => Ok(Self::Set(res)),
            Expression::Map(res) => Ok(Self::Map(res)),
            Expression::Pipeline(res) => Ok(Self::Pipeline(res)),
            Expression::GroupInit(_) | Expression::Prefix(_) | Expression::Infix(_) => {
                Err(format!(
                    r#"Cannot convert Expression type '{:?}' to Indexable"#,
                    value
                ))
            }
            Expression::Grouped(res) => Self::try_from(*res.expr),
        }
    }
}

/// Productions that can be accessed
/// - `aqua.arms[1].wind_up().punch()~`
/// - in this case, aqua is the accessor and the rest are accessed
#[derive(Clone, Debug)]
pub enum Accessed {
    Token(Token),
    FnCall(FnCall),
    IndexedId(IndexedId),
}
impl Accessed {
    fn as_position(&self) -> &dyn Position {
        match self {
            Self::Token(tok) => tok,
            Self::FnCall(fn_call) => fn_call,
            Self::IndexedId(id) => id,
        }
    }
}
impl<'a> Position<'a> for Accessed {
    fn offset(&self) -> Offset {
        self.as_position().offset()
    }
}
impl<'a> Production<'a> for Accessed {
    fn to_formatted_string(&self, source: &'a str, n: usize) -> String {
        match self {
            Self::Token(tok) => tok.to_formatted_string(source, n),
            Self::FnCall(fn_call) => fn_call.to_formatted_string(source, n),
            Self::IndexedId(id) => id.to_formatted_string(source, n),
        }
    }
}
impl Default for Accessed {
    fn default() -> Self {
        Self::Token(Token::default())
    }
}
// }}}
