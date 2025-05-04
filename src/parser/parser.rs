use crate::errors::parse_errors::{
    EmptyBodyError, EmptyBodyErrorType, NonCallableInPipelineError, UnexpectedTokenError,
};
use crate::lexer::token::{Position, Token, TokenKind};
use crate::parser::productions::*;
use std::collections::HashMap;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Debug)]
enum Precedence {
    #[default]
    Lowest,
    LogicalOr,  // or
    LogicalAnd, // and
    Equality,   // == !=
    Comparison, // > >= < <=
    Sum,        // + -
    Product,    // * / %
    Prefix,     // not -
}
impl Precedence {
    fn of(token_kind: TokenKind) -> Self {
        match token_kind {
            TokenKind::Or => Self::LogicalOr,
            TokenKind::And => Self::LogicalAnd,
            TokenKind::Equal | TokenKind::NotEqual => Self::Equality,
            TokenKind::LessThan
            | TokenKind::LessEqual
            | TokenKind::GreaterThan
            | TokenKind::GreaterEqual => Self::Comparison,
            TokenKind::Plus | TokenKind::Dash => Self::Sum,
            TokenKind::Multiply | TokenKind::Divide | TokenKind::Modulo => Self::Product,
            TokenKind::Not => Self::Prefix,
            _ => Self::Lowest,
        }
    }
}

pub struct Parser<'a> {
    pub program: Program,
    pub error: String,
    source: &'a str,
    line_starts: &'a Vec<usize>,
    tokens: Vec<Token>,
    pos: usize,
    prefix_parse_fns: HashMap<TokenKind, for<'b> fn(&mut Self) -> Result<Expression, ()>>,
    infix_parse_fns:
        HashMap<TokenKind, for<'b> fn(&mut Self, Expression) -> Result<Expression, ()>>,
    body_parse_fns: HashMap<TokenKind, for<'b> fn(&mut Self) -> Result<Statement, ()>>,
}
impl<'a> Parser<'a> {
    // TODO: expected values for some are inaccurate. For example:
    // - parse_expression: only expects Terminator when a Dot or Pipe can
    // follow it as well.
    // - and more... i forgot the rest sorry...
    pub fn new(source: &'a str, line_starts: &'a Vec<usize>, tokens: Vec<Token>) -> Self {
        let mut parser = Self {
            program: Program::default(),
            tokens,
            source,
            line_starts,
            error: "".to_string(),
            pos: 0,
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            body_parse_fns: HashMap::new(),
        };
        parser.register_init();
        parser
    }

    // REGISTERING PARSERS {{{

    fn register_init(&mut self) {
        // starting tokens of values
        self.register_prefix(vec![
            // prefix operators
            (TokenKind::Dash, Self::parse_prefix_expression),
            (TokenKind::Not, Self::parse_prefix_expression),
            // grouped literals
            (TokenKind::LParen, Self::parse_grouped_expression),
            (TokenKind::LBracket, Self::parse_array_literal),
            (TokenKind::Hash, Self::parse_hash_literal),
            // units
            // (TokenKind::Identifier, Self::parse_ident_expression),
            (TokenKind::Identifier, Self::parse_ident_expression_unit),
            (TokenKind::Type, Self::parse_group_init),
            (TokenKind::IntLiteral, Self::parse_literal),
            (TokenKind::FloatLiteral, Self::parse_literal),
            (TokenKind::StringLiteral, Self::parse_literal),
            (TokenKind::CharLiteral, Self::parse_literal),
            (TokenKind::Fax, Self::parse_literal),
            (TokenKind::Cap, Self::parse_literal),
            (TokenKind::Nuww, Self::parse_literal),
        ]);
        // operators
        self.register_infix(vec![
            // math
            TokenKind::Plus,
            TokenKind::Dash,
            TokenKind::Multiply,
            TokenKind::Divide,
            TokenKind::Modulo,
            // comparison
            TokenKind::Equal,
            TokenKind::NotEqual,
            TokenKind::LessThan,
            TokenKind::LessEqual,
            TokenKind::GreaterThan,
            TokenKind::GreaterEqual,
            // logical
            TokenKind::And,
            TokenKind::Or,
        ]);
        // starting tokens for body statements.
        self.register_body(vec![
            // STATEMENTS
            (TokenKind::Hi, Self::parse_declaration),
            (TokenKind::Identifier, Self::parse_ident_statement),
            (TokenKind::Iwf, Self::parse_if),
            (TokenKind::Mash, Self::parse_mash),
            (TokenKind::Fow, Self::parse_for),
            (TokenKind::Continue, Self::parse_continue),
            (TokenKind::Bweak, Self::parse_break),
            (TokenKind::Wetuwn, Self::parse_return),
            // EXPRESSIONS
            // prefix operators
            (TokenKind::Dash, Self::parse_expression_statement),
            (TokenKind::Not, Self::parse_expression_statement),
            // grouped literals
            (TokenKind::LParen, Self::parse_expression_statement),
            (TokenKind::LBracket, Self::parse_expression_statement),
            (TokenKind::Hash, Self::parse_expression_statement),
            // units
            // (TokenKind::Identifier, Self::parse_expression_statement),
            (TokenKind::Type, Self::parse_expression_statement),
            (TokenKind::IntLiteral, Self::parse_expression_statement),
            (TokenKind::FloatLiteral, Self::parse_expression_statement),
            (TokenKind::StringLiteral, Self::parse_expression_statement),
            (TokenKind::CharLiteral, Self::parse_expression_statement),
            (TokenKind::Fax, Self::parse_expression_statement),
            (TokenKind::Cap, Self::parse_expression_statement),
            (TokenKind::Nuww, Self::parse_expression_statement),
        ]);
    }

    // }}}

    // MOVEMENT METHODS {{{

    /// advances cursor n times
    fn advance(&mut self, n: usize) -> () {
        for _ in 0..n {
            if self.curr_tok().kind == TokenKind::EOF {
                return;
            }
            self.pos += 1;
        }
    }
    /// reverses cursor n times
    fn reverse(&mut self, n: usize) -> () {
        for _ in 0..n {
            if self.curr_tok().kind == TokenKind::EOF {
                return;
            }
            self.pos -= 1;
        }
    }

    // }}}

    // STATE TRACKING {{{

    fn curr_tok(&self) -> Token {
        self.tokens
            .get(self.pos)
            .map(|&tok| tok)
            .unwrap_or_default()
    }
    fn peek_tok(&self) -> Token {
        self.tokens
            .get(self.pos + 1)
            .map(|&tok| tok)
            .unwrap_or_default()
    }

    // }}}

    // TOP LEVEL PARSERS {{{

    pub fn parse_program(&mut self) -> Result<(), ()> {
        let mut main: Option<Function> = None;
        let mut functions: Vec<Function> = vec![];
        let mut groups: Vec<Group> = vec![];
        let mut methods: Vec<GroupMethod> = vec![];
        let mut contracts: Vec<Contract> = vec![];
        let mut globals: Vec<Statement> = vec![];
        let mut end = (0, 0);

        while !self.curr_tok_is(TokenKind::EOF) {
            match self.curr_tok().kind {
                TokenKind::Fun => match self.peek_tok().kind {
                    TokenKind::Main => main = Some(self.parse_function()?),
                    TokenKind::Type => methods.push(self.parse_method()?),
                    _ => functions.push(self.parse_function()?),
                },
                TokenKind::Group => groups.push(self.parse_group()?),
                TokenKind::Contract => contracts.push(self.parse_contract()?),
                TokenKind::Hi => globals.push(self.parse_declaration()?),
                _ => {
                    self.unexpected_token_error(
                        Some("[INVALID GLOBAL TOKEN]"),
                        Some("Can only declare functions, groups, contracts, or variables here"),
                        self.curr_tok(),
                        vec![
                            TokenKind::Fun,
                            TokenKind::Group,
                            TokenKind::Contract,
                            TokenKind::Hi,
                        ],
                    );
                    return Err(());
                }
            }
            if self.peek_tok_is(TokenKind::EOF) {
                end = self.curr_tok().pos_end(&self.line_starts);
            }
            self.advance(1);
        }
        self.program = Program::new(
            main,
            functions,
            groups,
            methods,
            contracts,
            globals,
            self.source,
            end,
        )
        .map_err(|e| self.error = e.to_string())?;
        Ok(())
    }

    /// starts with [TokenKind::Fun] in current
    /// ends with [TokenKind::RBrace] in current
    fn parse_function(&mut self) -> Result<Function, ()> {
        let start = self.curr_tok();
        let id = self.expect_peek_is_in(&[TokenKind::Identifier, TokenKind::Main])?;
        self.expect_peek_is(TokenKind::Dash)?;
        let dtype = self
            .expect_peek_is_type()
            .and_then(|tok| self.parse_data_type(tok))?;
        self.expect_peek_is(TokenKind::LParen)?;
        let params = self.parse_params()?;
        self.expect_peek_is(TokenKind::LBrace)?;
        let body = self.parse_body(EmptyBodyErrorType::Fn)?;
        Ok(Function::new(
            id,
            dtype,
            params,
            body,
            (start.offset.start, self.curr_tok().offset.end),
        ))
    }

    /// starts with [TokenKind::Fun] in current
    /// ends with [TokenKind::RBrace] in current
    fn parse_method(&mut self) -> Result<GroupMethod, ()> {
        let start = self.curr_tok();
        let group = self.expect_peek_is_type()?;
        let mutable = self
            .peek_tok_is(TokenKind::Bang)
            .then(|| self.advance(1))
            .is_some();
        let id = self.expect_peek_is(TokenKind::Identifier)?;
        self.expect_peek_is(TokenKind::Dash)?;
        let dtype = self
            .expect_peek_is_type()
            .and_then(|tok| self.parse_data_type(tok))?;
        self.expect_peek_is(TokenKind::LParen)?;
        let params = self.parse_params()?;
        self.expect_peek_is(TokenKind::LBrace)?;
        let body = self.parse_body(EmptyBodyErrorType::Method)?;
        Ok(GroupMethod::new(
            id,
            group,
            mutable,
            dtype,
            params,
            body,
            (start.offset.start, self.curr_tok().offset.end),
        ))
    }

    /// starts with [TokenKind::Group] in current
    /// ends with [TokenKind::RBrace] in current
    fn parse_group(&mut self) -> Result<Group, ()> {
        let start = self.curr_tok();
        let id = self.expect_peek_is(TokenKind::Type)?;
        self.expect_peek_is(TokenKind::LBrace)?;
        // disallow empty bodies
        if self.peek_tok_is(TokenKind::RBrace) {
            self.empty_body_error(
                self.curr_tok(),
                self.peek_tok(),
                vec![TokenKind::Identifier],
                EmptyBodyErrorType::Group,
                self.source
                    .lines()
                    .nth(self.peek_tok().line(&self.line_starts))
                    .unwrap_or_default(),
            );
            self.advance(1);
            return Err(());
        }
        let fields = self.parse_fields()?;
        Ok(Group::new(
            id,
            fields,
            (start.offset.start, self.curr_tok().offset.end),
        ))
    }

    /// starts with [TokenKind::Contract] in current
    /// ends with [TokenKind::RBrace] in current
    fn parse_contract(&mut self) -> Result<Contract, ()> {
        let start = self.curr_tok();
        let id = self.expect_peek_is(TokenKind::Type)?;
        self.expect_peek_is(TokenKind::LBrace)?;
        if self.peek_tok_is(TokenKind::RBrace) {
            self.empty_body_error(
                self.curr_tok(),
                self.peek_tok(),
                vec![TokenKind::Identifier],
                EmptyBodyErrorType::Contract,
                self.source
                    .lines()
                    .nth(self.peek_tok().line(&self.line_starts))
                    .unwrap_or_default(),
            );
            return Err(());
        }
        let mut signatures: Vec<FnSignature> = vec![];
        while !self.peek_tok_is(TokenKind::RBrace) {
            signatures.push(self.parse_fn_signature()?);
        }
        self.expect_peek_is(TokenKind::RBrace)?;
        Ok(Contract::new(
            id,
            signatures,
            (start.offset.start, self.curr_tok().offset.end),
        ))
    }

    /// starts with [TokenKind::LParen] in current
    /// ends with [TokenKind::RParen] in current
    fn parse_params(&mut self) -> Result<Vec<Param>, ()> {
        let mut params: Vec<Param> = vec![];
        while !self.peek_tok_is(TokenKind::RParen) {
            let param = self.parse_param()?;
            self.allow_hanging_comma(TokenKind::RParen)?;
            if param.variadic && !self.peek_tok_is(TokenKind::RParen) {
                self.unexpected_token_error(
                    Some("[PARAMETER AFTER VARIADIC]"),
                    Some("Variadic parameter should be the last parameter."),
                    self.peek_tok(),
                    vec![TokenKind::RParen],
                );
                return Err(());
            }
            params.push(param);
        }
        self.advance(1);
        Ok(params)
    }

    /// starts with [TokenKind::Identifier] in peek
    /// ends with [DataType]'s last token in current
    fn parse_param(&mut self) -> Result<Param, ()> {
        let id = self.expect_peek_is(TokenKind::Identifier)?;
        self.expect_peek_is(TokenKind::Dash)?;
        let dtype = self
            .expect_peek_is_type()
            .and_then(|tok| self.parse_data_type(tok))?;
        let variadic = self
            .peek_tok_is(TokenKind::Ellipsis)
            .then(|| self.advance(1))
            .is_some();
        Ok(Param::new(
            id,
            dtype,
            variadic,
            (id.offset.start, self.curr_tok().offset.end),
        ))
    }

    /// difference with [Self::parse_params] is that this does not allow variadic fields
    ///
    /// starts with [TokenKind::LBrace] in current
    /// ends with [TokenKind::RBrace] in current
    fn parse_fields(&mut self) -> Result<Vec<GroupField>, ()> {
        let mut fields = vec![];
        while !self.peek_tok_is(TokenKind::RBrace) {
            fields.push(self.parse_field()?);
        }
        self.expect_peek_is(TokenKind::RBrace)?;
        Ok(fields)
    }

    /// difference with [Self::parse_param] is that this does not allow variadic fields
    ///
    /// starts with [TokenKind::Identifier] in peek
    /// ends with [TokenKind::Terminator] in current
    fn parse_field(&mut self) -> Result<GroupField, ()> {
        let id = self.expect_peek_is(TokenKind::Identifier)?;
        self.expect_peek_is(TokenKind::Dash)?;
        let dtype = self
            .expect_peek_is_type()
            .and_then(|tok| self.parse_data_type(tok))?;
        let offset_end = self
            .expect_peek_is(TokenKind::Terminator)
            .and_then(|tok| Ok(tok.offset.end))?;
        Ok(GroupField::new(id, dtype, (id.offset.start, offset_end)))
    }

    /// starts with [TokenKind::Identifier] in peek
    /// ends with [TokenKind::Terminator] in current
    fn parse_fn_signature(&mut self) -> Result<FnSignature, ()> {
        let id = self
            .expect_peek_is(TokenKind::Identifier)
            .and_then(|tok| Ok(tok))?;
        self.expect_peek_is(TokenKind::Dash)?;
        let dtype = self
            .expect_peek_is_type()
            .and_then(|tok| self.parse_data_type(tok))?;
        self.expect_peek_is(TokenKind::LParen)?;
        let mut params = vec![];
        while !self.peek_tok_is(TokenKind::RParen) {
            params.push(
                self.expect_peek_is_type()
                    .and_then(|tok| self.parse_data_type(tok))?,
            );
            self.allow_hanging_comma(TokenKind::RParen)?;
        }
        self.expect_peek_is(TokenKind::RParen)?;
        let offset_end = self
            .expect_peek_is(TokenKind::Terminator)
            .and_then(|tok| Ok(tok.offset.end))?;
        Ok(FnSignature::new(
            id,
            dtype,
            params,
            (id.offset.start, offset_end),
        ))
    }

    /// starts with [TokenKind::LBrace] in current
    /// ends with [TokenKind::RBrace] in current
    fn parse_body(&mut self, body_type: EmptyBodyErrorType) -> Result<Body, ()> {
        let mut statements: Vec<Statement> = vec![];
        let start = self.curr_tok();
        // disallow empty bodies
        if self.peek_tok_is(TokenKind::RBrace) {
            self.empty_body_error(
                self.curr_tok(),
                self.peek_tok(),
                self.body_parse_fns
                    .clone()
                    .into_iter()
                    .map(|(token, _)| token)
                    .collect::<Vec<_>>(),
                body_type,
                self.source
                    .lines()
                    .nth(self.peek_tok().line(&self.line_starts))
                    .unwrap_or_default(),
            );
            self.advance(1);
            return Err(());
        }
        while !self.peek_tok_is(TokenKind::RBrace) {
            self.advance(1);
            let (expected, body_parser) = match body_type {
                EmptyBodyErrorType::For => {
                    let filtered_refs = &self
                        .body_parse_fns
                        .iter()
                        .filter(|&v| !&[TokenKind::Bweak, TokenKind::Continue].contains(&v.0))
                        .collect::<HashMap<_, _>>();
                    (
                        filtered_refs
                            .iter()
                            .map(|(&&tokens, _)| tokens)
                            .collect::<Vec<_>>(),
                        filtered_refs
                            .iter()
                            .map(|(&&a, &&b)| (a, b))
                            .collect::<HashMap<_, _>>(),
                    )
                }
                _ => {
                    let parser_refs = self.body_parse_fns.iter().collect::<HashMap<_, _>>();
                    (
                        parser_refs
                            .iter()
                            .map(|(&&tokens, _)| tokens)
                            .collect::<Vec<_>>(),
                        parser_refs
                            .iter()
                            .map(|(&&tok, &&parser)| (tok, parser))
                            .collect::<HashMap<_, _>>(),
                    )
                }
            };
            let stmt = match body_parser.get(&self.curr_tok().kind) {
                Some(parser) => parser(self)?,
                None => {
                    self.no_parsing_fn_error(
                        MissingParserErrorHeader::Body,
                        self.curr_tok(),
                        expected,
                    );
                    return Err(());
                }
            };
            statements.push(stmt);
        }
        let offset_end = self
            .expect_peek_is(TokenKind::RBrace)
            .and_then(|tok| Ok(tok.offset.end))?;
        Ok(Body::new(statements, (start.offset.start, offset_end)))
    }

    // }}}

    // DATA TYPE PARSERS {{{

    /// starts with [DataType]'s first token in current
    /// ends with [DataType]'s last token in current
    /// end token may be:
    /// - [DataType] token literal (`chan`, `kun`, `senpai`...)
    /// - [TokenKind::RBracket] (`chan[1]`, `kun[3]` ...)
    /// - [TokenKind::RBrace] (`chan{}`, `kun{senpai}` ...)
    fn parse_data_type(&mut self, tok: Token) -> Result<DataType, ()> {
        match self.peek_tok().kind {
            TokenKind::LBracket => self.parse_vector_type(Vectorable::Token(tok)),
            TokenKind::LBrace => {
                self.advance(2);
                match self.curr_tok().kind {
                    TokenKind::RBrace => match self.peek_tok().kind {
                        TokenKind::LBracket => self.parse_vector_type(Vectorable::Set(
                            SetType::new(tok, (tok.offset.start, tok.offset.end)),
                        )),
                        _ => Ok(DataType::Set(SetType::new(
                            tok,
                            (tok.offset.start, tok.offset.end),
                        ))),
                    },
                    TokenKind::Type
                    | TokenKind::Chan
                    | TokenKind::Kun
                    | TokenKind::Senpai
                    | TokenKind::Kouhai
                    | TokenKind::Sama
                    | TokenKind::San
                    | TokenKind::Dono => {
                        let inner = Box::new(self.parse_data_type(self.curr_tok())?);
                        let offset_end = self
                            .expect_peek_is(TokenKind::RBrace)
                            .and_then(|tok| Ok(tok.offset.end))?;
                        let res = MapType::new(tok, inner, (tok.offset.start, offset_end));
                        match self.peek_tok().kind {
                            TokenKind::LBracket => self.parse_vector_type(Vectorable::Map(res)),
                            _ => Ok(DataType::Map(res)),
                        }
                    }
                    _ => {
                        self.unexpected_token_error(
                            Some("[INVALID INNER DATA TYPE]"),
                            Some("Only data types are allowed to be inner types"),
                            self.curr_tok(),
                            TokenKind::data_types(),
                        );
                        self.advance(2);
                        return Err(());
                    }
                }
            }
            _ => Ok(DataType::Token(tok)),
        }
    }

    /// starts with [TokenKind::LBracket] in peek
    /// ends with [TokenKind::RBracket] in current
    fn parse_vector_type(&mut self, id: Vectorable) -> Result<DataType, ()> {
        self.advance(1);
        let dim = self.expect_peek_is(TokenKind::IntLiteral)?;
        let offset = self
            .expect_peek_is(TokenKind::RBracket)
            .and_then(|tok| Ok((id.offset().start, tok.offset.end)))?;
        Ok(DataType::Vec(VecType::new(id, dim, offset)))
    }

    // }}}

    // STATEMENT PARSERS {{{
    /// starts with [TokenKind::Hi] in current
    /// ends with [TokenKind::Terminator] in current
    fn parse_declaration(&mut self) -> Result<Statement, ()> {
        let start = self.curr_tok();
        let id = self.expect_peek_is(TokenKind::Identifier)?;
        let dtype = self
            .peek_tok_is(TokenKind::Dash)
            .then(|| {
                self.advance(1);
                self.peek_tok_is_type()
                    .then(|| {
                        self.advance(1);
                        self.parse_data_type(self.curr_tok()).ok()
                    })
                    .flatten()
            })
            .flatten();
        let mutable = self
            .peek_tok_is(TokenKind::Bang)
            .then(|| self.advance(1))
            .is_some();
        let optional = self
            .peek_tok_is(TokenKind::Question)
            .then(|| self.advance(1))
            .is_some();
        self.expect_peek_is(TokenKind::Assign)?;
        let expr = self.parse_expression(Precedence::Lowest)?;
        let offset_end = self
            .expect_peek_is(TokenKind::Terminator)
            .and_then(|tok| Ok(tok.offset.end))?;
        Ok(Statement::Declaration(Declaration::new(
            id,
            dtype,
            mutable,
            optional,
            expr,
            (start.offset.start, offset_end),
        )))
    }

    /// starts with the ending token of an [Assignable] in current:
    /// - [TokenKind::Identifier]
    /// - [TokenKind::RBracket]
    /// - [TokenKind::RParen]
    /// ends with [TokenKind::Terminator] in current
    fn parse_assignment(&mut self, id: Assignable) -> Result<Assignment, ()> {
        let offset_start = id.offset().start;
        let assign_op = self.expect_peek_is_assign_op()?;
        let expr = self.parse_expression(Precedence::Lowest)?;
        let offset_end = self
            .expect_peek_is(TokenKind::Terminator)
            .and_then(|tok| Ok(tok.offset.end))?;
        Ok(Assignment::new(
            id,
            assign_op,
            expr,
            (offset_start, offset_end),
        ))
    }

    /// starts with [TokenKind::Wetuwn] in current
    /// ends with [TokenKind::Terminator] in current
    fn parse_return(&mut self) -> Result<Statement, ()> {
        let start = self.curr_tok();
        let expr = self.parse_expression(Precedence::Lowest)?;
        let offset_end = self
            .expect_peek_is(TokenKind::Terminator)
            .and_then(|tok| Ok(tok.offset.end))?;
        Ok(Statement::Return(ReturnStatement::new(
            expr,
            (start.offset.start, offset_end),
        )))
    }

    /// starts with [TokenKind::Iwf] in current
    /// ends with [TokenKind::RBrace] in current
    fn parse_if(&mut self) -> Result<Statement, ()> {
        let start = self.curr_tok();
        let condition = self.parse_expression(Precedence::Lowest).and_then(|expr| {
            self.expect_peek_is(TokenKind::LBrace)?;
            Ok(expr)
        })?;
        let body = self.parse_body(EmptyBodyErrorType::If)?;
        let elifs = match self.peek_tok().kind {
            TokenKind::Ewif => self.parse_elifs()?,
            _ => vec![],
        };
        let else_block = match self.peek_tok().kind {
            TokenKind::Ewse => {
                self.expect_peek_is(TokenKind::Ewse)?;
                self.expect_peek_is(TokenKind::LBrace)?;
                Some(self.parse_body(EmptyBodyErrorType::If)?)
            }
            _ => None,
        };
        let offset_end = self.curr_tok().offset.end;
        Ok(Statement::If(IfStatement::new(
            condition,
            body,
            elifs,
            else_block,
            (start.offset.start, offset_end),
        )))
    }

    /// starts with [TokenKind::Ewif] in peek
    /// ends with [TokenKind::RBrace] in current
    fn parse_elifs(&mut self) -> Result<Vec<ElifStatement>, ()> {
        let mut res: Vec<ElifStatement> = vec![];
        while self.peek_tok_is(TokenKind::Ewif) {
            let start = self
                .expect_peek_is(TokenKind::Ewif)
                .and_then(|tok| Ok(tok))?;
            let condition = self.parse_expression(Precedence::Lowest).and_then(|expr| {
                self.expect_peek_is(TokenKind::LBrace)?;
                Ok(expr)
            })?;
            let body = self.parse_body(EmptyBodyErrorType::If)?;
            let offset_end = self.curr_tok().offset.end;
            res.push(ElifStatement::new(
                condition,
                body,
                (start.offset.start, offset_end),
            ))
        }
        Ok(res)
    }

    /// Abstracts over [Parser::parse_for_loop] and [Parser::parse_for_each]
    /// - for loop: `fow hi i-chan = 0~ i < n~ i+1 { ...body }`
    /// - for each: `fow item in collection { ...body }`
    ///
    /// starts with [TokenKind::Fow] in current
    /// ends with [TokenKind::RBrace] in current
    fn parse_for(&mut self) -> Result<Statement, ()> {
        match self.peek_tok().kind {
            TokenKind::Hi => self.parse_for_loop(),
            TokenKind::Identifier => self.parse_for_each(),
            _ => Err(self.unexpected_token_error(
                Some("[INVALID FOW INITIALZATION]"),
                Some("Declare a variable (hi i = 1~ i<4~ i+1) or use an iterable (i in [1,2,3])"),
                self.peek_tok(),
                vec![TokenKind::Hi, TokenKind::Identifier],
            )),
        }
    }

    /// starts with [TokenKind::Fow] in current and [TokenKind::Hi] in peek
    /// ends with [TokenKind::RBrace] in current
    fn parse_for_loop(&mut self) -> Result<Statement, ()> {
        let start = self.curr_tok();
        self.expect_peek_is(TokenKind::Hi)?;
        let init = match self.parse_declaration()? {
            Statement::Declaration(decl) => decl,
            _ => unreachable!("for loop declaration parsing always returns declaration"),
        };
        let condition = self.parse_expression(Precedence::Lowest).and_then(|cond| {
            self.expect_peek_is(TokenKind::Terminator)?;
            Ok(cond)
        })?;
        let update = self
            .parse_expression(Precedence::Lowest)
            .and_then(|update| {
                self.expect_peek_is(TokenKind::LBrace)?;
                Ok(update)
            })?;
        let body = self.parse_body(EmptyBodyErrorType::For)?;
        let offset_end = self.curr_tok().offset.end;
        Ok(Statement::ForLoop(ForLoop::new(
            init,
            condition,
            update,
            body,
            (start.offset.start, offset_end),
        )))
    }

    /// starts with [TokenKind::Identifier] in peek
    /// ends with [TokenKind::RBrace] in current
    fn parse_for_each(&mut self) -> Result<Statement, ()> {
        let start = self.curr_tok();
        let item_id = self.expect_peek_is(TokenKind::Identifier)?;
        self.expect_peek_is(TokenKind::In)?;
        let collection = self.parse_expression(Precedence::Lowest)?;
        let body = self
            .expect_peek_is(TokenKind::LBrace)
            .and_then(|_| self.parse_body(EmptyBodyErrorType::For))?;
        let offset_end = self.curr_tok().offset.end;
        Ok(Statement::ForEach(ForEach::new(
            item_id,
            collection,
            body,
            (start.offset.start, offset_end),
        )))
    }

    // TODO: break statement position info does not include Terminator
    /// starts with [TokenKind::Bweak] in current
    /// ends with [TokenKind::Terminator] in current
    fn parse_break(&mut self) -> Result<Statement, ()> {
        let tok = self.curr_tok();
        self.expect_peek_is(TokenKind::Terminator)?;
        Ok(Statement::Break(tok))
    }

    // TODO: continue statement position info does not include Terminator
    /// starts with [TokenKind::Continue] in current
    /// ends with [TokenKind::Terminator] in current
    fn parse_continue(&mut self) -> Result<Statement, ()> {
        let tok = self.curr_tok();
        self.expect_peek_is(TokenKind::Terminator)?;
        Ok(Statement::Continue(tok))
    }

    /// starts with [TokenKind::Mash] in current
    /// ends with [TokenKind::RBrace] in current
    fn parse_mash(&mut self) -> Result<Statement, ()> {
        let start = self.curr_tok();
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek_is(TokenKind::LBrace)?;
        // disallow empty bodies
        if self.peek_tok_is(TokenKind::RBrace) {
            self.empty_body_error(
                self.curr_tok(),
                self.peek_tok(),
                vec![TokenKind::Type, TokenKind::Default],
                EmptyBodyErrorType::Mash,
                self.source
                    .lines()
                    .nth(self.peek_tok().line(&self.line_starts))
                    .unwrap_or_default(),
            );
            self.advance(1);
            return Err(());
        }
        let mut cases: Vec<Case> = vec![];
        while self.peek_tok_is_type() {
            cases.push(self.parse_case()?);
        }
        let default = match self.peek_tok().kind {
            TokenKind::Default => {
                self.advance(1);
                self.expect_peek_is(TokenKind::Colon)?;
                Some(self.parse_case_body()?)
            }
            _ => None,
        };
        let offset_end = self
            .expect_peek_is(TokenKind::RBrace)
            .and_then(|tok| Ok(tok.offset.end))?;
        Ok(Statement::Mash(MashStatement::new(
            expr,
            cases,
            default,
            (start.offset.start, offset_end),
        )))
    }

    /// starts with a [DataType]'s first token in peek
    /// ends with a [DataType]'s first token in peek
    fn parse_case(&mut self) -> Result<Case, ()> {
        let start = self.peek_tok();
        let case_type = self
            .expect_peek_is_type()
            .and_then(|tok| self.parse_data_type(tok))?;
        let body = self
            .expect_peek_is(TokenKind::Colon)
            .and_then(|_| self.parse_case_body())?;
        let offset_end = self.curr_tok().offset.end;
        Ok(Case::new(case_type, body, (start.offset.start, offset_end)))
    }

    /// starts with [TokenKind::Colon] in current
    /// ends with any of the ff:
    /// - [DataType]'s first token in peek (another case next)
    /// - [TokenKind::Default] in peek (default case next)
    /// - [TokenKind::RBrace] in peek (end of mash statement)
    fn parse_case_body(&mut self) -> Result<Body, ()> {
        let mut statements: Vec<Statement> = vec![];
        let start = self.curr_tok();
        // disallow empty bodies
        if self.peek_tok_is_type() || self.peek_tok_is_in(&[TokenKind::Default, TokenKind::RBrace])
        {
            self.empty_body_error(
                self.curr_tok(),
                self.peek_tok(),
                self.body_parse_fns
                    .clone()
                    .into_iter()
                    .map(|(token, _)| token)
                    .collect::<Vec<_>>(),
                EmptyBodyErrorType::MashCase,
                self.source
                    .lines()
                    .nth(self.peek_tok().line(&self.line_starts))
                    .unwrap_or_default(),
            );
            self.advance(1);
            return Err(());
        }
        while !self.peek_tok_is_type()
            && !self.peek_tok_is_in(&[TokenKind::Default, TokenKind::RBrace])
        {
            self.advance(1);
            let stmt = match self.body_parse_fns.get(&self.curr_tok().kind) {
                Some(parser) => parser(self)?,
                None => {
                    self.no_parsing_fn_error(
                        MissingParserErrorHeader::Body,
                        self.curr_tok(),
                        self.body_parse_fns
                            .clone()
                            .into_iter()
                            .map(|(tokens, _)| tokens)
                            .collect::<Vec<_>>(),
                    );
                    return Err(());
                }
            };
            statements.push(stmt);
        }
        let offset_end = match self.peek_tok().kind {
            TokenKind::Type
            | TokenKind::Chan
            | TokenKind::Kun
            | TokenKind::Senpai
            | TokenKind::Kouhai
            | TokenKind::San
            | TokenKind::Sama
            | TokenKind::Dono
            | TokenKind::Default
            | TokenKind::RBrace => self.curr_tok().offset.end,
            _ => {
                self.unexpected_token_error(
                    None,
                    None,
                    self.peek_tok(),
                    TokenKind::data_types()
                        .into_iter()
                        .chain(vec![TokenKind::Type, TokenKind::Default])
                        .collect::<Vec<_>>(),
                );
                return Err(());
            }
        };
        Ok(Body::new(statements, (start.offset.start, offset_end)))
    }

    /// This parses three possible ident statements: assignments, function/method calls, pipelines
    /// ```
    /// aqua = 1~
    /// aqua.arms[2] = LONG~
    /// aqua.arms[1].punch()~
    /// aqua.scream() | voice_crack()~
    /// ```
    /// starts with [TokenKind::Identifier] in current
    /// ends with [TokenKind::Terminator] in current
    fn parse_ident_statement(&mut self) -> Result<Statement, ()> {
        let id = self.parse_ident_expression()?;
        match self.curr_tok().kind {
            TokenKind::Identifier | TokenKind::RBracket => match self.peek_tok().kind {
                TokenKind::Assign
                | TokenKind::PlusEqual
                | TokenKind::DashEqual
                | TokenKind::MultiplyEqual
                | TokenKind::DivideEqual
                | TokenKind::ModuloEqual => {
                    Ok(Statement::Assignment(self.parse_assignment(id.try_into()?)?))
                }
                _ => {
                    self.unexpected_token_error(
                        None,
                        None,
                        self.peek_tok(),
                        TokenKind::assign_ops()
                            .iter()
                            .map(|&kind| kind)
                            .chain(vec![TokenKind::Pipe])
                            .collect::<Vec<_>>()
                    );
                    Err(())
                }
            },
            TokenKind::RParen => {
                self.expect_peek_is(TokenKind::Terminator)?;
                match id {
                    Expression::FnCall(fn_call) => Ok(Statement::FnCall(fn_call)),
                    Expression::Access(AccessType::Method(method)) => Ok(Statement::Method(method)),
                    Expression::Pipeline(pipe) => Ok(Statement::Pipeline(pipe)),
                    _ => unreachable!(
                        r#"parse_ident_statement(): parse_ident_value() that returned at RParen is neither FnCall or MethodAccess"#
                    ),
                }
            }
            _ => unreachable!(
                "parse_ident_statement(): parse_ident_value() did not return at RParen, Identifier, or RBracket, got {}",
                self.curr_tok().kind
            ),
        }
    }

    /// starts with [TokenKind::Identifier] in current and [TokenKind::LParen] in peek
    /// ends with [TokenKind::RParen] in current
    fn parse_fn_call(&mut self) -> Result<FnCall, ()> {
        let id = self.curr_tok();
        self.expect_peek_is(TokenKind::LParen)?;
        let mut args: Vec<Expression> = vec![];
        while !self.peek_tok_is(TokenKind::RParen) {
            args.push(self.parse_expression(Precedence::Lowest)?);
            self.allow_hanging_comma(TokenKind::RParen)?;
        }
        let offset_end = self
            .expect_peek_is(TokenKind::RParen)
            .and_then(|tok| Ok(tok.offset.end))?;
        Ok(FnCall::new(
            id,
            args,
            FnSignature::default(),
            (id.offset.start, offset_end),
        ))
    }

    /// Abstracts over different expression statement parsers. Each will parse
    /// an expression that may be followed by a pipe, which will lead to a
    /// pipeline statement.
    ///
    /// starts with valid starting token for an [Expression] in current.
    /// ends with [TokenKind::Terminator] in current
    fn parse_expression_statement(&mut self) -> Result<Statement, ()> {
        self.reverse(1);
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek_is(TokenKind::Terminator)?;
        Ok(Statement::Expression(expr))
    }

    // }}}

    // EXPRESSION GROUP PARSERS {{{

    /// Parses an expression unit, which may be followed by a pipeline.
    ///
    /// starts with valid starting token for an [Expression] in peek.
    /// ends with final token of the [Expression] in current
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ()> {
        let expr = self.parse_expression_unit(precedence)?;
        if self.peek_tok_is(TokenKind::Pipe) {
            self.parse_pipeline_expression(expr)
        } else {
            Ok(expr)
        }
    }

    /// Parses a single expression unit, with single meaning not in a pipeline.
    ///
    /// starts with valid starting token for an [Expression] in peek.
    /// ends with final token of the [Expression] in current
    fn parse_expression_unit(&mut self, precedence: Precedence) -> Result<Expression, ()> {
        self.advance(1);
        // prefix parsing
        let mut left = match self.prefix_parse_fns.get(&self.curr_tok().kind) {
            Some(parser) => parser(self)?,
            None => {
                self.no_parsing_fn_error(
                    MissingParserErrorHeader::Prefix,
                    self.curr_tok(),
                    self.prefix_parse_fns
                        .iter()
                        .map(|(&token, _)| token)
                        .collect::<Vec<_>>(),
                );
                return Err(());
            }
        };
        left = self.parse_expression_post_prefix(left, precedence)?;
        // infix parsing
        while !self.peek_tok_is_in(&[TokenKind::EOF, TokenKind::Terminator, TokenKind::Pipe])
            && precedence < Precedence::of(self.peek_tok().kind)
        {
            self.advance(1);
            left = match self.infix_parse_fns.get(&self.curr_tok().kind) {
                Some(parser) => parser(self, left)?,
                None => {
                    self.no_parsing_fn_error(
                        MissingParserErrorHeader::Infix,
                        self.curr_tok(),
                        self.infix_parse_fns
                            .iter()
                            .map(|(&token, _)| token)
                            .collect::<Vec<_>>(),
                    );
                    return Err(());
                }
            };
        }
        Ok(left)
    }

    /// starts with prefix operator in current
    /// ends with prefix operand in current
    fn parse_prefix_expression(&mut self) -> Result<Expression, ()> {
        let op = self.curr_tok();
        let right = self.parse_expression_unit(Precedence::Prefix)?;
        Ok(Expression::Prefix(PrefixExpression::new(
            op,
            Box::new(right),
        )))
    }

    /// starts with infix operator in current
    /// ends with infix operand in current
    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ()> {
        let op = self.curr_tok();
        let right = self.parse_expression_unit(Precedence::of(self.curr_tok().kind))?;
        Ok(Expression::Infix(InfixExpression::new(
            Box::new(left),
            op,
            Box::new(right),
        )))
    }

    /// starts with [TokenKind::LParen] in current
    /// ends with [TokenKind::RParen] in current
    fn parse_grouped_expression(&mut self) -> Result<Expression, ()> {
        let start = self.curr_tok();
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek_is(TokenKind::RParen)?;
        match expr {
            // avoid grouping an already grouped expression
            Expression::Grouped(res) => Ok(Expression::Grouped(res)),
            _ => Ok(Expression::Grouped(GroupedExpression::new(
                Box::new(expr),
                (start.offset.start, self.curr_tok().offset.end),
            ))),
        }
    }

    /// Parses expressions that may or may not be:
    /// - indexed (`aqua["age"]`)
    /// - accessed (`aqua.scream()`)
    ///
    /// starts with valid starting token for an [Expression] in peek.
    /// ends with final token of the [Expression] in current
    fn parse_expression_post_prefix(
        &mut self,
        expr: Expression,
        precedence: Precedence,
    ) -> Result<Expression, ()> {
        if expr.is_indexable() && self.peek_tok_is(TokenKind::LBracket) {
            self.parse_indexed_expression(expr)
                .and_then(|idx_expr| self.parse_expression_post_prefix(idx_expr, precedence))
        } else if expr.is_accessible() && self.peek_tok_is(TokenKind::Dot) {
            self.parse_access_expression(expr)
                .and_then(|acc_expr| self.parse_expression_post_prefix(acc_expr, precedence))
        } else {
            Ok(expr)
        }
    }

    /// Parses single idents that may or may not have field/method accesses,
    /// which may or may not be followed by a pipeline.
    /// ```
    /// aqua
    /// aqua.age | pow(2)
    /// aqua.likes[10]
    /// aqua.hands[1].fingers[3].raise() | colorize()
    /// ```
    /// starts with [TokenKind::Identifier] in current
    /// ends with any of the ff in current:
    /// - [TokenKind::Identifier]
    /// - [TokenKind::RParen]
    /// - [TokenKind::RBracket]
    fn parse_ident_expression(&mut self) -> Result<Expression, ()> {
        self.parse_ident_expression_unit().and_then(|id_expr| {
            if self.peek_tok_is(TokenKind::Pipe) {
                self.parse_pipeline_expression(id_expr)
            } else {
                Ok(id_expr)
            }
        })
    }

    /// Parses single idents that may or may not have field/method accesses
    /// ```
    /// aqua
    /// aqua.age
    /// aqua.likes[10]
    /// aqua.hands[1].fingers[3].raise()
    /// ```
    /// starts with [TokenKind::Identifier] in current
    /// ends with any of the ff in current:
    /// - [TokenKind::Identifier]
    /// - [TokenKind::RParen]
    /// - [TokenKind::RBracket]
    fn parse_ident_expression_unit(&mut self) -> Result<Expression, ()> {
        self.parse_single_accessed()
            .and_then(|accessor| self.parse_access_expression(accessor.into()))
    }

    /// This parses accessed fields/methods which are idents that can but don't
    /// access fields/methods.
    /// ```
    /// aqua
    /// aqua()
    /// aqua[1]
    /// aqua()[1]
    /// ```
    /// starts with [TokenKind::Identifier] in current
    /// ends with any of the ff:
    /// - [TokenKind::Identifier]
    /// - [TokenKind::RParen]
    /// - [TokenKind::RBracket]
    fn parse_single_accessed(&mut self) -> Result<Accessed, ()> {
        let initial_id = self.curr_tok();
        let (idx_id, acc_id) = if self.peek_tok_is(TokenKind::LParen) {
            let fn_call = self.parse_fn_call()?;
            match self.peek_tok().kind {
                TokenKind::LBracket => (Indexable::FnCall(fn_call), Accessed::Token(initial_id)),
                _ => (Indexable::Token(initial_id), Accessed::FnCall(fn_call)),
            }
        } else {
            (Indexable::Token(initial_id), Accessed::Token(initial_id))
        };
        let final_id = if self.peek_tok_is(TokenKind::LBracket) {
            self.advance(1);
            let indices  = self.parse_array_literal().and_then(|a| match a {
                Expression::Array(a) => match a.exprs.len() {
                    0 => Err(
                        self.unexpected_token_error(
                            Some("[EMPTY INDEX]"),
                            Some(r#"To get an item from an iterable, put at least one index"#),
                            self.curr_tok(),
                            self.prefix_parse_fns
                                .iter()
                                .map(|(&kind, _)| kind)
                                .collect()
                        )
                    ),
                    _ => Ok(a.exprs),
                },
                _ => unreachable!(
                    r#"parse_ident_accessor(): impossible for parse_array_literal() to return a non ArrayLiteral"#,
                ),
            })?;
            Accessed::IndexedId(IndexedId::new(idx_id, indices, self.curr_tok().offset.end))
        } else {
            acc_id
        };
        Ok(final_id)
    }

    /// Parses indexed expressions. Expressions to be indexed must be passed in
    /// and parsed beforehand.
    /// # Example
    /// ```
    /// aqua[1]
    /// aqua.hands[2]
    /// aqua.names()[3]
    /// [1,2,3,4,5][4]
    /// "string"[5]
    /// colors()[6,7]
    /// ```
    /// starts with [TokenKind::LBracket] in peek
    /// ends with [TokenKind::RBracket] in current
    fn parse_indexed_expression(&mut self, expr: Expression) -> Result<Expression, ()> {
        let id = Indexable::try_from(expr).expect(
            r#"parse_indexed_expression(): converting to indexable cannot fail as it was checked beforehand"#
        );
        self.expect_peek_is(TokenKind::LBracket)
            .and_then(|tok| Ok(tok.pos(&self.line_starts)))?;
        let indices: Vec<Expression> = self.parse_array_literal().and_then(|a| match a {
            Expression::Array(a) => Ok(a.exprs),
            _ => unreachable!(
                r#"parse_indexed_expression(): impossible for parse_array_literal() to return a non ArrayLiteral"#,
            ),
        })?;
        Ok(Expression::Indexed(IndexedId::new(
            id,
            indices,
            self.curr_tok().offset.end,
        )))
    }

    /// Parses access expressions. Expression to be indexed must be passed in
    /// and parsed beforehand.
    /// # Example
    /// ```
    /// aqua.hands
    /// aqua.scream().max_volume()
    /// colors[1].hsl()
    /// [1,2,3,4,5].rsort()
    /// "string".upper()
    /// ```
    /// starts with [TokenKind::LBracket] in peek
    /// ends with [TokenKind::RBracket] in current
    fn parse_access_expression(&mut self, expr: Expression) -> Result<Expression, ()> {
        let mut accessed: Vec<Accessed> = vec![];
        while self.peek_tok_is(TokenKind::Dot) {
            self.advance(1);
            self.expect_peek_is(TokenKind::Identifier)?;
            accessed.push(self.parse_single_accessed()?);
        }
        if accessed.len() > 0 {
            match accessed.last().unwrap_or(&Accessed::default()) {
                Accessed::Token(_) => Ok(Expression::Access(AccessType::Field(GroupAccess::new(
                    Box::new(expr),
                    accessed,
                    std::marker::PhantomData,
                )))),
                Accessed::IndexedId(res) => match res.id {
                    Indexable::Token(_) => Ok(Expression::Access(AccessType::Field(
                        GroupAccess::new(Box::new(expr), accessed, std::marker::PhantomData),
                    ))),
                    Indexable::FnCall(_) => Ok(Expression::Access(AccessType::Method(
                        GroupAccess::new(Box::new(expr), accessed, std::marker::PhantomData),
                    ))),
                    _ => unreachable!(
                        r#"parse_access_expression(): indexed result from parse_single_accessed() can only be a token or fn call"#
                    ),
                },
                Accessed::FnCall(_) => Ok(Expression::Access(AccessType::Method(
                    GroupAccess::new(Box::new(expr), accessed, std::marker::PhantomData),
                ))),
            }
        } else {
            Ok(expr)
        }
    }

    /// Parses a possible pipeline expression where the initial value of the
    /// pipeline was parsed beforehand. Just returns the already parsed value
    /// if there is no pipe (`|`) following it.
    /// Ensures that only callables are parsed inside the pipeline.
    ///
    /// starts with [TokenKind::Pipe] in peek
    /// ends with [TokenKind::RParen] in current
    fn parse_pipeline_expression(&mut self, first_value: Expression) -> Result<Expression, ()> {
        let start = first_value.pos(&self.line_starts);
        let mut rest: Vec<Callable> = vec![];
        while self.peek_tok_is(TokenKind::Pipe) {
            self.expect_peek_is(TokenKind::Pipe)?;
            rest.push(self.parse_pipeline_callable(start)?);
        }
        match &rest.len() {
            0 => Ok(first_value),
            _ => Ok(Expression::Pipeline(Pipeline::new(
                Box::new(first_value),
                rest,
            ))),
        }
    }

    /// This parses a single item but ensures that only callables are parsed.
    /// If the ident parsed isn't a callable (variable, field, indexed var,
    /// etc.), this will create an error
    ///
    /// # Usage
    /// Used in parsing [Pipeline]s where only callables are expected in between pipes
    ///
    /// starts with valid starting token for an [Expression] in peek.
    /// ends with [TokenKind::RParen] in current
    fn parse_pipeline_callable(&mut self, start: (usize, usize)) -> Result<Callable, ()> {
        let callable = self.parse_expression_unit(Precedence::Lowest)?;
        match self.curr_tok().kind {
            // TODO: cloned callable
            TokenKind::RParen => match callable.clone() {
                Expression::FnCall(call) => Ok(Callable::Fn(call)),
                Expression::Access(AccessType::Method(method)) => Ok(Callable::Method(method)),
                Expression::GroupInit(init) => Ok(Callable::GroupInit(init)),
                Expression::Grouped(grouped) => match *grouped.expr {
                    Expression::FnCall(call) => Ok(Callable::Fn(call)),
                    Expression::Access(AccessType::Method(method)) => Ok(Callable::Method(method)),
                    Expression::GroupInit(init) => Ok(Callable::GroupInit(init)),
                    Expression::Grouped(_) => unreachable!(
                        r#"parse_pipeline_callable: grouped expression cannot contain another grouped expression by itself"#
                    ),
                    _ => Err(self.non_callable_in_pipeline_error(callable, start)),
                },
                _ => unreachable!(
                    r#"parse_pipeline_callable: Expression that ended with RParen is not FnCall, MethodAccess, GroupInit, or GroupedExpression"#
                ),
            },
            _ => Err(self.non_callable_in_pipeline_error(callable, start)),
        }
    }

    // }}}

    // EXPRESSION UNIT PARSERS {{{

    /// just returns the current token
    fn parse_literal(&mut self) -> Result<Expression, ()> {
        Ok(Expression::Token(self.curr_tok()))
    }

    /// This parses a group initializer
    /// ```
    /// GroupName(arg1, arg2, ...)
    /// ```
    /// starts with [TokenKind::Type] in current
    /// ends with [TokenKind::LParen] in current
    fn parse_group_init(&mut self) -> Result<Expression, ()> {
        let id = self.curr_tok();
        self.expect_peek_is(TokenKind::LParen)?;
        let mut args: Vec<Expression> = vec![];
        while !self.peek_tok_is(TokenKind::RParen) {
            args.push(self.parse_expression(Precedence::Lowest)?);
            self.allow_hanging_comma(TokenKind::RParen)?;
        }
        self.expect_peek_is(TokenKind::RParen)?;
        Ok(Expression::GroupInit(GroupInit::new(
            id,
            args,
            self.curr_tok().offset.end,
        )))
    }

    /// starts with [TokenKind::LBracket] in current
    /// ends with [TokenKind::RBracket] in current
    fn parse_array_literal(&mut self) -> Result<Expression, ()> {
        let start = self.curr_tok();
        let mut exprs: Vec<Expression> = vec![];
        while !self.peek_tok_is(TokenKind::RBracket) {
            exprs.push(self.parse_expression(Precedence::Lowest)?);
            self.allow_hanging_comma(TokenKind::RBracket)?;
        }
        self.expect_peek_is(TokenKind::RBracket)?;
        Ok(Expression::Array(ArrayLiteral::new(
            exprs,
            (start.offset.start, self.curr_tok().offset.end),
        )))
    }

    /// starts with [TokenKind::Hash] in current
    /// starts with [TokenKind::RBracket] in current
    ///
    /// Abstracts over parsing hashset and hashmap literals depending on whether the first item is
    /// followed by a colon or a comma
    /// `#[item, item]` => hashset
    /// `#[item: item]` => hashmap
    fn parse_hash_literal(&mut self) -> Result<Expression, ()> {
        let start = self.curr_tok();
        self.expect_peek_is(TokenKind::LBracket)?;
        // empty literal
        match self.peek_tok().kind {
            // empty set: #[]
            TokenKind::RBracket => {
                self.advance(1);
                return Ok(Expression::Set(SetLiteral::new(
                    vec![],
                    (start.offset.start, self.curr_tok().offset.end),
                )));
            }
            // empty map: #[:]
            TokenKind::Colon => {
                self.advance(1);
                self.expect_peek_is(TokenKind::RBracket)?;
                return Ok(Expression::Map(MapLiteral::new(
                    vec![],
                    (start.offset.start, self.curr_tok().offset.end),
                )));
            }
            _ => (),
        }
        let first_expr = self.parse_expression(Precedence::Lowest)?;
        match self.peek_tok().kind {
            TokenKind::RBracket => {
                self.advance(1);
                Ok(Expression::Set(SetLiteral::new(
                    vec![first_expr],
                    (start.offset.start, self.curr_tok().offset.end),
                )))
            }
            TokenKind::Comma => self.parse_set_literal(first_expr, start),
            TokenKind::Colon => self.parse_map_literal(first_expr, start),
            _ => {
                self.expect_peek_is_in(&[TokenKind::Comma, TokenKind::Colon, TokenKind::RBracket])?;
                self.advance(2);
                Err(())
            }
        }
    }

    /// A [Token] with kind [TokenKind::Hash] needs to be passed in for
    /// position information.
    ///
    /// starts with [TokenKind::Comma] in peek (always), with first val already parsed
    /// ends with [TokenKind::RBracket] in current
    fn parse_set_literal(
        &mut self,
        first_expr: Expression,
        hash_token: Token,
    ) -> Result<Expression, ()> {
        let mut exprs: Vec<Expression> = vec![first_expr];
        self.expect_peek_is(TokenKind::Comma)?;
        while !self.peek_tok_is(TokenKind::RBracket) {
            exprs.push(self.parse_expression(Precedence::Lowest)?);
            self.allow_hanging_comma(TokenKind::RBracket)?;
        }
        self.expect_peek_is(TokenKind::RBracket)?;
        Ok(Expression::Set(SetLiteral::new(
            exprs,
            (hash_token.offset.start, self.curr_tok().offset.end),
        )))
    }

    /// A [Token] with kind [TokenKind::Hash] needs to be passed in for
    /// position information.
    ///
    /// starts with [TokenKind::Colon] in peek (always), with first key already parsed
    /// ends with [TokenKind::RBracket] in current
    fn parse_map_literal(
        &mut self,
        first_key: Expression,
        hash_token: Token,
    ) -> Result<Expression, ()> {
        self.expect_peek_is(TokenKind::Colon)?;
        let first_expr = self.parse_expression(Precedence::Lowest)?;
        let mut exprs: Vec<(Expression, Expression)> = vec![(first_key, first_expr)];
        self.allow_hanging_comma(TokenKind::RBracket)?;
        while !self.peek_tok_is(TokenKind::RBracket) {
            let key = self.parse_expression(Precedence::Lowest)?;
            self.expect_peek_is(TokenKind::Colon)?;
            let expr = self.parse_expression(Precedence::Lowest)?;
            exprs.push((key, expr));
            self.allow_hanging_comma(TokenKind::RBracket)?;
        }
        self.expect_peek_is(TokenKind::RBracket)?;
        Ok(Expression::Map(MapLiteral::new(
            exprs,
            (hash_token.offset.start, self.curr_tok().offset.end),
        )))
    }

    // }}}

    // HELPER METHODS {{{

    /// maps a TokenKind to a respective parsing function
    /// user can supply own parsing function
    fn register_prefix(&mut self, args: Vec<(TokenKind, fn(&mut Self) -> Result<Expression, ()>)>) {
        args.into_iter().for_each(|(token_kind, parse_fn)| {
            self.prefix_parse_fns.insert(token_kind, parse_fn);
        })
    }

    /// maps a TokenKind to a respective parsing function
    /// function is always `Self::parse_infix_expression()`
    fn register_infix(&mut self, args: Vec<TokenKind>) {
        args.into_iter().for_each(|token_kind| {
            self.infix_parse_fns
                .insert(token_kind, Self::parse_infix_expression);
        })
    }

    /// maps a TokenKind to a respective parsing function
    /// user can supply own parsing function
    fn register_body(&mut self, args: Vec<(TokenKind, fn(&mut Self) -> Result<Statement, ()>)>) {
        args.into_iter().for_each(|(token_kind, parse_fn)| {
            self.body_parse_fns.insert(token_kind, parse_fn);
        })
    }

    /// checks current token's type against given TokenKind
    fn curr_tok_is(&self, token_kind: TokenKind) -> bool {
        self.curr_tok().kind == token_kind
    }

    /// checks peek token's type against given TokenKind
    fn peek_tok_is(&self, token_kind: TokenKind) -> bool {
        self.peek_tok().kind == token_kind
    }

    /// checks peek token's type against given TokenKinds
    fn peek_tok_is_in(&self, token_types: &[TokenKind]) -> bool {
        token_types.contains(&self.peek_tok().kind)
    }

    /// checks peek token's type is a DataType
    fn peek_tok_is_type(&self) -> bool {
        match self.peek_tok().kind {
            TokenKind::Type
            | TokenKind::Chan
            | TokenKind::Kun
            | TokenKind::Senpai
            | TokenKind::Kouhai
            | TokenKind::San
            | TokenKind::Sama
            | TokenKind::Dono => true,
            _ => false,
        }
    }

    // TODO: for all expect_peek methods, add header and context as parameters
    /// advances cursor 1 time if peek tok is eq to given
    /// adds an error otherwise
    fn expect_peek_is(&mut self, token_kind: TokenKind) -> Result<Token, ()> {
        match self.peek_tok().kind == token_kind {
            true => {
                self.advance(1);
                Ok(self.curr_tok())
            }
            false => {
                Err(self.unexpected_token_error(None, None, self.peek_tok(), vec![token_kind]))
            }
        }
    }

    /// advances cursor 1 time if peek tok is eq to any of the given
    /// adds an error otherwise
    fn expect_peek_is_in(&mut self, token_kinds: &[TokenKind]) -> Result<Token, ()> {
        match token_kinds.contains(&self.peek_tok().kind) {
            true => {
                self.advance(1);
                Ok(self.curr_tok())
            }
            false => {
                Err(self.unexpected_token_error(None, None, self.peek_tok(), token_kinds.to_vec()))
            }
        }
    }

    /// advances cursor 1 time if peek tok is a valid uwu type
    /// adds an error otherwise
    fn expect_peek_is_type(&mut self) -> Result<Token, ()> {
        match self.peek_tok().kind {
            TokenKind::Type
            | TokenKind::Chan
            | TokenKind::Kun
            | TokenKind::Senpai
            | TokenKind::Kouhai
            | TokenKind::San
            | TokenKind::Sama
            | TokenKind::Dono => {
                self.advance(1);
                Ok(self.curr_tok())
            }
            _ => Err(self.unexpected_token_error(
                None,
                None,
                self.peek_tok(),
                TokenKind::data_types(),
            )),
        }
    }

    /// advances cursor 1 time if peek tok is a valid assignment operator
    /// adds an error otherwise
    fn expect_peek_is_assign_op(&mut self) -> Result<Token, ()> {
        match self.peek_tok().kind {
            TokenKind::Assign
            | TokenKind::PlusEqual
            | TokenKind::DashEqual
            | TokenKind::MultiplyEqual
            | TokenKind::DivideEqual
            | TokenKind::ModuloEqual => {
                self.advance(1);
                Ok(self.curr_tok())
            }
            _ => Err(self.unexpected_token_error(
                None,
                None,
                self.peek_tok(),
                TokenKind::assign_ops(),
            )),
        }
    }

    /// allows hanging comma on enclosed, comma separated expressions
    fn allow_hanging_comma(&mut self, closing: TokenKind) -> Result<(), ()> {
        if self.peek_tok_is(TokenKind::Comma) {
            Ok(self.advance(1))
        } else if self.peek_tok_is(closing) {
            Ok(())
        } else {
            self.unexpected_token_error(
                None,
                None,
                self.peek_tok(),
                vec![TokenKind::Comma, closing],
            );
            Err(())
        }
    }

    /// Helper method to lift the expression value to a string relevant to
    /// error reporting in [NonCallableInPipelineError]
    fn expression_header_in_pipeline_error(&self, expr: &Expression) -> String {
        match expr {
            Expression::Token(res) => res.kind.to_string(),
            Expression::Indexed(res) => match &res.id {
                Indexable::Token(res) => {
                    format!("indexed {}", res.kind.to_string().to_ascii_lowercase())
                }
                Indexable::Array(_) => "indexed array literal".into(),
                Indexable::Set(_) => "indexed set literal".into(),
                Indexable::Map(_) => "indexed map literal".into(),
                Indexable::FnCall(_) => "indexed fn call".into(),
                Indexable::Access(res) => match &res {
                    AccessType::Field(_) => "field access".into(),
                    AccessType::Method(_) => "method access".into(),
                },
                Indexable::Pipeline(_) => unreachable!(
                    r#"NonCallableInPipeError::new(): A pipeline cannot exist within another pipeline"#
                ),
            },
            Expression::Access(access) => match &access {
                AccessType::Field(field) => {
                    match field.accessed.last().unwrap_or(&Accessed::default()) {
                        Accessed::Token(_) => "field access".into(),
                        Accessed::IndexedId(_) => "indexed field access".into(),
                        _ => unreachable!(
                            r#"NonCallableInPipeError::new(): cannot be FnCall when the error is it isn't a callable in the first place"#
                        ),
                    }
                }
                AccessType::Method(method) => {
                    match &method.accessed.last().unwrap_or(&Accessed::default()) {
                        Accessed::FnCall(_) => "method access".into(),
                        Accessed::IndexedId(_) => "indexed method access".into(),
                        _ => unreachable!(
                            r#"NonCallableInPipeError::new(): cannot be Method when the error is it isn't a callable in the first place"#
                        ),
                    }
                }
            },
            Expression::FnCall(_) => unreachable!(
                r#"NonCallableInPipeError::new(): cannot be FnCall when the error is it isn't a callable in the first place"#
            ),
            Expression::Infix(_) => "infix operation".into(),
            Expression::Grouped(res) => self.expression_header_in_pipeline_error(&*res.expr),
            Expression::Set(_) => "set literal".into(),
            Expression::Map(_) => "map literal".into(),
            Expression::Array(_) => "array literal".into(),
            Expression::Prefix(_) => "prefix expression".into(),
            Expression::Pipeline(_) => unreachable!(
                r#"NonCallableInPipeError::new(): A pipeline cannot exist within another pipeline"#
            ),
            Expression::GroupInit(_) => "group initialization".into(),
        }
    }

    // }}}

    // ERROR METHODS {{{

    fn unexpected_token_error(
        &mut self,
        header: Option<&'a str>,
        context: Option<&'a str>,
        actual: Token,
        expected: Vec<TokenKind>,
    ) {
        self.error = UnexpectedTokenError::new(
            self.source,
            self.line_starts,
            actual,
            expected.to_vec(),
            header,
            context,
        )
        .to_string()
    }

    fn empty_body_error(
        &mut self,
        opening: Token,
        actual: Token,
        expected: Vec<TokenKind>,
        body_type: EmptyBodyErrorType,
        line_text: &'a str,
    ) {
        self.error = EmptyBodyError::new(
            opening.buffer_pos(&self.line_starts),
            actual.source_str(self.source),
            actual.buffer_pos(&self.line_starts),
            expected,
            body_type,
            line_text,
        )
        .to_string()
    }

    fn no_parsing_fn_error(
        &mut self,
        error_header: MissingParserErrorHeader,
        actual: Token,
        expected: Vec<TokenKind>,
    ) {
        self.error = UnexpectedTokenError::new(
            self.source,
            self.line_starts,
            actual,
            expected,
            Some(error_header.header()),
            None,
        )
        .to_string()
    }

    fn non_callable_in_pipeline_error(
        &mut self,
        non_callable: Expression,
        pipeline_start: (usize, usize),
    ) {
        self.error = NonCallableInPipelineError::new(
            self.source
                .lines()
                .skip(pipeline_start.0)
                .take(self.curr_tok().line(&self.line_starts) - pipeline_start.0 + 1)
                .collect(),
            pipeline_start,
            non_callable.source_str(self.source),
            &non_callable.to_formatted_string(self.source, 0),
            &self.expression_header_in_pipeline_error(&non_callable),
        )
        .to_string()
    }

    // }}}
}

#[derive(Clone, Copy)]
enum MissingParserErrorHeader {
    Prefix,
    Infix,
    Body,
}
impl MissingParserErrorHeader {
    fn header(&self) -> &'static str {
        match self {
            Self::Prefix => "[NOT A VALID VALUE STARTING TOKEN]",
            Self::Infix => "[NOT A VALID OPERATOR]",
            Self::Body => "[NOT A VALID STATEMENT STARTING TOKEN]",
        }
    }
}
