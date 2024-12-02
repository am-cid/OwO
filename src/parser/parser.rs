use crate::errors::lex_errors::CompilerError;
use crate::errors::parse_errors::{
    BodyType, EmptyBodyError, NoMainError, NonCallableInPipelineError,
    ParamAfterVariadicParamError, UnexpectedTokenError,
};
use crate::lexer::token::{Token, TokenKind};
use crate::parser::data_types::{DataType, MapType, SetType, VecType, Vectorable};
use crate::parser::identifiers::*;
use crate::parser::productions::*;
use std::collections::HashMap;

#[derive(PartialEq, Eq, PartialOrd, Ord, Default)]
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
    Exponent,   // ^
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
            TokenKind::Exponent => Self::Exponent,
            _ => Self::Lowest,
        }
    }
}

pub struct Parser {
    pub source: &'static str,
    pub program: Program,
    pub error: String,
    tokens: Vec<Token>,
    pos: usize,
    prefix_parse_fns: HashMap<TokenKind, fn(&mut Self) -> Result<Expression, ()>>,
    infix_parse_fns: HashMap<TokenKind, fn(&mut Self, Expression) -> Result<Expression, ()>>,
    body_parse_fns: HashMap<TokenKind, fn(&mut Self) -> Result<Statement, ()>>,
}
impl Parser {
    pub fn new(source: &'static str, tokens: Vec<Token>) -> Self {
        let mut parser = Self {
            source,
            program: Program::default(),
            tokens,
            error: "".to_string(),
            pos: 0,
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            body_parse_fns: HashMap::new(),
        };
        parser.register_init();
        parser
    }
    /*
     * REGISTERING PARSING FNS
     */
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
            (TokenKind::Identifier, Self::parse_ident_expression),
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
            TokenKind::Exponent,
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
        // starting tokens for body statements
        self.register_body(vec![
            (TokenKind::Hi, Self::parse_declaration),
            (TokenKind::Identifier, Self::parse_ident_statement),
            (TokenKind::Iwf, Self::parse_if),
            (TokenKind::Mash, Self::parse_mash),
            (TokenKind::Fow, Self::parse_for),
            (TokenKind::Continue, Self::parse_continue),
            (TokenKind::Bweak, Self::parse_break),
            (TokenKind::Wetuwn, Self::parse_return),
        ]);
    }
    /*
     * MOVEMENT METHODS
     */
    /// advances cursor n times
    fn advance(&mut self, n: usize) -> () {
        for _ in 0..n {
            if self.curr_tok().kind == TokenKind::EOF {
                return;
            }
            self.pos += 1;
        }
    }
    /*
     * STATE TRACKING
     */
    fn curr_tok(&self) -> Token {
        self.tokens
            .iter()
            .nth(self.pos)
            .cloned()
            .unwrap_or_default()
    }
    fn peek_tok(&self) -> Token {
        self.tokens
            .iter()
            .nth(self.pos + 1)
            .cloned()
            .unwrap_or_default()
    }
    /*
     * PARSING FUNCTIONS
     */
    pub fn parse_program(&mut self) -> Result<(), ()> {
        // Program fields
        let mut main: Option<Function> = None;
        let mut functions: Vec<Function> = vec![];
        let mut groups: Vec<Group> = vec![];
        let mut methods: Vec<GroupMethod> = vec![];
        let mut contracts: Vec<Contract> = vec![];
        let mut globals: Vec<Statement> = vec![];
        // keeping track of range
        let start = self.curr_tok().pos;
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
                        Some("INVALID GLOBAL TOKEN"),
                        None,
                        self.curr_tok(),
                        &[
                            TokenKind::Fun,
                            TokenKind::Group,
                            TokenKind::Contract,
                            TokenKind::Hi,
                        ],
                    );
                    break;
                }
            }
            if self.peek_tok_is(TokenKind::EOF) {
                end = self.curr_tok().end_pos;
            }
            self.advance(1);
        }
        match main {
            Some(main) => {
                self.program = Program {
                    main,
                    functions,
                    groups,
                    methods,
                    contracts,
                    globals,
                    range: Range::new(start, end),
                };
                Ok(())
            }
            None => {
                self.error =
                    NoMainError::new(self.source.lines().nth(end.0).unwrap_or_default(), end)
                        .message();
                Err(())
            }
        }
    }
    /// starts with [TokenType::Fun] in current
    /// ends with [TokenType::RBrace] in current
    fn parse_function(&mut self) -> Result<Function, ()> {
        let start = self.curr_tok().pos;
        let id = self.expect_peek_is_in(&[TokenKind::Identifier, TokenKind::Main])?;
        self.expect_peek_is(TokenKind::Dash)?;
        let dtype = self
            .expect_peek_is_type()
            .and_then(|tok| self.parse_data_type(tok, self.curr_tok().pos))?;
        self.expect_peek_is(TokenKind::LParen)?;
        let params = self.parse_params()?;
        self.expect_peek_is(TokenKind::LBrace)?;
        let body = self.parse_body(BodyType::Fn)?;
        let end = self.curr_tok().end_pos;
        Ok(Function {
            id,
            dtype,
            params,
            body,
            range: Range::new(start, end),
        })
    }
    /// starts with [TokenType::Fun] in current
    /// ends with [TokenType::RBrace] in current
    fn parse_method(&mut self) -> Result<GroupMethod, ()> {
        let start = self.curr_tok().pos;
        let group = self.expect_peek_is_type()?;
        let mutable = self
            .peek_tok_is(TokenKind::Bang)
            .then(|| self.advance(1))
            .is_some();
        let id = self.expect_peek_is(TokenKind::Identifier)?;
        self.expect_peek_is(TokenKind::Dash)?;
        let dtype = self
            .expect_peek_is_type()
            .and_then(|tok| self.parse_data_type(tok, self.curr_tok().pos))?;
        self.expect_peek_is(TokenKind::LParen)?;
        let params = self.parse_params()?;
        self.expect_peek_is(TokenKind::LBrace)?;
        let body = self.parse_body(BodyType::Method)?;
        let end = self.curr_tok().end_pos;
        Ok(GroupMethod {
            id,
            group,
            mutable,
            dtype,
            params,
            body,
            range: Range::new(start, end),
        })
    }
    /// starts with [TokenType::Group] in current
    /// ends with [TokenType::RBrace] in current
    fn parse_group(&mut self) -> Result<Group, ()> {
        let start = self.curr_tok().pos;
        let id = self.expect_peek_is(TokenKind::Type)?;
        let mut contracts: Vec<Token> = vec![];
        match self.peek_tok().kind {
            TokenKind::LBracket => {
                self.advance(1);
                while !self.peek_tok_is(TokenKind::RBracket) {
                    contracts.push(self.expect_peek_is(TokenKind::Type)?);
                    self.allow_hanging_comma(TokenKind::RBracket)?;
                }
                self.expect_peek_is(TokenKind::RBracket)?;
            }
            _ => (),
        }
        self.expect_peek_is(TokenKind::LBrace)?;
        // disallow empty bodies
        if self.peek_tok_is(TokenKind::RBrace) {
            self.empty_body_error(
                vec![TokenKind::Identifier],
                BodyType::Group,
                self.source
                    .lines()
                    .nth(self.peek_tok().pos.0)
                    .unwrap_or_default(),
                self.peek_tok().pos,
            );
            self.advance(1);
            return Err(());
        }
        let fields = self.parse_fields()?;
        let end = self.curr_tok().end_pos;
        Ok(Group {
            id,
            contracts,
            fields,
            range: Range::new(start, end),
        })
    }
    /// starts with [TokenType::LParen] in current
    /// ends with [TokenType::RParen] in current
    fn parse_params(&mut self) -> Result<Vec<Param>, ()> {
        let mut params: Vec<Param> = vec![];
        while !self.peek_tok_is(TokenKind::RParen) {
            let (param, is_variadic) = self.parse_param()?;
            params.push(param.clone());
            self.allow_hanging_comma(TokenKind::RParen)?;
            if is_variadic && !self.peek_tok_is(TokenKind::RParen) {
                let actual = self.peek_tok();
                let extra_param = self.parse_param();
                match extra_param {
                    Ok((extra, _)) => {
                        self.error = ParamAfterVariadicParamError::new(
                            self.source
                                .lines()
                                .nth(param.range.start.0)
                                .unwrap_or_default(),
                            self.source
                                .lines()
                                .nth(extra.range.start.0)
                                .unwrap_or_default(),
                            param,
                            extra,
                        )
                        .message();
                        // self.advance(2);
                        return Err(());
                    }
                    Err(()) => {
                        self.unexpected_token_error(None, None, actual, &[TokenKind::RParen]);
                        // self.advance(2);
                        return Err(());
                    }
                }
            }
        }
        self.advance(1);
        Ok(params)
    }
    /// starts with [TokenType::Identifier] in peek
    /// ends with [DataType]'s last token in current
    fn parse_param(&mut self) -> Result<(Param, bool), ()> {
        let id = self.expect_peek_is(TokenKind::Identifier)?;
        self.expect_peek_is(TokenKind::Dash)?;
        let dtype = self
            .expect_peek_is_type()
            .and_then(|tok| self.parse_data_type(tok, self.curr_tok().pos))?;
        let variadic = self
            .peek_tok_is(TokenKind::Ellipsis)
            .then(|| self.advance(1))
            .is_some();
        Ok((
            Param {
                id,
                dtype,
                variadic,
                range: Range::new(id.pos, self.curr_tok().end_pos),
            },
            variadic,
        ))
    }
    /// difference with [Self::parse_params] is that this does not allow variadic fields
    ///
    /// starts with [TokenType::LBrace] in current
    /// ends with [TokenType::RBrace] in current
    fn parse_fields(&mut self) -> Result<Vec<GroupField>, ()> {
        let mut fields = vec![];
        while !self.peek_tok_is(TokenKind::RBrace) {
            let field = self.parse_field()?;
            self.expect_peek_is(TokenKind::Terminator)?;
            fields.push(field);
        }
        self.advance(1);
        Ok(fields)
    }
    /// difference with [Self::parse_param] is that this does not allow variadic fields
    ///
    /// starts with [TokenType::Identifier] in peek
    /// ends with [DataType]'s last token in current
    fn parse_field(&mut self) -> Result<GroupField, ()> {
        let id = self.expect_peek_is(TokenKind::Identifier)?;
        self.expect_peek_is(TokenKind::Dash)?;
        let dtype = self
            .expect_peek_is_type()
            .and_then(|tok| self.parse_data_type(tok, self.curr_tok().pos))?;
        Ok(GroupField {
            id,
            dtype,
            range: Range::new(id.pos, self.curr_tok().end_pos),
        })
    }
    /// starts with [TokenType::Contract] in current
    /// ends with [TokenType::RBrace] in current
    fn parse_contract(&mut self) -> Result<Contract, ()> {
        let start = self.curr_tok().pos;
        let id = self.expect_peek_is(TokenKind::Type)?;
        self.expect_peek_is(TokenKind::LBrace)?;
        if self.peek_tok_is(TokenKind::RBrace) {
            self.empty_body_error(
                vec![TokenKind::Identifier],
                BodyType::Contract,
                self.source
                    .lines()
                    .nth(self.peek_tok().pos.0)
                    .unwrap_or_default(),
                self.peek_tok().pos,
            );
            return Err(());
        }
        let mut signatures: Vec<FnSignature> = vec![];
        while !self.peek_tok_is(TokenKind::RBrace) {
            signatures.push(self.parse_fn_signature()?);
        }
        let end = self
            .expect_peek_is(TokenKind::RBrace)
            .and_then(|tok| Ok(tok.end_pos))?;
        Ok(Contract {
            id,
            signatures,
            range: Range::new(start, end),
        })
    }

    /// starts with [TokenType::Identifier] in peek
    /// ends with [TokenType::Terminator] in current
    fn parse_fn_signature(&mut self) -> Result<FnSignature, ()> {
        let (fn_id, start) = self
            .expect_peek_is(TokenKind::Identifier)
            .and_then(|tok| Ok((tok, tok.pos)))?;
        self.expect_peek_is(TokenKind::Dash)?;
        let dtype = self
            .expect_peek_is_type()
            .and_then(|tok| self.parse_data_type(tok, self.curr_tok().pos))?;
        self.expect_peek_is(TokenKind::LParen)?;
        let mut params = vec![];
        while !self.peek_tok_is(TokenKind::RParen) {
            params.push(
                self.expect_peek_is_type()
                    .and_then(|tok| self.parse_data_type(tok, self.curr_tok().pos))?,
            );
            self.allow_hanging_comma(TokenKind::RParen)?;
        }
        self.expect_peek_is(TokenKind::RParen)?;
        let end = self
            .expect_peek_is(TokenKind::Terminator)
            .and_then(|tok| Ok(tok.end_pos))?;
        Ok(FnSignature {
            id: fn_id,
            dtype,
            params,
            range: Range::new(start, end),
        })
    }
    /// starts with [TokenType::LBrace] in current
    /// ends with [TokenType::RBrace] in current
    fn parse_body(&mut self, body_type: BodyType) -> Result<Body, ()> {
        let mut statements: Vec<Statement> = vec![];
        let start = self.curr_tok().pos;
        // disallow empty bodies
        if self.peek_tok_is(TokenKind::RBrace) {
            self.empty_body_error(
                self.body_parse_fns
                    .clone()
                    .into_iter()
                    .map(|(token, _)| token)
                    .collect::<Vec<TokenKind>>(),
                body_type,
                self.source
                    .lines()
                    .nth(self.peek_tok().pos.0)
                    .unwrap_or_default(),
                self.peek_tok().pos,
            );
            self.advance(1);
            return Err(());
        }
        while !self.peek_tok_is(TokenKind::RBrace) {
            self.advance(1);
            let stmt = match self.body_parse_fns.get(&self.curr_tok().kind) {
                Some(parser) => parser(self)?,
                None => {
                    self.no_parsing_fn_error(
                        ParsingFnType::Body,
                        self.curr_tok(),
                        self.body_parse_fns
                            .clone()
                            .into_iter()
                            .map(|(tokens, _)| tokens)
                            .into_iter()
                            .collect::<Vec<_>>(),
                    );
                    return Err(());
                }
            };
            statements.push(stmt);
        }
        let end = self
            .expect_peek_is(TokenKind::RBrace)
            .and_then(|tok| Ok(tok.end_pos))?;
        Ok(Body {
            statements,
            range: Range::new(start, end),
        })
    }
    /// starts with [TokenType::Hi] in current
    /// ends with [TokenType::Terminator] in current
    fn parse_declaration(&mut self) -> Result<Statement, ()> {
        let start = self.curr_tok().pos;
        let id = self.expect_peek_is(TokenKind::Identifier)?;
        self.expect_peek_is(TokenKind::Dash)?;
        let dtype = self
            .expect_peek_is_type()
            .and_then(|tok| self.parse_data_type(tok, self.curr_tok().pos))?;
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
        let end = self
            .expect_peek_is(TokenKind::Terminator)
            .and_then(|tok| Ok(tok.end_pos))?;
        Ok(Statement::Declaration(Declaration {
            id,
            dtype,
            optional,
            mutable,
            expr,
            range: Range::new(start, end),
        }))
    }
    /// starts with [DataType]'s first token in current
    /// ends with [DataType]'s last token in current
    /// end token may be:
    /// - [DataType] token literal (`chan`, `kun`, `senpai`...)
    /// - [TokenType::RBracket] (`chan[1]`, `kun[3]` ...)
    /// - [TokenType::RBrace] (`chan{}`, `kun{senpai}` ...)
    fn parse_data_type(&mut self, tok: Token, start: (usize, usize)) -> Result<DataType, ()> {
        match self.peek_tok().kind {
            TokenKind::LBracket => self.parse_vector_type(Vectorable::Token(tok), start),
            TokenKind::LBrace => {
                self.advance(2);
                match self.curr_tok().kind {
                    TokenKind::RBrace => match self.peek_tok().kind {
                        TokenKind::LBracket => self.parse_vector_type(
                            Vectorable::Set(SetType {
                                tok,
                                range: Range::new(start, self.curr_tok().end_pos),
                            }),
                            start,
                        ),
                        _ => Ok(DataType::Set(SetType {
                            tok,
                            range: Range::new(start, self.curr_tok().end_pos),
                        })),
                    },
                    TokenKind::Type
                    | TokenKind::Chan
                    | TokenKind::Kun
                    | TokenKind::Senpai
                    | TokenKind::Kouhai
                    | TokenKind::Sama
                    | TokenKind::San
                    | TokenKind::Dono => {
                        let inner = Box::new(self.parse_data_type(self.curr_tok(), start)?);
                        let end = self
                            .expect_peek_is(TokenKind::RBrace)
                            .and_then(|tok| Ok(tok.end_pos))?;
                        let res = MapType {
                            tok,
                            inner,
                            range: Range::new(start, end),
                        };
                        match self.peek_tok().kind {
                            TokenKind::LBracket => {
                                self.parse_vector_type(Vectorable::Map(res), start)
                            }
                            _ => Ok(DataType::Map(res)),
                        }
                    }
                    _ => {
                        self.unexpected_token_error(
                            None,
                            None,
                            self.curr_tok(),
                            &TokenKind::data_types(),
                        );
                        self.advance(2);
                        return Err(());
                    }
                }
            }
            _ => Ok(DataType::Token(tok)),
        }
    }
    /// starts with [TokenType::RBrace] in current and [TokenType::LBracket] in peek
    /// ends with [TokenType::RBracket] in current
    fn parse_vector_type(&mut self, id: Vectorable, start: (usize, usize)) -> Result<DataType, ()> {
        self.advance(1);
        let dim = self.expect_peek_is(TokenKind::IntLiteral)?;
        self.expect_peek_is(TokenKind::RBracket)?;
        Ok(DataType::Vec(VecType {
            id,
            dim,
            range: Range::new(start, self.curr_tok().end_pos),
        }))
    }
    /// starts with [TokenType::Identifier] in current
    /// ends with [TokenType::Terminator] in current
    fn parse_assignment(&mut self, id: Assignable) -> Result<Assignment, ()> {
        let start = id.range().start;
        let assign_op = self.expect_peek_is_assign_op()?;
        let expr = self.parse_expression(Precedence::Lowest)?;
        let end = self
            .expect_peek_is(TokenKind::Terminator)
            .and_then(|tok| Ok(tok.end_pos))?;
        Ok(Assignment {
            id,
            dtype: None,
            assign_op,
            expr,
            range: Range::new(start, end),
        })
    }
    /// starts with valid starting token for a Value in peek
    /// ends with final token of the Value in current
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ()> {
        self.advance(1);
        let mut left = match self.prefix_parse_fns.get(&self.curr_tok().kind) {
            Some(parser) => parser(self)?,
            None => {
                self.no_parsing_fn_error(
                    ParsingFnType::Prefix,
                    self.curr_tok(),
                    self.prefix_parse_fns
                        .clone()
                        .into_iter()
                        .map(|(token, _)| token)
                        .collect::<Vec<TokenKind>>(),
                );
                return Err(());
            }
        };
        while !self.peek_tok_is_in(&[TokenKind::EOF, TokenKind::Terminator])
            && precedence < Precedence::of(self.peek_tok().kind)
        {
            self.advance(1);
            left = match self.infix_parse_fns.get(&self.curr_tok().kind) {
                Some(parser) => parser(self, left.clone())?,
                None => {
                    self.no_parsing_fn_error(
                        ParsingFnType::Infix,
                        self.curr_tok(),
                        self.prefix_parse_fns
                            .clone()
                            .into_iter()
                            .map(|(token, _)| token)
                            .collect::<Vec<TokenKind>>(),
                    );
                    return Err(());
                }
            };
        }
        Ok(left)
    }

    /*
     * STATEMENT PARSERS
     */
    /// starts with [TokenType::Wetuwn] in current
    /// ends with [TokenType::Terminator] in current
    fn parse_return(&mut self) -> Result<Statement, ()> {
        let start = self.curr_tok().pos;
        let expr = self.parse_expression(Precedence::Lowest)?;
        let end = self
            .expect_peek_is(TokenKind::Terminator)
            .and_then(|tok| Ok(tok.end_pos))?;
        Ok(Statement::Return(ReturnStatement {
            expr,
            range: Range::new(start, end),
        }))
    }
    /// starts with [TokenType::Iwf] in current
    /// ends with [TokenType::RBrace] in current
    fn parse_if(&mut self) -> Result<Statement, ()> {
        let start = self.curr_tok().pos;
        let condition = self.parse_expression(Precedence::Lowest).and_then(|expr| {
            self.expect_peek_is(TokenKind::LBrace)?;
            Ok(expr)
        })?;
        let body = self.parse_body(BodyType::If)?;
        let elifs = match self.peek_tok().kind {
            TokenKind::Ewif => self.parse_elifs()?,
            _ => vec![],
        };
        let else_block = match self.peek_tok().kind {
            TokenKind::Ewse => {
                self.expect_peek_is(TokenKind::Ewse)?;
                self.expect_peek_is(TokenKind::LBrace)?;
                Some(self.parse_body(BodyType::If)?)
            }
            _ => None,
        };
        let end = self.curr_tok().end_pos;
        Ok(Statement::If(IfStatement {
            condition,
            body,
            elifs,
            else_block,
            range: Range::new(start, end),
        }))
    }
    /// starts with [TokenType::Ewif] in peek
    /// ends with [TokenType::RBrace] in current
    fn parse_elifs(&mut self) -> Result<Vec<ElifStatement>, ()> {
        let mut res: Vec<ElifStatement> = vec![];
        while self.peek_tok_is(TokenKind::Ewif) {
            let start = self
                .expect_peek_is(TokenKind::Ewif)
                .and_then(|tok| Ok(tok.pos))?;
            let condition = self.parse_expression(Precedence::Lowest).and_then(|expr| {
                self.expect_peek_is(TokenKind::LBrace)?;
                Ok(expr)
            })?;
            let body = self.parse_body(BodyType::If)?;
            let end = self.curr_tok().end_pos;
            res.push(ElifStatement {
                condition,
                body,
                range: Range::new(start, end),
            })
        }
        Ok(res)
    }
    /// Abstracts over [Parser::parse_for_loop] and [Parser::parse_for_each]
    /// - for loop: `fow hi i-chan = 0~ i < n~ i+1 { ...body }`
    /// - for each: `fow item in collection { ...body }`
    ///
    /// starts with [TokenType::Fow] in current
    /// ends with [TokenType::RBrace] in current
    fn parse_for(&mut self) -> Result<Statement, ()> {
        let start = self.curr_tok().pos;
        match self.peek_tok().kind {
            TokenKind::Hi => self.parse_for_loop(start),
            TokenKind::Identifier => self.parse_for_each(start),
            _ => Err(self.unexpected_token_error(
                None,
                None,
                self.peek_tok(),
                &[TokenKind::Hi, TokenKind::Identifier],
            )),
        }
    }
    /// starts with [TokenType::Hi] in peek
    /// ends with [TokenType::RBrace] in current
    fn parse_for_loop(&mut self, start: (usize, usize)) -> Result<Statement, ()> {
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
        let body = self.parse_body(BodyType::For)?;
        let end = self.curr_tok().end_pos;
        Ok(Statement::ForLoop(ForLoop {
            init,
            condition,
            update,
            body,
            range: Range::new(start, end),
        }))
    }
    /// starts with [TokenType::Identifier] in peek
    /// ends with [TokenType::RBrace] in current
    fn parse_for_each(&mut self, start: (usize, usize)) -> Result<Statement, ()> {
        let item_id = self.expect_peek_is(TokenKind::Identifier)?;
        self.expect_peek_is(TokenKind::In)?;
        let collection = self.parse_expression(Precedence::Lowest)?;
        let body = self
            .expect_peek_is(TokenKind::LBrace)
            .and_then(|_| self.parse_body(BodyType::For))?;
        let end = self.curr_tok().end_pos;
        Ok(Statement::ForEach(ForEach {
            item_id,
            collection,
            body,
            range: Range::new(start, end),
        }))
    }
    /// starts with [TokenType::Bweak] in current
    /// ends with [TokenType::Terminator] in current
    fn parse_break(&mut self) -> Result<Statement, ()> {
        let tok = self.curr_tok();
        self.expect_peek_is(TokenKind::Terminator)?;
        Ok(Statement::Break(tok))
    }
    /// starts with [TokenType::Continue] in current
    /// ends with [TokenType::Terminator] in current
    fn parse_continue(&mut self) -> Result<Statement, ()> {
        let tok = self.curr_tok();
        self.expect_peek_is(TokenKind::Terminator)?;
        Ok(Statement::Continue(tok))
    }
    /// starts with [TokenType::Mash] in current
    /// ends with [TokenType::RBrace] in current
    fn parse_mash(&mut self) -> Result<Statement, ()> {
        let start = self.curr_tok().pos;
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek_is(TokenKind::LBrace)?;
        // disallow empty bodies
        if self.peek_tok_is(TokenKind::RBrace) {
            self.empty_body_error(
                self.body_parse_fns
                    .clone()
                    .into_iter()
                    .map(|(token, _)| token)
                    .collect::<Vec<TokenKind>>(),
                BodyType::Mash,
                self.source
                    .lines()
                    .nth(self.peek_tok().pos.0)
                    .unwrap_or_default(),
                self.peek_tok().pos,
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
        let end = self
            .expect_peek_is(TokenKind::RBrace)
            .and_then(|tok| Ok(tok.end_pos))?;
        Ok(Statement::Mash(MashStatement {
            expr,
            cases,
            default,
            range: Range { start, end },
        }))
    }
    /// starts with a [DataType]'s first token in current
    /// ends with a [DataType]'s first token in peek
    fn parse_case(&mut self) -> Result<Case, ()> {
        let start = self.peek_tok().pos;
        let case_type = self
            .expect_peek_is_type()
            .and_then(|tok| self.parse_data_type(tok, start))?;
        let body = self
            .expect_peek_is(TokenKind::Colon)
            .and_then(|_| self.parse_case_body())?;
        let end = self.curr_tok().pos;
        Ok(Case {
            case_type,
            body,
            range: Range::new(start, end),
        })
    }

    /// starts with [TokenType::Colon] in current
    /// ends with any of the ff:
    /// - [DataType]'s first token in peek (another case next)
    /// - [TokenType::Default] in peek (default case next)
    /// - [TokenType::RBrace] in peek (end of mash statement)
    fn parse_case_body(&mut self) -> Result<Body, ()> {
        let mut statements: Vec<Statement> = vec![];
        let start = self.curr_tok().pos;
        // disallow empty bodies
        if self.peek_tok_is_type() || self.peek_tok_is_in(&[TokenKind::Default, TokenKind::RBrace])
        {
            self.empty_body_error(
                self.body_parse_fns
                    .clone()
                    .into_iter()
                    .map(|(token, _)| token)
                    .collect::<Vec<TokenKind>>(),
                BodyType::MashCase,
                self.source
                    .lines()
                    .nth(self.peek_tok().pos.0)
                    .unwrap_or_default(),
                self.peek_tok().pos,
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
                        ParsingFnType::Body,
                        self.curr_tok(),
                        self.body_parse_fns
                            .clone()
                            .into_iter()
                            .map(|(tokens, _)| tokens)
                            .into_iter()
                            .collect::<Vec<_>>(),
                    );
                    return Err(());
                }
            };
            statements.push(stmt);
        }
        let end = match self.peek_tok().kind {
            TokenKind::Type
            | TokenKind::Chan
            | TokenKind::Kun
            | TokenKind::Senpai
            | TokenKind::Kouhai
            | TokenKind::San
            | TokenKind::Sama
            | TokenKind::Dono
            | TokenKind::Default
            | TokenKind::RBrace => self.curr_tok().end_pos,
            _ => {
                self.unexpected_token_error(
                    None,
                    None,
                    self.peek_tok(),
                    &[
                        TokenKind::Type,
                        TokenKind::Chan,
                        TokenKind::Kun,
                        TokenKind::Senpai,
                        TokenKind::Kouhai,
                        TokenKind::San,
                        TokenKind::Sama,
                        TokenKind::Dono,
                        TokenKind::Default,
                    ],
                );
                return Err(());
            }
        };
        Ok(Body {
            statements,
            range: Range::new(start, end),
        })
    }

    /// This parses three possible ident statements: assignments, function/method calls, pipelines
    /// ```
    /// aqua = 1~
    /// aqua.arms[2] = LONG~
    /// aqua.arms[1].punch()~
    /// aqua.scream() | voice_crack()~
    /// ```
    /// starts with [TokenType::Identifier] in current
    /// ends with [TokenType::Terminator] in current
    fn parse_ident_statement(&mut self) -> Result<Statement, ()> {
        let id = self.parse_ident_expression()?;
        match self.curr_tok().kind {
            TokenKind::Identifier | TokenKind::RBracket => match self.peek_tok().kind {
                TokenKind::Assign
                | TokenKind::PlusEqual
                | TokenKind::DashEqual
                | TokenKind::MultiplyEqual
                | TokenKind::DivideEqual
                | TokenKind::ModuloEqual
                | TokenKind::ExponentEqual => {
                    let id = match id {
                        Expression::Ident(Identifier::Token(tok)) => Assignable::Token(tok),
                        Expression::Ident(Identifier::Indexed(idx)) => Assignable::Indexed(idx),
                        Expression::Ident(Identifier::Access(AccessType::Field(field))) => Assignable::Access(field),
                        _ => unreachable!(
                            "parse_ident_statement: parse_ident_value ending in Identifier and RBracket id not return Value::{{Token, Indexed, or Access:Field}}"
                        ),
                    };
                    Ok(Statement::Assignment(self.parse_assignment(id)?))
                }
                _ => {
                    self.unexpected_token_error(
                        None,
                        None,
                        self.peek_tok(),
                        &TokenKind::assign_ops()
                            .into_iter()
                            .chain(vec![TokenKind::Pipe])
                            .collect::<Vec<TokenKind>>()
                    );
                    Err(())
                }
            },
            TokenKind::RParen => {
                self.expect_peek_is(TokenKind::Terminator)?;
                match id {
                    Expression::Ident(Identifier::FnCall(fn_call)) => Ok(Statement::FnCall(fn_call)),
                    Expression::Ident(Identifier::Access(AccessType::Method(method))) => Ok(Statement::Method(method)),
                    Expression::Pipeline(pipe) => Ok(Statement::Pipeline(pipe)),
                    _ => unreachable!("parse_ident_statement(): parse_ident_value() that returned at RParen is neither FnCall or MethodAccess"),
                }
            }
            _ => unreachable!(
                "parse_ident_statement(): parse_ident_value() did not return at RParen, Identifier, or RBracket, got {}",
                self.curr_tok().kind
            ),
        }
    }
    /// starts with [TokenType::LParen] in current, must pass in Identifier
    /// ends with [TokenType::RParen] in current
    fn parse_fn_call(&mut self, id: Token) -> Result<FnCall, ()> {
        let start = id.pos;
        let mut args: Vec<Expression> = vec![];
        while !self.peek_tok_is(TokenKind::RParen) {
            args.push(self.parse_expression(Precedence::Lowest)?);
            self.allow_hanging_comma(TokenKind::RParen)?;
        }
        let end = self
            .expect_peek_is(TokenKind::RParen)
            .and_then(|tok| Ok(tok.end_pos))?;
        Ok(FnCall {
            id,
            args,
            range: Range::new(start, end),
            signature: FnSignature::default(),
        })
    }
    /*
     * EXPRESSION PARSERS
     */
    /// starts with prefix operator in current
    /// ends with prefix operand in current
    fn parse_prefix_expression(&mut self) -> Result<Expression, ()> {
        let start = self.curr_tok().pos;
        let op = self.curr_tok();
        let right = self.parse_expression(Precedence::Prefix)?;
        let end = right.range().end;
        Ok(Expression::Prefix(PrefixExpression {
            op,
            right: Box::new(right),
            range: Range::new(start, end),
        }))
    }
    /// starts with infix operator in current
    /// ends with infix operand in current
    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ()> {
        let start = left.range().start;
        let op = self.curr_tok();
        let right = self.parse_expression(Precedence::of(self.curr_tok().kind))?;
        let end = right.range().end;
        Ok(Expression::Infix(InfixExpression {
            left: Box::new(left),
            op,
            right: Box::new(right),
            range: Range::new(start, end),
        }))
    }
    /// starts with [TokenType::LParen] in current
    /// ends with [TokenType::RParen] in current
    fn parse_grouped_expression(&mut self) -> Result<Expression, ()> {
        let start = self.curr_tok().pos;
        let expr = Box::new(self.parse_expression(Precedence::Lowest)?);
        let end = self
            .expect_peek_is(TokenKind::RParen)
            .and_then(|tok| Ok(tok.end_pos))?;
        Ok(Expression::Grouped(GroupedExpression {
            expr,
            range: Range::new(start, end),
        }))
    }
    /*
     * LITERAL PARSERS
     */
    /// just returns the current token
    fn parse_literal(&mut self) -> Result<Expression, ()> {
        Ok(Expression::Ident(Identifier::Token(self.curr_tok())))
    }
    /// starts with [TokenType::LBracket] in current
    /// ends with [TokenType::RBracket] in current
    fn parse_array_literal(&mut self) -> Result<Expression, ()> {
        let start = self.curr_tok().pos;
        let mut exprs: Vec<Expression> = vec![];
        while !self.peek_tok_is(TokenKind::RBracket) {
            exprs.push(self.parse_expression(Precedence::Lowest)?);
            match self.peek_tok().kind {
                TokenKind::Comma => {
                    self.advance(1);
                }
                TokenKind::RBracket => (),
                _ => {
                    self.unexpected_token_error(
                        None,
                        None,
                        self.peek_tok(),
                        &[TokenKind::Comma, TokenKind::RBracket],
                    );
                }
            }
        }
        let end = self
            .expect_peek_is(TokenKind::RBracket)
            .and_then(|tok| Ok(tok.end_pos))?;
        Ok(Expression::Array(ArrayLiteral {
            exprs,
            range: Range::new(start, end),
        }))
    }
    /// starts with Hash in current
    /// starts with RBracket in current
    ///
    /// Abstracts over parsing hashset and hashmap literals depending on whether the first item is
    /// followed by a colon or a comma
    /// `#[item, item]` => hashset
    /// `#[item: item]` => hashmap
    fn parse_hash_literal(&mut self) -> Result<Expression, ()> {
        let start = self.curr_tok().pos;
        self.expect_peek_is(TokenKind::LBracket)?;
        // empty literal
        match self.peek_tok().kind {
            // empty set: #[]
            TokenKind::RBracket => {
                self.advance(1);
                return Ok(Expression::Set(SetLiteral {
                    exprs: vec![],
                    range: Range::new(start, self.curr_tok().end_pos),
                }));
            }
            // empty map: #[:]
            TokenKind::Colon => {
                self.advance(1);
                self.expect_peek_is(TokenKind::RBracket)?;
                return Ok(Expression::Map(MapLiteral {
                    exprs: vec![],
                    range: Range::new(start, self.curr_tok().end_pos),
                }));
            }
            _ => (),
        }
        let first_expr = self.parse_expression(Precedence::Lowest)?;
        match self.peek_tok().kind {
            TokenKind::RBracket => {
                self.advance(1);
                Ok(Expression::Set(SetLiteral {
                    exprs: vec![first_expr],
                    range: Range::new(start, self.curr_tok().end_pos),
                }))
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
    /// starts with [TokenType::Comma] in peek (always), with first val already parsed
    /// ends with [TokenType::RBracket] in current
    fn parse_set_literal(
        &mut self,
        first_expr: Expression,
        start: (usize, usize),
    ) -> Result<Expression, ()> {
        let mut exprs: Vec<Expression> = vec![first_expr];
        self.expect_peek_is(TokenKind::Comma)?;
        while !self.peek_tok_is(TokenKind::RBracket) {
            exprs.push(self.parse_expression(Precedence::Lowest)?);
            self.allow_hanging_comma(TokenKind::RBracket)?;
        }
        let end = self
            .expect_peek_is(TokenKind::RBracket)
            .and_then(|tok| Ok(tok.end_pos))?;
        Ok(Expression::Set(SetLiteral {
            exprs,
            range: Range::new(start, end),
        }))
    }
    /// starts with [TokenType::Colon] in peek (always), with first key already parsed
    /// ends with [TokenType::RBracket] in current
    fn parse_map_literal(
        &mut self,
        first_key: Expression,
        start: (usize, usize),
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
        let end = self
            .expect_peek_is(TokenKind::RBracket)
            .and_then(|tok| Ok(tok.end_pos))?;
        Ok(Expression::Map(MapLiteral {
            exprs,
            range: Range::new(start, end),
        }))
    }

    /*
     * IDENTIFIER EXPRESSION PARSERS
     */
    /// This parses every possible ident expression, ranging from just a variable to pipelines to
    /// chained accesses
    /// ```
    /// aqua
    /// aqua.age
    /// aqua.arms[1].punch()
    /// aqua.scream() | voice_crack()
    /// ```
    /// starts with [TokenType::Identifier] in current
    /// ends with any of the ff in current:
    /// - [TokenType::Identifier]
    /// - [TokenType::RParen]
    /// - [TokenType::RBracket]
    fn parse_ident_expression(&mut self) -> Result<Expression, ()> {
        let start = self.curr_tok().pos;
        let first = self.parse_ident()?;
        let mut rest: Vec<Callable> = vec![];
        while self.peek_tok_is(TokenKind::Pipe) {
            self.expect_peek_is(TokenKind::Pipe)?;
            self.expect_peek_is(TokenKind::Identifier)?;
            rest.push(self.parse_callable()?);
        }
        let end = self.curr_tok().end_pos;
        match &rest.len() {
            0 => match first {
                Identifier::Token(res) => Ok(Expression::Ident(Identifier::Token(res))),
                Identifier::FnCall(res) => Ok(Expression::Ident(Identifier::FnCall(res))),
                Identifier::Indexed(res) => Ok(Expression::Ident(Identifier::Indexed(res))),
                Identifier::Access(res) => Ok(Expression::Ident(Identifier::Access(res))),
            },
            _ => Ok(Expression::Pipeline(Pipeline {
                first: Box::new(first),
                rest,
                range: Range::new(start, end),
            })),
        }
    }
    /// This parses a single ident like [Parser::parse_ident_value] but ensures that only callables
    /// are parsed. If the ident parsed isn't a callable (variable, field, indexed var, etc.) This
    /// will create an error
    ///
    /// # Usage
    /// Used in parsing [Pipeline]s where only callables are expected in between pipes
    ///
    /// starts with [TokenType::Identifier] in current
    /// ends with [TokenType::RParen] in current
    fn parse_callable(&mut self) -> Result<Callable, ()> {
        let id = self.parse_ident()?;
        match self.curr_tok().kind {
            TokenKind::RParen => {
                match id {
                    Identifier::FnCall(call) => Ok(Callable::Fn(call)),
                    Identifier::Access(AccessType::Method(method)) => Ok(Callable::Method(method)),
                    _ => unreachable!("parse_callable(): parse_ident() that returned at RParen is neither FnCall or MethodAccess"),
                }
            }
            _ => {
                self.error = NonCallableInPipelineError::new(
                    self.source
                        .lines()
                        .skip(id.range().start.0)
                        .take(id.range().end.0 - id.range().start.0 + 1)
                        .collect()
                    , id)
                    .message();
                Err(())
            },
        }
    }
    /// Parses single idents that may or may not have field/method accesses
    /// ```
    /// aqua
    /// aqua.age
    /// aqua.likes[10]
    /// aqua.hands[1].fingers[3].raise()
    /// ```
    /// starts with [TokenType::Identifier] in current
    /// ends with any of the ff in current:
    /// - [TokenType::Identifier]
    /// - [TokenType::RParen]
    /// - [TokenType::RBracket]
    fn parse_ident(&mut self) -> Result<Identifier, ()> {
        let start = self.curr_tok().pos;
        let mut accessed: Vec<Accessor> = vec![];
        let first = self.parse_ident_accessor()?;
        accessed.push(first.clone());
        while self.peek_tok_is(TokenKind::Dot) {
            self.advance(1);
            self.expect_peek_is(TokenKind::Identifier)?;
            accessed.push(self.parse_ident_accessor()?);
        }
        if accessed.len() > 1 {
            match accessed.last().cloned().unwrap_or_default() {
                Accessor::Token(tok) => Ok(Identifier::Access(AccessType::Field(GroupAccess {
                    accessed,
                    access_type: std::marker::PhantomData,
                    range: Range::new(start, tok.end_pos),
                }))),
                Accessor::IndexedId(idx) => {
                    Ok(Identifier::Access(AccessType::Field(GroupAccess {
                        accessed,
                        access_type: std::marker::PhantomData,
                        range: Range::new(start, idx.range().end),
                    })))
                }
                Accessor::FnCall(fn_call) => {
                    Ok(Identifier::Access(AccessType::Method(GroupAccess {
                        accessed,
                        access_type: std::marker::PhantomData,
                        range: Range::new(start, fn_call.range().end),
                    })))
                }
            }
        } else {
            match first {
                Accessor::Token(tok) => Ok(Identifier::Token(tok)),
                Accessor::IndexedId(idx) => Ok(Identifier::Indexed(idx)),
                Accessor::FnCall(fn_call) => Ok(Identifier::FnCall(fn_call)),
            }
        }
    }
    /// This parses singlular idents that can but don't access fields/methods
    /// ```
    /// aqua
    /// aqua()
    /// aqua[1]
    /// ```
    /// starts with [TokenType::Identifier] in current
    /// ends with any of the ff:
    /// - [TokenType::Identifier]
    /// - [TokenType::RParen]
    /// - [TokenType::RBracket]
    fn parse_ident_accessor(&mut self) -> Result<Accessor, ()> {
        let mut init_id: Indexable = Indexable::Token(self.curr_tok());
        let mut id: Accessor = Accessor::Token(self.curr_tok());
        // parse fn call
        if self.peek_tok_is(TokenKind::LParen) {
            self.advance(1);
            let fn_call = self.parse_fn_call(match init_id {
                Indexable::Token(tok) => tok,
                _ => unreachable!(
                    "parse_ident_accessor(): impossible for self.curr_tok() to return a non-Token"
                ),
            })?;
            match self.peek_tok().kind {
                TokenKind::LBracket => {
                    // init_id will be used in parsing indexed ident below
                    init_id = Indexable::FnCall(fn_call);
                }
                _ => {
                    // avoid unnecessary cloning if not going to parse indexed ident
                    id = Accessor::FnCall(fn_call);
                }
            }
        }
        // parse indexed ident
        if self.peek_tok_is(TokenKind::LBracket) {
            self.advance(1);
            let start = init_id.range().start;
            let indices: Vec<Expression> = self.parse_array_literal().and_then(|a| match a {
                Expression::Array(a) => Ok(a.exprs),
                _ => unreachable!(),
            })?;
            let end = self.curr_tok().pos;
            id = Accessor::IndexedId(IndexedId {
                id: init_id,
                indices,
                range: Range::new(start, end),
            });
        }
        Ok(id)
    }
    /// This parses a group initializer
    /// ```
    /// GroupName(arg1, arg2, ...)
    /// ```
    /// starts with [TokenType::Type] in current
    /// ends with [TokenType::LParen] in current
    fn parse_group_init(&mut self) -> Result<Expression, ()> {
        let id = self.curr_tok();
        let start = id.pos;
        self.expect_peek_is(TokenKind::LParen)?;
        let mut args: Vec<Expression> = vec![];
        while !self.peek_tok_is(TokenKind::RParen) {
            args.push(self.parse_expression(Precedence::Lowest)?);
            self.allow_hanging_comma(TokenKind::RParen)?;
        }
        let end = self
            .expect_peek_is(TokenKind::RParen)
            .and_then(|tok| Ok(tok.end_pos))?;
        Ok(Expression::GroupInit(GroupInit {
            id,
            args,
            range: Range::new(start, end),
        }))
    }

    /*
     * HELPER FUNCTIONS
     */
    /// maps a TokenType to a respective parsing function
    /// user can supply own parsing function
    fn register_prefix(
        &mut self,
        args: Vec<(TokenKind, fn(&mut Parser) -> Result<Expression, ()>)>,
    ) {
        args.into_iter().for_each(|(token_kind, parse_fn)| {
            self.prefix_parse_fns.insert(token_kind, parse_fn);
        })
    }
    /// maps a TokenType to a respective parsing function
    /// function is always `Self::parse_infix_expression()`
    fn register_infix(&mut self, args: Vec<TokenKind>) {
        args.into_iter().for_each(|token_kind| {
            self.infix_parse_fns
                .insert(token_kind, Self::parse_infix_expression);
        })
    }
    /// maps a TokenType to a respective parsing function
    /// user can supply own parsing function
    fn register_body(&mut self, args: Vec<(TokenKind, fn(&mut Parser) -> Result<Statement, ()>)>) {
        args.into_iter().for_each(|(token_kind, parse_fn)| {
            self.body_parse_fns.insert(token_kind, parse_fn);
        })
    }
    /// checks current token's type against given TokenType
    fn curr_tok_is(&mut self, token_kind: TokenKind) -> bool {
        self.curr_tok().kind == token_kind
    }
    /// checks peek token's type against given TokenType
    fn peek_tok_is(&mut self, token_kind: TokenKind) -> bool {
        self.peek_tok().kind == token_kind
    }
    /// checks peek token's type against given TokenTypes
    fn peek_tok_is_in(&mut self, token_types: &[TokenKind]) -> bool {
        token_types.contains(&self.peek_tok().kind)
    }
    /// checks peek token's type is a DataType
    fn peek_tok_is_type(&mut self) -> bool {
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
    /// advances cursor 1 time if peek tok is eq to given
    /// adds an error otherwise
    fn expect_peek_is(&mut self, token_kind: TokenKind) -> Result<Token, ()> {
        match self.peek_tok().kind == token_kind {
            true => {
                self.advance(1);
                Ok(self.curr_tok())
            }
            false => {
                self.error = UnexpectedTokenError::new(
                    self.peek_tok(),
                    vec![token_kind],
                    None,
                    None,
                    self.source
                        .lines()
                        .nth(self.peek_tok().pos.0)
                        .unwrap_or_default(),
                    self.peek_tok().pos,
                    self.peek_tok().end_pos,
                )
                .message();
                Err(())
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
                self.error = UnexpectedTokenError::new(
                    self.peek_tok(),
                    token_kinds.to_vec(),
                    None,
                    None,
                    self.source
                        .lines()
                        .nth(self.peek_tok().pos.0)
                        .unwrap_or_default(),
                    self.peek_tok().pos,
                    self.peek_tok().end_pos,
                )
                .message();
                Err(())
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
            _ => {
                self.error = UnexpectedTokenError::new(
                    self.peek_tok(),
                    TokenKind::data_types(),
                    None,
                    None,
                    self.source
                        .lines()
                        .nth(self.peek_tok().pos.0)
                        .unwrap_or_default(),
                    self.peek_tok().pos,
                    self.peek_tok().end_pos,
                )
                .message();
                Err(())
            }
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
            | TokenKind::ModuloEqual
            | TokenKind::ExponentEqual => {
                self.advance(1);
                Ok(self.curr_tok())
            }
            _ => {
                self.error = UnexpectedTokenError::new(
                    self.peek_tok(),
                    TokenKind::assign_ops(),
                    None,
                    None,
                    self.source
                        .lines()
                        .nth(self.peek_tok().pos.0)
                        .unwrap_or_default(),
                    self.peek_tok().pos,
                    self.peek_tok().end_pos,
                )
                .message();
                Err(())
            }
        }
    }
    /// allows hanging comma on enclosed, comma separated expressions
    fn allow_hanging_comma(&mut self, closing: TokenKind) -> Result<(), ()> {
        if self.peek_tok_is(TokenKind::Comma) {
            Ok(self.advance(1))
        } else if self.peek_tok_is(closing) {
            Ok(())
        } else {
            self.unexpected_token_error(None, None, self.peek_tok(), &[TokenKind::Comma, closing]);
            Err(())
        }
    }

    /*
     * ERROR FUNCTIONS
     */
    fn unexpected_token_error(
        &mut self,
        header: Option<&'static str>,
        context: Option<&'static str>,
        actual: Token,
        expected: &[TokenKind],
    ) {
        self.error = UnexpectedTokenError::new(
            actual,
            expected.to_vec(),
            header,
            context,
            self.source.lines().nth(actual.pos.0).unwrap_or_default(),
            actual.pos,
            actual.end_pos,
        )
        .message()
    }
    fn empty_body_error(
        &mut self,
        expected: Vec<TokenKind>,
        body_type: BodyType,
        line_text: &'static str,
        r_bracket_pos: (usize, usize),
    ) {
        self.error = EmptyBodyError::new(expected, body_type, line_text, r_bracket_pos).message()
    }
    fn no_parsing_fn_error(
        &mut self,
        parsing_fn_type: ParsingFnType,
        actual: Token,
        expected: Vec<TokenKind>,
    ) {
        self.error = UnexpectedTokenError::new(
            actual,
            expected.to_vec(),
            Some(parsing_fn_type.header()),
            None,
            self.source.lines().nth(actual.pos.0).unwrap_or_default(),
            actual.pos,
            actual.end_pos,
        )
        .message()
    }
}

#[derive(Clone, Copy)]
enum ParsingFnType {
    Prefix,
    Infix,
    Body,
}
impl ParsingFnType {
    fn header(&self) -> &'static str {
        match self {
            Self::Prefix => "NOT A VALID VALUE STARTING TOKEN",
            Self::Infix => "NOT A VALID OPERATOR",
            Self::Body => "NOT A VALID STATEMENT STARTING TOKEN",
        }
    }
}
