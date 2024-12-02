use crate::errors::lex_errors::CompilerError;
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
