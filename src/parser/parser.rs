use std::{collections::HashMap, marker::PhantomData};

use crate::{
    errors::parse_errors::{BodyType, EmptyBodyError, ParserError, UnexpectedTokenError},
    lexer::token::{Position, Token, TokenKind},
    parser::productions::{
        AccessType, Accessed, ArrayLiteral, Assignable, Assignment, Body, Case, Contract, DataType,
        Declaration, ElifStatement, Expression, FnCall, FnSignature, ForEach, ForLoop, Function,
        Group, GroupAccess, GroupField, GroupInit, GroupMethod, GroupedExpression, IfStatement,
        Indexable, IndexedId, InfixExpression, MapLiteral, MapType, MashStatement, Param, Pipeline,
        PrefixExpression, Program, ReturnStatement, SetLiteral, SetType, Statement, VecType,
        Vectorable,
    },
};

/// Max amount of errors till recovery strategies starts discarding tokens and
/// assume its ok
const MAX_LOCAL_ERROR_COUNT: usize = 3;

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
    fn of(kind: TokenKind) -> Self {
        match kind {
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

#[derive(Debug)]
pub struct Parser<'src> {
    pub program: Program,
    pub errors: Vec<ParserError<'src>>,
    source: &'src str,
    line_starts: &'src Vec<usize>,
    tokens: Vec<Token>,
    pos: usize,
    prefix_parse_fns: HashMap<TokenKind, fn(&mut Self) -> Result<Expression, ()>>,
    infix_parse_fns: HashMap<TokenKind, fn(&mut Self, Expression) -> Result<Expression, ()>>,
    body_parse_fns: HashMap<TokenKind, fn(&mut Self) -> Statement>,
}
impl<'src> Parser<'src> {
    // TODO: expected values for some are inaccurate. For example:
    // - parse_expression: only expects Terminator when a Dot or Pipe can
    // follow it as well.
    // - and more... i forgot the rest sorry...
    pub fn new(source: &'src str, line_starts: &'src Vec<usize>, tokens: Vec<Token>) -> Self {
        let mut parser = Self {
            program: Program::default(),
            tokens,
            source,
            line_starts,
            errors: vec![],
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
            (TokenKind::Hi, Self::parse_declaration_statement),
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
    fn advance(&mut self, n: usize) {
        for _ in 0..n {
            if self.curr_tok().kind == TokenKind::EOF {
                return;
            }
            self.pos += 1;
        }
    }
    /// advances cursor n times if the condition is met
    fn advance_if<F>(&mut self, n: usize, mut condition: F)
    where
        F: FnMut(&mut Self) -> bool,
    {
        if condition(self) {
            self.advance(n);
        }
    }
    /// advances cursor until the condition is met
    fn advance_until<F>(&mut self, mut condition: F)
    where
        F: FnMut(&mut Self) -> bool,
    {
        while !condition(self) && self.curr_tok().kind != TokenKind::EOF {
            self.advance(1);
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
    /// Peeks the token `n` after the peek
    /// ```
    /// assert_eq!(self.peek_tok(), self.peek_tok_nth(0));
    /// ```
    fn peek_tok_nth(&self, n: usize) -> Token {
        self.tokens
            .get(self.pos + 1 + n)
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
                    TokenKind::Main => main = Some(self.parse_function()),
                    TokenKind::Type => methods.push(self.parse_method()),
                    _ => functions.push(self.parse_function()),
                },
                TokenKind::Group => groups.push(self.parse_group()),
                TokenKind::Contract => contracts.push(self.parse_contract()),
                TokenKind::Hi => globals.push(self.parse_declaration_statement()),
                _ => {
                    self.unexpected_token_error(
                        Some("INVALID GLOBAL TOKEN"),
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
        .map_err(|e| self.errors.push(ParserError::NoMain(e)))?;
        Ok(())
    }

    /// starts with [TokenKind::Fun] in current
    /// ends with [TokenKind::RBrace] in current
    fn parse_method(&mut self) -> GroupMethod {
        let start = self.curr_tok();
        let group = self
            .expect_peek_is_type_()
            .ok_then(|_, actual| Ok(actual))
            .or_recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[
                    TokenKind::Identifier,
                    TokenKind::Bang,
                    TokenKind::Dash,
                    TokenKind::LParen,
                    TokenKind::LBrace,
                ]) || parser.peek_tok_is_type()
            })
            .finish();
        let mutable = self
            .peek_tok_is(TokenKind::Bang)
            .then(|| self.advance(1))
            .is_some();
        let function = self.parse_function();
        GroupMethod::new(
            function.id,
            group,
            mutable,
            function.dtype,
            function.params,
            function.body,
            (start.offset.start, self.curr_tok().offset.end),
        )
    }

    /// starts with [TokenKind::Fun] in current
    /// ends with [TokenKind::RBrace] in current
    fn parse_function(&mut self) -> Function {
        let start_offset = self.curr_tok().offset.start;
        let (id, error_count) = self
            .expect_peek_is_in_(&[TokenKind::Identifier, TokenKind::Main])
            .ok_then(|_, actual| Ok(actual))
            .or_recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[TokenKind::Dash, TokenKind::LParen, TokenKind::LBrace])
                    || parser.peek_tok_is_type()
            })
            .error_header("MISSING FUNCTION NAME")
            .finish_with_error_count();

        let ((), error_count) = self
            .expect_peek_is_(TokenKind::Dash)
            .recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[TokenKind::LParen, TokenKind::LBrace])
                    || parser.peek_tok_is_type()
            })
            .limit_errors(error_count)
            .error_context("Need a dash before declaring the type.")
            .finish_with_error_count();

        let (dtype, error_count) = self
            .expect_peek_is_type_()
            .ok_then(|parser, actual| parser.parse_data_type(actual))
            .or_recover()
            .skip_until(|parser| {
                // follow set
                parser.peek_tok_is_in(&[TokenKind::LParen, TokenKind::LBrace])
                // end of dtypes
                || parser.curr_tok_is_in(&[TokenKind::RBracket, TokenKind::RBrace])
                || parser.curr_tok_is_type()
            })
            .limit_errors(error_count)
            .error_header("INVALID TYPE")
            .error_context("User defined types must start with a capital letter.")
            .finish_with_error_count();

        let params = self
            .expect_peek_is_(TokenKind::LParen)
            .ok_then(|parser, _| Ok(parser.parse_params()))
            .or_recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[TokenKind::LBrace]) || parser.curr_tok_is(TokenKind::RParen)
            })
            .limit_errors(error_count)
            .error_header("MISSING OPENING ( FOR PARAMETERS")
            .finish();

        let body = self
            .expect_peek_is_(TokenKind::LBrace)
            .ok_then(|parser, _| Ok(parser.parse_body(BodyType::Fn)))
            .or_recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[
                    TokenKind::Hi,
                    TokenKind::Fun,
                    TokenKind::Group,
                    TokenKind::Contract,
                ]) || parser.curr_tok_is(TokenKind::RBrace)
            })
            .limit_errors(error_count)
            .finish();

        Function::new(
            id,
            dtype,
            params,
            body,
            (start_offset, self.curr_tok().offset.end),
        )
    }

    /// starts with [TokenKind::LParen] in current
    /// ends with [TokenKind::RParen] in current
    fn parse_params(&mut self) -> Vec<Param> {
        let mut params: Vec<Param> = vec![];
        while !self.peek_tok_is(TokenKind::RParen) {
            let param = self.parse_param();
            self.allow_hanging_comma(TokenKind::RParen);
            if param.variadic {
                self.expect_peek_is_(TokenKind::RParen)
                    .recover()
                    .skip_until_peek_is(TokenKind::RParen)
                    .error_header("VARIADIC PARAMETER NOT LAST")
                    .error_context("Variadic parameter should be the last parameter.")
                    .finish()
            }
            params.push(param);
        }
        self.advance(1);
        params
    }

    /// starts with [TokenKind::Identifier] in peek
    /// ends with [DataType]'s last token in current
    fn parse_param(&mut self) -> Param {
        let (id, error_count) = self
            .expect_peek_is_(TokenKind::Identifier)
            .ok_then(|_, actual| Ok(actual))
            .or_recover()
            .skip_one_if(|parser| {
                !parser.peek_tok_is(TokenKind::Dash) && parser.peek_tok_nth(1).kind.is_type()
            })
            .finish_with_error_count();
        let ((), error_count) = self
            .expect_peek_is_(TokenKind::Dash)
            .recover()
            .skip_one_if(|parser| {
                !parser.peek_tok_is_type() && parser.peek_tok_nth(1).kind == TokenKind::Ellipsis
            })
            .limit_errors(error_count)
            .finish_with_error_count();
        let dtype = self
            .expect_peek_is_type_()
            .ok_then(|parser, actual| parser.parse_data_type(actual))
            .or_recover()
            .skip_until_peek_is_in(&[TokenKind::Ellipsis, TokenKind::Comma, TokenKind::RParen])
            .limit_errors(error_count)
            .finish();
        let variadic = self
            .peek_tok_is(TokenKind::Ellipsis)
            .then(|| self.advance(1))
            .is_some();
        Param::new(
            id,
            dtype,
            variadic,
            (id.offset.start, self.curr_tok().offset.end),
        )
    }

    /// starts with [TokenKind::Group] in current
    /// ends with [TokenKind::RBrace] in current
    fn parse_group(&mut self) -> Group {
        let start = self.curr_tok();
        let id = self
            .expect_peek_is_(TokenKind::Type)
            .ok_then(|_, actual| Ok(actual))
            .or_recover()
            .skip_until_peek_is(TokenKind::LBrace)
            .finish();
        let found_opening = self
            .expect_peek_is_(TokenKind::LBrace)
            .ignore()
            .finish_result()
            .is_ok();
        if found_opening && self.peek_tok_is(TokenKind::RBrace) {
            self.empty_body_error(
                self.curr_tok(),
                self.peek_tok(),
                vec![TokenKind::Identifier],
                BodyType::Group,
                self.source
                    .lines()
                    .nth(self.peek_tok().line(&self.line_starts))
                    .unwrap_or_default(),
            );
        }
        let fields = self.parse_fields();
        self.expect_peek_is_(TokenKind::RBrace)
            .recover()
            .skip_until_peek_is_in(&[
                TokenKind::Hi,
                TokenKind::Fun,
                TokenKind::Group,
                TokenKind::Contract,
            ])
            .finish();
        Group::new(id, fields, (start.offset.start, self.curr_tok().offset.end))
    }

    /// difference with [Self::parse_params] is that this does not allow variadic fields
    ///
    /// starts with [TokenKind::LBrace] in current
    /// ends with [TokenKind::Terminator] in current
    /// ends with [TokenKind::RBrace] in peek
    fn parse_fields(&mut self) -> Vec<GroupField> {
        let mut fields = vec![];
        while !self.peek_tok_is(TokenKind::RBrace) {
            fields.push(self.parse_field());
        }
        fields
    }

    /// difference with [Self::parse_param] is that this does not allow variadic fields
    ///
    /// starts with [TokenKind::Identifier] in peek
    /// ends with [TokenKind::Terminator] in current
    fn parse_field(&mut self) -> GroupField {
        let id = self
            .expect_peek_is_(TokenKind::Identifier)
            .ok_then(|_, actual| Ok(actual))
            .or_recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[TokenKind::Dash, TokenKind::Terminator])
                    || parser.peek_tok_is_type()
            })
            .finish();
        self.expect_peek_is_(TokenKind::Dash)
            .recover()
            .skip_until(|parser| {
                parser.peek_tok_is(TokenKind::Terminator) || parser.peek_tok_is_type()
            })
            .finish();
        let dtype = self
            .expect_peek_is_type_()
            .ok_then(|parser, actual| parser.parse_data_type(actual))
            .or_recover()
            .skip_until_peek_is_in(&[
                TokenKind::Terminator,
                TokenKind::Identifier,
                TokenKind::RBrace,
            ])
            .finish();
        self.expect_peek_is_(TokenKind::Terminator)
            .ok_then(|_, actual| Ok(actual))
            .or_recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[TokenKind::Identifier, TokenKind::RBrace])
                    || parser.curr_tok_is(TokenKind::Terminator)
            })
            .finish();
        GroupField::new(id, dtype, (id.offset.start, self.curr_tok().offset.end))
    }

    /// starts with [TokenKind::Contract] in current
    /// ends with [TokenKind::RBrace] in current
    fn parse_contract(&mut self) -> Contract {
        let start = self.curr_tok();
        let id = self
            .expect_peek_is_(TokenKind::Type)
            .ok_then(|_, actual| Ok(actual))
            .or_recover()
            .skip_until_peek_is(TokenKind::LBrace)
            .finish();
        let found_opening = self
            .expect_peek_is_(TokenKind::LBrace)
            .ignore()
            .finish_result()
            .is_ok();
        if found_opening && self.peek_tok_is(TokenKind::RBrace) {
            self.empty_body_error(
                self.curr_tok(),
                self.peek_tok(),
                vec![TokenKind::Identifier],
                BodyType::Contract,
                self.source
                    .lines()
                    .nth(self.peek_tok().line(&self.line_starts))
                    .unwrap_or_default(),
            );
        }
        let signatures = self.parse_fn_signatures();
        self.expect_peek_is_(TokenKind::RBrace)
            .recover()
            .skip_until_peek_is_in(&[
                TokenKind::Hi,
                TokenKind::Fun,
                TokenKind::Group,
                TokenKind::Contract,
            ])
            .finish();
        Contract::new(
            id,
            signatures,
            (start.offset.start, self.curr_tok().offset.end),
        )
    }

    /// starts with [TokenKind::Identifier] in peek
    /// ends with [TokenKind::Terminator] in current
    /// ends with [TokenKind::RBrace] in peek
    fn parse_fn_signatures(&mut self) -> Vec<FnSignature> {
        let mut signatures: Vec<FnSignature> = vec![];
        while !self.peek_tok_is(TokenKind::RBrace) {
            signatures.push(self.parse_fn_signature());
        }
        signatures
    }

    /// starts with [TokenKind::Identifier] in peek
    /// ends with [TokenKind::Terminator] in current
    fn parse_fn_signature(&mut self) -> FnSignature {
        let (id, error_count) = self
            .expect_peek_is_(TokenKind::Identifier)
            .ok_then(|_, actual| Ok(actual))
            .or_recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[
                    TokenKind::Dash,
                    TokenKind::LParen,
                    TokenKind::RParen,
                    TokenKind::Terminator,
                ]) || parser.peek_tok_is_type()
            })
            .finish_with_error_count();
        let ((), error_count) = self
            .expect_peek_is_(TokenKind::Dash)
            .recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[
                    TokenKind::LParen,
                    TokenKind::RParen,
                    TokenKind::Terminator,
                ]) || parser.peek_tok_is_type()
            })
            .limit_errors(error_count)
            .finish_with_error_count();
        let (dtype, error_count) = self
            .expect_peek_is_type_()
            .ok_then(|parser, actual| parser.parse_data_type(actual))
            .or_recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[
                    TokenKind::LParen,
                    TokenKind::RParen,
                    TokenKind::Terminator,
                ])
            })
            .limit_errors(error_count)
            .finish_with_error_count();
        let params = self.parse_fn_signature_params();
        self.expect_peek_is_(TokenKind::Terminator)
            .recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[TokenKind::Identifier, TokenKind::RBrace])
                    || parser.curr_tok_is(TokenKind::Terminator)
            })
            .limit_errors(error_count)
            .finish();
        FnSignature::new(
            id,
            dtype,
            params,
            (id.offset.start, self.curr_tok().offset.end),
        )
    }

    /// starts with [TokenKind::LParen] in peek
    /// ends with [TokenKind::RParen] in current
    /// ends with [TokenKind::Terminator] in peek
    fn parse_fn_signature_params(&mut self) -> Vec<DataType> {
        let ((), mut error_count) = self
            .expect_peek_is_(TokenKind::LParen)
            .recover()
            .skip_until_peek_is_in(&[TokenKind::RParen, TokenKind::Terminator])
            .finish_with_error_count();
        let mut params = vec![];
        while !self.peek_tok_is(TokenKind::RParen) {
            let (param, ec) = self
                .expect_peek_is_type_()
                .ok_then(|parser, actual| parser.parse_data_type(actual))
                .or_recover()
                .skip_one_if(|parser| {
                    !parser.peek_tok_is_in(&[TokenKind::Comma, TokenKind::RParen])
                })
                .limit_errors(error_count)
                .finish_with_error_count();
            error_count = ec;
            params.push(param);
            self.allow_hanging_comma(TokenKind::RParen);
        }
        self.expect_peek_is_(TokenKind::RParen)
            .recover()
            .skip_until(|parser| parser.peek_tok_is(TokenKind::Terminator))
            .limit_errors(error_count)
            .finish();
        params
    }

    /// starts with [TokenKind::LBrace] in current
    /// ends with [TokenKind::RBrace] in current
    fn parse_body(&mut self, body_type: BodyType) -> Body {
        let start = self.curr_tok();
        let statements = if self.peek_tok_is(TokenKind::RBrace) {
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
            vec![]
        } else {
            let mut stmts = vec![];
            while !self.peek_tok_is(TokenKind::RBrace) {
                stmts.push(self.parse_body_statement(body_type));
            }
            stmts
        };
        self.expect_peek_is_(TokenKind::RBrace).ignore().finish();
        Body::new(statements, (start.offset.start, self.curr_tok().offset.end))
    }

    /// starts with [TokenKind::LBrace] in current
    /// ends with [TokenKind::RBrace] or [TokenKind::Terminator] in current
    fn parse_body_statement(&mut self, body_type: BodyType) -> Statement {
        let expected = match body_type {
            BodyType::For => self
                .body_parse_fns
                .iter()
                .filter(|&v| !&[TokenKind::Bweak, TokenKind::Continue].contains(&v.0))
                .map(|(&tokens, _)| tokens)
                .collect::<Vec<_>>(),
            _ => self
                .body_parse_fns
                .iter()
                .map(|(&tokens, _)| tokens)
                .collect::<Vec<_>>(),
        };
        self.peek_is_in_(&expected)
            .ok_then(|parser, actual| {
                let body_parser = match body_type {
                    BodyType::For => parser
                        .body_parse_fns
                        .iter()
                        .filter(|&v| !&[TokenKind::Bweak, TokenKind::Continue].contains(&v.0))
                        .map(|(&a, &b)| (a, b))
                        .collect::<HashMap<_, _>>(),
                    _ => parser
                        .body_parse_fns
                        .iter()
                        .map(|(&tok, &parser)| (tok, parser))
                        .collect::<HashMap<_, _>>(),
                };
                match body_parser.get(&actual.kind) {
                    Some(stmt_parser) => Ok(stmt_parser(parser)),
                    None => Err(()),
                }
            })
            .or_recover()
            .skip_until(|parser| {
                parser.curr_tok_is(TokenKind::Terminator) || parser.peek_tok_is_in(&expected)
            })
            .error_header(MissingParserErrorHeader::Body.header())
            .finish()
    }

    // }}}

    // DATA TYPE PARSERS {{{

    // TODO: cannot parse optional and mutable as part of the data type
    // - add a new data type struct with fields
    //  - kind: DataTypeKind (which will be the old data type enum)
    //  - mutable: bool
    //  - optional: bool

    /// starts with [DataType]'s first token in current
    /// ends with [DataType]'s last token in current
    /// end token may be:
    /// - [DataType] token literal (`chan`, `kun`, `senpai`...)
    /// - [TokenKind::RBracket] (`chan[1]`, `kun[3]` ...)
    /// - [TokenKind::RBrace] (`chan{}`, `kun{senpai}` ...)
    fn parse_data_type(&mut self, tok: Token) -> Result<DataType, ()> {
        match self.peek_tok().kind {
            TokenKind::LBracket => Ok(self.parse_vector_type(Vectorable::Token(tok))),
            TokenKind::LBrace => {
                self.advance(2);
                match self.curr_tok().kind {
                    TokenKind::RBrace => match self.peek_tok().kind {
                        TokenKind::LBracket => Ok(self.parse_vector_type(Vectorable::Set(
                            SetType::new(tok, (tok.offset.start, tok.offset.end)),
                        ))),
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
                        let inner =
                            Box::new(self.parse_data_type(self.curr_tok()).unwrap_or_default());
                        let offset_end = self
                            .expect_peek_is_(TokenKind::RBrace)
                            .ok_then(|_, actual| Ok(actual))
                            .or_recover()
                            .skip_one_if(|parser| {
                                parser.peek_tok_nth(1).kind == TokenKind::LBracket
                            })
                            .finish()
                            .offset
                            .end;
                        let res = MapType::new(tok, inner, (tok.offset.start, offset_end));
                        match self.peek_tok().kind {
                            TokenKind::LBracket => Ok(self.parse_vector_type(Vectorable::Map(res))),
                            _ => Ok(DataType::Map(res)),
                        }
                    }
                    _ => {
                        self.unexpected_token_error(
                            Some("INVALID INNER DATA TYPE"),
                            Some("Only data types are allowed to be inner types"),
                            self.curr_tok(),
                            TokenKind::data_types(),
                        );
                        self.advance_if(1, |parser| parser.peek_tok_is(TokenKind::RBrace));
                        Err(())
                    }
                }
            }
            _ => Ok(DataType::Token(tok)),
        }
        // TODO: put mutable + optional parsing here
    }

    /// starts with [TokenKind::LBracket] in peek
    /// ends with [TokenKind::RBracket] in current
    /// ends with other closing delims in current if recovered from error
    fn parse_vector_type(&mut self, id: Vectorable) -> DataType {
        self.advance(1);
        let dim = self
            .expect_peek_is_(TokenKind::IntLiteral)
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish();
        let offset_end = self
            .expect_peek_is_(TokenKind::RBracket)
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish()
            .offset
            .end;
        let offset = (id.offset().start, offset_end);
        DataType::Vec(VecType::new(id, dim, offset))
    }

    // }}}

    // STATEMENT PARSERS {{{
    /// Just calls [Parser::parse_declaration] and maps the [Declaration] to a
    /// `Result<Statement, ()>`
    ///
    /// starts with [TokenKind::Hi] in peek
    /// ends with [TokenKind::Terminator] in current
    fn parse_declaration_statement(&mut self) -> Statement {
        Statement::Declaration(self.parse_declaration())
    }

    /// starts with [TokenKind::Hi] in peek
    /// ends with [TokenKind::Terminator] in current
    fn parse_declaration(&mut self) -> Declaration {
        let (start, error_count) = self
            .expect_peek_is_(TokenKind::Hi)
            .ok_then(|_, actual| Ok(actual))
            .or_recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[
                    TokenKind::Identifier,
                    TokenKind::Dash,
                    TokenKind::Terminator,
                ]) || parser.peek_tok_is_type()
                    || parser.peek_tok_is_assign()
            })
            .finish_with_error_count();
        let (id, error_count) = self
            .expect_peek_is_(TokenKind::Identifier)
            .ok_then(|_, actual| Ok(actual))
            .or_recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[TokenKind::Dash, TokenKind::Terminator])
                    || parser.peek_tok_is_type()
                    || parser.peek_tok_is_assign()
            })
            .limit_errors(error_count)
            .finish_with_error_count();

        let (dtype, error_count) = if self.peek_tok_is(TokenKind::Dash) {
            self.advance(1);
            let (dtype, error_count) = self
                .expect_peek_is_type_()
                .ok_then(|parser, actual| parser.parse_data_type(actual))
                .or_recover()
                .skip_until(|parser| {
                    parser.peek_tok_is(TokenKind::Terminator)
                        || parser.peek_tok_is_assign()
                        || parser.curr_tok_is_in(&[TokenKind::RBracket, TokenKind::RBrace])
                        || parser.curr_tok_is_type()
                })
                .limit_errors(error_count)
                .finish_with_error_count();
            (Some(dtype), error_count)
        } else {
            (None, error_count)
        };

        let mutable = self
            .peek_tok_is(TokenKind::Bang)
            .then(|| self.advance(1))
            .is_some();
        let optional = self
            .peek_tok_is(TokenKind::Question)
            .then(|| self.advance(1))
            .is_some();

        self.expect_peek_is_assign_op_()
            .ignore()
            .limit_errors(error_count)
            .finish_with_error_count();

        let expr = self
            .parse_expression(Precedence::Lowest)
            .unwrap_or_default();

        self.expect_peek_is_(TokenKind::Terminator)
            .ignore()
            .finish();

        Declaration::new(
            id,
            dtype,
            mutable,
            optional,
            expr,
            (start.offset.start, self.curr_tok().offset.end),
        )
    }

    /// starts with the ending token of an [Assignable] in current:
    /// - [TokenKind::Identifier]
    /// - [TokenKind::RBracket]
    /// - [TokenKind::RParen]
    /// ends with [TokenKind::Terminator] in current
    fn parse_assignment(&mut self, id: Assignable) -> Assignment {
        let offset_start = id.offset().start;
        let assign_op = self
            .expect_peek_is_assign_op_()
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish();
        let expr = self
            .parse_expression(Precedence::Lowest)
            .unwrap_or_default();
        self.expect_peek_is_(TokenKind::Terminator)
            .ignore()
            .finish();
        Assignment::new(
            id,
            assign_op,
            expr,
            (offset_start, self.curr_tok().offset.end),
        )
    }

    /// starts with [TokenKind::Wetuwn] in peek
    /// ends with [TokenKind::Terminator] in current
    fn parse_return(&mut self) -> Statement {
        let start = self
            .expect_peek_is_(TokenKind::Wetuwn)
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish();
        let expr = self
            .parse_expression(Precedence::Lowest)
            .unwrap_or_default();
        self.expect_peek_is_(TokenKind::Terminator)
            .ignore()
            .finish();
        Statement::Return(ReturnStatement::new(
            expr,
            (start.offset.start, self.curr_tok().offset.end),
        ))
    }

    /// starts with [TokenKind::Iwf] in peek
    /// ends with [TokenKind::RBrace] in current
    fn parse_if(&mut self) -> Statement {
        let start = self
            .expect_peek_is_(TokenKind::Iwf)
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish();
        let condition = self
            .parse_expression(Precedence::Lowest)
            .unwrap_or_default();
        let (body, error_count) = self
            .expect_peek_is_(TokenKind::LBrace)
            .ok_then(|parser, _| Ok(parser.parse_body(BodyType::If)))
            .or_recover()
            .skip_one_if(|parser| parser.peek_tok_is(TokenKind::RBrace))
            .finish_with_error_count();
        let elifs = match self.peek_tok().kind {
            TokenKind::Ewif => self.parse_elifs(),
            _ => vec![],
        };
        let else_block = match self.peek_tok().kind {
            TokenKind::Ewse => {
                self.expect_peek_is_(TokenKind::Ewse)
                    .ignore()
                    .limit_errors(error_count)
                    .finish_with_error_count();
                Some(
                    self.expect_peek_is_(TokenKind::LBrace)
                        .ok_then(|parser, _| Ok(parser.parse_body(BodyType::If)))
                        .ignore_err()
                        .finish(),
                )
            }
            _ => None,
        };
        Statement::If(IfStatement::new(
            condition,
            body,
            elifs,
            else_block,
            (start.offset.start, self.curr_tok().offset.end),
        ))
    }

    /// starts with [TokenKind::Ewif] in peek
    /// ends with [TokenKind::RBrace] in current
    fn parse_elifs(&mut self) -> Vec<ElifStatement> {
        let mut res: Vec<ElifStatement> = vec![];
        let mut error_count = 0;
        while self.peek_tok_is(TokenKind::Ewif) {
            let (start, ec) = self
                .expect_peek_is_(TokenKind::Ewif)
                .ok_then(|_, actual| Ok(actual))
                .ignore_err()
                .limit_errors(error_count)
                .finish_with_error_count();
            error_count = ec;

            let condition = self
                .parse_expression(Precedence::Lowest)
                .unwrap_or_default();

            let ((), ec) = self
                .expect_peek_is_(TokenKind::LBrace)
                .ignore()
                .limit_errors(error_count)
                .finish_with_error_count();
            error_count = ec;

            let body = self.parse_body(BodyType::If);
            res.push(ElifStatement::new(
                condition,
                body,
                (start.offset.start, self.curr_tok().offset.end),
            ))
        }
        res
    }

    /// Abstracts over [Parser::parse_for_loop] and [Parser::parse_for_each]
    /// - for loop: `fow hi i-chan = 0~ i < n~ i+1 { ...body }`
    /// - for each: `fow item in collection { ...body }`
    ///
    /// starts with [TokenKind::Fow] in peek
    /// ends with [TokenKind::RBrace] in current
    fn parse_for(&mut self) -> Statement {
        self.expect_peek_is_(TokenKind::Fow).ignore().finish();
        match self.peek_tok().kind {
            TokenKind::Hi => self.parse_for_loop(),
            TokenKind::Identifier => self.parse_for_each(),
            _ => {
                self.unexpected_token_error(
                    Some("INVALID FOW INITIALZATION"),
                    Some(
                        "Declare a variable (hi i = 1~ i<4~ i+1) or use an iterable (i in [1,2,3])",
                    ),
                    self.peek_tok(),
                    vec![TokenKind::Hi, TokenKind::Identifier],
                );
                Statement::default()
            }
        }
    }

    /// starts with [TokenKind::Fow] in current and [TokenKind::Hi] in peek
    /// ends with [TokenKind::RBrace] in current
    fn parse_for_loop(&mut self) -> Statement {
        let start = self.curr_tok();
        let (init, error_count) = self
            .expect_peek_is_(TokenKind::Hi)
            .ok_then(|parser, _| Ok(parser.parse_declaration()))
            .ignore_err()
            .finish_with_error_count();
        let condition = self
            .parse_expression(Precedence::Lowest)
            .unwrap_or_default();
        let ((), error_count) = self
            .expect_peek_is_(TokenKind::Terminator)
            .ignore()
            .limit_errors(error_count)
            .finish_with_error_count();
        let update = self
            .parse_expression(Precedence::Lowest)
            .unwrap_or_default();
        let body = self
            .expect_peek_is_(TokenKind::LBrace)
            .ok_then(|parser, _| Ok(parser.parse_body(BodyType::For)))
            .or_recover()
            .skip_one_if(|parser| parser.peek_tok_is(TokenKind::RBrace))
            .limit_errors(error_count)
            .finish();
        Statement::ForLoop(ForLoop::new(
            init,
            condition,
            update,
            body,
            (start.offset.start, self.curr_tok().offset.end),
        ))
    }

    /// starts with [TokenKind::Identifier] in peek
    /// ends with [TokenKind::RBrace] in current
    fn parse_for_each(&mut self) -> Statement {
        let start = self.curr_tok();
        let item_id = self
            .expect_peek_is_(TokenKind::Identifier)
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish();
        self.expect_peek_is_(TokenKind::In).ignore().finish();
        let collection = self
            .parse_expression(Precedence::Lowest)
            .unwrap_or_default();
        let body = self
            .expect_peek_is_(TokenKind::LBrace)
            .ok_then(|parser, _| Ok(parser.parse_body(BodyType::For)))
            .or_recover()
            .skip_one_if(|parser| parser.peek_tok_is(TokenKind::RBrace))
            .finish();
        let offset_end = self.curr_tok().offset.end;
        Statement::ForEach(ForEach::new(
            item_id,
            collection,
            body,
            (start.offset.start, offset_end),
        ))
    }

    // TODO: break statement position info does not include Terminator
    /// starts with [TokenKind::Bweak] in peek
    /// ends with [TokenKind::Terminator] in current
    fn parse_break(&mut self) -> Statement {
        let tok = self
            .expect_peek_is_(TokenKind::Bweak)
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish();
        self.expect_peek_is_(TokenKind::Terminator)
            .ignore()
            .finish();
        Statement::Break(tok)
    }

    // TODO: continue statement position info does not include Terminator
    /// starts with [TokenKind::Continue] in peek
    /// ends with [TokenKind::Terminator] in current
    fn parse_continue(&mut self) -> Statement {
        let tok = self
            .expect_peek_is_(TokenKind::Continue)
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish();
        self.expect_peek_is_(TokenKind::Terminator)
            .ignore()
            .finish();
        Statement::Continue(tok)
    }

    /// starts with [TokenKind::Mash] in peek
    /// ends with [TokenKind::RBrace] in current
    fn parse_mash(&mut self) -> Statement {
        let start = self
            .expect_peek_is_(TokenKind::Mash)
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish();
        let expr = self
            .parse_expression(Precedence::Lowest)
            .unwrap_or_default();
        let found_opening = self
            .expect_peek_is_(TokenKind::LBrace)
            .ignore()
            .finish_result()
            .is_ok();
        let cases = if found_opening && self.peek_tok_is(TokenKind::RBrace) {
            self.empty_body_error(
                self.curr_tok(),
                self.peek_tok(),
                vec![TokenKind::Type, TokenKind::Default],
                BodyType::Mash,
                self.source
                    .lines()
                    .nth(self.peek_tok().line(&self.line_starts))
                    .unwrap_or_default(),
            );
            vec![]
        } else {
            let mut cases: Vec<Case> = vec![];
            while self.peek_tok_is_type() {
                cases.push(self.parse_case());
            }
            cases
        };
        let default = match self.peek_tok().kind {
            TokenKind::Default => {
                self.advance(1);
                self.expect_peek_is_(TokenKind::Colon).ignore().finish();
                Some(self.parse_case_body())
            }
            _ => None,
        };
        self.expect_peek_is_(TokenKind::RBrace).ignore().finish();
        Statement::Mash(MashStatement::new(
            expr,
            cases,
            default,
            (start.offset.start, self.curr_tok().offset.end),
        ))
    }

    /// starts with a [DataType]'s first token in peek
    /// ends with a [DataType]'s first token in peek
    fn parse_case(&mut self) -> Case {
        let start = self.peek_tok();
        let case_type = self
            .expect_peek_is_type_()
            .ok_then(|parser, actual| parser.parse_data_type(actual))
            .ignore_err()
            .finish();
        let body = self
            .expect_peek_is_(TokenKind::Colon)
            .ok_then(|parser, _| Ok(parser.parse_case_body()))
            .ignore_err()
            .finish();
        Case::new(
            case_type,
            body,
            (start.offset.start, self.curr_tok().offset.end),
        )
    }

    /// starts with [TokenKind::Colon] in current
    /// ends with any of the ff:
    /// - [DataType]'s first token in peek (another case next)
    /// - [TokenKind::Default] in peek (default case next)
    /// - [TokenKind::RBrace] in peek (end of mash statement)
    fn parse_case_body(&mut self) -> Body {
        let start = self.curr_tok();
        let statements = if self.peek_tok_is_type()
            || self.peek_tok_is_in(&[TokenKind::Default, TokenKind::RBrace])
        {
            self.empty_body_error(
                self.curr_tok(),
                self.peek_tok(),
                self.body_parse_fns
                    .clone()
                    .into_iter()
                    .map(|(token, _)| token)
                    .collect::<Vec<_>>(),
                BodyType::MashCase,
                self.source
                    .lines()
                    .nth(self.peek_tok().line(&self.line_starts))
                    .unwrap_or_default(),
            );
            vec![]
        } else {
            let mut statements: Vec<Statement> = vec![];
            while !self.peek_tok_is_type()
                && !self.peek_tok_is_in(&[TokenKind::Default, TokenKind::RBrace])
            {
                statements.push(self.parse_body_statement(BodyType::Mash));
            }
            statements
        };
        let offset_end = match self.peek_tok().kind {
            kind if kind.is_type() || matches!(kind, TokenKind::Default | TokenKind::RBrace) => {
                self.curr_tok().offset.end
            }
            _ => {
                unreachable!("parse_case_body: did not end at Default, Type, data types, or RBrace")
            }
        };
        Body::new(statements, (start.offset.start, offset_end))
    }

    /// This parses four possible ident statements: assignments,
    /// function/method calls, pipelines, and unused ident expressions
    /// ```
    /// aqua = 1~
    /// aqua.arms[2] = LONG~
    /// aqua.arms[1].punch()~
    /// aqua.scream() | voice_crack()~
    /// aqua~
    /// ```
    /// starts with [TokenKind::Identifier] in peek
    /// ends with [TokenKind::Terminator] in current
    fn parse_ident_statement(&mut self) -> Statement {
        let id = self.parse_ident_expression();
        match self.curr_tok().kind {
            TokenKind::Identifier | TokenKind::RBracket => match self.peek_tok().kind {
                kind if kind.is_assign() => {
                    Statement::Assignment(self.parse_assignment(id.try_into().expect(
                        r#"parse_ident_statement: ended at Ident/RBracket should be assignable"#,
                    )))
                }
                _ => {
                    self.expect_peek_is_(TokenKind::Terminator)
                        .ignore()
                        .finish();
                    Statement::Expression(id)
                }
            },
            TokenKind::RParen => {
                self.expect_peek_is_(TokenKind::Terminator)
                    .ignore()
                    .finish();
                match id {
                    Expression::FnCall(fn_call) => Statement::FnCall(fn_call),
                    Expression::Access(AccessType::Method(method)) => Statement::Method(method),
                    Expression::Pipeline(pipe) => Statement::Pipeline(pipe),
                    _ => unreachable!(
                        r#"parse_ident_statement: returned at RParen is neither FnCall, MethodAccess, or Pipeline"#
                    ),
                }
            }
            _ => unreachable!(
                r#"parse_ident_statement: parse_ident_expression should only return at RParen, Ident, or RBracket, got {}"#,
                self.curr_tok().kind
            ),
        }
    }

    /// starts with [TokenKind::Identifier] in current and [TokenKind::LParen] in peek
    /// ends with [TokenKind::RParen] in current
    fn parse_fn_call(&mut self) -> FnCall {
        let id = self.curr_tok();
        let args = self
            .expect_peek_is_(TokenKind::LParen)
            .ok_then(|parser, _| {
                let mut args: Vec<Expression> = vec![];
                while !parser.peek_tok_is(TokenKind::RParen) {
                    args.push(
                        parser
                            .parse_expression(Precedence::Lowest)
                            .unwrap_or_default(),
                    );
                    parser.allow_hanging_comma(TokenKind::RParen);
                }
                Ok(args)
            })
            .ignore_err()
            .finish();
        self.expect_peek_is_(TokenKind::RParen).ignore().finish();
        FnCall::new(
            id,
            args,
            FnSignature::default(),
            (id.offset.start, self.curr_tok().offset.end),
        )
    }

    /// Abstracts over different expression statement parsers. Each will parse
    /// an expression that may be followed by a pipe, which will lead to a
    /// pipeline statement.
    ///
    /// starts with valid starting token for an [Expression] in peek.
    /// ends with [TokenKind::Terminator] in current
    fn parse_expression_statement(&mut self) -> Statement {
        let expr = self
            .parse_expression(Precedence::Lowest)
            .unwrap_or_default();
        self.expect_peek_is_(TokenKind::Terminator)
            .ignore()
            .finish();
        Statement::Expression(expr)
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
            let indices = self.parse_array_literal().and_then(|a| match a {
                Expression::Array(a) => Ok(a.exprs),
                _ => unreachable!(
                    r#"parse_ident_accessor: parse_array_literal only returns an ArrayLiteral"#,
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
            r#"parse_indexed_expression: converting cannot fail as it was checked beforehand"#,
        );
        self.expect_peek_is(TokenKind::LBracket)
            .and_then(|tok| Ok(tok.pos(&self.line_starts)))?;
        let indices: Vec<Expression> = self.parse_array_literal().and_then(|a| match a {
            Expression::Array(a) => Ok(a.exprs),
            _ => unreachable!(
                r#"parse_indexed_expression: parse_array_literal only returns an ArrayLiteral"#
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
                    PhantomData,
                )))),
                Accessed::IndexedId(res) => match res.id {
                    Indexable::Token(_) => Ok(Expression::Access(AccessType::Field(
                        GroupAccess::new(Box::new(expr), accessed, PhantomData),
                    ))),
                    Indexable::FnCall(_) => Ok(Expression::Access(AccessType::Method(
                        GroupAccess::new(Box::new(expr), accessed, PhantomData),
                    ))),
                    _ => unreachable!(
                        r#"parse_access_expression: indexed result from parse_single_accessed can only be a Token or FnCall"#
                    ),
                },
                Accessed::FnCall(_) => Ok(Expression::Access(AccessType::Method(
                    GroupAccess::new(Box::new(expr), accessed, PhantomData),
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
        let mut rest: Vec<Expression> = vec![];
        while self.peek_tok_is(TokenKind::Pipe) {
            self.expect_peek_is(TokenKind::Pipe)?;
            rest.push(self.parse_expression_unit(Precedence::Lowest)?);
        }
        match &rest.len() {
            0 => Ok(first_value),
            _ => Ok(Expression::Pipeline(Pipeline::new(
                Box::new(first_value),
                rest,
            ))),
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

    /// checks current token's type against given TokenKinds
    fn curr_tok_is_in(&self, token_types: &[TokenKind]) -> bool {
        token_types.contains(&self.curr_tok().kind)
    }

    /// checks peek token's type is a DataType
    fn curr_tok_is_type(&self) -> bool {
        matches!(
            self.curr_tok().kind,
            TokenKind::Type
                | TokenKind::Chan
                | TokenKind::Kun
                | TokenKind::Senpai
                | TokenKind::Kouhai
                | TokenKind::San
                | TokenKind::Sama
                | TokenKind::Dono
        )
    }

    /// checks peek token's type against given TokenKind
    fn peek_tok_is(&self, token_kind: TokenKind) -> bool {
        self.peek_tok().kind == token_kind
    }

    /// checks peek token's type against given TokenKinds
    fn peek_tok_is_in(&self, token_types: &[TokenKind]) -> bool {
        token_types.contains(&self.peek_tok().kind)
    }

    /// checks peek token's type is an assign operator
    fn peek_tok_is_assign(&self) -> bool {
        self.peek_tok().kind.is_assign()
    }
    /// checks peek token's type is a DataType
    fn peek_tok_is_type(&self) -> bool {
        self.peek_tok().kind.is_type()
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
    /// ends with [TokenKind::Comma] in current and passed in closing in peek,
    /// regardless of whether the expect failed or not
    fn allow_hanging_comma(&mut self, closing: TokenKind) {
        Expectation::new(vec![TokenKind::Comma], self.peek_tok(), self, true)
            .ok_then(|parser, _| {
                if parser.peek_tok_is(TokenKind::Comma) {
                    Ok(parser.advance(1))
                } else if parser.peek_tok_is(closing) {
                    Ok(())
                } else {
                    Err(())
                }
            })
            .or_recover()
            .skip_until(|parser| {
                parser.peek_tok_is(closing) || parser.curr_tok_is(TokenKind::Comma)
            })
            .finish();
    }

    // }}}

    // RECOVER EXPECT METHODS {{{
    /// advances cursor 1 time if peek tok is eq to given
    /// adds an error otherwise
    fn expect_peek_is_(&mut self, token_kind: TokenKind) -> Expectation<'src, '_> {
        let ok = self.peek_tok().kind == token_kind;
        let actual = if ok {
            self.advance(1);
            self.curr_tok()
        } else {
            self.peek_tok()
        };
        Expectation::new(vec![token_kind], actual, self, ok)
    }

    /// advances cursor 1 time if peek tok is eq to any of the given
    /// adds an error otherwise
    fn expect_peek_is_in_(&mut self, token_kinds: &[TokenKind]) -> Expectation<'src, '_> {
        let ok = token_kinds.contains(&self.peek_tok().kind);
        let actual = if ok {
            self.advance(1);
            self.curr_tok()
        } else {
            self.peek_tok()
        };
        println!("expect peek is in {:?}, ok {ok}", token_kinds);
        Expectation::new(token_kinds.to_vec(), actual, self, ok)
    }

    /// checks peek token's type is a DataType
    fn expect_peek_is_type_(&mut self) -> Expectation<'src, '_> {
        let expecteds = TokenKind::data_types();
        let ok = expecteds.contains(&self.peek_tok().kind);
        let actual = if ok {
            self.advance(1);
            self.curr_tok()
        } else {
            self.peek_tok()
        };
        Expectation::new(expecteds, actual, self, ok)
    }

    /// advances cursor 1 time if peek tok is a valid assignment operator
    /// adds an error otherwise
    fn expect_peek_is_assign_op_(&mut self) -> Expectation<'src, '_> {
        let expecteds = TokenKind::assign_ops();
        let ok = expecteds.contains(&self.peek_tok().kind);
        let actual = if ok {
            self.advance(1);
            self.curr_tok()
        } else {
            self.peek_tok()
        };
        Expectation::new(expecteds, actual, self, ok)
    }

    // }}}

    // ERROR METHODS {{{

    fn unexpected_token_error(
        &mut self,
        header: Option<&'src str>,
        context: Option<&'src str>,
        actual: Token,
        expected: Vec<TokenKind>,
    ) {
        self.errors
            .push(ParserError::Unexpected(UnexpectedTokenError::new(
                self.source,
                self.line_starts,
                actual,
                expected.to_vec(),
                header,
                context,
            )))
    }

    fn empty_body_error(
        &mut self,
        opening: Token,
        actual: Token,
        expected: Vec<TokenKind>,
        body_type: BodyType,
        line_text: &'src str,
    ) {
        self.errors.push(ParserError::EmptyBody(EmptyBodyError::new(
            opening.buffer_pos(&self.line_starts),
            actual.source_str(self.source),
            actual.buffer_pos(&self.line_starts),
            expected,
            body_type,
            line_text,
        )))
    }

    fn no_parsing_fn_error(
        &mut self,
        error_header: MissingParserErrorHeader,
        actual: Token,
        expected: Vec<TokenKind>,
    ) {
        self.errors
            .push(ParserError::Unexpected(UnexpectedTokenError::new(
                self.source,
                self.line_starts,
                actual,
                expected,
                Some(error_header.header()),
                None,
            )))
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
            Self::Prefix => "NOT A VALID EXPRESSION STARTING TOKEN",
            Self::Infix => "NOT A VALID OPERATOR",
            Self::Body => "NOT A VALID STATEMENT STARTING TOKEN",
        }
    }
}

// ERROR RECOVERY {{{

/// Stores the expected and actual value of an expect operation.
///
/// Expectation allows an action to be done lazily when the expect operation is
/// true. Action when ok can be none (unit `()`) as well. Afterwards, the error
/// can be recovered from by using recovery strategies (ignoring error as well).
#[derive(Debug)]
struct Expectation<'src, 'parser> {
    pub expected: Vec<TokenKind>,
    pub actual: Token,
    pub parser: &'parser mut Parser<'src>,
    ok: bool,
}
impl<'src, 'parser> Expectation<'src, 'parser> {
    pub fn new(
        expected: Vec<TokenKind>,
        actual: Token,
        parser: &'parser mut Parser<'src>,
        ok: bool,
    ) -> Self {
        Self {
            expected,
            actual,
            parser,
            ok,
        }
    }
    /// Adds an action to be done when the expect operation is ok, which can
    /// be chained with `or_recover()` to chain recovery strategies, or ignore the
    /// error.
    /// # Example
    /// ```
    /// // hi aqua-chan = 10~
    /// //         ^^^^
    /// let dtype = self.expect_peek_is_type()
    ///     .ok_then(|parser, actual| parser.parse_data_type(actual))
    ///     .ignore_err();
    ///     .finish();
    /// ```
    pub fn ok_then<O, R>(self, then: O) -> OkThen<'src, 'parser, O, R>
    where
        O: FnMut(&mut Parser, Token) -> Result<R, ()>,
        R: Default,
    {
        OkThen {
            expectation: self,
            ok_action: then,
        }
    }
    /// Do not do an action when the expect operation failed and just do an
    /// action when recovering. This can be chained with recovery strategies
    /// (like ignore error).
    ///
    /// Useful when you want to ignore the token because it does not need to
    /// be parsed
    /// # Example
    /// ```
    /// // hi aqua-chan = 10~
    /// //        ^
    /// self.expect_peek_is(TokenKind::Dash)
    ///     .recover()
    ///     .skip_until_peek_is_type()
    ///     .finish();
    ///
    /// let dtype = self.expect_peek_is_type()
    ///     .ok_then(|parser, actual| parser.parse_data_type(actual))
    ///     .ignore_err();
    ///     .finish();
    /// ```
    pub fn recover(self) -> Recovery<'src, 'parser, (), ()> {
        Recovery::<(), ()> {
            expectation: self,
            ok_action: (),
            rtype: PhantomData,
        }
    }
    /// Do not add a recovery strategy and or an ok action, regardless of the
    /// outcome of the expect operation.
    /// # Example
    /// ```
    /// // hi aqua-chan = 10~
    /// //        ^
    /// self.expect_peek_is(TokenKind::Dash)
    ///     .ignore()
    ///     .finish()
    /// ```
    pub fn ignore(self) -> FinishRecovery<'src, 'parser, (), (), impl FnMut(&mut Parser)> {
        FinishRecovery {
            expectation: self,
            ok_action: (),
            err_action: |_| {},
            rtype: PhantomData,
            error_count: 0,
            header: None,
            context: None,
        }
    }
}

struct OkThen<'src, 'parser, O, R>
where
    O: FnMut(&mut Parser, Token) -> Result<R, ()>,
    R: Default,
{
    expectation: Expectation<'src, 'parser>,
    ok_action: O,
}
impl<'src, 'parser, O, R> OkThen<'src, 'parser, O, R>
where
    O: FnMut(&mut Parser, Token) -> Result<R, ()>,
    R: Default,
{
    /// Add a recovery strategy when the expect operation failed, along with
    /// the ok action already given.
    /// # Example
    /// ```
    /// // hi aqua-chan = 10~
    /// //    ^^^^
    /// self.expect_peek_is(TokenKind::Identifier)
    ///     .ok_then(|parser, actual| Ok(actual))
    ///     .or_recover()
    ///     .skip_until_peek_is(TokenKind::Dash)
    ///     .finish()
    /// ```
    pub fn or_recover(self) -> Recovery<'src, 'parser, O, R> {
        Recovery {
            expectation: self.expectation,
            ok_action: self.ok_action,
            rtype: PhantomData,
        }
    }
    /// Do not add a recovery strategy and ignore the error when the expect
    /// operation failed.
    /// # Example
    /// ```
    /// // hi aqua-chan = 10~
    /// //        ^
    /// self.expect_peek_is(TokenKind::Dash)
    ///     .ok_then(|parser, actual| {
    ///         parser.advance()
    ///         parser.parse_data_type(parser.curr_tok()))
    ///     })
    ///     .ignore_err()
    ///     .finish()
    /// ```
    pub fn ignore_err(self) -> FinishRecovery<'src, 'parser, O, R, impl FnMut(&mut Parser)> {
        FinishRecovery {
            expectation: self.expectation,
            ok_action: self.ok_action,
            err_action: |_| {},
            rtype: PhantomData,
            error_count: 0,
            header: None,
            context: None,
        }
    }
    /// Do the ok action when the expect operation is ok. Otherwise, add an
    /// error without recovering.
    /// # Example
    /// ```
    /// // hi aqua-chan = 10~
    /// //         ^^^^
    /// self.expect_peek_is_type()
    ///     .ok_then(|parser, actual| parser.parse_data_type(actual))
    ///     .must()
    /// ```
    pub fn must(mut self) -> Result<R, ()> {
        if self
            .expectation
            .expected
            .contains(&self.expectation.actual.kind)
        {
            (self.ok_action)(self.expectation.parser, self.expectation.actual)
        } else {
            self.expectation.parser.errors.push(ParserError::Unexpected(
                UnexpectedTokenError::new(
                    self.expectation.parser.source,
                    self.expectation.parser.line_starts,
                    self.expectation.actual,
                    self.expectation.expected,
                    None,
                    None,
                ),
            ));
            Err(())
        }
    }
}

/// Helper trait for extracting the ok action from both a closure and a unit.
/// This is to allow `()` to be passed as an ok action when wanting to ignore
/// the ok case and either: just want to recover, or ignore error aka ignoring
/// both cases.
trait OkAction<R> {
    /// Extract the ok action, whether is is a unit `()` or an actual action
    fn call_ok(self, parser: &mut Parser, token: Token) -> Result<R, ()>;
}
impl<R: Default> OkAction<R> for () {
    fn call_ok(self, _: &mut Parser, _: Token) -> Result<R, ()> {
        Ok(R::default())
    }
}
impl<O, R> OkAction<R> for O
where
    O: FnMut(&mut Parser, Token) -> Result<R, ()>,
    R: Default,
{
    fn call_ok(mut self, parser: &mut Parser, token: Token) -> Result<R, ()> {
        self(parser, token)
    }
}

/// Contains different recovery strategies with limiting local errors.
struct Recovery<'src, 'parser, O, R>
where
    O: OkAction<R>,
    R: Default,
{
    expectation: Expectation<'src, 'parser>,
    ok_action: O,
    rtype: PhantomData<R>,
}
impl<'src, 'parser, O, R> Recovery<'src, 'parser, O, R>
where
    O: OkAction<R>,
    R: Default,
{
    /// Generic skip until a condition is met.
    /// # Example
    /// ```
    /// self.expect_peek_is(TokenKind::LBrace)
    ///     .recover()
    ///     .skip_until(|parser| parser.peek_tok_nth(1).kind == TokenKind::RBrace)
    ///     .finish()
    /// ```
    pub fn skip_until<C>(
        self,
        mut condition: C,
    ) -> FinishRecovery<'src, 'parser, O, R, impl FnMut(&mut Parser)>
    where
        C: FnMut(&mut Parser) -> bool,
    {
        FinishRecovery {
            expectation: self.expectation,
            ok_action: self.ok_action,
            err_action: move |parser| {
                while !condition(parser) {
                    parser.advance(1)
                }
            },
            rtype: PhantomData,
            error_count: 0,
            header: None,
            context: None,
        }
    }
    /// Generic skip one if a condition is met.
    /// # Example
    /// ```
    /// self.expect_peek_is_type()
    ///     .recover()
    ///     .skip_one_if(|parser| parser.peek_tok_is(TokenKind::Terminator))
    ///     .finish()
    /// ```
    pub fn skip_one_if<C>(
        self,
        mut condition: C,
    ) -> FinishRecovery<'src, 'parser, O, R, impl FnMut(&mut Parser)>
    where
        C: FnMut(&mut Parser) -> bool,
    {
        FinishRecovery {
            expectation: self.expectation,
            ok_action: self.ok_action,
            err_action: move |parser| {
                if condition(parser) {
                    parser.advance(1)
                }
            },
            rtype: PhantomData,
            error_count: 0,
            header: None,
            context: None,
        }
    }
    /// Generic skip while the condition is being met.
    /// # Example
    /// ```
    /// self.expect_peek_is(TokenKind::RParen)
    ///     .recover()
    ///     .skip_while(|parser| parser.curr_tok_is(TokenKind::Comma))
    ///     .finish()
    /// ```
    #[inline(always)]
    pub fn skip_while<C>(
        self,
        mut condition: C,
    ) -> FinishRecovery<'src, 'parser, O, R, impl FnMut(&mut Parser)>
    where
        C: FnMut(&mut Parser) -> bool,
    {
        self.skip_until(move |parser| !condition(parser))
    }
    /// Skip until the peek token is the one of the given token kinds
    /// # Example
    /// ```
    /// // (aqua-chan, shion-chan, ojou-chan,,,) {
    /// //                                   ^
    /// self.expect_peek_is(TokenKind::RParen)
    ///     .recover()
    ///     .skip_until_peek_is_in(&[TokenKind::RParen, TokenKind::LBrace])
    ///     .finish()
    /// ```
    pub fn skip_until_peek_is_in(
        self,
        kinds: &'src [TokenKind],
    ) -> FinishRecovery<'src, 'parser, O, R, impl FnMut(&mut Parser) + use<'src, O, R>> {
        self.skip_until(|parser| kinds.contains(&parser.peek_tok().kind))
    }
    /// Skip until the peek token is the given token kind
    /// # Example
    /// ```
    /// // (aqua-chan, shion-chan, ojou-chan,,,) {
    /// //                                   ^
    /// self.expect_peek_is(TokenKind::RParen)
    ///     .recover()
    ///     .skip_until_peek_is(TokenKind::RParen)
    ///     .finish()
    /// ```
    pub fn skip_until_peek_is(
        self,
        kind: TokenKind,
    ) -> FinishRecovery<'src, 'parser, O, R, impl FnMut(&mut Parser)> {
        self.skip_until(move |parser| parser.peek_tok().kind == kind)
    }
    /// Skip until the peek token is a type
    /// # Example
    /// ```
    /// // hi aqua-chan = 10~
    /// //        ^
    /// self.expect_peek_is(TokenKind::Dash)
    ///     .recover()
    ///     .skip_until_peek_is_type()
    ///     .finish()
    /// ```
    pub fn skip_until_peek_is_type(
        self,
    ) -> FinishRecovery<'src, 'parser, O, R, impl FnMut(&mut Parser)> {
        self.skip_until(|parser| {
            matches!(
                parser.peek_tok().kind,
                TokenKind::Type
                    | TokenKind::Chan
                    | TokenKind::Kun
                    | TokenKind::Senpai
                    | TokenKind::Kouhai
                    | TokenKind::San
                    | TokenKind::Sama
                    | TokenKind::Dono
            )
        })
    }
}

struct FinishRecovery<'src, 'parser, O, R, E>
where
    O: OkAction<R>,
    R: Default,
    E: FnMut(&mut Parser),
{
    expectation: Expectation<'src, 'parser>,
    ok_action: O,
    err_action: E,
    rtype: PhantomData<R>,
    error_count: usize,
    header: Option<&'src str>,
    context: Option<&'src str>,
}
impl<'src, 'parser, O, R, E> FinishRecovery<'src, 'parser, O, R, E>
where
    O: OkAction<R>,
    R: Default,
    E: FnMut(&mut Parser),
{
    /// Returns the ok variant of the result of the ok action when the exepct
    /// operation is ok, given any. If no ok action was given, a unit type will
    /// be returned.
    /// # Example
    /// ```
    /// // hi aqua-chan = 10~
    /// //         ^^^^
    /// let dtype = self.expect_peek_is_type()
    ///     .ok_action(|parser, actual| parser.parse_data_type(actual))
    ///     .or_recover()
    ///     .skip_until_peek_is(TokenKind::Assign)
    ///     .finish();
    ///
    /// // hi aqua-chan = 10~
    /// //              ^
    /// let unit: () = self.expect_peek_is(TokenKind::Assign)
    ///     .recover()
    ///     .skip_until_peek_is(TokenKind::Terminator)
    ///     .finish();
    /// ```
    pub fn finish(self) -> R {
        self.finish_result().unwrap_or_default()
    }
    /// Just like [FinishRecovery::finish] but with the error count. Error
    /// count is incremented when an error occured, otherwise it stays the same.
    /// # Example
    /// ```
    /// let error_count = 0;
    ///
    /// // hi aqua-chan = 10~
    /// //         ^^^^
    /// let (dtype, error_count): (DataType, usize) = self
    ///     .expect_peek_is_type()
    ///     .ok_action(|parser, actual| parser.parse_data_type(actual))
    ///     .or_recover()
    ///     .skip_until_peek_is(TokenKind::Assign)
    ///     .finish_with_error_count(error_count);
    ///
    /// // hi aqua-chan = 10~
    /// //              ^
    /// let (unit, error_count): ((), usize) = self
    ///     .expect_peek_is(TokenKind::Assign)
    ///     .recover()
    ///     .skip_until_peek_is(TokenKind::Terminator)
    ///     .finish_with_error_count(error_count);
    /// ```
    pub fn finish_with_error_count(self) -> (R, usize) {
        match self.finish_result_with_error_count() {
            (Ok(res), ec) => (res, ec),
            (Err(()), ec) => (R::default(), ec),
        }
    }
    /// Just like [FinishRecovery::finish] but with as a Result.
    /// # Example
    /// ```
    /// // hi aqua-chan = 10~
    /// //         ^^^^
    /// let dtype = self.expect_peek_is_type()
    ///     .ok_action(|parser, actual| parser.parse_data_type(actual))
    ///     .or_recover()
    ///     .skip_until_peek_is(TokenKind::Assign)
    ///     .finish_as_result()
    ///     .unwrap_or_else(|_| {
    ///         println!("Your logging message or something");
    ///         DataType::default()
    ///     });
    ///
    /// // hi aqua-chan = 10~
    /// //              ^
    /// let unit: () = self.expect_peek_is(TokenKind::Assign)
    ///     .recover()
    ///     .skip_until_peek_is(TokenKind::Terminator)
    ///     .finish_as_result()
    ///     .expect("Your panic message or something");
    /// ```
    pub fn finish_result(mut self) -> Result<R, ()> {
        if self.expectation.ok {
            self.ok_action
                .call_ok(self.expectation.parser, self.expectation.actual)
                .map_err(|_| (self.err_action)(self.expectation.parser))
        } else {
            (self.err_action)(self.expectation.parser);
            self.expectation.parser.errors.push(ParserError::Unexpected(
                UnexpectedTokenError::new(
                    self.expectation.parser.source,
                    self.expectation.parser.line_starts,
                    self.expectation.actual,
                    self.expectation.expected,
                    self.header,
                    self.context,
                ),
            ));
            Err(())
        }
    }
    /// Just like [FinishRecovery::finish_result] but with the error count. Error
    /// count is incremented when an error occured, otherwise it stays the same.
    ///
    /// This is useful for limiting local errors from ballooning.
    /// # Example
    /// ```
    /// let error_count = 0;
    ///
    /// // hi aqua-chan = 10~
    /// //         ^^^^
    /// let (dtype, error_count): (Result<DataType, ()>, usize) = self
    ///     .expect_peek_is_type()
    ///     .ok_action(|parser, actual| parser.parse_data_type(actual))
    ///     .or_recover()
    ///     .skip_until_peek_is(TokenKind::Assign)
    ///     .finish_with_error_count();
    ///
    /// // hi aqua-chan = 10~
    /// //              ^
    /// let (unit_result, error_count): (Result<(), ()>, usize) = self
    ///     .expect_peek_is(TokenKind::Assign)
    ///     .recover()
    ///     .skip_until_peek_is(TokenKind::Terminator)
    ///     .finish_with_error_count();
    /// ```
    pub fn finish_result_with_error_count(self) -> (Result<R, ()>, usize) {
        let error_count = self.error_count;
        match self.finish_result() {
            Ok(res) => (Ok(res), error_count),
            Err(()) => (Err(()), error_count + 1),
        }
    }
    /// Allows to limit local errors by passing in an error count. The error
    /// count is assumed to be accumulated by the consumer outside this struct.
    ///
    /// This is useful for limiting local errors from ballooning.
    /// # Example
    /// ```
    /// // hi aqua-chan = 10~
    /// //        ^
    /// let error_count = 0;
    /// let MAX_ERROR_COUNT = 3;
    ///
    /// let error_count = self.expect_peek_is(TokenKind::Dash)
    ///     .recover()
    ///     .skip_until_peek_is_type()
    ///     .limit_errors(error_count)
    ///     .finish_with_error_count();
    ///
    /// let error_count = self.expect_peek_is_type()
    ///     .recover()
    ///     .skip_until_peek_is_assign()
    ///     .limit_errors(error_count)
    ///     .finish_with_error_count();
    /// ```
    pub fn limit_errors(mut self, error_count: usize) -> Self {
        if error_count > MAX_LOCAL_ERROR_COUNT {
            self.expectation.ok = true
        }
        self
    }
    /// Adds an error header to the error message when the expect operation
    /// failed.
    /// ```
    /// // hi aqua-chan = 10~
    /// //    ^^^^
    /// let id = self
    ///     .expect_peek_is(TokenKind::Identifier)
    ///     .ok_then(|_, actual| Ok(actual))
    ///     .or_recover()
    ///     .skip_until_peek_is(TokenKind::Dash)
    ///     .error_header("MISSING FUNCTION NAME")
    ///     .finish();
    /// ```
    pub fn error_header(mut self, header: &'src str) -> Self {
        self.header = Some(header);
        self
    }
    /// Adds an error context to the error message when the expect operation
    /// failed.
    /// ```
    /// // hi aqua-chan = 10~
    /// //    ^^^^
    /// let id = self
    ///     .expect_peek_is(TokenKind::Identifier)
    ///     .ok_then(|_, actual| Ok(actual))
    ///     .or_recover()
    ///     .skip_until_peek_is(TokenKind::Dash)
    ///     .error_header("MISSING DECLARATION NAME")
    ///     .error_context("Bruh, you need to name your variables...")
    ///     .finish();
    /// ```
    pub fn error_context(mut self, header: &'src str) -> Self {
        self.context = Some(header);
        self
    }
}

// }}}
