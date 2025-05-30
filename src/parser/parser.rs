use std::{collections::HashMap, fmt::Debug, marker::PhantomData};

use crate::{
    errors::parse_errors::{
        BodyType, EmptyBodyError, NoMainError, ParserError, UnexpectedTokenError,
    },
    lexer::token::{Position, Token, TokenKind},
    parser::productions::{
        AccessType, Accessed, ArrayLiteral, Assignable, Assignment, Body, Case, Contract, DataType,
        DataTypeUnit, Declaration, ElifStatement, Expression, FnCall, FnSignature, ForEach,
        ForLoop, Function, Group, GroupAccess, GroupField, GroupInit, GroupMethod,
        GroupedExpression, IfStatement, Indexable, IndexedId, InfixExpression, MapLiteral, MapType,
        MashStatement, Param, ParamUnit, Pipeline, PrefixExpression, Program, ReturnStatement,
        SetLiteral, SetType, Statement, VariadicParam, VecType, Vectorable,
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
    prefix_parse_fns: HashMap<TokenKind, fn(&mut Self) -> Expression>,
    infix_parse_fns: HashMap<TokenKind, fn(&mut Self, Expression) -> Expression>,
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
            (TokenKind::LBracket, Self::parse_array_literal_expression),
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

    pub fn parse_program(&mut self) {
        let mut main: Option<Function> = None;
        let mut functions: Vec<Function> = vec![];
        let mut groups: Vec<Group> = vec![];
        let mut methods: Vec<GroupMethod> = vec![];
        let mut contracts: Vec<Contract> = vec![];
        let mut globals: Vec<Declaration> = vec![];
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
                TokenKind::Hi => globals.push(self.parse_declaration()),
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
                    self.advance_until(|parser| {
                        parser.peek_tok_is_in(&[
                            TokenKind::Fun,
                            TokenKind::Group,
                            TokenKind::Contract,
                            TokenKind::Hi,
                        ])
                    });
                }
            }
            if self.peek_tok_is(TokenKind::EOF) {
                end = self.curr_tok().pos_end(&self.line_starts);
            }
            self.advance(1);
        }
        match main {
            Some(_) => (),
            None => self.errors.push(ParserError::NoMain(NoMainError::new(
                self.source.lines().nth(end.0).unwrap_or_default(),
                (end.0 + 1, end.1 + 1),
            ))),
        }
        self.program = Program::new(
            main,
            functions,
            groups,
            methods,
            contracts,
            globals,
            self.source,
        );
    }

    /// starts with [TokenKind::Fun] in current
    /// ends with [TokenKind::RBrace] in current
    fn parse_method(&mut self) -> GroupMethod {
        let start = self.curr_tok();
        let group = self
            .expect_peek_is_type()
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
            .expect_peek_is_in(&[TokenKind::Identifier, TokenKind::Main])
            .ok_then(|_, actual| Ok(actual))
            .or_recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[TokenKind::Dash, TokenKind::LParen, TokenKind::LBrace])
                    || parser.peek_tok_is_type()
            })
            .error_header("MISSING FUNCTION NAME")
            .finish_with_error_count();

        let ((), error_count) = self
            .expect_peek_is(TokenKind::Dash)
            .recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[TokenKind::LParen, TokenKind::LBrace])
                    || parser.peek_tok_is_type()
            })
            .limit_errors(error_count)
            .error_context("Need a dash before declaring the type.")
            .finish_with_error_count();

        let (dtype, error_count) = self
            .expect_peek_is_type()
            .ok_then(|parser, _| parser.parse_data_type())
            .or_recover()
            .skip_until(|parser| {
                // follow set
                parser.peek_tok_is_in(&[TokenKind::LParen, TokenKind::LBrace])
                // end of dtypes
                || parser.curr_tok_is_in(&[TokenKind::RBracket, TokenKind::RBrace, TokenKind::Question])
                || parser.curr_tok_is_type()
            })
            .limit_errors(error_count)
            .error_header("INVALID TYPE")
            .error_context("User defined types must start with a capital letter.")
            .finish_with_error_count();

        let params = self
            .expect_peek_is(TokenKind::LParen)
            .ok_then(|parser, _| Ok(parser.parse_params()))
            .or_recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[TokenKind::LBrace]) || parser.curr_tok_is(TokenKind::RParen)
            })
            .limit_errors(error_count)
            .error_header("MISSING OPENING ( FOR PARAMETERS")
            .finish();

        let body = self
            .expect_peek_is(TokenKind::LBrace)
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
            match param {
                Param::Unit(_) => (),
                Param::Variadic(_) => self
                    .safe_expect_peek_is(TokenKind::RParen)
                    .recover()
                    .skip_until_peek_is(TokenKind::RParen)
                    .error_header("VARIADIC PARAMETER NOT LAST")
                    .error_context("Variadic parameter should be the last parameter.")
                    .finish(),
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
            .expect_peek_is(TokenKind::Identifier)
            .ok_then(|_, actual| Ok(actual))
            .or_recover()
            .skip_one_if(|parser| {
                !parser.peek_tok_is(TokenKind::Dash) && parser.peek_tok_nth(1).kind.is_type()
            })
            .finish_with_error_count();
        let ((), error_count) = self
            .expect_peek_is(TokenKind::Dash)
            .recover()
            .skip_one_if(|parser| {
                !parser.peek_tok_is_type() && parser.peek_tok_nth(1).kind == TokenKind::Ellipsis
            })
            .limit_errors(error_count)
            .finish_with_error_count();
        let dtype = self
            .expect_peek_is_type()
            .ok_then(|parser, _| parser.parse_data_type())
            .or_recover()
            .skip_until(|parser| {
                // follow set
                parser.peek_tok_is_in(&[TokenKind::Ellipsis, TokenKind::Comma, TokenKind::RParen])
                // end of dtype
                || parser.curr_tok_is_in(&[
                    TokenKind::RBracket,
                    TokenKind::RBrace,
                    TokenKind::Question,
                ])
                || parser.curr_tok_is_type()
            })
            .limit_errors(error_count)
            .finish();
        if self.peek_tok_is(TokenKind::Ellipsis) {
            let param_offset_end = self.curr_tok().offset.end;
            self.advance(1);
            let optional = self
                .peek_tok_is(TokenKind::Question)
                .then(|| self.advance(1))
                .is_some();
            Param::Variadic(VariadicParam::new(
                id,
                dtype,
                optional,
                (id.offset.start, param_offset_end),
                (id.offset.start, self.curr_tok().offset.end),
            ))
        } else {
            Param::Unit(ParamUnit::new(
                id,
                dtype,
                (id.offset.start, self.curr_tok().offset.end),
            ))
        }
    }

    /// starts with [TokenKind::Group] in current
    /// ends with [TokenKind::RBrace] in current
    fn parse_group(&mut self) -> Group {
        let start = self.curr_tok();
        let id = self
            .expect_peek_is(TokenKind::Type)
            .ok_then(|_, actual| Ok(actual))
            .or_recover()
            .skip_until_peek_is(TokenKind::LBrace)
            .finish();
        let found_opening = self
            .expect_peek_is(TokenKind::LBrace)
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
        self.expect_peek_is(TokenKind::RBrace)
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
            .expect_peek_is(TokenKind::Identifier)
            .ok_then(|_, actual| Ok(actual))
            .or_recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[TokenKind::Dash, TokenKind::Terminator])
                    || parser.peek_tok_is_type()
            })
            .finish();
        self.expect_peek_is(TokenKind::Dash)
            .recover()
            .skip_until(|parser| {
                parser.peek_tok_is(TokenKind::Terminator) || parser.peek_tok_is_type()
            })
            .finish();
        let dtype = self
            .expect_peek_is_type()
            .ok_then(|parser, _| parser.parse_data_type())
            .or_recover()
            .skip_until(|parser| {
                // follow set
                parser.peek_tok_is_in(&[
                    TokenKind::Terminator,
                    TokenKind::Identifier,
                    TokenKind::RBrace,
                ])
                // end of dtype
                || parser.curr_tok_is_in(&[
                    TokenKind::RBracket,
                    TokenKind::RBrace,
                    TokenKind::Question,
                ])
                || parser.curr_tok_is_type()
            })
            .finish();
        self.expect_peek_is(TokenKind::Terminator)
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
            .expect_peek_is(TokenKind::Type)
            .ok_then(|_, actual| Ok(actual))
            .or_recover()
            .skip_until_peek_is(TokenKind::LBrace)
            .finish();
        let found_opening = self
            .expect_peek_is(TokenKind::LBrace)
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
        self.expect_peek_is(TokenKind::RBrace)
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
            .expect_peek_is(TokenKind::Identifier)
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
            .expect_peek_is(TokenKind::Dash)
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
            .expect_peek_is_type()
            .ok_then(|parser, _| parser.parse_data_type())
            .or_recover()
            .skip_until(|parser| {
                // follow set
                parser.peek_tok_is_in(&[
                    TokenKind::LParen,
                    TokenKind::RParen,
                    TokenKind::Terminator,
                ])
                // end of dtype
                || parser.curr_tok_is_in(&[
                    TokenKind::RBracket,
                    TokenKind::RBrace,
                    TokenKind::Question,
                ])
                || parser.curr_tok_is_type()
            })
            .limit_errors(error_count)
            .finish_with_error_count();
        let params = self.parse_fn_signature_params();
        self.expect_peek_is(TokenKind::Terminator)
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
            .expect_peek_is(TokenKind::LParen)
            .recover()
            .skip_until_peek_is_in(&[TokenKind::RParen, TokenKind::Terminator])
            .finish_with_error_count();
        let mut params = vec![];
        while !self.peek_tok_is(TokenKind::RParen) {
            let (param, ec) = self
                .expect_peek_is_type()
                .ok_then(|parser, _| parser.parse_data_type())
                .or_recover()
                .skip_until(|parser| {
                    // follow set
                    parser.peek_tok_is_in(&[TokenKind::Comma, TokenKind::RParen])
                    // end of dtypes
                    || parser.curr_tok_is_in(&[TokenKind::RBracket, TokenKind::RBrace, TokenKind::Question])
                    || parser.curr_tok_is_type()
                })
                .limit_errors(error_count)
                .finish_with_error_count();
            error_count = ec;
            params.push(param);
            self.allow_hanging_comma(TokenKind::RParen);
        }
        self.expect_peek_is(TokenKind::RParen)
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
        self.expect_peek_is(TokenKind::RBrace).ignore().finish();
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
        self.safe_expect_peek_is_in(&expected)
            .ok_then(|parser, actual| {
                let body_parser = match body_type {
                    BodyType::For => parser
                        .body_parse_fns
                        .iter()
                        .filter(|&v| !&[TokenKind::Bweak, TokenKind::Continue].contains(&v.0))
                        .map(|(&tok, &parser)| (tok, parser))
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

    /// starts with [DataType]'s first token in current
    /// ends with [DataType]'s last token in current
    /// end token may be:
    /// - [DataType] token literal (`chan`, `kun`, `senpai`...)
    /// - [TokenKind::RBracket] (`chan[1]`, `kun[3]` ...)
    /// - [TokenKind::RBrace] (`chan{}`, `kun{senpai}` ...)
    /// - [TokenKind::Question] (`chan?`, `senpai[1]?`, `kun{san}?` ...)
    fn parse_data_type(&mut self) -> Result<DataType, ()> {
        let unit = self.parse_data_type_unit();
        match self.peek_tok().kind {
            TokenKind::LBracket => Ok(self.parse_vector_type(Vectorable::Unit(unit))),
            TokenKind::LBrace => self.parse_hash_type(unit),
            _ => Ok(DataType::Unit(unit)),
        }
    }

    /// starts with a [DataTypeUnit] token in current
    /// ends with [DataTypeUnit] token or [TokenKind::Question] in current
    fn parse_data_type_unit(&mut self) -> DataTypeUnit {
        let tok = self.curr_tok();
        let optional = self
            .peek_tok_is(TokenKind::Question)
            .then(|| self.advance(1))
            .is_some();
        DataTypeUnit::new(
            tok,
            optional,
            (tok.offset.start, self.curr_tok().offset.end),
        )
    }

    /// starts with [TokenKind::LBracket] in peek
    /// ends with any of the ff in current:
    /// - [TokenKind::RBracket]
    /// - [TokenKind::Question] (if optional)
    fn parse_vector_type(&mut self, id: Vectorable) -> DataType {
        self.advance(1);
        let dim = self
            .expect_peek_is(TokenKind::IntLiteral)
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish();
        self.expect_peek_is(TokenKind::RBracket).ignore().finish();
        let optional = self
            .peek_tok_is(TokenKind::Question)
            .then(|| self.advance(1))
            .is_some();
        let offset_start = id.offset().start;
        DataType::Vec(VecType::new(
            id,
            dim,
            optional,
            (offset_start, self.curr_tok().offset.end),
        ))
    }

    /// starts with [TokenKind::LBrace] in peek
    /// ends with [TokenKind::RBrace] in current
    ///
    /// ends with any of the ff in current:
    /// - [TokenKind::RBrace] (ended normally)
    /// - [TokenKind::Question] (is optional)
    /// - [TokenKind::RBracket] (parsed vec type)
    fn parse_hash_type(&mut self, unit: DataTypeUnit) -> Result<DataType, ()> {
        self.advance(1);
        self.safe_expect_peek_is_in(&[
            TokenKind::RBrace,
            TokenKind::Type,
            TokenKind::Chan,
            TokenKind::Kun,
            TokenKind::Senpai,
            TokenKind::Kouhai,
            TokenKind::San,
            TokenKind::Sama,
            TokenKind::Dono,
        ])
        .ok_then(|parser, actual| match actual.kind {
            TokenKind::RBrace => Ok(parser.parse_set_type(unit.clone(), false)),
            kind if kind.is_type() => parser.parse_map_type(unit.clone()),
            _ => Err(()),
        })
        .ignore_err()
        .error_header("INVALID INNER DATA TYPE")
        .error_context("Only data types are allowed to be inner types")
        .finish_result()
    }

    /// starts with any of the ff in peek
    /// - [TokenKind::RBrace] (called normally)
    /// - [TokenKind::Question] (recursive call with found question mark)
    ///
    /// ends with any of the ff in current:
    /// - [TokenKind::RBrace] (ended normally)
    /// - [TokenKind::Question] (is optional)
    /// - [TokenKind::RBracket] (parsed vec type)
    fn parse_set_type(&mut self, unit: DataTypeUnit, optional: bool) -> DataType {
        self.advance(1);
        let unit_offset = unit.offset();
        match self.peek_tok().kind {
            TokenKind::Question => self.parse_set_type(unit, true),
            TokenKind::LBracket => self.parse_vector_type(Vectorable::Set(SetType::new(
                unit,
                optional,
                (unit_offset.start, unit_offset.end),
            ))),
            _ => DataType::Set(SetType::new(
                unit,
                optional,
                (unit_offset.start, unit_offset.end),
            )),
        }
    }

    /// starts with [DataTypeUnit] starting token in peek
    /// ends with any of the ff:
    /// - [TokenKind::RBrace] (ended normally)
    /// - [TokenKind::Question] (is optional)
    /// - [TokenKind::RBracket] (parsed vec type)
    fn parse_map_type(&mut self, unit: DataTypeUnit) -> Result<DataType, ()> {
        self.advance(1);
        let inner = Box::new(self.parse_data_type()?);
        self.expect_peek_is(TokenKind::RBrace)
            .recover()
            .skip_one_if(|parser| parser.peek_tok_nth(1).kind == TokenKind::LBracket)
            .finish();
        let offset_start = unit.offset().start;
        let res = MapType::new(
            unit,
            inner,
            self.peek_tok_is(TokenKind::Question)
                .then(|| self.advance(1))
                .is_some(),
            (offset_start, self.curr_tok().offset.end),
        );
        match self.peek_tok().kind {
            TokenKind::LBracket => Ok(self.parse_vector_type(Vectorable::Map(res))),
            _ => Ok(DataType::Map(res)),
        }
    }

    // }}}

    // STATEMENT PARSERS {{{
    /// Just calls [Parser::parse_declaration] and maps the [Declaration] to a
    /// `Result<Statement, ()>`
    ///
    /// starts with [TokenKind::Hi] in peek
    /// ends with [TokenKind::Terminator] in current
    fn parse_declaration_statement(&mut self) -> Statement {
        self.expect_peek_is(TokenKind::Hi)
            .recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[
                    TokenKind::Identifier,
                    TokenKind::Dash,
                    TokenKind::Terminator,
                ]) || parser.peek_tok_is_type()
                    || parser.peek_tok_is_assign()
            })
            .finish();
        Statement::Declaration(self.parse_declaration())
    }

    /// starts with [TokenKind::Hi] in current
    /// ends with [TokenKind::Terminator] in current
    fn parse_declaration(&mut self) -> Declaration {
        let start = self.curr_tok();
        let (id, error_count) = self
            .expect_peek_is(TokenKind::Identifier)
            .ok_then(|_, actual| Ok(actual))
            .or_recover()
            .skip_until(|parser| {
                parser.peek_tok_is_in(&[TokenKind::Dash, TokenKind::Terminator])
                    || parser.peek_tok_is_type()
                    || parser.peek_tok_is_assign()
            })
            .finish_with_error_count();

        let (dtype, error_count) = if self.peek_tok_is(TokenKind::Dash) {
            self.advance(1);
            let (dtype, error_count) = self
                .expect_peek_is_type()
                .ok_then(|parser, _| parser.parse_data_type())
                .or_recover()
                .skip_until(|parser| {
                    // follow set
                    parser.peek_tok_is(TokenKind::Terminator)
                    || parser.peek_tok_is_assign()
                    // end of dtype
                    || parser.curr_tok_is_in(&[
                        TokenKind::RBracket,
                        TokenKind::RBrace,
                        TokenKind::Question,
                    ])
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

        self.expect_peek_is_assign_operator()
            .ignore()
            .limit_errors(error_count)
            .finish_with_error_count();

        let expr = self.parse_expression(Precedence::Lowest);

        self.expect_peek_is(TokenKind::Terminator).ignore().finish();

        Declaration::new(
            id,
            dtype,
            mutable,
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
            .expect_peek_is_assign_operator()
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish();
        let expr = self.parse_expression(Precedence::Lowest);
        self.expect_peek_is(TokenKind::Terminator).ignore().finish();
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
            .expect_peek_is(TokenKind::Wetuwn)
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish();
        let expr = self.parse_expression(Precedence::Lowest);
        self.expect_peek_is(TokenKind::Terminator).ignore().finish();
        Statement::Return(ReturnStatement::new(
            expr,
            (start.offset.start, self.curr_tok().offset.end),
        ))
    }

    /// starts with [TokenKind::Iwf] in peek
    /// ends with [TokenKind::RBrace] in current
    fn parse_if(&mut self) -> Statement {
        let start = self
            .expect_peek_is(TokenKind::Iwf)
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish();
        let condition = self.parse_expression(Precedence::Lowest);
        let (body, error_count) = self
            .expect_peek_is(TokenKind::LBrace)
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
                self.expect_peek_is(TokenKind::Ewse)
                    .ignore()
                    .limit_errors(error_count)
                    .finish_with_error_count();
                Some(
                    self.expect_peek_is(TokenKind::LBrace)
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
                .expect_peek_is(TokenKind::Ewif)
                .ok_then(|_, actual| Ok(actual))
                .ignore_err()
                .limit_errors(error_count)
                .finish_with_error_count();
            error_count = ec;

            let condition = self.parse_expression(Precedence::Lowest);

            let ((), ec) = self
                .expect_peek_is(TokenKind::LBrace)
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
        self.expect_peek_is(TokenKind::Fow).ignore().finish();
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
            .expect_peek_is(TokenKind::Hi)
            .ok_then(|parser, _| Ok(parser.parse_declaration()))
            .ignore_err()
            .finish_with_error_count();
        let condition = self.parse_expression(Precedence::Lowest);
        let ((), error_count) = self
            .expect_peek_is(TokenKind::Terminator)
            .ignore()
            .limit_errors(error_count)
            .finish_with_error_count();
        let update = self.parse_expression(Precedence::Lowest);
        let body = self
            .expect_peek_is(TokenKind::LBrace)
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
            .expect_peek_is(TokenKind::Identifier)
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish();
        self.expect_peek_is(TokenKind::In).ignore().finish();
        let collection = self.parse_expression(Precedence::Lowest);
        let body = self
            .expect_peek_is(TokenKind::LBrace)
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
            .expect_peek_is(TokenKind::Bweak)
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish();
        self.expect_peek_is(TokenKind::Terminator).ignore().finish();
        Statement::Break(tok)
    }

    // TODO: continue statement position info does not include Terminator
    /// starts with [TokenKind::Continue] in peek
    /// ends with [TokenKind::Terminator] in current
    fn parse_continue(&mut self) -> Statement {
        let tok = self
            .expect_peek_is(TokenKind::Continue)
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish();
        self.expect_peek_is(TokenKind::Terminator).ignore().finish();
        Statement::Continue(tok)
    }

    /// starts with [TokenKind::Mash] in peek
    /// ends with [TokenKind::RBrace] in current
    fn parse_mash(&mut self) -> Statement {
        let start = self
            .expect_peek_is(TokenKind::Mash)
            .ok_then(|_, actual| Ok(actual))
            .ignore_err()
            .finish();
        let expr = self.parse_expression(Precedence::Lowest);
        let found_opening = self
            .expect_peek_is(TokenKind::LBrace)
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
                self.expect_peek_is(TokenKind::Colon).ignore().finish();
                Some(self.parse_case_body())
            }
            _ => None,
        };
        self.expect_peek_is(TokenKind::RBrace).ignore().finish();
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
            .expect_peek_is_type()
            .ok_then(|parser, _| parser.parse_data_type())
            .ignore_err()
            .finish();
        let body = self
            .expect_peek_is(TokenKind::Colon)
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
                kind if kind.is_assign_operator() => {
                    Statement::Assignment(self.parse_assignment(id.try_into().expect(
                        r#"parse_ident_statement: ended at Ident/RBracket should be assignable"#,
                    )))
                }
                _ => {
                    self.expect_peek_is(TokenKind::Terminator).ignore().finish();
                    Statement::Expression(id)
                }
            },
            TokenKind::RParen => {
                self.expect_peek_is(TokenKind::Terminator).ignore().finish();
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
            .expect_peek_is(TokenKind::LParen)
            .ok_then(|parser, _| {
                let mut args: Vec<Expression> = vec![];
                while !parser.peek_tok_is(TokenKind::RParen) {
                    args.push(parser.parse_expression(Precedence::Lowest));
                    parser.allow_hanging_comma(TokenKind::RParen);
                }
                Ok(args)
            })
            .ignore_err()
            .finish();
        self.expect_peek_is(TokenKind::RParen).ignore().finish();
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
        let expr = self.parse_expression(Precedence::Lowest);
        self.expect_peek_is(TokenKind::Terminator).ignore().finish();
        Statement::Expression(expr)
    }

    // }}}

    // EXPRESSION GROUP PARSERS {{{

    /// Parses an expression unit, which may be followed by a pipeline.
    ///
    /// starts with valid starting token for an [Expression] in peek.
    /// ends with final token of the [Expression] in current
    fn parse_expression(&mut self, precedence: Precedence) -> Expression {
        let expr = self.parse_expression_unit(precedence);
        if self.peek_tok_is(TokenKind::Pipe) {
            self.parse_pipeline_expression(expr)
        } else {
            expr
        }
    }

    /// Parses a single expression unit, with single meaning not in a pipeline.
    ///
    /// starts with valid starting token for an [Expression] in peek.
    /// ends with final token of the [Expression] in current
    fn parse_expression_unit(&mut self, precedence: Precedence) -> Expression {
        // prefix parsing
        let expected_prefix = self
            .prefix_parse_fns
            .iter()
            .map(|(&tokens, _)| tokens)
            .collect::<Vec<_>>();
        let (left, mut error_count) = self
            .expect_peek_is_in(&expected_prefix)
            .ok_then(
                |parser, actual| match parser.prefix_parse_fns.get(&actual.kind) {
                    Some(p) => Ok(p(parser)),
                    None => Err(()),
                },
            )
            .or_recover()
            .skip_until_peek_is_in(&[
                TokenKind::LBracket,
                TokenKind::Dot,
                TokenKind::Terminator,
                TokenKind::Pipe,
            ])
            .error_header(MissingParserErrorHeader::Prefix.header())
            .finish_with_error_count();
        let mut left = self.parse_expression_post_prefix(left, precedence);

        // infix parsing
        while !self.peek_tok_is_in(&[TokenKind::EOF, TokenKind::Terminator, TokenKind::Pipe])
            && precedence < Precedence::of(self.peek_tok().kind)
        {
            (left, error_count) = self
                .expect_peek_is_operator()
                .ok_then(|parser, actual| {
                    actual
                        .kind
                        .is_operator()
                        .then(|| parser.parse_infix_expression(left.clone()))
                        .ok_or(())
                })
                .ignore_err()
                .limit_errors(error_count)
                .error_header(MissingParserErrorHeader::Infix.header())
                .finish_with_error_count();
        }
        left
    }

    /// starts with prefix operator in current
    /// ends with prefix operand in current
    fn parse_prefix_expression(&mut self) -> Expression {
        let op = self.curr_tok();
        let right = self.parse_expression_unit(Precedence::Prefix);
        Expression::Prefix(PrefixExpression::new(op, Box::new(right)))
    }

    /// starts with infix operator in current
    /// ends with infix operand in current
    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        let op = self.curr_tok();
        let right = self.parse_expression_unit(Precedence::of(self.curr_tok().kind));
        Expression::Infix(InfixExpression::new(Box::new(left), op, Box::new(right)))
    }

    /// starts with [TokenKind::LParen] in current
    /// ends with [TokenKind::RParen] in current
    fn parse_grouped_expression(&mut self) -> Expression {
        let start = self.curr_tok();
        let expr = self.parse_expression(Precedence::Lowest);
        self.expect_peek_is(TokenKind::RParen).ignore().finish();
        match expr {
            already_grouped @ Expression::Grouped(_) => already_grouped,
            _ => Expression::Grouped(GroupedExpression::new(
                Box::new(expr),
                (start.offset.start, self.curr_tok().offset.end),
            )),
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
    ) -> Expression {
        if expr.is_indexable() && self.peek_tok_is(TokenKind::LBracket) {
            let idx_expr = self.parse_indexed_expression(expr);
            self.parse_expression_post_prefix(idx_expr, precedence)
        } else if expr.is_accessible() && self.peek_tok_is(TokenKind::Dot) {
            let acc_expr = self.parse_access_expression(expr);
            self.parse_expression_post_prefix(acc_expr, precedence)
        } else {
            expr
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
    /// starts with [TokenKind::Identifier] in peek
    /// ends with any of the ff in current:
    /// - [TokenKind::Identifier]
    /// - [TokenKind::RParen]
    /// - [TokenKind::RBracket]
    fn parse_ident_expression(&mut self) -> Expression {
        self.expect_peek_is(TokenKind::Identifier).ignore().finish();
        let id_expr_unit = self.parse_ident_expression_unit();
        if self.peek_tok_is(TokenKind::Pipe) {
            self.parse_pipeline_expression(id_expr_unit)
        } else {
            id_expr_unit
        }
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
    fn parse_ident_expression_unit(&mut self) -> Expression {
        let accessor = self.parse_single_accessed();
        self.parse_access_expression(accessor.into())
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
    fn parse_single_accessed(&mut self) -> Accessed {
        let initial_id = self.curr_tok();
        let (idx_id, acc_id) = if self.peek_tok_is(TokenKind::LParen) {
            let fn_call = self.parse_fn_call();
            match self.peek_tok().kind {
                TokenKind::LBracket => (Indexable::FnCall(fn_call), Accessed::Token(initial_id)),
                _ => (Indexable::Token(initial_id), Accessed::FnCall(fn_call)),
            }
        } else {
            (Indexable::Token(initial_id), Accessed::Token(initial_id))
        };
        if self.peek_tok_is(TokenKind::LBracket) {
            self.advance(1);
            let indices = self.parse_array_literal().exprs;
            Accessed::IndexedId(IndexedId::new(idx_id, indices, self.curr_tok().offset.end))
        } else {
            acc_id
        }
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
    fn parse_indexed_expression(&mut self, expr: Expression) -> Expression {
        let id = Indexable::try_from(expr).expect(
            r#"parse_indexed_expression: converting cannot fail as it was checked beforehand"#,
        );
        self.expect_peek_is(TokenKind::LBracket).ignore().finish();
        let indices: Vec<Expression> = self.parse_array_literal().exprs;
        Expression::Indexed(IndexedId::new(id, indices, self.curr_tok().offset.end))
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
    fn parse_access_expression(&mut self, expr: Expression) -> Expression {
        let mut accessed: Vec<Accessed> = vec![];
        while self.peek_tok_is(TokenKind::Dot) {
            self.advance(1);
            self.expect_peek_is(TokenKind::Identifier).ignore().finish();
            accessed.push(self.parse_single_accessed());
        }
        if accessed.len() > 0 {
            match accessed.last().unwrap_or(&Accessed::default()) {
                Accessed::Token(_) => Expression::Access(AccessType::Field(GroupAccess::new(
                    Box::new(expr),
                    accessed,
                    PhantomData,
                ))),
                Accessed::IndexedId(res) => {
                    match res.id {
                        Indexable::Token(_) => Expression::Access(AccessType::Field(
                            GroupAccess::new(Box::new(expr), accessed, PhantomData),
                        )),
                        Indexable::FnCall(_) => Expression::Access(AccessType::Method(
                            GroupAccess::new(Box::new(expr), accessed, PhantomData),
                        )),
                        _ => unreachable!(
                            r#"parse_access_expression: indexed result from parse_single_accessed can only be a Token or FnCall"#
                        ),
                    }
                }
                Accessed::FnCall(_) => Expression::Access(AccessType::Method(GroupAccess::new(
                    Box::new(expr),
                    accessed,
                    PhantomData,
                ))),
            }
        } else {
            expr
        }
    }

    /// Parses a possible pipeline expression where the initial value of the
    /// pipeline was parsed beforehand. Just returns the already parsed value
    /// if there is no pipe (`|`) following it.
    /// Ensures that only callables are parsed inside the pipeline.
    ///
    /// starts with [TokenKind::Pipe] in peek
    /// ends with [TokenKind::RParen] in current
    fn parse_pipeline_expression(&mut self, first_value: Expression) -> Expression {
        let mut rest: Vec<Expression> = vec![];
        while self.peek_tok_is(TokenKind::Pipe) {
            rest.push(
                self.expect_peek_is(TokenKind::Pipe)
                    .ok_then(|parser, _| Ok(parser.parse_expression_unit(Precedence::Lowest)))
                    .ignore_err()
                    .finish(),
            );
        }
        match &rest.len() {
            0 => first_value,
            _ => Expression::Pipeline(Pipeline::new(Box::new(first_value), rest)),
        }
    }

    // }}}

    // EXPRESSION UNIT PARSERS {{{

    /// just returns the current token
    fn parse_literal(&mut self) -> Expression {
        Expression::Token(self.curr_tok())
    }

    /// This parses a group initializer
    /// ```
    /// GroupName(arg1, arg2, ...)
    /// ```
    /// starts with [TokenKind::Type] in current
    /// ends with [TokenKind::LParen] in current
    fn parse_group_init(&mut self) -> Expression {
        let id = self.curr_tok();
        self.expect_peek_is(TokenKind::LParen).ignore().finish();
        let mut args: Vec<Expression> = vec![];
        while !self.peek_tok_is(TokenKind::RParen) {
            args.push(self.parse_expression(Precedence::Lowest));
            self.allow_hanging_comma(TokenKind::RParen);
        }
        self.expect_peek_is(TokenKind::RParen).ignore().finish();
        Expression::GroupInit(GroupInit::new(id, args, self.curr_tok().offset.end))
    }

    /// Just maps the [ArrayLiteral] from [Parser::parse_array_literal] to an
    /// [Expression]
    ///
    /// starts with [TokenKind::LBracket] in current
    /// ends with [TokenKind::RBracket] in current
    fn parse_array_literal_expression(&mut self) -> Expression {
        Expression::Array(self.parse_array_literal())
    }

    /// starts with [TokenKind::LBracket] in current
    /// ends with [TokenKind::RBracket] in current
    fn parse_array_literal(&mut self) -> ArrayLiteral {
        let start = self.curr_tok();
        let mut exprs: Vec<Expression> = vec![];
        while !self.peek_tok_is(TokenKind::RBracket) {
            exprs.push(self.parse_expression(Precedence::Lowest));
            self.allow_hanging_comma(TokenKind::RBracket);
        }
        self.expect_peek_is(TokenKind::RBracket).ignore().finish();
        ArrayLiteral::new(exprs, (start.offset.start, self.curr_tok().offset.end))
    }

    /// starts with [TokenKind::Hash] in current
    /// starts with [TokenKind::RBracket] in current
    ///
    /// Abstracts over parsing hashset and hashmap literals depending on whether the first item is
    /// followed by a colon or a comma
    /// `#[item, item]` => hashset
    /// `#[item: item]` => hashmap
    fn parse_hash_literal(&mut self) -> Expression {
        let start = self.curr_tok();
        self.expect_peek_is(TokenKind::LBracket).ignore().finish();
        // empty literal
        match self.peek_tok().kind {
            // empty set: #[]
            TokenKind::RBracket => {
                self.advance(1);
                return Expression::Set(SetLiteral::new(
                    vec![],
                    (start.offset.start, self.curr_tok().offset.end),
                ));
            }
            // empty map: #[:]
            TokenKind::Colon => {
                self.advance(1);
                self.expect_peek_is(TokenKind::RBracket).ignore().finish();
                return Expression::Map(MapLiteral::new(
                    vec![],
                    (start.offset.start, self.curr_tok().offset.end),
                ));
            }
            _ => (),
        }
        let first_expr = self.parse_expression(Precedence::Lowest);
        self.expect_peek_is_in(&[TokenKind::RBracket, TokenKind::Comma, TokenKind::Colon])
            .ok_then(|parser, actual| {
                match actual.kind {
                    TokenKind::RBracket => Ok(Expression::Set(SetLiteral::new(
                        vec![first_expr.clone()],
                        (start.offset.start, parser.curr_tok().offset.end),
                    ))),
                    TokenKind::Comma => Ok(parser.parse_set_literal(first_expr.clone(), start)),
                    TokenKind::Colon => Ok(parser.parse_map_literal(first_expr.clone(), start)),
                    // TODO: test #[aqua aqua] whether i want to skip till ] or just ignore the
                    // error and do nothing
                    _ => Err(()),
                }
            })
            .ignore_err()
            .finish()
    }

    /// A [Token] with kind [TokenKind::Hash] needs to be passed in for
    /// position information.
    ///
    /// starts with [TokenKind::Comma] in current (always), with first val already parsed
    /// ends with [TokenKind::RBracket] in current
    fn parse_set_literal(&mut self, first_expr: Expression, hash_token: Token) -> Expression {
        let mut exprs: Vec<Expression> = vec![first_expr];
        while !self.peek_tok_is(TokenKind::RBracket) {
            exprs.push(self.parse_expression(Precedence::Lowest));
            self.allow_hanging_comma(TokenKind::RBracket);
        }
        self.expect_peek_is(TokenKind::RBracket).ignore().finish();
        Expression::Set(SetLiteral::new(
            exprs,
            (hash_token.offset.start, self.curr_tok().offset.end),
        ))
    }

    /// A [Token] with kind [TokenKind::Hash] needs to be passed in for
    /// position information.
    ///
    /// starts with [TokenKind::Colon] in current (always), with first key already parsed
    /// ends with [TokenKind::RBracket] in current
    fn parse_map_literal(&mut self, first_key: Expression, hash_token: Token) -> Expression {
        let first_expr = self.parse_expression(Precedence::Lowest);
        let mut exprs: Vec<(Expression, Expression)> = vec![(first_key, first_expr)];
        self.allow_hanging_comma(TokenKind::RBracket);
        while !self.peek_tok_is(TokenKind::RBracket) {
            let key = self.parse_expression(Precedence::Lowest);
            self.expect_peek_is(TokenKind::Colon).ignore().finish();
            let expr = self.parse_expression(Precedence::Lowest);
            exprs.push((key, expr));
            self.allow_hanging_comma(TokenKind::RBracket);
        }
        self.expect_peek_is(TokenKind::RBracket).ignore().finish();
        Expression::Map(MapLiteral::new(
            exprs,
            (hash_token.offset.start, self.curr_tok().offset.end),
        ))
    }

    // }}}

    // HELPER METHODS {{{

    /// maps a TokenKind to a respective parsing function
    /// user can supply own parsing function
    fn register_prefix(&mut self, args: Vec<(TokenKind, fn(&mut Self) -> Expression)>) {
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
    fn register_body(&mut self, args: Vec<(TokenKind, fn(&mut Self) -> Statement)>) {
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
        self.peek_tok().kind.is_assign_operator()
    }
    /// checks peek token's type is a DataType
    fn peek_tok_is_type(&self) -> bool {
        self.peek_tok().kind.is_type()
    }

    /// allows hanging comma on enclosed, comma separated expressions
    /// ends with [TokenKind::Comma] in current and passed in closing in peek,
    /// regardless of whether the expect failed or not
    fn allow_hanging_comma(&mut self, closing: TokenKind) {
        self.safe_expect_peek_is_in(&[TokenKind::Comma, closing])
            .ok_then(|parser, actual| match actual.kind {
                TokenKind::Comma => Ok(parser.advance(1)),
                actual if actual == closing => Ok(()),
                _ => Err(()),
            })
            .or_recover()
            .skip_until(|parser| {
                parser.peek_tok_is(closing) || parser.curr_tok_is(TokenKind::Comma)
            })
            .finish();
    }

    // }}}

    // RECOVER EXPECT METHODS {{{

    /// advances cursor 1 time if peek tok is eq to any of the given
    /// allows to recover from the error otherwise
    fn expect_peek_is_in(&mut self, token_kinds: &[TokenKind]) -> Expectation<'src, '_> {
        let ok = token_kinds.contains(&self.peek_tok().kind);
        let actual = if ok {
            self.advance(1);
            self.curr_tok()
        } else {
            self.peek_tok()
        };
        Expectation::new(token_kinds.to_vec(), actual, self, ok)
    }

    /// advances cursor 1 time if peek tok is eq to given
    /// allows to recover from the error otherwise
    fn expect_peek_is(&mut self, token_kind: TokenKind) -> Expectation<'src, '_> {
        self.expect_peek_is_in(&[token_kind])
    }

    /// Like [Parser::expect_peek_is] but does not advance cursor 1 time if peek
    /// tok is eq to any of the given.
    fn safe_expect_peek_is(&mut self, token_kind: TokenKind) -> Expectation<'src, '_> {
        self.safe_expect_peek_is_in(&[token_kind])
    }

    /// Like [Parser::expect_peek_is_in] but does not advance cursor 1 time if
    /// peek tok is eq to any of the given.
    fn safe_expect_peek_is_in(&mut self, token_kinds: &[TokenKind]) -> Expectation<'src, '_> {
        let ok = token_kinds.contains(&self.peek_tok().kind);
        Expectation::new(token_kinds.to_vec(), self.peek_tok(), self, ok)
    }

    /// advances cursor 1 time if peek token's type is a DataType
    /// allows to recover from the error otherwise
    fn expect_peek_is_type(&mut self) -> Expectation<'src, '_> {
        let ok = self.peek_tok().kind.is_type();
        let actual = if ok {
            self.advance(1);
            self.curr_tok()
        } else {
            self.peek_tok()
        };
        Expectation::new(TokenKind::data_types(), actual, self, ok)
    }

    /// advances cursor 1 time if peek tok is a valid assignment operator
    /// allows to recover from the error otherwise
    fn expect_peek_is_assign_operator(&mut self) -> Expectation<'src, '_> {
        let ok = self.peek_tok().kind.is_assign_operator();
        let actual = if ok {
            self.advance(1);
            self.curr_tok()
        } else {
            self.peek_tok()
        };
        Expectation::new(TokenKind::assign_operators(), actual, self, ok)
    }

    /// advances cursor 1 time if peek tok is eq to any of the given
    /// allows to recover from the error otherwise
    fn expect_peek_is_operator(&mut self) -> Expectation<'src, '_> {
        let ok = self.peek_tok().kind.is_operator();
        let actual = if ok {
            self.advance(1);
            self.curr_tok()
        } else {
            self.peek_tok()
        };
        Expectation::new(TokenKind::operators(), actual, self, ok)
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
    R: Default + Debug,
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
    R: Default + Debug,
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
