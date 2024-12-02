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
