use std::collections::HashMap;

use crate::{
    lexer::token::{Token, TokenType},
    parser::productions::*,
};

pub struct Parser {
    pub tokens: Vec<Token>,

    pub program: String,
    pub errors: Vec<String>,

    pub pos: usize,
    pub curr_tok: Token,
    pub peek_tok: Token,

    pub prefix_parse_fns: HashMap<TokenType, fn(&mut Self)>,
    pub prefix_special_parse_fns: HashMap<TokenType, fn(&mut Self)>,
    pub infix_parse_fns: HashMap<TokenType, fn(&mut Self, Box<dyn Expression>)>,
    pub infix_special_parse_fns: HashMap<TokenType, fn(&mut Self, Box<dyn Expression>)>,
    pub postfix_parse_fns: HashMap<TokenType, fn(&mut Self, Box<dyn Expression>)>,
    pub in_block_parse_fns: HashMap<TokenType, fn(&mut Self)>,

    pub expected_prefix_tokens: Vec<TokenType>,
    pub expected_prefix_special_tokens: Vec<TokenType>,
    pub expected_infix_tokens: Vec<TokenType>,
    pub expected_infix_special_tokens: Vec<TokenType>,
    pub expected_postfix_tokens: Vec<TokenType>,
    pub expected_in_block_tokens: Vec<TokenType>,
}
impl Parser {
    pub fn new(mut tokens: Vec<Token>) -> Result<Parser, String> {
        if tokens.is_empty() {
            return Err("Empty source".to_string());
        }
        let mut tokens_iter = tokens.clone().into_iter();
        let curr_tok =
            tokens_iter
                .next()
                .unwrap_or(Token::new(TokenType::EOF, "\0", (0, 0), (0, 0)));
        let peek_tok =
            tokens_iter
                .next()
                .unwrap_or(Token::new(TokenType::EOF, "\0", (0, 0), (0, 0)));
        let last_tok =
            tokens_iter
                .last()
                .unwrap_or(Token::new(TokenType::EOF, "\0", (0, 0), (0, 0)));
        tokens.push(Token::new(
            TokenType::EOF,
            "\0",
            (last_tok.pos.0, last_tok.pos.1 + 1),
            (last_tok.end_pos.0, last_tok.end_pos.1 + 1),
        ));
        Ok(Parser {
            tokens: tokens.to_vec(),
            program: String::new(),
            errors: vec![],

            pos: 0,
            curr_tok,
            peek_tok,

            prefix_parse_fns: HashMap::new(),
            prefix_special_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            infix_special_parse_fns: HashMap::new(),
            postfix_parse_fns: HashMap::new(),
            in_block_parse_fns: HashMap::new(),

            expected_prefix_tokens: vec![],
            expected_prefix_special_tokens: vec![],
            expected_infix_tokens: vec![],
            expected_infix_special_tokens: vec![],
            expected_postfix_tokens: vec![],
            expected_in_block_tokens: vec![],
        })
    }
    pub fn parse_program(&mut self) {
        self.register_init();
        while !self.curr_tok_is(TokenType::EOF) {
            match self.curr_tok.kind {
                TokenType::Fwunc => self.parse_func(),
                TokenType::Gwobaw => {
                    self.advance(1); // consume the gwobaw token
                    self.parse_declaration();
                }
                TokenType::Cwass => self.parse_class(),
                _ => {
                    // TODO: add wrong global starting token
                    println!("Unexpected token: {}", self.curr_tok);
                }
            }
            self.advance(1);
        }
    }

    fn advance(&mut self, times: usize) -> () {
        if self.curr_tok_is(TokenType::EOF) {
            return;
        }
        for _ in 0..times {
            self.pos += 1;
            self.curr_tok = self.peek_tok;
            if self.peek_tok_is(TokenType::EOF) {
                return;
            }
            self.peek_tok = self.tokens[self.pos];
        }
    }
    fn register_init(&mut self) -> () {
        self.register_prefix(TokenType::Dash, Self::parse_prefix_expression);
        self.register_prefix(TokenType::LParen, Self::parse_grouped_expression);
        self.register_prefix(TokenType::Identifier, Self::parse_ident);

        self.register_prefix_special(TokenType::LParen, Self::parse_grouped_expression);
        self.register_prefix_special(TokenType::LBrace, Self::parse_array);
        self.register_prefix_special(TokenType::StringPartStart, Self::parse_gen_string);

        // literals (just returns curr_tok)
        self.register_prefix(TokenType::IntLiteral, Self::parse_literal);
        self.register_prefix(TokenType::FloatLiteral, Self::parse_literal);
        self.register_prefix(TokenType::Fax, Self::parse_literal);
        self.register_prefix(TokenType::Cap, Self::parse_literal);

        self.register_prefix_special(TokenType::IntLiteral, Self::parse_literal);
        self.register_prefix_special(TokenType::StringLiteral, Self::parse_gen_string);
        self.register_prefix_special(TokenType::FloatLiteral, Self::parse_literal);
        self.register_prefix_special(TokenType::Fax, Self::parse_literal);
        self.register_prefix_special(TokenType::Cap, Self::parse_literal);
        self.register_prefix_special(TokenType::Nuw, Self::parse_literal);
        self.register_prefix_special(TokenType::Inpw, Self::parse_gen_string);

        // infixes
        self.register_infix(TokenType::Equal, Self::parse_infix_special_expression);
        self.register_infix(TokenType::NotEqual, Self::parse_infix_special_expression);
        self.register_infix(TokenType::And, Self::parse_infix_special_expression);
        self.register_infix(TokenType::Or, Self::parse_infix_special_expression);
        self.register_infix(TokenType::LessThan, Self::parse_infix_expression);
        self.register_infix(TokenType::LessEqual, Self::parse_infix_expression);
        self.register_infix(TokenType::GreaterThan, Self::parse_infix_expression);
        self.register_infix(TokenType::GreaterEqual, Self::parse_infix_expression);

        self.register_infix_special(TokenType::Equal, Self::parse_infix_special_expression);
        self.register_infix_special(TokenType::NotEqual, Self::parse_infix_special_expression);
        self.register_infix_special(TokenType::And, Self::parse_infix_special_expression);
        self.register_infix_special(TokenType::Or, Self::parse_infix_special_expression);

        // self.register_infix(TokenType::Concat, self.parse_infix_expression)
        self.register_infix(TokenType::Plus, Self::parse_infix_expression);
        self.register_infix(TokenType::Dash, Self::parse_infix_expression);
        self.register_infix(TokenType::Multiply, Self::parse_infix_expression);
        self.register_infix(TokenType::Divide, Self::parse_infix_expression);
        self.register_infix(TokenType::Modulo, Self::parse_infix_expression);

        // postfixes
        self.register_postfix(TokenType::Identifier, Self::parse_postfix_expression);
        self.register_postfix(TokenType::IntLiteral, Self::parse_postfix_expression);
        self.register_postfix(TokenType::FloatLiteral, Self::parse_postfix_expression);
        self.register_postfix(TokenType::RParen, Self::parse_postfix_expression);
        // self.register_postfix(TokenType::RBrace, Self::parse_postfix_expression);

        // in blocks
        self.register_in_block(TokenType::Identifier, Self::parse_ident_statement);
        self.register_in_block(TokenType::Iwf, Self::parse_if_statement);
        self.register_in_block(TokenType::Wetuwn, Self::parse_return_statement);
        self.register_in_block(TokenType::Whiwe, Self::parse_while_statement);
        self.register_in_block(TokenType::DoWhiwe, Self::parse_while_statement);
        self.register_in_block(TokenType::Fow, Self::parse_for_statement);
        self.register_in_block(TokenType::Pwint, Self::parse_print);
    }

    // helper fns
    fn curr_tok_is(&self, token_type: TokenType) -> bool {
        self.curr_tok.kind == token_type
    }
    fn peek_tok_is(&self, token_type: TokenType) -> bool {
        self.peek_tok.kind == token_type
    }
    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_tok_is(token_type) {
            self.advance(1);
            true
        } else {
            false
        }
    }

    // register_init helper fns
    fn register_prefix(&mut self, token_type: TokenType, parser: fn(&mut Self)) -> () {
        self.prefix_parse_fns.insert(token_type, parser);
        self.expected_prefix_tokens.push(token_type);
    }
    fn register_prefix_special(&mut self, token_type: TokenType, parser: fn(&mut Self)) -> () {
        self.prefix_special_parse_fns.insert(token_type, parser);
        self.expected_prefix_special_tokens.push(token_type);
    }
    fn register_infix(
        &mut self,
        token_type: TokenType,
        parser: fn(&mut Self, Box<dyn Expression>),
    ) -> () {
        self.infix_parse_fns.insert(token_type, parser);
        self.expected_infix_tokens.push(token_type);
    }
    fn register_infix_special(
        &mut self,
        token_type: TokenType,
        parser: fn(&mut Self, Box<dyn Expression>),
    ) -> () {
        self.infix_special_parse_fns.insert(token_type, parser);
        self.expected_infix_special_tokens.push(token_type);
    }
    fn register_postfix(
        &mut self,
        token_type: TokenType,
        parser: fn(&mut Self, Box<dyn Expression>),
    ) -> () {
        self.postfix_parse_fns.insert(token_type, parser);
        self.expected_postfix_tokens.push(token_type);
    }
    fn register_in_block(&mut self, token_type: TokenType, parser: fn(&mut Self)) -> () {
        self.in_block_parse_fns.insert(token_type, parser);
        self.expected_in_block_tokens.push(token_type);
    }
}
