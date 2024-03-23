
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
}
