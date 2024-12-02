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
        parser
    }
}
