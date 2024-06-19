use crate::lexer::{
    lexer::Lexer,
    token::{Token, TokenType},
};
use std::{collections::HashMap, vec};

fn uwu_tokens() -> HashMap<&'static str, TokenType> {
    HashMap::from([
        ("aqua", TokenType::Identifier),
        ("Aqua", TokenType::Type),
        ("chan", TokenType::Chan),
        ("kun", TokenType::Kun),
        ("senpai", TokenType::Senpai),
        ("kouhai", TokenType::Kouhai),
        ("san", TokenType::San),
        ("sama", TokenType::Sama),
        ("dono", TokenType::Dono),
        ("+", TokenType::Plus),
        ("-", TokenType::Dash),
        ("*", TokenType::Multiply),
        ("/", TokenType::Divide),
        ("%", TokenType::Modulo),
        ("^", TokenType::Exponent),
        ("+=", TokenType::PlusEqual),
        ("-=", TokenType::DashEqual),
        ("*=", TokenType::MultiplyEqual),
        ("/=", TokenType::DivideEqual),
        ("%=", TokenType::ModuloEqual),
        ("^=", TokenType::ExponentEqual),
        ("<", TokenType::LessThan),
        (">", TokenType::GreaterThan),
        ("<=", TokenType::LessEqual),
        (">=", TokenType::GreaterEqual),
        ("and", TokenType::And),
        ("or", TokenType::Or),
        ("not", TokenType::Not),
        ("==", TokenType::Equal),
        ("!=", TokenType::NotEqual),
        ("=", TokenType::Assign),
        ("(", TokenType::LParen),
        (")", TokenType::RParen),
        ("[", TokenType::LBracket),
        ("]", TokenType::RBracket),
        ("{", TokenType::LBrace),
        // ("}", TokenType::RBrace), // TODO: causes infinite loop, might have something to do
        // with reversing
        (".", TokenType::Dot),
        ("?", TokenType::Question),
        ("!", TokenType::Bang),
        (",", TokenType::Comma),
        ("|", TokenType::Pipe),
        ("~", TokenType::Terminator),
        (" ", TokenType::Whitespace),
        ("\t", TokenType::Tab),
        ("\n", TokenType::Newline),
        ("\r", TokenType::Return),
        ("hi", TokenType::Hi),
        ("main", TokenType::Main),
        ("fun", TokenType::Fun),
        ("gwoup", TokenType::Group),
        ("contwact", TokenType::Contract),
        ("wetuwn", TokenType::Wetuwn),
        ("in", TokenType::In),
        ("assewt", TokenType::Assewt),
        ("uwu", TokenType::Uwu),
        ("pwint", TokenType::Pwint),
        ("inpwt", TokenType::Inpwt),
        ("iwf", TokenType::Iwf),
        ("ewif", TokenType::Ewif),
        ("ewse", TokenType::Ewse),
        ("mash", TokenType::Mash),
        ("default", TokenType::Default),
        ("fow", TokenType::Fow),
        ("bweak", TokenType::Bweak),
        ("continue", TokenType::Continue),
        ("1234567890", TokenType::IntLiteral),
        ("12345.67890", TokenType::FloatLiteral),
        ("fax", TokenType::Fax),
        ("cap", TokenType::Cap),
        (r#""string literal""#, TokenType::StringLiteral),
        (r#""string start{"#, TokenType::StringPartStart),
        (r#"}string start{"#, TokenType::StringPartMid),
        (r#"}string start""#, TokenType::StringPartEnd),
        ("'c'", TokenType::CharLiteral),
        ("nuww", TokenType::Nuww),
        (">.< single line comment", TokenType::SingleLineComment),
    ])
}

#[test]
fn individual_tokens() {
    let tokens = uwu_tokens();
    for (text, kind) in tokens.clone() {
        let mut l = Lexer::new(text);
        l.tokenize();
        assert!(l.errors.len() == 0);
        assert!(
            l.tokens.len() == 1,
            "Expected 1 token\nGot {}\nSource: {}\nTokens: {:?}",
            l.tokens.len(),
            text,
            l.tokens,
        );
        assert!(
            l.tokens[0]
                == Token {
                    kind,
                    text,
                    pos: (0, 0),
                    end_pos: (0, text.len() - 1),
                },
            "Expected:\n\t{}, {}, {:?}, {:?}\nGot:\n\t{}, {}, {:?}, {:?}",
            kind,
            text,
            (0, 0),
            (0, text.len() - 1),
            l.tokens[0].kind,
            l.tokens[0].text,
            l.tokens[0].pos,
            l.tokens[0].end_pos,
        );
    }
    // with whitespace delimiter
    for whitespace in vec![" ", "\n", "\t", "\r"] {
        for (text, kind) in tokens.clone() {
            let new_source = text.to_owned() + whitespace;
            let mut l = Lexer::new(Box::leak(new_source.clone().into_boxed_str()));
            l.tokenize();
            l.pretty_print(true);
            assert!(l.errors.len() == 0);
            assert!(
                l.tokens.len()
                    == match kind {
                        TokenType::SingleLineComment => match whitespace {
                            " " | "\t" => 1,
                            _ => 2,
                        },
                        _ => 2,
                    },
                "Expected {}\nGot {}",
                match kind {
                    TokenType::SingleLineComment => match whitespace {
                        " " | "\t" => 1,
                        _ => 2,
                    },
                    _ => 2,
                },
                l.tokens.len(),
            );
            assert!(
                l.tokens[0] == Token {
                    kind,
                    text: match kind {
                        TokenType::SingleLineComment => match whitespace {
                            " " | "\t" => Box::leak(new_source.into_boxed_str()),
                            _ => text,
                        }
                        _ => text,
                    },
                    pos: (0, 0),
                    end_pos: (0, 
                        match kind {
                        TokenType::SingleLineComment => match whitespace {
                            " " | "\t" => text.len(),
                            _ => text.len() - 1,
                        }
                        _ => text.len() - 1,
                    })
                },
                "Expected:\n\tkind: '{}', text: '{}', pos: {:?}, end_pos: {:?}\nGot\n\tkind: '{}', text: '{}', pos: {:?}, end_pos: {:?}\n\nSource: {}",
                kind,
                text,
                (0, 0),
                (0, match kind {
                    TokenType::SingleLineComment => match whitespace {
                        " " | "\t" => text.len(),
                        _ => text.len() - 1,
                    }
                    _ => text.len() - 1,
                }),
                l.tokens[0].kind,
                l.tokens[0].text,
                l.tokens[0].pos,
                l.tokens[0].end_pos,
                text,
            );
        }
    }
}
#[test]
fn individual_tokens_delimited_by_whitespaces() {
    let tokens = uwu_tokens();
    for whitespace in vec![" ", "\n", "\t", "\r"] {
        for (text, kind) in tokens.clone() {
            let new_source = text.to_owned() + whitespace;
            let mut l = Lexer::new(Box::leak(new_source.clone().into_boxed_str()));
            l.tokenize();
            l.pretty_print(true);
            assert!(l.errors.len() == 0);
            assert!(l.tokens.len() == match kind {
                    TokenType::SingleLineComment => match whitespace {
                        " " | "\t" => 1,
                        _ => 2,
                    },
                    _ => 2,
                },
                "Expected {}\nGot {}",
                match kind {
                    TokenType::SingleLineComment => match whitespace {
                        " " | "\t" => 1,
                        _ => 2,
                    },
                    _ => 2,
                },
                l.tokens.len(),
            );
            assert!(l.tokens[0] == Token {
                    kind,
                    text: match kind {
                        TokenType::SingleLineComment => match whitespace {
                            " " | "\t" => Box::leak(new_source.into_boxed_str()),
                            _ => text,
                        }
                        _ => text,
                    },
                    pos: (0, 0),
                    end_pos: (0, 
                        match kind {
                        TokenType::SingleLineComment => match whitespace {
                            " " | "\t" => text.len(),
                            _ => text.len() - 1,
                        }
                        _ => text.len() - 1,
                    })
                },
                "Expected:\n\tkind: '{}', text: '{}', pos: {:?}, end_pos: {:?}\nGot\n\tkind: '{}', text: '{}', pos: {:?}, end_pos: {:?}\n\nSource: {}",
                kind,
                text,
                (0, 0),
                (0, match kind {
                    TokenType::SingleLineComment => match whitespace {
                        " " | "\t" => text.len(),
                        _ => text.len() - 1,
                    }
                    _ => text.len() - 1,
                }),
                l.tokens[0].kind,
                l.tokens[0].text,
                l.tokens[0].pos,
                l.tokens[0].end_pos,
                text,
            );
        }
    }
}
