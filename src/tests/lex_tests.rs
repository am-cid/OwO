use crate::lexer::{
    lexer::Lexer,
    token::{Token, TokenKind},
};
use std::{collections::HashMap, vec};

fn uwu_tokens() -> HashMap<&'static str, TokenKind> {
    HashMap::from([
        ("aqua", TokenKind::Identifier),
        ("Aqua", TokenKind::Type),
        ("chan", TokenKind::Chan),
        ("kun", TokenKind::Kun),
        ("senpai", TokenKind::Senpai),
        ("kouhai", TokenKind::Kouhai),
        ("san", TokenKind::San),
        ("sama", TokenKind::Sama),
        ("dono", TokenKind::Dono),
        ("+", TokenKind::Plus),
        ("-", TokenKind::Dash),
        ("*", TokenKind::Multiply),
        ("/", TokenKind::Divide),
        ("%", TokenKind::Modulo),
        ("^", TokenKind::Exponent),
        ("+=", TokenKind::PlusEqual),
        ("-=", TokenKind::DashEqual),
        ("*=", TokenKind::MultiplyEqual),
        ("/=", TokenKind::DivideEqual),
        ("%=", TokenKind::ModuloEqual),
        ("^=", TokenKind::ExponentEqual),
        ("<", TokenKind::LessThan),
        (">", TokenKind::GreaterThan),
        ("<=", TokenKind::LessEqual),
        (">=", TokenKind::GreaterEqual),
        ("and", TokenKind::And),
        ("or", TokenKind::Or),
        ("not", TokenKind::Not),
        ("==", TokenKind::Equal),
        ("!=", TokenKind::NotEqual),
        ("=", TokenKind::Assign),
        ("(", TokenKind::LParen),
        (")", TokenKind::RParen),
        ("[", TokenKind::LBracket),
        ("]", TokenKind::RBracket),
        ("{", TokenKind::LBrace),
        ("}", TokenKind::RBrace),
        (".", TokenKind::Dot),
        ("?", TokenKind::Question),
        ("!", TokenKind::Bang),
        ("...", TokenKind::Ellipsis),
        (",", TokenKind::Comma),
        (":", TokenKind::Colon),
        ("#", TokenKind::Hash),
        ("|", TokenKind::Pipe),
        ("~", TokenKind::Terminator),
        (" ", TokenKind::Whitespace),
        ("\t", TokenKind::Tab),
        ("\n", TokenKind::Newline),
        ("\r", TokenKind::CarriageReturn),
        ("hi", TokenKind::Hi),
        ("main", TokenKind::Main),
        ("fun", TokenKind::Fun),
        ("gwoup", TokenKind::Group),
        ("contwact", TokenKind::Contract),
        ("wetuwn", TokenKind::Wetuwn),
        ("iwf", TokenKind::Iwf),
        ("ewif", TokenKind::Ewif),
        ("ewse", TokenKind::Ewse),
        ("mash", TokenKind::Mash),
        ("default", TokenKind::Default),
        ("assewt", TokenKind::Assewt),
        ("fow", TokenKind::Fow),
        ("bweak", TokenKind::Bweak),
        ("continue", TokenKind::Continue),
        ("in", TokenKind::In),
        ("1234567890", TokenKind::IntLiteral),
        ("12345.67890", TokenKind::FloatLiteral),
        ("fax", TokenKind::Fax),
        ("cap", TokenKind::Cap),
        (r#""string literal""#, TokenKind::StringLiteral),
        ("'c'", TokenKind::CharLiteral),
        ("nuww", TokenKind::Nuww),
        (">_< single line comment", TokenKind::Comment),
    ])
}

#[test]
fn individual_tokens() {
    let tokens = uwu_tokens();
    for (text, kind) in tokens.clone() {
        let mut l = Lexer::new(text);
        l.tokenize();
        assert!(l.errors.len() == 0, "{}", l.errors.join("\n"));
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
                    range: (0, text.len() - 1)
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
}
#[test]
fn individual_tokens_delimited_by_whitespaces() {
    let tokens = uwu_tokens();
    for whitespace in vec![" ", "\n", "\t", "\r"] {
        for (text, kind) in tokens.clone() {
            let new_source = text.to_owned() + whitespace;
            let mut l = Lexer::new(Box::leak(new_source.clone().into_boxed_str()));
            l.tokenize();
            l.pretty_print_tokens();
            assert!(l.errors.len() == 0);
            assert!(
                l.tokens.len()
                    == match kind {
                        TokenKind::Comment => match whitespace {
                            " " | "\t" => 1,
                            _ => 2,
                        },
                        _ => 2,
                    },
                "Expected {}\nGot {}",
                match kind {
                    TokenKind::Comment => match whitespace {
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
                        TokenKind::Comment => match whitespace {
                            " " | "\t" => Box::leak(new_source.into_boxed_str()),
                            _ => text,
                        }
                        _ => text,
                    },
                    pos: (0, 0),
                    end_pos: (0,
                        match kind {
                        TokenKind::Comment => match whitespace {
                            " " | "\t" => text.len(),
                            _ => text.len() - 1,
                        }
                        _ => text.len() - 1,
                    }),
                    range: (0, text.len()-1),
                },
                "Expected:\n\tkind: '{}', text: '{}', pos: {:?}, end_pos: {:?}\nGot\n\tkind: '{}', text: '{}', pos: {:?}, end_pos: {:?}\n\nSource: {}",
                kind,
                text,
                (0, 0),
                (0, match kind {
                    TokenKind::Comment => match whitespace {
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
fn identifiers_with_reserved_words_in_the_name() {
    let tokens = uwu_tokens()
        .into_iter()
        .filter(|tok| {
            tok.0
                .starts_with(|ch: char| ch.is_ascii_alphabetic() && ch.is_ascii_lowercase())
        })
        .map(|(tok, _)| (tok.to_string() + "a").leak())
        .collect::<Vec<_>>();
    for text in tokens {
        let new_source = text;
        let mut l = Lexer::new(new_source);
        l.tokenize();
        assert!(l.errors.len() == 0);
        assert!(l.tokens.len() == 1);
        assert!(
            l.tokens.first().unwrap().kind == TokenKind::Identifier,
            "Expected {}\nGot {}",
            TokenKind::Identifier,
            l.tokens.first().unwrap().kind
        );
    }
}
