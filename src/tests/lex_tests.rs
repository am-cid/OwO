use crate::lexer::{
    lexer::Lexer,
    token::{Offset, Position, Token, TokenKind},
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
        ("+=", TokenKind::PlusEqual),
        ("-=", TokenKind::DashEqual),
        ("*=", TokenKind::MultiplyEqual),
        ("/=", TokenKind::DivideEqual),
        ("%=", TokenKind::ModuloEqual),
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
        let l = Lexer::new(text.to_string());
        assert!(
            l.errors.len() == 0,
            "unexpected errors:\n{}",
            l.errors
                .into_iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        );
        assert!(
            l.tokens.len() == 1,
            "Expected 1 token\nGot {}\nSource: {}\nTokens: {:?}",
            l.tokens.len(),
            text,
            l.tokens,
        );
        let token = l.tokens.first().unwrap();
        let expected_pos_end = (0, text.len() - 1);
        assert!(
            token.kind == kind
                && token.source_str(&l.source) == text
                && token.pos(&l.line_starts) == (0, 0)
                && token.pos_end(&l.line_starts) == expected_pos_end
                && token.offset.range() == (0..text.len()),
            "Expected:\n\t{:?}, {:?}, {:?}, {:?}, {:?}\nGot:\n\t{:?}, {:?}, {:?}, {:?}, {:?}",
            kind,
            text,
            (0, 0),
            expected_pos_end,
            (0..text.len()),
            l.tokens[0].kind,
            l.tokens[0].source_str(&l.source),
            l.tokens[0].pos(&l.line_starts),
            l.tokens[0].pos_end(&l.line_starts),
            l.tokens[0].offset.range(),
        );
    }
}

#[test]
fn individual_tokens_followed_by_whitespaces() {
    let tokens = uwu_tokens();
    for whitespace in vec![" ", "\n", "\t", "\r"] {
        for (text, kind) in tokens.clone() {
            let new_source = text.to_owned() + whitespace;
            let l = Lexer::new(new_source);
            l.debug_tokens();
            assert!(
                l.errors.len() == 0,
                "Expected 0\nGot {}: {:?}\ntext: {:?}",
                l.errors.len(),
                l.errors,
                l.source,
            );
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
            let token = l.tokens.first().unwrap();
            let expected_text = match (kind, whitespace) {
                (TokenKind::Comment, " " | "\t") => text.to_string() + &whitespace,
                _ => text.into(),
            };
            let expected_pos_end = (0, expected_text.len() - 1);
            assert!(
                token.kind == kind
                    && token.source_str(&l.source) == expected_text
                    && token.pos(&l.line_starts) == (0, 0)
                    && token.pos_end(&l.line_starts) == expected_pos_end
                    && token.offset.range() == (0..expected_text.len()),
                "Expected:\n\t{}, {:?}, {:?}, {:?}, {:?}\nGot:\n\t{}, {:?}, {:?}, {:?}, {:?}",
                kind,
                expected_text,
                (0, 0),
                expected_pos_end,
                (0..expected_text.len()),
                token.kind,
                token.source_str(&l.source),
                token.pos(&l.line_starts),
                token.pos_end(&l.line_starts),
                token.offset.range(),
            );
        }
    }
}

#[test]
fn individual_tokens_preceded_by_whitespaces() {
    let tokens = uwu_tokens();
    for whitespace in vec!["\n", " ", "\t", "\r"] {
        for (text, kind) in tokens.clone() {
            let new_source = whitespace.to_owned() + text;
            let l = Lexer::new(new_source);
            l.debug_tokens();
            assert!(
                l.errors.len() == 0,
                "Expected 0\nGot {}: {:?}\ntext: {:?}",
                l.errors.len(),
                l.errors,
                l.source,
            );
            assert!(l.tokens.len() == 2, "Expected 2\nGot {}", l.tokens.len(),);
            let token = l.tokens.last().unwrap();
            let expected_pos_start = match whitespace {
                "\n" => (1, 0),
                _ => (0, 1),
            };
            let expected_pos_end = match whitespace {
                "\n" => (1, text.len() - 1),
                _ => (0, text.len()),
            };
            assert!(
                token.kind == kind
                    && token.source_str(&l.source) == text
                    && token.pos(&l.line_starts) == expected_pos_start
                    && token.pos_end(&l.line_starts) == expected_pos_end
                    && token.offset.range() == (1..text.len() + 1),
                "Expected:\n\t{}, {:?}, {:?}, {:?}, {:?}\nGot:\n\t{}, {:?}, {:?}, {:?}, {:?}",
                kind,
                text,
                expected_pos_start,
                expected_pos_end,
                (1..text.len() + 1),
                token.kind,
                token.source_str(&l.source),
                token.pos(&l.line_starts),
                token.pos_end(&l.line_starts),
                token.offset.range(),
            );
        }
    }
}

#[test]
fn individual_tokens_followed_by_invalid_character() {
    let tokens = uwu_tokens();
    let invalid_character = "@";
    for (text, kind) in tokens.clone() {
        let new_source = text.to_owned() + invalid_character;
        let l = Lexer::new(new_source);
        l.debug_tokens();
        assert!(
            l.errors.len()
                == match kind {
                    TokenKind::Comment => 0,
                    _ => 1,
                },
            "Expected {}\nGot {}: {:?}\ntext: {:?}",
            match kind {
                TokenKind::Comment => 0,
                _ => 1,
            },
            l.errors.len(),
            l.errors,
            l.source,
        );
        assert!(l.tokens.len() == 1, "Expected 1\nGot {}", l.tokens.len(),);
        let token = l.tokens.first().unwrap();
        let expected = Token::new(
            kind,
            Offset::new(
                0,
                match kind {
                    TokenKind::Comment => text.len() + 1,
                    _ => text.len(),
                },
            ),
        );
        assert!(
            token.eq_all(&expected, &l.source),
            "Expected:\n\t{}, {}, {:?}, {:?}, {:?}\nGot:\n\t{}, {}, {:?}, {:?}, {:?}",
            expected.kind,
            expected.source_str(&l.source).to_string(),
            expected.pos(&l.line_starts),
            expected.pos_end(&l.line_starts),
            expected.offset.range(),
            token.kind,
            token.source_str(&l.source),
            token.pos(&l.line_starts),
            token.pos_end(&l.line_starts),
            token.offset.range(),
        );
    }
}

#[test]
fn individual_tokens_preceded_by_invalid_character() {
    let tokens = uwu_tokens();
    let invalid_character = "@";
    for (text, kind) in tokens.clone() {
        let new_source = invalid_character.to_owned() + text;
        let l = Lexer::new(new_source);
        l.debug_tokens();
        assert!(l.errors.len() == 1);
        assert!(l.tokens.len() == 1, "Expected 1\nGot {}", l.tokens.len(),);
        let token = l.tokens.last().unwrap();
        let expected = Token::new(kind, Offset::new(1, text.len() + 1));
        assert!(
            token.eq_all(&expected, &l.source),
            "Expected:\n\t{}, {}, {:?}, {:?}, {:?}\nGot:\n\t{}, {}, {:?}, {:?}, {:?}",
            expected.kind,
            expected.source_str(&l.source).to_string(),
            expected.pos(&l.line_starts),
            expected.pos_end(&l.line_starts),
            expected.offset.range(),
            token.kind,
            token.source_str(&l.source),
            token.pos(&l.line_starts),
            token.pos_end(&l.line_starts),
            token.offset.range(),
        );
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
        let l = Lexer::new(new_source.to_string());
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

#[test]
fn escaping_quotes_in_string() {
    let tokens = vec![r#""\"""#, r#""a\"""#, r#""\\\"!\\\"""#];
    for text in tokens {
        let l = Lexer::new(text.to_string());
        assert!(l.errors.len() == 0);
        assert!(l.tokens.len() == 1);
        assert!(
            l.tokens.first().unwrap().kind == TokenKind::StringLiteral,
            "Expected {}\nGot {}",
            TokenKind::StringLiteral,
            l.tokens.first().unwrap().kind
        );
        assert!(
            l.tokens.first().unwrap().source_str(&l.source) == text,
            "Expected {}\nGot {}",
            text,
            l.tokens.first().unwrap().source_str(&l.source)
        );
    }
}

#[test]
fn escaped_escaper_backslash_in_string() {
    let mut l = Lexer::new(r#""\\""""#.to_string());
    assert!(l.errors.len() == 0);
    assert!(l.tokens.len() == 2);
    let last_tok = l.tokens.pop().unwrap();
    let first_tok = l.tokens.pop().unwrap();
    assert!(
        first_tok.kind == TokenKind::StringLiteral,
        "Expected {}\nGot {}",
        TokenKind::StringLiteral,
        l.tokens.first().unwrap().kind
    );
    assert!(
        first_tok.source_str(&l.source) == r#""\\""#,
        "Expected {}\nGot {}",
        r#""\\""#,
        first_tok.source_str(&l.source)
    );
    assert!(
        last_tok.kind == TokenKind::StringLiteral,
        "Expected {}\nGot {}",
        TokenKind::StringLiteral,
        last_tok.kind
    );
    assert!(
        last_tok.source_str(&l.source) == r#""""#,
        "Expected {}\nGot {}",
        r#""""#,
        last_tok.source_str(&l.source)
    );
}

#[test]
fn lexer_errors() {
    let l = Lexer::new("@".to_string());
    assert!(l.errors.len() == 1);
    assert!(l.tokens.len() == 0);
    let first_err = l.errors.first().unwrap();
    assert!(
        first_err.to_string().starts_with("[UNKNOWN TOKEN '@']"),
        "Expected [UNKNOWN TOKEN '@'] error\nGot {}",
        first_err,
    );

    let l = Lexer::new(r#"""#.to_string());
    assert!(l.errors.len() == 1);
    assert!(l.tokens.len() == 0);
    let first_err = l.errors.first().unwrap();
    assert!(
        first_err.to_string().starts_with("[UNCLOSED STRING]"),
        "Expected [UNCLOSED STRING] error\nGot {}",
        first_err,
    );

    let l = Lexer::new("'".to_string());
    assert!(l.errors.len() == 1);
    assert!(l.tokens.len() == 0);
    let first_err = l.errors.first().unwrap();
    assert!(
        first_err.to_string().starts_with("[UNCLOSED CHARACTER]"),
        "Expected [UNCLOSED CHARACTER] error\nGot {}",
        first_err,
    );
}
