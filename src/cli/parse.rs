use std::{fs, path::Path};

use crate::{
    cli::commands::Command,
    lexer::{lexer::Lexer, token::TokenKind},
    parser::{parser::Parser, productions::Production},
    utils::{path::PathExt, string::StringExt},
};

pub struct Parse {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl Command for Parse {
    fn help_msg(verbose: bool) {
        let mut title = "parse".pad_right(16).fill_left(2).bold();
        if verbose {
            title = title.underline();
        }
        println!(
            "{}{}\n{}\n",
            title,
            "Parses a selected source file.",
            "Outputs an abstract syntax tree (AST).".fill_left(18),
        );
        if verbose {
            println!(
                "\n{}{}",
                "Usage:".bold().underline().fill_left(2),
                "owo parse path/to/source.uwu".fill_left(10),
            );
        }
    }
    fn parse(&self) -> Result<(), String> {
        Path::new(&self.arg)
            .canonicalize()
            .map_err(|_| format!("Failed to canonicalize path: '{}'", self.arg))?
            .must_be_file()?
            .extension()
            .map_or(false, |ext| ext == "uwu")
            .then(|| ())
            .ok_or(format!("\"{}\" is not a .uwu file", self.arg))
    }
    fn exec(&self) -> Result<(), String> {
        let abs_path = Path::new(&self.arg)
            .canonicalize()
            .expect("This was already validated. Something else went wrong!")
            .display()
            .to_string();
        let trimmed_path = abs_path.strip_prefix(r#"\\?\"#).unwrap_or(&abs_path);
        // read file
        let source = Box::leak(
            fs::read_to_string(trimmed_path)
                .unwrap_or_default()
                .into_boxed_str(),
        );
        let mut l = Lexer::new(source);
        l.tokenize();
        l.pretty_print_tokens();
        if l.errors.len() > 0 {
            l.errors.into_iter().for_each(|err| println!("{}", err));
            return Err(format!("Failed to tokenize file: {}", trimmed_path));
        }
        let mut p = Parser::new(
            source,
            l.tokens
                .into_iter()
                .filter(|tok| match tok.kind {
                    TokenKind::Whitespace
                    | TokenKind::Newline
                    | TokenKind::Tab
                    | TokenKind::CarriageReturn
                    | TokenKind::Comment
                    | TokenKind::EOF => false,
                    _ => true,
                })
                .collect::<Vec<_>>(),
        );
        match p.parse_program() {
            Ok(()) => Ok(println!("{}", p.program.string(0))),
            Err(()) => {
                println!("{}", p.error);
                Err(format!("Failed to parse file: {}", trimmed_path))
            }
        }
    }
}
