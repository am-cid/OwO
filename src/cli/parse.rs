use std::{
    fs,
    path::{Path, PathBuf},
};

use crate::{
    cli::commands::Command,
    lexer::{
        lexer::Lexer,
        token::{Token, TokenKind},
    },
    parser::{parser::Parser, productions::Production},
    utils::{path::PathExt, string::StringExt},
};

pub struct Parse {
    arg: String,
    path: PathBuf,
    flags: Option<Vec<String>>,
}
impl Command for Parse {
    fn new(arg: String, flags: Option<Vec<String>>) -> Self {
        Self {
            arg,
            path: "".into(),
            flags,
        }
    }
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
    fn validate(&mut self) -> Result<(), String> {
        self.path = Path::new(&self.arg)
            .canonicalize()
            .map_err(|_| format!("Failed to canonicalize path: '{}'", self.arg))?
            .must_be_file()?;
        self.path
            .extension()
            .map_or(false, |ext| ext == "uwu")
            .then(|| ())
            .ok_or(format!("\"{}\" is not a .uwu file", self.arg))?;
        Ok(())
    }
    fn exec(&self) -> Result<(), String> {
        let source: String = fs::read_to_string(self.path.as_path()).unwrap_or_default();
        let lexer = Lexer::new(source);
        lexer.debug_tokens();
        if lexer.errors.len() > 0 {
            lexer
                .errors
                .into_iter()
                .for_each(|err| println!("{}", err.source_string(&lexer.source)));
            return Err(format!("Failed to tokenize file: {}", self.path.display()));
        }
        let mut p = Parser::new(
            &lexer.source,
            &lexer.line_starts,
            lexer
                .tokens
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
                .collect::<Vec<Token>>(),
        );
        let _ = p.parse_program();
        println!("{}", p.program.to_formatted_string(&lexer.source, 0));
        p.errors
            .iter()
            .for_each(|err| println!("{}", err.to_string()));
        if p.errors.len() > 0 {
            println!("Failed to parse file: {}", self.path.display());
        }
        Ok(())
    }
}
