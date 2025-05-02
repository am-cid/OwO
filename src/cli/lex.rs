use std::{
    fs,
    path::{Path, PathBuf},
};

use crate::{
    cli::commands::Command,
    lexer::{lexer::Lexer, token::Offset},
    utils::{path::PathExt, string::StringExt},
};

pub struct Lex {
    arg: String,
    path: PathBuf,
    flags: Option<Vec<String>>,
}
impl Command for Lex {
    fn new(arg: String, flags: Option<Vec<String>>) -> Self {
        Self {
            arg,
            path: "".into(),
            flags,
        }
    }
    fn help_msg(verbose: bool) {
        let title = "lex".pad_right(16).fill_left(2).bold();
        println!(
            "{}{}\n{}",
            if verbose { title.underline() } else { title },
            "Tokenizes a selected source file.",
            "Outputs a list of token objects.".fill_left(18),
        );
        if verbose {
            println!(
                "\n{}{}",
                "Usage:".bold().underline().fill_left(2),
                "owo lex path/to/source.uwu".fill_left(10),
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
        let abs_path = self.path.display().to_string();
        let trimmed_path = abs_path.strip_prefix(r#"\\?\"#).unwrap_or(&abs_path);
        let source: String = fs::read_to_string(trimmed_path).unwrap_or_default();
        let max_line_len = source
            .lines()
            .map(|l| l.chars().count())
            .max()
            .unwrap_or_default();
        println!(
            "source:\n{border}\n{source}\n{border}\n",
            border = "-".repeat(max_line_len),
        );
        let lexer = Lexer::new(source);
        lexer.debug_tokens();
        if lexer.errors.len() > 0 {
            for error in lexer.errors {
                println!("{}", error);
            }
        }
        Ok(())
    }
}
