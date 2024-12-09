use std::{
    fs,
    path::{Path, PathBuf},
};

use crate::{
    cli::commands::Command,
    lexer::lexer::Lexer,
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
        let abs_path = Path::new(&self.arg)
            .canonicalize()
            .expect("This was already validated. Something else went wrong!")
            .display()
            .to_string();
        let trimmed_path = abs_path.strip_prefix(r#"\\?\"#).unwrap_or(&abs_path);
        let source: &'static str = fs::read_to_string(trimmed_path).unwrap_or_default().leak();
        println!(
            "source:\n{}\n{}\n{}",
            "-".repeat(
                source
                    .lines()
                    .map(|l| l.chars().count())
                    .max()
                    .unwrap_or_default()
            ),
            source,
            "-".repeat(
                source
                    .lines()
                    .map(|l| l.chars().count())
                    .max()
                    .unwrap_or_default()
            )
        );
        let mut l = Lexer::new(source);
        l.tokenize();
        l.pretty_print_tokens();
        if l.errors.len() > 0 {
            l.errors.into_iter().for_each(|err| println!("{}", err));
            Err(format!("Failed to retokenize file: {}", trimmed_path))
        } else {
            Ok(())
        }
        // // TEST
        // let mut l = Lexer::new("hi aqua-chan = 1~");
        // l.tokenize();
        // l.pretty_print_tokens();
        // l = l.retokenize("anatanoteki-desu", (3, 18));
        // l = l.retokenize(" = 2~", (19, 23));
        // l = l.retokenize(" >_< comment!", (24, 36));
        // l = l.retokenize("yo", (0, 1));
        // if l.errors.len() > 0 {
        //     l.errors.into_iter().for_each(|err| println!("{}", err));
        //     Err(format!("Failed to retokenize file: {}", trimmed_path))
        // } else {
        //     Ok(())
        // }
    }
}
