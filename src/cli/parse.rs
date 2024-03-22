use std::path::Path;

use crate::{
    cli::commands::Command,
    utils::{path::PathExt, string::StringExt},
};

pub struct Parse {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl Command for Parse {
    fn help_msg(verbose: bool) {
        let mut title = "parse".to_string().pad_right(16).fill_left(2).bold();
        if verbose {
            title = title.underline();
        }
        println!("{}{}", title, "Parses a selected source file.");
        println!(
            "{}{}\n",
            "".to_string().pad_right(16).fill_left(2),
            "Outputs an abstract syntax tree (AST)."
        );
        if verbose {
            println!("\n{}", "Usage:".to_string().bold().underline().fill_left(2));
            println!("{}", "owo lex path/to/source.uwu".to_string().fill_left(17));
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
        todo!()
    }
}
