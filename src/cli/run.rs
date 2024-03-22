use crate::{
    cli::commands::Command,
    utils::{path::PathExt, string::StringExt},
};

use std::path::Path;

pub struct Run {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl Command for Run {
    fn help_msg(verbose: bool) {
        let mut title = "run".to_string().pad_right(16).fill_left(2).bold();
        if verbose {
            title = title.underline();
        }
        println!(
            "{}{}\n{}\n",
            title,
            "Compiles a selected source file and runs it.",
            "Does not make an executable.".to_string().fill_left(18),
        );
        if verbose {
            println!(
                "\n{}{}",
                "Usage:".to_string().bold().underline().fill_left(2),
                "owo run path/to/source.uwu".to_string().fill_left(10),
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
        todo!()
    }
}
