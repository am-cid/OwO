use std::path::{Path, PathBuf};

use crate::{
    cli::commands::Command,
    utils::{path::PathExt, string::StringExt},
};

pub struct Analyze {
    arg: String,
    path: PathBuf,
    flags: Option<Vec<String>>,
}
impl Command for Analyze {
    fn new(arg: String, flags: Option<Vec<String>>) -> Self {
        Self {
            arg,
            path: "".into(),
            flags,
        }
    }
    fn help_msg(verbose: bool) {
        let mut title = "analyze".pad_right(16).fill_left(2).bold();
        if verbose {
            title = title.underline();
        }
        println!(
            "{}{}\n{}{}\n",
            title,
            "Analyzes a selected source file for semantic errors.",
            "Outputs success/fail messages for each ".fill_left(18),
            "test case.".italic()
        );
        if verbose {
            println!(
                "{}\n{}\n{}",
                "The test cases are the ff:".fill_left(18),
                "- Member Check".fill_left(22),
                "- Type Check".fill_left(22),
            );
            print!(
                "\n{}{}",
                "Usage:".bold().underline().fill_left(2),
                "owo analyze path/to/source.uwu".fill_left(10)
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
        todo!()
    }
}
