use crate::{cli::commands::Command, utils::string::StringExt};

pub struct Version {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl Command for Version {
    fn new(arg: String, flags: Option<Vec<String>>) -> Self {
        Self { arg, flags }
    }
    fn help_msg(verbose: bool) {
        let mut title = "version".pad_right(16).fill_left(2).bold();
        if verbose {
            title = title.underline();
        }
        println!(
            "{}{}{}\n",
            title,
            "prints the current version of",
            " OwO".bold(),
        );
        if verbose {
            println!(
                "\n{}{}",
                "Usage:".pad_right(15).bold().underline().fill_left(2),
                "owo version",
            );
        }
    }
    fn validate(&mut self) -> Result<(), String> {
        match self.arg.as_str() {
            "" => Ok(()),
            _ => Err(format!("'version' takes no arguments, got '{}'", self.arg)),
        }
    }
    fn exec(&self) -> Result<(), String> {
        println!("{}", env!("CARGO_PKG_VERSION"));
        Ok(())
    }
}
