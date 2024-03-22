use crate::cli::{commands::Command, styling::StringExt};

pub struct Version {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl Command for Version {
    fn help_msg(verbose: bool) {
        let mut title = "version".to_string().pad_right(16).fill_left(2).bold();
        if verbose {
            title = title.underline();
        }
        println!(
            "{}{}{}",
            title,
            "prints the current version of",
            " OwO".to_string().bold()
        );
        if verbose {
            println!(
                "\n{}{}",
                "Usage:"
                    .to_string()
                    .pad_right(15)
                    .bold()
                    .underline()
                    .fill_left(2),
                "owo version".to_string()
            );
        }
    }
    fn parse(&self) -> Result<(), String> {
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
