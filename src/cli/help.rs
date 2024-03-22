use crate::{
    cli::{
        analyze::Analyze, commands::Command, compile::Compile, lex::Lex, parse::Parse, run::Run,
        version::Version,
    },
    utils::string::StringExt,
};

pub struct Help {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl Command for Help {
    fn help_msg(verbose: bool) {
        let mut title = "help".to_string().pad_right(16).fill_left(2).bold();
        if verbose {
            title = title.underline();
        }
        println!("{}{}", title, "prints this help message or the verbose");
        println!(
            "{}{}\n",
            "".to_string().pad_right(16).fill_left(2),
            "help message for a specific command if specified"
        );
        if verbose {
            println!("{}", "Usage:".to_string().bold().underline().fill_left(2));
            println!("{}", "owo help".to_string().fill_left(18));
            println!("{}", "owo help compile".to_string().fill_left(18));
            println!("{}", "owo help help".to_string().fill_left(18));
        }
    }
    fn parse(&self) -> Result<(), String> {
        match self.arg.as_str() {
            "" | "help" | "lex" | "parse" | "analyze" | "compile" | "run" | "version" => Ok(()),
            _ => Err(format!(
                "'{}' is not a valid arg for 'help'. See 'help help' for more information",
                self.arg
            )),
        }
    }
    fn exec(&self) -> Result<(), String> {
        // H = cursor to top left, 2J = clear screen
        print!("\x1B[H\x1B[2J");
        match self.arg.as_str() {
            "" => {
                println!(
                    "{}Compiler for UwU++",
                    "OwO".to_string().pad_right(18).bold().underline()
                );
                print!(
                    "{}",
                    "Version:".to_string().pad_right(18).bold().underline()
                );
                println!("{}", env!("CARGO_PKG_VERSION"));
                println!(
                    "{}{} {} {}\n",
                    "Usage:".to_string().pad_right(18).bold().underline(),
                    "owo".to_string().bold(),
                    "<optional_command>".to_string().italic(),
                    "<optional_arg>".to_string().italic()
                );
                println!("{}: ", "Optional Commands".to_string().bold().underline());
                Self::help_msg(false);
                Version::help_msg(false);
                Lex::help_msg(false);
                Parse::help_msg(false);
                Analyze::help_msg(false);
                Compile::help_msg(false);
                Run::help_msg(false);
                Self::help_msg(false);
            }
            _ => {
                match self.arg.as_str() {
                    "help" => Self::help_msg(true),
                    "lex" => Lex::help_msg(true),
                    "parse" => Parse::help_msg(true),
                    "analyze" => Analyze::help_msg(true),
                    "compile" => Compile::help_msg(true),
                    "run" => Run::help_msg(true),
                    "version" => Version::help_msg(true),
                    _ => unreachable!(),
                };
            }
        }
        Ok(())
    }
}
