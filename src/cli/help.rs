use crate::cli::{
    analyze::AnalyzeCommand, commands::Command, compile::CompileCommand, lex::LexCommand,
    parse::ParseCommand, run::RunCommand, styling::StringExt, version::VersionCommand,
};

pub struct HelpCommand {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl Command for HelpCommand {
    fn help_msg(verbose: bool) {
        let mut title = "help".to_string().pad_right(15).fill_left(2).bold();
        if verbose {
            title = title.underline();
        }
        println!("{}{}", title, "prints this help message or the verbose");
        println!(
            "{}{}\n",
            "".to_string().pad_right(15).fill_left(2),
            "help message for a specific command if specified"
        );
        if verbose {
            println!("\n{}", "Usage:".to_string().bold().underline().fill_left(2));
            println!("{}", "owo help".to_string().fill_left(17));
            println!("{}", "owo help compile".to_string().fill_left(17));
            println!("{}", "owo help help".to_string().fill_left(17));
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
                    "{}",
                    "OwO Compiler for UwU++".to_string().bold().underline()
                );
                print!("{}", "Version".to_string().pad_right(16).bold().underline());
                println!("{}", env!("CARGO_PKG_VERSION"));
                println!(
                    "{} {} {} {}\n",
                    "Usage:".to_string().pad_right(16).bold().underline(),
                    "owo".to_string().bold(),
                    "<optional_command>".to_string().italic(),
                    "<optional_arg>".to_string().italic()
                );
                println!("{}: ", "Optional Commands".to_string().bold().underline());
                LexCommand::help_msg(false);
                ParseCommand::help_msg(false);
                AnalyzeCommand::help_msg(false);
                CompileCommand::help_msg(false);
                RunCommand::help_msg(false);
                VersionCommand::help_msg(false);
                Self::help_msg(false);
            }
            _ => {
                match self.arg.as_str() {
                    "help" => Self::help_msg(true),
                    "lex" => LexCommand::help_msg(true),
                    "parse" => ParseCommand::help_msg(true),
                    "analyze" => AnalyzeCommand::help_msg(true),
                    "compile" => CompileCommand::help_msg(true),
                    "run" => RunCommand::help_msg(true),
                    "version" => VersionCommand::help_msg(true),
                    _ => unreachable!(),
                };
            }
        }
        Ok(())
    }
}
