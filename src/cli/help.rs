use crate::cli::{analyze, commands::Command, compile, lex, parse, run, version};

pub struct HelpCommand {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl Command for HelpCommand {
    fn help_msg(verbose: bool) -> String {
        todo!()
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
        match self.arg.as_str() {
            "" | "help" => Self::help_msg(true),
            "lex" => lex::LexCommand::help_msg(true),
            "parse" => parse::ParseCommand::help_msg(true),
            "analyze" => analyze::AnalyzeCommand::help_msg(true),
            "compile" => compile::CompileCommand::help_msg(true),
            "run" => run::RunCommand::help_msg(true),
            "version" => version::VersionCommand::help_msg(true),
            _ => unreachable!(),
        };
        Ok(())
    }
}
