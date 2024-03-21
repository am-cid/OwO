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
        todo!()
    }
    fn exec(&self) -> Result<(), String> {
        todo!()
    }
}
