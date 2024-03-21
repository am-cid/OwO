use crate::cli::commands::Command;

pub struct AnalyzeCommand {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl Command for AnalyzeCommand {
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
