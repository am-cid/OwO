use crate::cli::commands::Command;

pub struct ParseCommand {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl Command for ParseCommand {
    fn help_msg(verbose: bool) {
        todo!()
    }
    fn parse(&self) -> Result<(), String> {
        todo!()
    }
    fn exec(&self) -> Result<(), String> {
        todo!()
    }
}
