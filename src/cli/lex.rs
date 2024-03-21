use crate::cli::commands::Command;

pub struct LexCommand {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl Command for LexCommand {
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
