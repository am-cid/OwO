use crate::cli::command::Command;

pub struct VersionCommand {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl Command for VersionCommand {
    fn help_msg(&self, verbose: bool) -> String {
        todo!()
    }
    fn parse(&self) -> Result<(), String> {
        todo!()
    }
    fn exec(&self) -> Result<(), String> {
        todo!()
    }
}
