use crate::cli::command::Command;

pub struct HelpCommand {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl Command for HelpCommand {
    fn parse(&self) -> Result<(), String> {
        todo!()
    }
    fn exec(&self) -> Result<(), String> {
        todo!()
    }
}
