use crate::cli::command::Command;

pub struct CompileCommand {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl Command for CompileCommand {
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
