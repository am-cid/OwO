use crate::cli::commands::CommandType;

pub struct CompileCommand {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl CommandType for CompileCommand {
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
