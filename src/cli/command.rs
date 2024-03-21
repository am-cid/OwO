pub trait Command {
    fn parse(&self) -> Result<(), String>;
    fn exec(&self) -> Result<(), String>;
}
