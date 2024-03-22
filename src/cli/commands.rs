use crate::cli::{
    analyze::Analyze, compile::Compile, help::Help, lex::Lex, parse::Parse, run::Run,
    version::Version,
};

pub trait Command {
    fn help_msg(verbose: bool)
    where
        Self: Sized;
    fn parse(&self) -> Result<(), String>;
    fn exec(&self) -> Result<(), String>;
}
// Put commands here
pub fn to_command(
    name: String,
    arg: String,
    flags: Option<Vec<String>>,
) -> Result<Box<dyn Command>, String> {
    match name.as_str() {
        "help" | "" => Ok(Box::new(Help { arg, flags })),
        "version" => Ok(Box::new(Version { arg, flags })),
        "lex" => Ok(Box::new(Lex { arg, flags })),
        "parse" => Ok(Box::new(Parse { arg, flags })),
        "analyze" => Ok(Box::new(Analyze { arg, flags })),
        "compile" => Ok(Box::new(Compile { arg, flags })),
        "run" => Ok(Box::new(Run { arg, flags })),
        _ => Err(format!("Unknown command {}", name)),
    }
}
pub fn tokenize(args: Vec<String>) -> Result<Box<dyn Command>, String> {
    let mut args_iter = args.into_iter();
    let command = args_iter.next().unwrap_or("".to_string());
    let arg = args_iter.next().unwrap_or("".to_string());
    let flags = match args_iter.len() {
        0 => None,
        _ => Some(args_iter.collect()),
    };
    to_command(command, arg, flags)
}
