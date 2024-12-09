use crate::cli::{
    analyze::Analyze, compile::Compile, help::Help, lex::Lex, parse::Parse, run::Run,
    version::Version,
};

/// owo cli command
pub trait Command {
    /// This should initialize other fields a command implementor have
    fn new(arg: String, flags: Option<Vec<String>>) -> Self
    where
        Self: Sized;
    /// Prints a short/verbose message depending on the verbose arg
    fn help_msg(verbose: bool)
    where
        Self: Sized;
    /// Should validate args and flags, then assign validated values to other fields (if applicable)
    fn validate(&mut self) -> Result<(), String>;
    /// Runs the functionality associated to the command
    fn exec(&self) -> Result<(), String>;
}
/// Tries to convert the passed in args to a [Command] implementor based on the name parameter.
/// Does not do any sort of validation
pub fn to_command(
    name: String,
    arg: String,
    flags: Option<Vec<String>>,
) -> Result<Box<dyn Command>, String> {
    match name.as_str() {
        "help" | "" => Ok(Box::new(Help::new(arg, flags))),
        "version" => Ok(Box::new(Version::new(arg, flags))),
        "lex" => Ok(Box::new(Lex::new(arg, flags))),
        "parse" => Ok(Box::new(Parse::new(arg, flags))),
        "analyze" => Ok(Box::new(Analyze::new(arg, flags))),
        "compile" => Ok(Box::new(Compile::new(arg, flags))),
        "run" => Ok(Box::new(Run::new(arg, flags))),
        _ => Err(format!("Unknown command {}", name)),
    }
}
/// collects the os args into easily parsable parts:
/// - the command
/// - the argument
/// - flags
/// in that order. This means that flags cannot come before the argument
pub fn parse_args(args: Vec<String>) -> Result<Box<dyn Command>, String> {
    let mut args_iter = args.into_iter();
    let command = args_iter.next().unwrap_or_default();
    let arg = args_iter.next().unwrap_or_default();
    let flags = match args_iter.len() {
        0 => None,
        _ => Some(args_iter.collect()),
    };
    to_command(command, arg, flags)
}
