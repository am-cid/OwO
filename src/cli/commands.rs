use crate::cli::help;

// Put commands here
fn to_command(
    name: String,
    arg: String,
    flags: Option<Vec<String>>,
) -> Result<impl Command, String> {
    match name.as_str() {
        "help" | "" => Ok(help::HelpCommand { arg, flags }),
        "version" => todo!(),
        "lex" => todo!(),
        "parse" => todo!(),
        "analyze" => todo!(),
        "compile" => todo!(),
        "run" => todo!(),
        _ => Err(format!("Unknown command {}", name)),
    }
}
pub fn tokenize(args: Vec<String>) -> Result<impl Command, String> {
    let mut args_iter = args.into_iter();
    let command = args_iter.next().unwrap_or("".to_string());
    let arg = args_iter.next().unwrap_or("".to_string());
    let flags = match args_iter.len() {
        0 => None,
        _ => Some(args_iter.collect()),
    };
    to_command(command, arg, flags)
}
pub trait Command {
    fn help_msg(verbose: bool);
    fn parse(&self) -> Result<(), String>;
    fn exec(&self) -> Result<(), String>;
}
