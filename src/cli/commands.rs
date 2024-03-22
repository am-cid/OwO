use crate::cli::{
    analyze::AnalyzeCommand, compile::CompileCommand, help::HelpCommand, lex::LexCommand,
    parse::ParseCommand, run::RunCommand, version::VersionCommand,
};

pub trait CommandType {
    fn help_msg(verbose: bool);
    fn parse(&self) -> Result<(), String>;
    fn exec(&self) -> Result<(), String>;
}
// Put commands here
pub enum Command {
    Help(HelpCommand),
    Version(VersionCommand),
    Lex(LexCommand),
    Parse(ParseCommand),
    Analyze(AnalyzeCommand),
    Compile(CompileCommand),
    Run(RunCommand),
}
impl Command {
    pub fn tokenize(args: Vec<String>) -> Result<Command, String> {
        let mut args_iter = args.into_iter();
        let command = args_iter.next().unwrap_or("".to_string());
        let arg = args_iter.next().unwrap_or("".to_string());
        let flags = match args_iter.len() {
            0 => None,
            _ => Some(args_iter.collect()),
        };
        Command::new(command, arg, flags)
    }
    fn new(name: String, arg: String, flags: Option<Vec<String>>) -> Result<Self, String> {
        match name.as_str() {
            "help" | "" => Ok(Self::Help(HelpCommand { arg, flags })),
            "version" => Ok(Self::Version(VersionCommand { arg, flags })),
            "lex" => Ok(Self::Lex(LexCommand { arg, flags })),
            "parse" => Ok(Self::Parse(ParseCommand { arg, flags })),
            "analyze" => Ok(Self::Analyze(AnalyzeCommand { arg, flags })),
            "compile" => Ok(Self::Compile(CompileCommand { arg, flags })),
            "run" => Ok(Self::Run(RunCommand { arg, flags })),
            _ => Err(format!("Unknown command {}", name)),
        }
    }
    pub fn parse(&self) -> Result<(), String> {
        match self {
            Self::Help(cmd) => cmd.parse(),
            Self::Version(cmd) => cmd.parse(),
            Self::Lex(cmd) => cmd.parse(),
            Self::Parse(cmd) => cmd.parse(),
            Self::Analyze(cmd) => cmd.parse(),
            Self::Compile(cmd) => cmd.parse(),
            Self::Run(cmd) => cmd.parse(),
        }
    }
    pub fn exec(&self) -> Result<(), String> {
        match self {
            Self::Help(cmd) => cmd.exec(),
            Self::Version(cmd) => cmd.exec(),
            Self::Lex(cmd) => cmd.exec(),
            Self::Parse(cmd) => cmd.exec(),
            Self::Analyze(cmd) => cmd.exec(),
            Self::Compile(cmd) => cmd.exec(),
            Self::Run(cmd) => cmd.exec(),
        }
    }
}
