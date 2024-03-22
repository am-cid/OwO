use std::{
    fs,
    path::{Path, PathBuf},
};

use crate::cli::{commands::CommandType, styling::StringExt};
use crate::lexer::lexer::Lexer;

pub struct LexCommand {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl CommandType for LexCommand {
    fn help_msg(verbose: bool) {
        let mut title = "lex".to_string().pad_right(15).fill_left(2).bold();
        if verbose {
            title = title.underline();
        }
        println!("{}{}", title, "Tokenizes a selected source file");
        if verbose {
            println!("\n{}", "Usage:".to_string().bold().underline().fill_left(2));
            println!("{}", "owo lex path/to/source.uwu".to_string().fill_left(17));
        }
    }
    fn parse(&self) -> Result<(), String> {
        // check if abs path exists
        Path::new(&self.arg)
            .canonicalize()
            .map_err(|_| format!("Failed to canonicalize path: '{}'", self.arg))?
            .must_be_file()?
            .display()
            .to_string()
            .ends_with(".uwu")
            .then(|| ())
            .ok_or(format!("\"{}\" is not a .uwu file", self.arg))
    }
    fn exec(&self) -> Result<(), String> {
        let abs_path = Path::new(&self.arg)
            .canonicalize()
            .map_err(|_| format!("Failed to canonicalize path: '{}'", self.arg))?
            .must_be_file()?
            .display()
            .to_string();
        let trimmed_path = abs_path.strip_prefix(r#"\\?\"#).unwrap_or(&abs_path);
        // read file
        let source = Box::leak(
            fs::read_to_string(trimmed_path)
                .unwrap_or("".to_string())
                .into_boxed_str(),
        );
        let mut l = Lexer::new(source);
        l.tokenize();
        for token in l.tokens {
            println!("{:?}", token);
        }
        Ok(())
    }
}

trait PathExt {
    fn must_be_file(&self) -> Result<PathBuf, String>;
}
impl PathExt for Path {
    fn must_be_file(&self) -> Result<PathBuf, String> {
        match self.is_file() {
            true => Ok(self.to_path_buf()),
            false => {
                let abs_path = self.display().to_string();
                let trimmed_path = abs_path.strip_prefix(r#"\\?\"#).unwrap_or(&abs_path);

                Err(format!("\"{}\" is not a file", trimmed_path))
            }
        }
    }
}
