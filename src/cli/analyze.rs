use std::path::Path;

use crate::{cli::commands::Command, utils::path::PathExt};

pub struct Analyze {
    pub arg: String,
    pub flags: Option<Vec<String>>,
}
impl Command for Analyze {
    fn help_msg(verbose: bool) {
        todo!()
    }
    fn parse(&self) -> Result<(), String> {
        Path::new(&self.arg)
            .canonicalize()
            .map_err(|_| format!("Failed to canonicalize path: '{}'", self.arg))?
            .must_be_file()?
            .extension()
            .map_or(false, |ext| ext == "uwu")
            .then(|| ())
            .ok_or(format!("\"{}\" is not a .uwu file", self.arg))
    }
    fn exec(&self) -> Result<(), String> {
        todo!()
    }
}
