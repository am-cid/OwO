use std::path::{Path, PathBuf};

pub trait PathExt {
    fn must_be_file(&self) -> Result<PathBuf, String>;
}
impl PathExt for Path {
    fn must_be_file(&self) -> Result<PathBuf, String> {
        match self.is_file() {
            true => Ok(self.to_path_buf()),
            false => Err(format!("\"{}\" is not a file", self.display())),
        }
    }
}
