use std::path::{Path, PathBuf};

pub trait PathExt {
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
