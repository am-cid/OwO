mod cli;
mod errors;
mod lexer;

use cli::command::{tokenize, Command};
use std::env;

fn main() {
    let os_args: Vec<String> = env::args().collect();
    tokenize(os_args)
        .and_then(|cmd| cmd.parse().map(move |_| cmd))
        .and_then(|cmd| cmd.exec())
        .unwrap_or_else(|e| println!("{}", e));
}
