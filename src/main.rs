mod cli;
mod errors;
mod lexer;

use cli::commands::tokenize;
use std::env;

fn main() {
    let os_args: Vec<String> = env::args().skip(1).collect();
    tokenize(os_args)
        .and_then(|cmd| cmd.parse().map(move |_| cmd))
        .and_then(|cmd| cmd.exec())
        .unwrap_or_else(|e| println!("{}", e));
}
