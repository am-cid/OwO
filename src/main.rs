mod cli;
mod errors;
mod lexer;
mod parser;
#[cfg(test)]
mod tests;
mod utils;

fn main() {
    cli::commands::parse_args(std::env::args().skip(1).collect())
        .and_then(|mut cmd| cmd.validate().map(move |_| cmd))
        .and_then(|cmd| cmd.exec())
        .unwrap_or_else(|e| println!("{e}"));
}
