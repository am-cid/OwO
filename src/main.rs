mod cli;
mod errors;
mod lexer;
mod parser;
#[cfg(test)]
mod tests;
mod utils;

fn main() {
    let os_args: Vec<String> = std::env::args().skip(1).collect();
    cli::commands::tokenize(os_args)
        .and_then(|cmd| cmd.parse().map(move |_| cmd))
        .and_then(|cmd| cmd.exec())
        .unwrap_or_else(|e| println!("{}", e));
}
