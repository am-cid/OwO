#![feature(let_chains, iter_advance_by)]
mod analyzer;
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

#[macro_export]
/// code that runs only on debug builds
macro_rules! debug_only {
    ($($block:tt)*) => {
        {
            #[cfg(debug_assertions)]
            {
                $($block)*
            }
            #[cfg(not(debug_assertions))]
            {}
        }
    };
}
