mod lexer;

use lexer::lexer::*;

fn main() {
    let source: &'static str = "iwf\newse\newse iwf";
    let mut lexer = Lexer::new(source);
    println!("{}", lexer);
}
