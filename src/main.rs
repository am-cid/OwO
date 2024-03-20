mod errors;
mod lexer;

use lexer::lexer::*;

fn main() {
    let source: &'static str = "_ ! _ chan|";
    let mut lexer = Lexer::new(source);
    println!("{}", source);
    println!("{}", lexer);
    lexer.tokenize();
    println!("{}", lexer);
    for token in lexer.tokens {
        println!("{:?}", token);
    }
    for error in lexer.errors {
        println!("{}", error);
    }
}
