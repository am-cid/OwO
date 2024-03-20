mod errors;
mod lexer;

use lexer::lexer::*;

fn main() {
    let source: &'static str = "chan| \"123|1.23?";
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
