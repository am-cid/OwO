mod lexer;

use lexer::lexer::*;

fn main() {
    let source: &'static str = "chan  chan chan \n chan";
    let mut lexer = Lexer::new(source);
    println!("{}", lexer);
    lexer.tokenize();
    println!("{}", lexer);
    println!("{:?}", lexer.tokens);
}
