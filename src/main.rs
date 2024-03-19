mod lexer;

use lexer::lexer::*;

fn main() {
    let source: &'static str = "chan  channel chan \n chan";
    let mut lexer = Lexer::new(source);
    println!("{}", lexer);
    lexer.tokenize();
    println!("{}", lexer);
    for token in lexer.tokens {
        println!("{:?}", token);
    }
}
