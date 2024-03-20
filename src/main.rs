mod lexer;

use lexer::lexer::*;

fn main() {
    let source: &'static str = "{ [[ ]] ( )  channel chan \n chan";
    let mut lexer = Lexer::new(source);
    println!("{}", source);
    println!("{}", lexer);
    lexer.tokenize();
    println!("{}", lexer);
    for token in lexer.tokens {
        println!("{:?}", token);
    }
}
