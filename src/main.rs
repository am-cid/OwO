mod lexer;

use lexer::lexer::*;

fn main() {
    let source: &'static str = "iwf\newse\newse iwf";
    let mut lexer = Lexer::new(source);
    println!("{}", lexer);
    assert_eq!(lexer.peek_str("iwx"), false);
    assert_eq!(lexer.peek_str("iwf"), true);
    lexer.advance(1);
    assert_eq!(lexer.peek_str("ewse"), true);
    lexer.advance(1);
    assert_eq!(lexer.peek_str("ewse iwf"), true);
    println!("{}", lexer);
}
