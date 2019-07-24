

mod tokens;
mod lexer;
mod ast;
mod parser;

use std::io::{BufRead};

fn main() {
    let mut source = String::new();
    std::io::stdin().lock().read_line(&mut source).expect("Oof");

    let ast = parser::Parser::parse(source.as_str());

    println!("{:?}", ast);

}
