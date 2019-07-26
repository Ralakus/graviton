
extern crate tachyon;

use std::io::BufRead;

fn main() {
    let mut source = String::new();
    std::io::stdin().lock().read_line(&mut source).expect("Oof");

    let ast = tachyon::frontend::parser::Parser::parse(source.as_str());

    println!("{:?}", ast);

}
