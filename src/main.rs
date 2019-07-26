
extern crate tachyon;

extern crate colored;
use colored::*;

use std::env;
use std::fs::File;

use memmap::Mmap;


fn main() {

    let path = env::args()
        .nth(1)
        .expect("supply a single path as the program argument");

    let file = File::open(path).expect("failed to open file");
    let mapped_file = unsafe { Mmap::map(&file).expect("failed to map file") };

    let source = std::str::from_utf8(&mapped_file[..]).unwrap();
    println!("{}\n{:#?}", "Source:".cyan(), source);

    let ast = tachyon::frontend::parser::Parser::parse(source);

    println!("{}\n{:#?}", "AST:".cyan(), ast);

}
