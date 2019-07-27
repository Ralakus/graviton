
extern crate tachyon;

extern crate colored;
use colored::*;

use std::env;
use std::fs::File;

use memmap::Mmap;

use std::io::BufRead;

fn main() {

    if env::args().len() > 1 {
        let path = env::args()
            .nth(1)
            .expect("supply a single path as the program argument");

        let file = File::open(path).expect("failed to open file");
        let mapped_file = unsafe { Mmap::map(&file).expect("failed to map file") };

        let source = std::str::from_utf8(&mapped_file[..]).unwrap();
        println!("{}\n{}", "Source:".cyan(), source);

        let ast = tachyon::frontend::parser::Parser::parse(source);

        match ast {
            Ok(a) => println!("{}\n{:#?}", "AST:".cyan(), a),
            Err(e) => {
                println!("{}", String::from("-").repeat(16).yellow());
                let mut line = 1;
                for l in source.lines() {
                    if line == e.pos.line {
                        println!("{}\n{}{}{}\n{} at {:?}",
                            l,
                            String::from("~").repeat((e.pos.col - 1) as usize).red(),
                            "^".red(),
                            String::from("~").repeat(l.len() - e.pos.col as usize ).red(),
                            e.msg,
                            e.pos
                        );

                    }
                    line += 1;
                }
                println!("{}", String::from("-").repeat(16).yellow());
            },
        };
    } else {
        let mut source = String::new();
        std::io::stdin().lock().read_line(&mut source).expect("Oof");

        println!("{}\n{}", "Source:".cyan(), source);

        let ast = tachyon::frontend::parser::Parser::parse(source.as_str());

        match ast {
            Ok(a) => println!("{}\n{:#?}", "AST:".cyan(), a),
            Err(e) => {
                println!("{}", String::from("-").repeat(16).yellow());
                let mut line = 1;
                for l in source.lines() {
                    if line == e.pos.line {
                        println!("{}\n{}{}{}\n{} at {:?}",
                            l,
                            String::from("~").repeat((e.pos.col - 1) as usize).red(),
                            "^".red(),
                            String::from("~").repeat(l.len() - e.pos.col as usize ).red(),
                            e.msg,
                            e.pos
                        );

                    }
                    line += 1;
                }
                println!("{}", String::from("-").repeat(16).yellow());
            },
        };
    }

}
