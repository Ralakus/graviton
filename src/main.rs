
use std::env;
use std::fs::File;

extern crate tachyon;

extern crate colored;
use colored::*;

use memmap::Mmap;

extern crate clap;
use clap::{Arg, App, SubCommand};

mod errors;
mod repl;

const VERSION: &'static str = env!("CARGO_PKG_VERSION");
const AUTHOR: &'static str = env!("CARGO_PKG_AUTHORS");
const DESCRIPTION: &'static str = env!("CARGO_PKG_DESCRIPTION");

fn main() {

    let debug_arg = Arg::with_name("Debug Level")
                .help("Sets debug level to run the compiler in")
                .short("d")
                .long("debug")
                .takes_value(true);

    let args = App::new("Tachyon")
                .version(VERSION)
                .author(AUTHOR)
                .about(DESCRIPTION)
                .arg(Arg::with_name("Input")
                    .help("Input file to process")
                    .index(1))
                .arg(debug_arg.clone())
                .subcommand(SubCommand::with_name("repl")
                    .about("Live REPL environment")
                    .version(VERSION)
                    .author(AUTHOR)
                    .arg(debug_arg))
                .get_matches();

    let debug_level = match args.value_of("Debug Level").unwrap_or("0") {
            "0" => 0,
            "1" => 1,
            "2" => 2,
            "3" => 3,
            _ => 0
        };

    if let Some(repl_args) = args.subcommand_matches("repl") {
        let debug_level = match repl_args.value_of("Debug Level").unwrap_or("0") {
                "0" => 0,
                "1" => 1,
                "2" => 2,
                "3" => 3,
                _ => 0
            };

        repl::repl(debug_level).unwrap();

        return;
    }


    let input = args.value_of("Input").unwrap();
    let mapped_file: memmap::Mmap;

    let file = File::open(input).expect("failed to open file");
    mapped_file = unsafe { Mmap::map(&file).expect("failed to map file") };

    let source = std::str::from_utf8(&mapped_file[..]).unwrap();

    println!("{}\n{}", "Source:".cyan(), source);

    let ast = tachyon::frontend::parser::Parser::parse(source);

    match ast {
        Ok(a) => {
            if debug_level >= 2 {
                println!("{}\n{:#?}", "AST:".cyan(), a);
            }
        },
        Err(errors) => {
            for e in errors {
                errors::report_error(&e, Some(source), Some(input));
            }
        },
    };

}
