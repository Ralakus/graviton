
use std::fs::*;
use std::io::Write;
use std::error::Error;

extern crate graviton;
use graviton::colored::*;
use graviton::errors;

use memmap::Mmap;

extern crate clap;
use clap::{Arg, App, SubCommand};

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

    let args = App::new("Graviton")
                .version(VERSION)
                .author(AUTHOR)
                .about(DESCRIPTION)
                .arg(Arg::with_name("Input")
                    .help("Input file to process")
                    .index(1))
                .arg(debug_arg.clone())
                .arg(Arg::with_name("no_run")
                    .help("Doesn't run inputted code")
                    .long("no_run"))
                .arg(Arg::with_name("emit")
                    .help("Emits the specified format [ast, bc, bytecode, none]")
                    .long("emit")
                    .short("e")
                    .takes_value(true))
                .arg(Arg::with_name("output")
                    .help("Specifies where to output compiled file or specified format")
                    .long("output")
                    .short("o")
                    .takes_value(true))
                .arg(Arg::with_name("type")
                    .help("Specifies the input format [src, source, ast, bc, bytecode]")
                    .long("type")
                    .short("t")
                    .takes_value(true))
                .subcommand(SubCommand::with_name("repl")
                    .about("Live REPL environment")
                    .version(VERSION)
                    .author(AUTHOR)
                    .arg(debug_arg))
                .get_matches();

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

    #[derive(PartialEq, Eq)]
    enum EmitType {
        Ast,
        Bytecode,
        None
    };

    let emit_type = match args.value_of("emit").unwrap_or("none") {
            "ast" => EmitType::Ast,
            "bytecode" | "bc" => EmitType::Bytecode,
            "none" => if let None = args.value_of("output") {
                    EmitType::None
                } else {
                    eprintln!("{}: Cannot emit nothing when output is specified, remove the \'o\', \"output\" argument", "Error".red());
                    return;
                },
            s => {
                eprintln!("{}: Invalid emit type {}", "Error".red(), s);
                return;
            }
        };

    #[derive(PartialEq, Eq)]
    enum InputType {
        Source,
        Ast,
        Bytecode,
    };

    let input_type = match args.value_of("type").unwrap_or("source") {
            "src" | "source" => InputType::Source,
            "ast" => if emit_type == EmitType::Ast {
                    eprintln!("{}: Input is already AST, rename file if a different name is desired", "Error".red());
                    return;
                } else {
                    InputType::Ast
                },
            "bytecode" | "bc" => {
                if emit_type == EmitType::Ast {
                    eprintln!("{}: Cannot emit AST from bytecode", "Error".red());
                    return;
                } else {
                    InputType::Bytecode
                }
            },
            s => {
                eprintln!("{}: Invalid input type {}", "Error".red(), s);
                return;
            }
        };

    let output: String = match args.value_of("output").unwrap_or(match emit_type {
            EmitType::Ast => "out.gast",
            EmitType::Bytecode | EmitType::None => "out.gbc",
        }) {
            s if !s.contains(".") => match emit_type {
                    EmitType::Ast => format!("{}.gast", s),
                    EmitType::Bytecode | EmitType::None => format!("{}.gbc", s)
                },
            s => s.to_string()
        };

    let debug_level = match args.value_of("Debug Level").unwrap_or("0") {
            "0" => 0,
            "1" => 1,
            "2" => 2,
            "3" => 3,
            _ => 0
        };

    let run_code = !args.is_present("no_run");

    let input = if let Some(input) = args.value_of("Input") {
        match input {
            s if !s.contains(".") => match input_type {
                    InputType::Source => format!("{}.grav", s),
                    InputType::Ast => format!("{}.gast", s),
                    InputType::Bytecode => format!("{}.gbc", s)
                },
            s => s.to_string()
        }
    } else {
        eprintln!("{}: Expects at least one argument for input", "Error".red());
        return;
    };
    let mapped_file: memmap::Mmap;

    let file = match File::open(input.as_str()) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("{}: {}: {}", "Error".red(), input, e.description());
            return;
        }
    };
    mapped_file = unsafe { Mmap::map(&file).expect("failed to map file") };

    let source = if input_type == InputType::Source { std::str::from_utf8(&mapped_file[..]).unwrap() } else { "" };

    let ast = if input_type == InputType::Source {
            graviton::frontend::parser::Parser::parse(source, Some(&*input))
        } else if input_type == InputType::Ast {
            Ok(rmp_serde::from_slice(&mapped_file[..]).unwrap())
        } else {
            let bc: graviton::backend::vm::Bytecode = rmp_serde::from_slice(&mapped_file[..]).unwrap();
            if debug_level >= 1 {
                println!("{:#?}", bc);
            }
            if run_code {
                let mut vm = graviton::backend::vm::StackVm::new();
                match vm.run(bc, debug_level) {
                    Ok(graviton::backend::vm::Value::Nil) => {},
                    Ok(result) => println!("=> {:?}", result),
                    Err(err) => errors::report_vm_error(&err, None, Some(input.as_str())),
                }
            }
            return;
        };

    match ast {
        Ok(a) => {
            if debug_level >= 2 {
                println!("{}\n{:#?}", "AST:".cyan(), a);
            }
            if emit_type == EmitType::Ast {
                let mut f = File::create(output).unwrap();
                f.write_all(&*rmp_serde::to_vec(&a).unwrap()).unwrap();
                return;
            }
            let bytecode = graviton::backend::vm::Bytecode::new(a);
            match bytecode {
                Ok(bc) => {
                    if debug_level >= 1 {
                        println!("{:#?}", bc);
                    }
                    if emit_type == EmitType::Bytecode {
                        let mut f = File::create(output).unwrap();
                        f.write_all(&*rmp_serde::to_vec(&bc).unwrap()).unwrap();
                        return;
                    }
                    if run_code {
                        let mut vm = graviton::backend::vm::StackVm::new();
                        match vm.run(bc, debug_level) {
                            Ok(graviton::backend::vm::Value::Nil) => {},
                            Ok(result) => println!("=> {:?}", result),
                            Err(err) => errors::report_vm_error(&err, if input_type == InputType::Source { Some(&*source) } else { None }, Some(input.as_str())),
                        }
                    }
                },
                Err(err) => errors::report_vm_error(&err, if input_type == InputType::Source { Some(&*source) } else { None }, Some(input.as_str())),
            };
        },
        Err(errors) => {
            for e in errors {
                errors::report_parser_error(&e, Some(source));
            }
        },
    };

}
