use std::error::Error;
use std::fs::*;
use std::io::Write;

extern crate graviton;
use graviton::colored::*;
use graviton::errors;

use memmap::Mmap;

extern crate clap;
use clap::{App, Arg, SubCommand};

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
        .arg(
            Arg::with_name("Input")
                .help("Input file to process")
                .index(1),
        )
        .arg(debug_arg.clone())
        .arg(
            Arg::with_name("no_run")
                .help("Doesn't run inputted code")
                .long("no_run"),
        )
        .arg(
            Arg::with_name("emit")
                .help("Emits the specified format [ast, bc, bytecode, none]")
                .long("emit")
                .short("e")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("output")
                .help("Specifies where to output compiled file or specified format")
                .long("output")
                .short("o")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("type")
                .help("Specifies the input format [src, source, ast, bc, bytecode]")
                .long("type")
                .short("t")
                .takes_value(true),
        )
        .subcommand(
            SubCommand::with_name("repl")
                .about("Live REPL environment")
                .version(VERSION)
                .author(AUTHOR)
                .arg(debug_arg),
        )
        .get_matches();

    if let Some(repl_args) = args.subcommand_matches("repl") {
        let debug_level = match repl_args.value_of("Debug Level").unwrap_or("0") {
            "0" => 0,
            "1" => 1,
            "2" => 2,
            "3" => 3,
            _ => 0,
        };

        repl::repl(debug_level).unwrap();

        return;
    }

    #[derive(PartialEq, Eq)]
    enum EmitType {
        Ast,
        Bytecode,
        None,
    };

    let emit_type = match args.value_of("emit").unwrap_or("none") {
        "ast" => EmitType::Ast,
        "bytecode" | "bc" => EmitType::Bytecode,
        "none" => {
            if let None = args.value_of("output") {
                EmitType::None
            } else {
                eprintln!("{}: Cannot emit nothing when output is specified, remove the \'o\', \"output\" argument", "Error".red());
                std::process::exit(1);
            }
        }
        s => {
            eprintln!("{}: Invalid emit type {}", "Error".red(), s);
            std::process::exit(1);
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
        "ast" => {
            if emit_type == EmitType::Ast {
                eprintln!(
                    "{}: Input is already AST, rename file if a different name is desired",
                    "Error".red()
                );
                std::process::exit(1);
            } else {
                InputType::Ast
            }
        }
        "bytecode" | "bc" => {
            if emit_type == EmitType::Ast {
                eprintln!("{}: Cannot emit AST from bytecode", "Error".red());
                std::process::exit(1);
            } else {
                InputType::Bytecode
            }
        }
        s => {
            eprintln!("{}: Invalid input type {}", "Error".red(), s);
            return;
        }
    };

    let output: String = match args.value_of("output").unwrap_or(match emit_type {
        EmitType::Ast => "out.gast",
        EmitType::Bytecode | EmitType::None => "out.gabc",
    }) {
        s if !s.contains(".") => match emit_type {
            EmitType::Ast => format!("{}.gast", s),
            EmitType::Bytecode | EmitType::None => format!("{}.gabc", s),
        },
        s => s.to_string(),
    };

    let debug_level = match args.value_of("Debug Level").unwrap_or("0") {
        "0" => 0,
        "1" => 1,
        "2" => 2,
        "3" => 3,
        _ => 0,
    };

    let run_code = !args.is_present("no_run");

    let input = if let Some(input) = args.value_of("Input") {
        match input {
            s if !s.contains(".") => match input_type {
                InputType::Source => format!("{}.grav", s),
                InputType::Ast => format!("{}.gast", s),
                InputType::Bytecode => format!("{}.gabc", s),
            },
            s => s.to_string(),
        }
    } else {
        eprintln!("{}: Expects at least one argument for input", "Error".red());
        std::process::exit(1);
    };
    let mapped_file: memmap::Mmap;

    let file = match File::open(input.as_str()) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("{}: {}: {}", "Error".red(), input, e.description());
            std::process::exit(1);
        }
    };
    mapped_file = unsafe { Mmap::map(&file).expect("failed to map file") };

    let source = if input_type == InputType::Source {
        std::str::from_utf8(&mapped_file[..]).unwrap()
    } else {
        ""
    };

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
                Ok(graviton::backend::vm::Value::Nil) => {}
                Ok(result) => println!("=> {:?}", result),
                Err(err) => errors::report_vm_error(&err, None, Some(input.as_str())),
            }
        }
        return;
    };

    match ast {
        Ok(mut a) => {
            match graviton::ast::semantic::SemanticAnalyzer::analyze(
                &mut a,
                Some(graviton::backend::vm::stdlib::get_stdlib_signatures()),
            ) {
                Ok(_) => {
                    if debug_level >= 2 {
                        println!("{}\n{:#?}", "Typed AST:".cyan(), a);
                    }

                    if emit_type == EmitType::Ast {
                        let mut f = File::create(output).unwrap();
                        f.write_all(&*rmp_serde::to_vec(&a).unwrap()).unwrap();
                        return;
                    }

                    // Link with musl `ld -o grav -s grav.o /usr/lib64/musl/crt1.o /usr/lib64/musl/crti.o /usr/lib64/musl/crtn.o /usr/lib64/musl/libc.a`
                    let result =
                        graviton::backend::native::Native::compile(String::from("grav"), &a)
                            .unwrap();
                    let file = std::fs::File::create("grav.o").unwrap();
                    result.artifact.write(file).unwrap();

                    if !cfg!(windows) && debug_level >= 3 {
                        std::process::Command::new("objdump")
                            .arg("-M")
                            .arg("intel")
                            .arg("-d")
                            .arg("grav.o")
                            .spawn()
                            .unwrap()
                            .wait()
                            .unwrap();
                    }

                    std::process::Command::new("cc")
                            .arg("grav.o")
                            .arg("-o")
                            .arg("grav")
                            .spawn()
                            .unwrap()
                            .wait()
                            .unwrap();
                    
                    /*std::process::Command::new("ld")
                        .arg("-o")
                        .arg("grav")
                        .arg("grav.o")
                        .arg("/usr/lib64/musl/crt1.o")
                        .arg("/usr/lib64/musl/crti.o")
                        .arg("/usr/lib64/musl/crtn.o")
                        .arg("/usr/lib64/musl/libc.a")
                        .spawn()
                        .unwrap()
                        .wait()
                        .unwrap();*/

                    if run_code {
                        println!("Running program...");
                        let exit_code = std::process::Command::new("./grav")
                            .spawn()
                            .unwrap()
                            .wait()
                            .unwrap()
                            .code();
                        if let Some(code) = exit_code {
                            println!("Exit code: {}", code);
                        }

                        std::process::Command::new("rm")
                            .arg("grav.o")
                            .arg("grav")
                            .spawn()
                            .unwrap()
                            .wait()
                            .unwrap();
                    } else {
                        std::process::Command::new("rm")
                            .arg("grav.o")
                            .spawn()
                            .unwrap()
                            .wait()
                            .unwrap();
                    }
                    
                }
                Err(errors) => {
                    if debug_level >= 2 {
                        println!("{}\n{:#?}", "Untyped AST:".cyan(), a);
                    }
                    for e in errors {
                        errors::report_semantic_error(
                            &e,
                            if input_type == InputType::Source {
                                Some(&*source)
                            } else {
                                None
                            },
                            Some(input.as_str()),
                        );
                    }
                    std::process::exit(1);
                }
            }
        }
        Err(errors) => {
            for e in errors {
                errors::report_parser_error(&e, Some(source));
            }
            std::process::exit(1);
        }
    };
}
