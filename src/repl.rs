use graviton::frontend::tokens::{Position, Token, TokenData, TokenType};
use std::io::{BufRead, Write};

use super::graviton;

use super::graviton::colored::*;
use super::graviton::errors;

pub fn repl(debug_level_in: i32) -> Result<(), String> {
    let mut debug_level = debug_level_in;

    let mut source = String::from("");
    let mut lex: graviton::frontend::lexer::Lexer;

    'repl: loop {
        source.clear();
        print!("> ");
        std::io::stdout().flush().unwrap();
        std::io::stdin()
            .lock()
            .read_line(&mut source)
            .expect("Error reading input");

        lex = graviton::frontend::lexer::Lexer::new(source.as_str());

        if let Some(tok) = lex.next() {
            if tok.type_ == TokenType::Colon {
                if let Some(t) = lex.next() {
                    match t.data {
                        TokenData::String(s) => match s.as_str() {
                            "exit" => return Ok(()),
                            "debug" => {
                                let val = lex.next().unwrap_or(Token {
                                    type_: TokenType::Number,
                                    data: TokenData::Number(0.0),
                                    pos: Position { line: -1, col: -1 },
                                });
                                if let TokenData::Number(n) = val.data {
                                    debug_level = n as i32;
                                } else {
                                    debug_level = 0;
                                }
                            }
                            _ => println!("Invalid command {}", s),
                        },
                        _ => println!("Invalid token type {:?}", t.type_),
                    }
                } else {
                    println!("{}", "Expected argument after \":\"".red());
                }
                continue 'repl;
            }
        }

        let ast = graviton::frontend::parser::Parser::parse(source.as_str(), None);

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
                    }
                    Err(errors) => {
                        if debug_level >= 2 {
                            println!("{}\n{:#?}", "Untyepd AST:".cyan(), a);
                        }
                        for e in errors {
                            errors::report_semantic_error(&e, Some(&*source), None);
                        }
                    }
                }
            }
            Err(errors) => {
                for e in errors {
                    errors::report_parser_error(&e, Some(source.as_str()));
                }
            }
        };
    }
}
