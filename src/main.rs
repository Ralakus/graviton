extern crate graviton as grav;

use clap::{App, Arg, SubCommand};
use colored::*;
use memmap::Mmap;
use std::{fs::File, io::Write};

mod repl;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHOR: &str = env!("CARGO_PKG_AUTHORS");
const DESCRIPTION: &str = env!("CARGO_PKG_DESCRIPTION");

fn strip_extension(name: &str) -> &str {
    let pos = match name.rfind('.') {
        Some(i) => i,
        None => return name,
    };

    &name[0..pos]
}

fn strip_filepath(name: &str) -> &str {
    let pos = match name.rfind('/') {
        Some(i) => i,
        None => return name,
    };

    &name[pos + 1..]
}

fn main() {
    let debug_arg = Arg::with_name("Debug Level")
        .help("Sets debug level to run the compiler in [0, 1, 2, 3]")
        .short("d")
        .long("debug")
        .takes_value(true);

    let input_arg = Arg::with_name("Input")
        .help("Input file to process")
        .index(1);

    let args = App::new("grav")
        .version(VERSION)
        .author(AUTHOR)
        .about(DESCRIPTION)
        .arg(input_arg.clone())
        .arg(debug_arg.clone())
        .arg(
            Arg::with_name("Emit")
                .help("Emits the specified format [ast, wasm, none]")
                .long("emit")
                .short("e")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("Output")
                .help("Specifies where to output compiled file or specified format")
                .long("output")
                .short("o")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("Type")
                .help("Specifies the input format [src, source, ast]")
                .long("type")
                .short("t")
                .takes_value(true),
        )
        .subcommand(
            SubCommand::with_name("repl")
                .about("Live REPL environment")
                .version(VERSION)
                .author(AUTHOR)
                .arg(debug_arg.clone()),
        )
        .subcommand(
            SubCommand::with_name("run")
                .about("Run code directly from source or ast")
                .version(VERSION)
                .author(AUTHOR)
                .arg(debug_arg)
                .arg(input_arg),
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

        match repl::repl(debug_level) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("{}: {}", "Error".red(), e);
                std::process::exit(1);
            }
        }

        return;
    }

    if let Some(run_args) = args.subcommand_matches("run") {
        let debug_level = match run_args.value_of("Debug Level").unwrap_or("0") {
            "0" => 0,
            "1" => 1,
            "2" => 2,
            "3" => 3,
            _ => 0,
        };

        let input = if let Some(input) = run_args.value_of("Input") {
            match input {
                s if !s.contains('.') => format!("{}.grav", s),
                s => s.to_string(),
            }
        } else {
            eprintln!("{}: Expects at least one argument for input", "Error".red());
            std::process::exit(1);
        };

        let file = match File::open(input.as_str()) {
            Ok(f) => f,
            Err(e) => {
                eprintln!("{}: {}: {}", "Error".red(), input, e);
                std::process::exit(1);
            }
        };

        let mapped_file = unsafe {
            match Mmap::map(&file) {
                Ok(f) => f,
                Err(e) => {
                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                    std::process::exit(1);
                }
            }
        };

        let source = match std::str::from_utf8(&mapped_file[..]) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("{}: {}: {}", "Error".red(), input, e);
                std::process::exit(1);
            }
        };

        let obj = match grav::compile_source(source, Some(&input), debug_level) {
            Ok(obj) => obj,
            Err(e) => {
                grav::report_notices(&e, Some(source));
                std::process::exit(1);
            }
        };

        match obj.write_file(&String::from("grav_tmp.o")) {
            Ok(_) => {}
            Err(e) => {
                grav::report_notices(&[e], Some(source));
                std::process::exit(1);
            }
        }

        if !cfg!(windows) && debug_level >= 3 {
            match std::process::Command::new("objdump")
                .arg("-M")
                .arg("intel")
                .arg("-d")
                .arg("grav_tmp.o")
                .spawn()
            {
                Ok(mut c) => match c.wait() {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!("{}: {}: {}", "Error".red(), input, e);
                        std::process::exit(1);
                    }
                },
                Err(e) => {
                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                    std::process::exit(1);
                }
            };
        }

        match std::process::Command::new("cc")
            .arg("grav_tmp.o")
            .arg("stdlib/graviton_driver.c")
            .arg("stdlib/graviton_lib.c")
            .arg("-o")
            .arg("grav_tmp")
            .spawn()
        {
            Ok(mut c) => match c.wait() {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                    std::process::exit(1);
                }
            },
            Err(e) => {
                eprintln!("{}: {}: {}", "Error".red(), input, e);
                std::process::exit(1);
            }
        };

        match std::process::Command::new("./grav_tmp").spawn() {
            Ok(mut c) => match c.wait() {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                    std::process::exit(1);
                }
            },
            Err(e) => {
                eprintln!("{}: {}: {}", "Error".red(), input, e);
                std::process::exit(1);
            }
        };

        match std::process::Command::new("rm")
            .arg("grav_tmp")
            .arg("grav_tmp.o")
            .spawn()
        {
            Ok(mut c) => match c.wait() {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                    std::process::exit(1);
                }
            },
            Err(e) => {
                eprintln!("{}: {}: {}", "Error".red(), input, e);
                std::process::exit(1);
            }
        };

        return;
    }

    let debug_level = match args.value_of("Debug Level").unwrap_or("0") {
        "0" => 0,
        "1" => 1,
        "2" => 2,
        "3" => 3,
        _ => 0,
    };

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum EmitType {
        Ast,
        Object,
        Executable,
        Wasm,
        None,
    }

    let emit_type = match args.value_of("Emit").unwrap_or("object") {
        "ast" => EmitType::Ast,
        "object" | "obj" => EmitType::Object,
        "executable" | "exe" => EmitType::Executable,
        "wasm" => EmitType::Wasm,
        "none" => {
            if args.value_of("Output").is_none() {
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

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum InputType {
        Source,
        Ast,
    }

    let input_type = match args.value_of("Type").unwrap_or("source") {
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
        s => {
            eprintln!("{}: Invalid input type {}", "Error".red(), s);
            return;
        }
    };

    let input = if let Some(input) = args.value_of("Input") {
        match input {
            s if !s.contains('.') => match input_type {
                InputType::Source => format!("{}.grav", s),
                InputType::Ast => format!("{}.gast", s),
            },
            s => s.to_string(),
        }
    } else {
        eprintln!("{}: Expects at least one argument for input", "Error".red());
        std::process::exit(1);
    };

    let output: String = match args.value_of("Output").unwrap_or(&match emit_type {
        EmitType::Ast => format!("{}.gast", strip_filepath(&strip_extension(&input))),
        EmitType::Object => format!("{}.o", strip_filepath(&strip_extension(&input))),
        EmitType::Executable => strip_filepath(&strip_extension(&input)).to_string(),
        EmitType::Wasm | EmitType::None => {
            format!("{}.wasm", strip_filepath(&strip_extension(&input)))
        }
    }) {
        s if !s.contains('.') => match emit_type {
            EmitType::Ast => format!("{}.gast", s),
            EmitType::Object => format!("{}.o", s),
            EmitType::Executable => s.to_string(),
            EmitType::Wasm | EmitType::None => format!("{}.wasm", s),
        },
        s => s.to_string(),
    };

    let file = match File::open(input.as_str()) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("{}: {}: {}", "Error".red(), input, e);
            std::process::exit(1);
        }
    };

    let mapped_file = unsafe {
        match Mmap::map(&file) {
            Ok(f) => f,
            Err(e) => {
                eprintln!("{}: {}: {}", "Error".red(), input, e);
                std::process::exit(1);
            }
        }
    };

    let module = match input_type {
        InputType::Source => {
            let source = match std::str::from_utf8(&mapped_file[..]) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                    std::process::exit(1);
                }
            };

            match grav::parse_source(source, Some(&input), debug_level) {
                Ok((obj, notices)) => {
                    grav::report_notices(&notices, Some(source));
                    obj
                }
                Err(e) => {
                    grav::report_notices(&e, Some(source));
                    std::process::exit(1);
                }
            }
        }
        InputType::Ast => match rmp_serde::from_slice(&mapped_file[..]) {
            Ok(a) => a,
            Err(e) => {
                eprintln!("{}: {}: {}", "Error".red(), input, e);
                std::process::exit(1);
            }
        },
    };

    match emit_type {
        EmitType::Ast => {
            let mut file = match File::create(output) {
                Ok(f) => f,
                Err(e) => {
                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                    std::process::exit(1);
                }
            };
            match file.write_all(match &rmp_serde::to_vec(&module) {
                Ok(vec) => vec,
                Err(e) => {
                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                    std::process::exit(1);
                }
            }) {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                    std::process::exit(1);
                }
            }
        }
        EmitType::Object => {
            let obj = match grav::compile_module(input.clone(), &module, debug_level) {
                Ok(o) => o,
                Err(e) => {
                    grav::report_notices(
                        &e,
                        if input_type == InputType::Source {
                            match std::str::from_utf8(&mapped_file[..]) {
                                Ok(s) => Some(s),
                                Err(e) => {
                                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                                    std::process::exit(1);
                                }
                            }
                        } else {
                            None
                        },
                    );
                    std::process::exit(1);
                }
            };

            match obj.write_file(&String::from("grav_tmp.o")) {
                Ok(_) => {}
                Err(e) => {
                    grav::report_notices(
                        &[e],
                        if input_type == InputType::Source {
                            match std::str::from_utf8(&mapped_file[..]) {
                                Ok(s) => Some(s),
                                Err(e) => {
                                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                                    std::process::exit(1);
                                }
                            }
                        } else {
                            None
                        },
                    );
                    std::process::exit(1);
                }
            };

            if !cfg!(windows) && debug_level >= 3 {
                match std::process::Command::new("objdump")
                    .arg("-M")
                    .arg("intel")
                    .arg("-d")
                    .arg("grav_tmp.o")
                    .spawn()
                {
                    Ok(mut c) => match c.wait() {
                        Ok(_) => {}
                        Err(e) => {
                            eprintln!("{}: {}: {}", "Error".red(), input, e);
                            std::process::exit(1);
                        }
                    },
                    Err(e) => {
                        eprintln!("{}: {}: {}", "Error".red(), input, e);
                        std::process::exit(1);
                    }
                };
            }

            match std::process::Command::new("cc")
                .arg("-c")
                .arg("stdlib/graviton_lib.c")
                .arg("-o")
                .arg("grav_lib.o")
                .spawn()
            {
                Ok(mut c) => match c.wait() {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!("{}: {}: {}", "Error".red(), input, e);
                        std::process::exit(1);
                    }
                },
                Err(e) => {
                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                    std::process::exit(1);
                }
            };

            match std::process::Command::new("ar")
                .arg("-rcs")
                .arg(output)
                .arg("grav_lib.o")
                .arg("grav_tmp.o")
                .spawn()
            {
                Ok(mut c) => match c.wait() {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!("{}: {}: {}", "Error".red(), input, e);
                        std::process::exit(1);
                    }
                },
                Err(e) => {
                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                    std::process::exit(1);
                }
            };

            match std::process::Command::new("rm")
                .arg("grav_tmp.o")
                .arg("grav_lib.o")
                .spawn()
            {
                Ok(mut c) => match c.wait() {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!("{}: {}: {}", "Error".red(), input, e);
                        std::process::exit(1);
                    }
                },
                Err(e) => {
                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                    std::process::exit(1);
                }
            };
        }
        EmitType::Executable => {
            let obj = match grav::compile_module(input.clone(), &module, debug_level) {
                Ok(o) => o,
                Err(e) => {
                    grav::report_notices(
                        &e,
                        if input_type == InputType::Source {
                            match std::str::from_utf8(&mapped_file[..]) {
                                Ok(s) => Some(s),
                                Err(e) => {
                                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                                    std::process::exit(1);
                                }
                            }
                        } else {
                            None
                        },
                    );
                    std::process::exit(1);
                }
            };

            match obj.write_file(&String::from("grav_tmp.o")) {
                Ok(_) => {}
                Err(e) => {
                    grav::report_notices(
                        &[e],
                        if input_type == InputType::Source {
                            match std::str::from_utf8(&mapped_file[..]) {
                                Ok(s) => Some(s),
                                Err(e) => {
                                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                                    std::process::exit(1);
                                }
                            }
                        } else {
                            None
                        },
                    );
                    std::process::exit(1);
                }
            };

            if !cfg!(windows) && debug_level >= 3 {
                match std::process::Command::new("objdump")
                    .arg("-d")
                    .arg("grav_tmp.o")
                    .spawn()
                {
                    Ok(mut c) => match c.wait() {
                        Ok(_) => {}
                        Err(e) => {
                            eprintln!("{}: {}: {}", "Error".red(), input, e);
                            std::process::exit(1);
                        }
                    },
                    Err(e) => {
                        eprintln!("{}: {}: {}", "Error".red(), input, e);
                        std::process::exit(1);
                    }
                };
            }

            match std::process::Command::new("cc")
                .arg("grav_tmp.o")
                .arg("stdlib/graviton_driver.c")
                .arg("stdlib/graviton_lib.c")
                .arg("-o")
                .arg(output)
                .spawn()
            {
                Ok(mut c) => match c.wait() {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!("{}: {}: {}", "Error".red(), input, e);
                        std::process::exit(1);
                    }
                },
                Err(e) => {
                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                    std::process::exit(1);
                }
            };

            match std::process::Command::new("rm").arg("grav_tmp.o").spawn() {
                Ok(mut c) => match c.wait() {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!("{}: {}: {}", "Error".red(), input, e);
                        std::process::exit(1);
                    }
                },
                Err(e) => {
                    eprintln!("{}: {}: {}", "Error".red(), input, e);
                    std::process::exit(1);
                }
            };
        }
        EmitType::Wasm => {
            eprintln!("Wasm output not yet supported");
            std::process::exit(1);
        }
        EmitType::None => {}
    }
}
