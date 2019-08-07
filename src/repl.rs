use super::*;
use std::io::{BufRead, Write};

pub fn repl(debug_level_in: i32) -> Result<(), String> {
    let mut debug_level = debug_level_in;

    let mut source = String::new();

    'repl: loop {
        source.clear();
        print!("> ");
        std::io::stdout().flush().unwrap();
        std::io::stdin()
            .lock()
            .read_line(&mut source)
            .expect("Error reading input");

        if source.len() > 0 {
            if source.chars().next().unwrap() == ':' {
                if source.len() > 1 {
                    let args: Vec<&str> = source[1..].split_ascii_whitespace().collect();
                    match args[0] {
                        "exit" => return Ok(()),
                        "debug" => {
                            if args.len() > 1 {
                                match args[1].parse::<i32>() {
                                    Ok(v) => debug_level = v,
                                    Err(e) => {
                                        eprintln!("{}: {}", "Error".red(), e.description());
                                        continue 'repl;
                                    }
                                }
                            } else {
                                debug_level = 0;
                            }
                        }
                        s => println!("Invalid command {}", s),
                    }
                } else {
                    println!("{}", "Expected argument after \":\"".red());
                }
                continue 'repl;
            }
        }

        let obj = match grav::compile_source(&source, None, debug_level) {
            Ok(o) => o,
            Err(e) => {
                e.report(Some(&source));
                continue 'repl;
            }
        };

        match obj.write_file(&String::from("grav_repl_tmp.o")) {
            Ok(_) => {}
            Err(e) => {
                grav::errors::report_native_error(&e, Some(&source));
                std::process::exit(1);
            }
        };

        if !cfg!(windows) && debug_level >= 3 {
            match std::process::Command::new("objdump")
                .arg("-d")
                .arg("grav_repl_tmp.o")
                .spawn()
            {
                Ok(mut c) => match c.wait() {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!("{}: {}", "Error".red(), e.description());
                        std::process::exit(1);
                    }
                },
                Err(e) => {
                    eprintln!("{}: {}", "Error".red(), e.description());
                    std::process::exit(1);
                }
            };
        }

        match std::process::Command::new("cc")
            .arg("grav_repl_tmp.o")
            .arg("stdlib/graviton_driver.c")
            .arg("stdlib/graviton_lib.c")
            .arg("-o")
            .arg("grav_repl_tmp")
            .spawn()
        {
            Ok(mut c) => match c.wait() {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("{}: {}", "Error".red(), e.description());
                    std::process::exit(1);
                }
            },
            Err(e) => {
                eprintln!("{}: {}", "Error".red(), e.description());
                std::process::exit(1);
            }
        };

        match std::process::Command::new("./grav_repl_tmp").spawn() {
            Ok(mut c) => match c.wait() {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("{}: {}", "Error".red(), e.description());
                    std::process::exit(1);
                }
            },
            Err(e) => {
                eprintln!("{}: {}", "Error".red(), e.description());
                std::process::exit(1);
            }
        };

        match std::process::Command::new("rm")
            .arg("grav_repl_tmp.o")
            .arg("grav_repl_tmp")
            .spawn()
        {
            Ok(mut c) => match c.wait() {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("{}: {}", "Error".red(), e.description());
                    std::process::exit(1);
                }
            },
            Err(e) => {
                eprintln!("{}: {}", "Error".red(), e.description());
                std::process::exit(1);
            }
        };
    }
}
