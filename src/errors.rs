
use super::tachyon::frontend::parser::ParseError;
use super::colored::*;

pub fn report_error<'a>(e: &ParseError, source: Option<&'a str>, file: Option<&'a str>) {
    if e.pos.line != -1 {
        if let Some(s) = source {
            if let Some(f) = file {
                let mut line = 1;
                for l in s.lines() {
                    if line == e.pos.line {
                        println!("{}: {}\n\tat: {}\n\t{}\n\t{}{}{}",
                            "Error".red(),
                            e.msg,
                            format!("{}{}:{}:{}{}", "[".bold(), f, e.pos.line, e.pos.col, "]".bold()),
                            l,
                            if e.pos.col > 1 { String::from("~").repeat((e.pos.col - 1) as usize).red() } else { "".red() },
                            "^".red(),
                            if (e.pos.col as usize) < l.len() { String::from("~").repeat(l.len() - e.pos.col as usize ).red() } else { "".red() },
                        );

                    }
                    line += 1;
                }
            } else {
                let mut line = 1;
                for l in s.lines() {
                    if line == e.pos.line {
                        println!("{}: {}\n\tat: {}\n\t{}\n\t{}{}{}",
                            "Error".red(),
                            e.msg,
                            format!("{}Line: {}, Col: {}{}", "[".bold(), e.pos.line, e.pos.col, "]".bold()),
                            l,
                            if e.pos.col > 1 { String::from("~").repeat((e.pos.col - 1) as usize).red() } else { "".red() },
                            "^".red(),
                            if (e.pos.col as usize) < l.len() { String::from("~").repeat(l.len() - e.pos.col as usize ).red() } else { "".red() },
                        );

                    }
                    line += 1;
                }
            }
        } else {
            if let Some(f) = file {
                println!("{}: {}\n\tat: {}",
                    "Error".red(),
                    e.msg,
                    format!("{}{}:{}:{}{}", "[".bold(), f, e.pos.line, e.pos.col, "]".bold()),
                );
            } else {
                println!("{}: {}\n\tat: {}",
                    "Error".red(),
                    e.msg,
                    format!("{}Line: {}, Col: {}{}", "[".bold(), e.pos.line, e.pos.col, "]".bold()),
                );
            }
        }
    }
}