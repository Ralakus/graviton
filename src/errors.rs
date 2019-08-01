
use super::frontend::parser::ParseError;
use super::backend::vm::VmError;
use super::colored::*;

pub fn report_parser_error<'a>(e: &ParseError, source: Option<&'a str>) {
    report_error_msg_with_pos(&e.msg, e.pos, source, if let Some(name) = &e.file { Some(&name) } else { None })
}

#[cfg(feature = "node_code_pos")]
pub fn report_vm_error<'a>(e: &VmError, source: Option<&'a str>, file: Option<&'a str>) {
    report_error_msg_with_pos(&e.msg, e.pos, source, file)
}

#[cfg(not(feature = "node_code_pos"))]
pub fn report_vm_error<'a>(e: &VmError, _source: Option<&'a str>, _file: Option<&'a str>) {
    println!("{}: {}", "Error".red(), e.msg);
}

pub fn report_error_msg_with_pos<'a>(msg: &String, pos: ast::Position, source: Option<&'a str>, file: Option<&'a str>) {
    if pos.line != -1 {
        if let Some(s) = source {
            if let Some(f) = file {
                let mut line = 1;
                for l in s.lines() {
                    if line == pos.line {
                        println!("{}: {}\n\tat: {}\n\t{}\n\t{}{}{}",
                            "Error".red(),
                            msg,
                            format!("{}{}:{}:{}{}", "[".bold(), f, pos.line, pos.col, "]".bold()),
                            l,
                            if pos.col > 1 { String::from("~").repeat((pos.col - 1) as usize).red() } else { "".red() },
                            "^".red(),
                            if (pos.col as usize) < l.len() { String::from("~").repeat(l.len() - pos.col as usize ).red() } else { "".red() },
                        );

                    }
                    line += 1;
                }
            } else {
                let mut line = 1;
                for l in s.lines() {
                    if line == pos.line {
                        println!("{}: {}\n\tat: {}\n\t{}\n\t{}{}{}",
                            "Error".red(),
                            msg,
                            format!("{}Line: {}, Col: {}{}", "[".bold(), pos.line, pos.col, "]".bold()),
                            l,
                            if pos.col > 1 { String::from("~").repeat((pos.col - 1) as usize).red() } else { "".red() },
                            "^".red(),
                            if (pos.col as usize) < l.len() { String::from("~").repeat(l.len() - pos.col as usize ).red() } else { "".red() },
                        );

                    }
                    line += 1;
                }
            }
        } else {
            if let Some(f) = file {
                println!("{}: {}\n\tat: {}",
                    "Error".red(),
                    msg,
                    format!("{}{}:{}:{}{}", "[".bold(), f, pos.line, pos.col, "]".bold()),
                );
            } else {
                println!("{}: {}\n\tat: {}",
                    "Error".red(),
                    msg,
                    format!("{}Line: {}, Col: {}{}", "[".bold(), pos.line, pos.col, "]".bold()),
                );
            }
        }
    }
}