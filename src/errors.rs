use super::ast::semantic::SemanticError;
use super::backend::native::NativeError;
use super::colored::*;
use super::frontend::parser::ParseError;

pub fn report_parser_error<'a>(e: &ParseError, source: Option<&'a str>) {
    report_error_msg_with_pos(
        &e.msg,
        e.pos,
        source,
        if let Some(name) = &e.file {
            Some(&name)
        } else {
            None
        },
    )
}

pub fn report_semantic_error<'a>(e: &SemanticError, source: Option<&'a str>) {
    report_error_msg_with_pos(
        &e.msg,
        e.pos,
        source,
        if let Some(name) = &e.file {
            Some(&name)
        } else {
            None
        },
    )
}

pub fn report_native_error<'a>(e: &NativeError, source: Option<&'a str>) {
    report_error_msg_with_pos(
        &e.msg,
        e.pos,
        source,
        if let Some(name) = &e.file {
            Some(&name)
        } else {
            None
        },
    )
}

pub fn report_error_msg_with_pos<'a>(
    msg: &String,
    pos: ast::Position,
    source: Option<&'a str>,
    file: Option<&'a str>,
) {
    if pos.line != -1 {
        if let Some(s) = source {
            if let Some(f) = file {
                let mut line = 1;
                for l in s.lines() {
                    if line == pos.line {
                        eprintln!(
                            "{}: {}\n\tat: {}\n\t{}\n\t{}{}{}",
                            "Error".red(),
                            msg,
                            format!("{}{}:{}:{}{}", "[".bold(), f, pos.line, pos.col, "]".bold()),
                            l,
                            if pos.col > 1 {
                                String::from("~").repeat((pos.col - 1) as usize).red()
                            } else {
                                "".red()
                            },
                            "^".red(),
                            if (pos.col as usize) < l.len() {
                                String::from("~").repeat(l.len() - pos.col as usize).red()
                            } else {
                                "".red()
                            },
                        );
                    }
                    line += 1;
                }
            } else {
                let mut line = 1;
                for l in s.lines() {
                    if line == pos.line {
                        eprintln!(
                            "{}: {}\n\tat: {}\n\t{}\n\t{}{}{}",
                            "Error".red(),
                            msg,
                            format!(
                                "{}Line: {}, Col: {}{}",
                                "[".bold(),
                                pos.line,
                                pos.col,
                                "]".bold()
                            ),
                            l,
                            if pos.col > 1 {
                                String::from("~").repeat((pos.col - 1) as usize).red()
                            } else {
                                "".red()
                            },
                            "^".red(),
                            if (pos.col as usize) < l.len() {
                                String::from("~").repeat(l.len() - pos.col as usize).red()
                            } else {
                                "".red()
                            },
                        );
                    }
                    line += 1;
                }
            }
        } else {
            if let Some(f) = file {
                eprintln!(
                    "{}: {}\n\tat: {}",
                    "Error".red(),
                    msg,
                    format!("{}{}:{}:{}{}", "[".bold(), f, pos.line, pos.col, "]".bold()),
                );
            } else {
                eprintln!(
                    "{}: {}\n\tat: {}",
                    "Error".red(),
                    msg,
                    format!(
                        "{}Line: {}, Col: {}{}",
                        "[".bold(),
                        pos.line,
                        pos.col,
                        "]".bold()
                    ),
                );
            }
        }
    } else if pos.line == -1 {
        if let Some(f) = file {
            eprintln!(
                "{}: {}\n\tat: {}",
                "Error".red(),
                msg,
                format!("{}{}:EOF{}", "[".bold(), f, "]".bold()),
            );
        } else {
            eprintln!(
                "{}: {}\n\tat: {}",
                "Error".red(),
                msg,
                format!("{}EOF{}", "[".bold(), "]".bold()),
            );
        }
    } else if pos.line == -2 {
        if let Some(f) = file {
            eprintln!(
                "{}: {}\n\tat: {}",
                "Error".red(),
                msg,
                format!("{}{}:begining of file{}", "[".bold(), f, "]".bold()),
            );
        } else {
            eprintln!(
                "{}: {}\n\tat: {}",
                "Error".red(),
                msg,
                format!("{}begining of file{}", "[".bold(), "]".bold()),
            );
        }
    }
}
