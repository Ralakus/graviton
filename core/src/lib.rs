use serde::{Deserialize, Serialize};

extern crate colored;
use colored::*;

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Position {
    pub line: i32,
    pub col: i32,
}

impl Position {
    pub fn new(line: i32, col: i32) -> Self {
        Position { line, col }
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line: {}, col: {}", self.line, self.col)
    }
}

#[repr(u8)]
#[derive(Debug, PartialEq, Clone, Copy, Eq)]
pub enum NoticeLevel {
    Notice,
    Warning,
    Error,
    Critical,
}

#[derive(Debug, Clone)]
pub struct Notice {
    pub from: String,
    pub msg: String,
    pub pos: Position,
    pub file: Option<String>,
    pub level: NoticeLevel,
}

impl<'a> Notice {
    pub fn report(&self, source: Option<&'a str>) {
        eprint!(
            "{}: ",
            match self.level {
                NoticeLevel::Notice => {
                    format!("{} Notice", self.from).cyan()
                }
                NoticeLevel::Warning => {
                    format!("{} Warning", self.from).cyan().yellow()
                }
                NoticeLevel::Error => {
                    format!("{} Error", self.from).cyan().red()
                }
                NoticeLevel::Critical => {
                    format!("{} Critical", self.from)
                        .cyan()
                        .bold()
                        .underline()
                        .red()
                }
            }
        );
        if self.pos.line > 0 {
            if let Some(s) = source {
                if let Some(f) = &self.file {
                    let mut line = 1;
                    for l in s.lines() {
                        if line == self.pos.line {
                            eprintln!(
                                "{}\n\tat: {}\n\t{}\n\t{}{}{}",
                                self.msg,
                                format!(
                                    "{}{}:{}:{}{}",
                                    "[".bold(),
                                    f,
                                    self.pos.line,
                                    self.pos.col,
                                    "]".bold()
                                ),
                                l,
                                if self.pos.col > 1 {
                                    String::from("~").repeat((self.pos.col - 1) as usize).red()
                                } else {
                                    "".red()
                                },
                                "^".red(),
                                if (self.pos.col as usize) < l.len() {
                                    String::from("~")
                                        .repeat(l.len() - self.pos.col as usize)
                                        .red()
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
                        if line == self.pos.line {
                            eprintln!(
                                "{}\n\tat: {}\n\t{}\n\t{}{}{}",
                                self.msg,
                                format!(
                                    "{}Line: {}, Col: {}{}",
                                    "[".bold(),
                                    self.pos.line,
                                    self.pos.col,
                                    "]".bold()
                                ),
                                l,
                                if self.pos.col > 1 {
                                    String::from("~").repeat((self.pos.col - 1) as usize).red()
                                } else {
                                    "".red()
                                },
                                "^".red(),
                                if (self.pos.col as usize) < l.len() {
                                    String::from("~")
                                        .repeat(l.len() - self.pos.col as usize)
                                        .red()
                                } else {
                                    "".red()
                                },
                            );
                        }
                        line += 1;
                    }
                }
            } else if let Some(f) = &self.file {
                eprintln!(
                    "{}\n\tat: {}",
                    self.msg,
                    format!(
                        "{}{}:{}:{}{}",
                        "[".bold(),
                        f,
                        self.pos.line,
                        self.pos.col,
                        "]".bold()
                    ),
                );
            } else {
                eprintln!(
                    "{}\n\tat: {}",
                    self.msg,
                    format!(
                        "{}Line: {}, Col: {}{}",
                        "[".bold(),
                        self.pos.line,
                        self.pos.col,
                        "]".bold()
                    ),
                );
            }
        } else if self.pos.line == -1 {
            if let Some(f) = &self.file {
                eprintln!(
                    "{}\n\tat: {}",
                    self.msg,
                    format!("{}{}:EOF{}", "[".bold(), f, "]".bold()),
                );
            } else {
                eprintln!(
                    "{}\n\tat: {}",
                    self.msg,
                    format!("{}EOF{}", "[".bold(), "]".bold()),
                );
            }
        } else if self.pos.line == -2 {
            if let Some(f) = &self.file {
                eprintln!(
                    "{}\n\tat: {}",
                    self.msg,
                    format!("{}{}:module{}", "[".bold(), f, "]".bold()),
                );
            } else {
                eprintln!(
                    "{}\n\tat: {}",
                    self.msg,
                    format!("{}module{}", "[".bold(), "]".bold()),
                );
            }
        } else if let Some(f) = &self.file {
            eprintln!(
                "{}\n\tat: {}",
                self.msg,
                format!("{}{}:??{}", "[".bold(), f, "]".bold()),
            );
        } else {
            eprintln!(
                "{}\n\tat: {}",
                self.msg,
                format!("{}??{}", "[".bold(), "]".bold()),
            );
        }
    }
}

pub fn contains_errors(notices: &[Notice]) -> bool {
    let mut were_errors = false;
    for n in notices {
        if let NoticeLevel::Error = n.level {
            were_errors = true;
        } else if let NoticeLevel::Critical = n.level {
            were_errors = true;
        }
    }
    were_errors
}
