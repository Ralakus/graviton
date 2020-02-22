use colored::Colorize;
use serde::{Deserialize, Serialize};

pub extern crate graviton_ast as ast;

/// The amount of spaces a tab is equal to in the terminal
const TERMINAL_TAB_SIZE: usize = 5;

/// Position in code encoded by line and column
#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

impl Position {
    /// Crates a position from a line an column
    pub fn new(line: usize, col: usize) -> Self {
        Position { line, col }
    }
}

impl std::fmt::Display for Position {
    /// The first element is the line and the second is the column
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}:{}]", self.line, self.col)
    }
}

/// Returns a tuple with the second element being the 3 lines before and after position and the third element being the little squiggly line (~~~^~~) under the line pointing to the column.
/// The forth (index 3) element of lines is always the line with the error and the one that the squiggly is pointing to.
/// It will only return none if the line is not found
pub fn locate_in_source(source: &str, pos: Position) -> Option<(usize, Vec<&str>, String)> {
    let start_line = if pos.line > 3 { pos.line - 3 } else { 1 };
    let lines: Vec<&str> = source
        .lines()
        .skip(start_line - 1)
        .take(7)
        .collect::<Vec<&str>>();

    let error_line = if lines.len() >= 2 { lines[3] } else { lines[0] };

    // Gets the amount of tabs before the column position for proper squiggly length
    let tab_count = error_line
        .chars()
        .enumerate()
        .filter(|(i, c)| *c == '\t' && *i < pos.col - 1)
        .count();

    let squiggly_line = format!(
        "{}{}{}",
        String::from("~").repeat(pos.col - 1 + tab_count * TERMINAL_TAB_SIZE),
        "^",
        if pos.col < error_line.len() {
            String::from("~").repeat(error_line.len() - pos.col)
        } else {
            String::new()
        }
    );

    Some((start_line, lines, squiggly_line))
}

/// The report level of a notice
#[repr(u8)]
#[derive(Debug, PartialEq, Clone, Copy, Eq)]
pub enum NoticeLevel {
    Notice,
    Warning,
    Error,
}

/// A notice to the user notifying about any issues or recommendations
#[derive(Debug, Clone)]
pub struct Notice {
    /// The stage of the compiler the notice is from
    pub from: String,
    /// The notice message
    pub msg: String,
    /// The notice position in code
    pub pos: Position,
    /// The file name the notice is from
    pub file: String,
    /// The level of the notice
    pub level: NoticeLevel,
}

impl Notice {
    /// Creates a new notice
    pub fn new(from: String, msg: String, pos: Position, file: String, level: NoticeLevel) -> Self {
        Notice {
            from,
            msg,
            pos,
            file,
            level,
        }
    }

    /// Prints notice with colour to stdout if notice level is warning or notice and stderr if error.
    /// Prints out source location of error if it's provided
    pub fn report(self, source: Option<&str>) {
        let colour = match self.level {
            NoticeLevel::Notice => "cyan",
            NoticeLevel::Warning => "yellow",
            NoticeLevel::Error => "red",
        };

        println!("{}: {}", self.from.color(colour), self.msg);
        println!(
            "\tat {}{}:{}:{}{}\n",
            "[".bold(),
            self.file,
            self.pos.line,
            self.pos.col,
            "]".bold()
        );

        if let Some(src) = source {
            if let Some((start_line, lines, squiggly)) = locate_in_source(src, self.pos) {
                lines.iter().enumerate().for_each(|(i, line)| {
                    println!(
                        "\t{} | {}",
                        (start_line + i).to_string().color(colour),
                        line
                    );

                    if i == 3 {
                        println!("\t{}{}", "--|~".color(colour), squiggly.color(colour));
                    }
                });

                println!();
            }
        }
    }
}
