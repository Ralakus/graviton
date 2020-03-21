use serde::{Deserialize, Serialize};

/// Coloured printing using ansi color commands
pub mod ansi;
/// Ir data types
pub mod ir;
/// Type signature data types
pub mod signature;

/// The amount of spaces a tab is equal to in the terminal
const TAB_WITDH: usize = 5;

/// Position in code encoded by line and column.
/// The line or column should not be zero since that is reserved for errors
#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Position {
    pub line: u32,
    pub col: u32,
}

impl Position {
    /// Crates a position from a line an column
    pub fn new(line: u32, col: u32) -> Self {
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
        .skip(start_line as usize - 1)
        .take(7)
        .collect::<Vec<&str>>();

    let error_line = if lines.len() > 3 {
        lines[3]
    } else {
        lines[pos.line as usize - 1]
    };

    // Gets the amount of tabs before the column position for proper squiggly length
    let tab_count = error_line
        .chars()
        .enumerate()
        .filter(|(i, c)| *c == '\t' && *i < pos.col as usize - 1)
        .count();

    let squiggly_line = format!(
        "{}{}{}",
        String::from("~").repeat(pos.col as usize - 1 + tab_count * TAB_WITDH),
        "^",
        if (pos.col as usize) < error_line.len() {
            String::from("~").repeat(error_line.len() - pos.col as usize)
        } else {
            String::new()
        }
    );

    Some((start_line as usize, lines, squiggly_line))
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
        let (colour, prefix) = match self.level {
            NoticeLevel::Notice => (ansi::Fg::Cyan, "[-]: "),
            NoticeLevel::Warning => (ansi::Fg::Yellow, "[*]: "),
            NoticeLevel::Error => (ansi::Fg::Red, "[!]: "),
        };

        println!(
            "{}{}{}{}: {}",
            colour,
            prefix,
            self.from,
            ansi::Fg::Reset,
            self.msg
        );
        println!("\tat [{}:{}:{}]\n", self.file, self.pos.line, self.pos.col,);

        if self.pos.line != 0 && self.pos.col != 0 {
            if let Some(src) = source {
                if let Some((start_line, lines, squiggly)) = locate_in_source(src, self.pos) {
                    lines.iter().enumerate().for_each(|(i, line)| {
                        println!(
                            "\t{}{}{} | {}",
                            colour,
                            start_line + i,
                            ansi::Fg::Reset,
                            line
                        );

                        if i == if lines.len() > 3 {
                            3
                        } else {
                            self.pos.line as usize - 1
                        } {
                            println!("\t{}- | {}{}", colour, squiggly, ansi::Fg::Reset);
                        }
                    });

                    println!();
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn notice() {
        let source = "// code\n//code 2\n//code 3\n// some code 4\n// some more code 5\nlet a = 14;\n// yet more code 7\n// code 8\n// code 9\n// code 10";

        let from = String::from("Letter analyzer");
        let msg = String::from("The letter \"a\" is a letter");
        let pos = core::Position::new(6, 5);
        let file = String::from("main.grav");

        let notice = core::Notice::new(
            from.clone(),
            msg.clone(),
            pos,
            file.clone(),
            core::NoticeLevel::Notice,
        );

        notice.report(Some(source));

        let notice = core::Notice::new(
            from.clone(),
            msg.clone(),
            pos,
            file.clone(),
            core::NoticeLevel::Warning,
        );

        notice.report(Some(source));

        let notice = core::Notice::new(from, msg, pos, file, core::NoticeLevel::Error);

        notice.report(Some(source));
    }
}
