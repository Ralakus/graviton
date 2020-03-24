use serde::{Deserialize, Serialize};

/// Coloured printing using ansi color commands
pub mod ansi;
/// Ir data types
pub mod ir;
/// Notice data type
pub mod notice;
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

    /// Returns a tuple with the second element being the 3 lines before and after position and the third element being the little squiggly line (~~~^~~) under the line pointing to the column.
    /// The forth (index 3) element of lines is always the line with the error and the one that the squiggly is pointing to.
    /// It will only return none if the line is not found
    pub fn locate_in_source(self, source: &str) -> Option<(usize, Vec<&str>, String)> {
        let start_line = if self.line > 3 { self.line - 3 } else { 1 };
        let lines: Vec<&str> = source
            .lines()
            .skip(start_line as usize - 1)
            .take(7)
            .collect::<Vec<&str>>();

        let error_line = if lines.len() > 3 {
            lines[3]
        } else {
            lines[self.line as usize - 1]
        };

        // Gets the amount of tabs before the column position for proper squiggly length
        let tab_count = error_line
            .chars()
            .enumerate()
            .filter(|(i, c)| *c == '\t' && *i < self.col as usize - 1)
            .count();

        let squiggly_line = format!(
            "{}{}{}",
            String::from("~").repeat(self.col as usize - 1 + tab_count * TAB_WITDH),
            "^",
            if (self.col as usize) < error_line.len() {
                String::from("~").repeat(error_line.len() - self.col as usize)
            } else {
                String::new()
            }
        );

        Some((start_line as usize, lines, squiggly_line))
    }
}

impl std::fmt::Display for Position {
    /// The first element is the line and the second is the column
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}:{}]", self.line, self.col)
    }
}
