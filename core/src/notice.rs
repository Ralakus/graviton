use super::{ansi, Position};

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
    #[must_use]
    pub const fn new(
        from: String,
        msg: String,
        pos: Position,
        file: String,
        level: NoticeLevel,
    ) -> Self {
        Self {
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
            if let Some((start_line, lines, squiggly)) =
                source.and_then(|src| self.pos.locate_in_source(src))
            {
                for (i, line) in lines.iter().enumerate() {
                    println!(
                        "\t{}{:4}{} | {}",
                        colour,
                        start_line + i,
                        ansi::Fg::Reset,
                        line
                    );


                    if i == std::cmp::min(3, self.pos.line as usize - 1) {
                        println!("\t{}---- | {}{}", colour, squiggly, ansi::Fg::Reset);
                    }
                }

                println!();
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        notice::{Notice, NoticeLevel},
        Position,
    };

    #[test]
    fn notice() {
        let source = "// code\n//code 2\n//code 3\n// some code 4\n// some more code 5\nlet a = 14;\n// yet more code 7\n// code 8\n// code 9\n// code 10";

        let from = String::from("Letter analyzer");
        let msg = String::from("The letter \"a\" is a letter");
        let pos = Position::new(6, 5);
        let file = String::from("main.grav");

        let notice = Notice::new(
            from.clone(),
            msg.clone(),
            pos,
            file.clone(),
            NoticeLevel::Notice,
        );

        notice.report(Some(source));

        let notice = Notice::new(
            from.clone(),
            msg.clone(),
            pos,
            file.clone(),
            NoticeLevel::Warning,
        );

        notice.report(Some(source));

        let notice = Notice::new(from, msg, pos, file, NoticeLevel::Error);

        notice.report(Some(source));
    }
}
