use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Position {
    pub line: i32,
    pub col: i32,
}

#[derive(Debug, Clone)]
pub struct Error {
    pub type_: String,
    pub msg: String,
    pub pos: Position,
    pub file: Option<String>,
}
