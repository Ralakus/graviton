#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::too_many_lines, clippy::module_name_repetitions, clippy::wildcard_imports)]

extern crate graviton_core as common;

use common::{
    ir,
    notice::{Notice, NoticeLevel},
    signature, Position,
};

pub mod lexer;
pub mod parser;
pub mod token;
