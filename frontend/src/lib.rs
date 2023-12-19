#![warn(clippy::all, clippy::nursery)]

extern crate graviton_core as common;

use common::{
    ir,
    notice::{Notice, NoticeLevel},
    signature, Position,
};

pub mod lexer;
pub mod parser;
pub mod token;
