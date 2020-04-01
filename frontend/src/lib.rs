#![warn(clippy::all, clippy::nursery)]

extern crate graviton_core as core;

use core::{
    ir,
    notice::{Notice, NoticeLevel},
    signature, Position,
};

pub mod lexer;
pub mod parser;
pub mod token;
