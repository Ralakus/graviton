extern crate graviton_core as core;

use core::{Notice, NoticeLevel, Position};

pub mod ast;
pub use ast::{
    Ast, AstNode, BinaryOperation, FunctionSignature, Module, PrimitiveType, TypeSignature,
    UnaryOperation, VariableSignature,
};
pub mod semantic;

mod new_ast;
