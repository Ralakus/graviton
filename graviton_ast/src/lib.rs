
pub mod ast;
pub use ast::{Ast, AstNode, UnaryOperation, BinaryOperation, TypeSignature, VariableSignature, FunctionSignature, PrimitiveType};
pub mod semantic;

use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub struct Position {
    pub line: i32,
    pub col: i32
}