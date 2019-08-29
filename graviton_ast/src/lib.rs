pub mod ast;
pub use ast::{
    Ast, AstNode, BinaryOperation, FunctionSignature, Module, PrimitiveType, TypeSignature,
    UnaryOperation, VariableSignature,
};
pub mod semantic;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Position {
    pub line: i32,
    pub col: i32,
}
