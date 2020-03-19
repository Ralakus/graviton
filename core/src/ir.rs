use serde::{Deserialize, Serialize};

use super::signature::TypeSignature;

type SignatureIndex = u8;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IrModule {
    pub name: String,
    pub instructions: Vec<u8>,
    pub signatures: Vec<TypeSignature>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IrInstruction<'a> {
    // --------------------
    // Branch structures
    // --------------------
    //
    If,
    IfBody,
    IfElseIf,
    IfElseIfBody,
    IfElse,
    IfEnd,

    While,
    WhileBody,
    WhileEnd,

    // --------------------
    // Declarations
    // --------------------
    //
    Function(SignatureIndex),
    FunctionParameter(&'a str),
    FunctionEnd,

    Return,
    ReturnEnd,

    Let(SignatureIndex, &'a str),
    LetEnd,

    LetMut(SignatureIndex, &'a str),
    LetMutEnd,

    // --------------------
    // Expressions
    // --------------------
    //
    Block,
    BlockExpression,
    BlockEnd,

    Import(SignatureIndex, &'a str),

    ExternFn(SignatureIndex),

    Struct(SignatureIndex),

    Call(u8),

    As(SignatureIndex),

    Identifier(&'a str),
    String(&'a str),
    Bool(bool),
    Float(f64),
    Integer(isize),

    // --------------------
    // Binary operations
    // --------------------
    //
    Add,
    Subtract,
    Multiply,
    Divide,

    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,

    And,
    Or,

    Assign,

    // --------------------
    // Unary operations
    // --------------------
    //
    Negate,

    Not,

    // --------------------
    // Statement
    // --------------------
    //
    Statement,
}
