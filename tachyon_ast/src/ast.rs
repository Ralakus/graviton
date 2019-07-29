
use serde::{Serialize, Deserialize};

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum BinaryOperation {
    Add,
    Subtract,
    Multiply,
    Divide,

    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equals,

    Assign
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum UnaryOperation {
    Negate,

    Not
}

#[derive(Hash, Clone, Debug, Serialize, Deserialize)]
pub struct TypeSignature {
    pub name: String
}

#[derive(Hash, Clone, Debug, Serialize, Deserialize)]
pub struct VariableSignature {
    pub name: String,
    pub type_sig: Option<TypeSignature>,
}

#[derive(Hash, Clone, Debug, Serialize, Deserialize)]
pub struct FunctionSignature {
    pub name: Option<String>,
    pub params: Vec<VariableSignature>,
    pub return_type: Option<TypeSignature>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Ast {
    // identifier name
    Identifier(String),

    // number value
    Number(f64),

    // string value
    String(String),

    // boolean value
    Bool(bool),

    // expr
    Statement(Box<Ast>),

    // operator, left expr, right expr
    Binary(BinaryOperation, Box<Ast>, Box<Ast>),

    // operator, expr
    Unary(UnaryOperation, Box<Ast>),

    // returned expression
    Return(Box<Ast>),

    // vector of expr
    Block(Vec<Ast>),

    // if cond, if expr, else if conds, else if exprs, optional else expr
    IfElse(Box<Ast>, Box<Ast>, Vec<(Box<Ast>, Box<Ast>)>, Option<Box<Ast>>),

    // while cond, while expr
    While(Box<Ast>, Box<Ast>),

    // variable name, mutable, optional value expr
    Let(VariableSignature, bool, Option<Box<Ast>>),

    // import name
    Import(String),

    // optional function name, function parameters, return type, implementation
    FnDef(FunctionSignature, Box<Ast>),

    // function name, arguments
    FnCall(String, Vec<Ast>),
}