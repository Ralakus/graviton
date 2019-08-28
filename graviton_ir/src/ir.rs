use super::ast;

use serde::{Serialize, Deserialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct Module {
    pub functions: Vec<Function>,
    pub globals: Vec<VarDecl>,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Instruction {
    Iconst(i64),
    Fconst(f64),

    Uadd(Box<Instruction>, Box<Instruction>),
    Iadd(Box<Instruction>, Box<Instruction>),

    Isub(Box<Instruction>, Box<Instruction>),
    Usub(Box<Instruction>, Box<Instruction>),

    Umul(Box<Instruction>, Box<Instruction>),
    Imul(Box<Instruction>, Box<Instruction>),

    Udiv(Box<Instruction>, Box<Instruction>),
    Idiv(Box<Instruction>, Box<Instruction>),

    Fadd(Box<Instruction>, Box<Instruction>),

    // Integer compare equal
    ICmpe(Box<Instruction>, Box<Instruction>),
    // Integer compare not equal
    ICmpne(Box<Instruction>, Box<Instruction>),
    // Integer compare greater than
    ICmpgt(Box<Instruction>, Box<Instruction>),
    // Integer compare less than
    ICmplt(Box<Instruction>, Box<Instruction>),
    // Integer compare greather than equal
    ICmpgte(Box<Instruction>, Box<Instruction>),
    // Integer compare less than equal
    ICmplte(Box<Instruction>, Box<Instruction>),

    // Floating point compare equal
    FCmpe(Box<Instruction>, Box<Instruction>),
    // Floating point compare not equal
    FCmpne(Box<Instruction>, Box<Instruction>),
    // Floating point compare greater than
    FCmpgt(Box<Instruction>, Box<Instruction>),
    // Floating point compare less than
    FCmplt(Box<Instruction>, Box<Instruction>),
    // Floating point compare greather than equal
    FCmpgte(Box<Instruction>, Box<Instruction>),
    // Floating point compare less than equal
    FCmplte(Box<Instruction>, Box<Instruction>),

    // Jump to block if instruction evaluates to zero (false) and passes arugments
    Brz(Box<Instruction>, Box<BasicBlock>, Vec<Instruction>),
    // Jump to block if instruction doesn't evaluates to zero (true) and passes arugments
    Brnz(Box<Instruction>, Box<BasicBlock>, Vec<Instruction>),

    // Calls a function directly
    Call(String, Vec<Instruction>),
    // Calls a function pointer
    CallPtr(Box<Instruction>, Vec<Instruction>),

    // Returns from function
    Return(Box<Instruction>)
}

#[derive(Debug, Serialize, Deserialize)]
pub struct BasicBlock {
    pub instructions: Vec<Instruction>,
    pub arguments: Vec<ast::TypeSignature>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub blocks: Vec<BasicBlock>,
    pub signature: ast::FunctionSignature,
    pub variables: Vec<VarDecl>
}

#[derive(Debug, Serialize, Deserialize)]
pub struct VarDecl {
    pub name: String,
    pub type_: Vec<ast::TypeSignature>,
    pub assign: Box<Instruction>,
}
