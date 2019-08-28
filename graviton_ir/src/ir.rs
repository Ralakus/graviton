use super::ast;

pub struct Module {
    functions: Vec<Function>,
    globals: Vec<VarDecl>,
}

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

    // Compare equal
    ICmpe(Box<Instruction>, Box<Instruction>),
    // Compare not equal
    ICmpne(Box<Instruction>, Box<Instruction>),
    // Compare greater than
    ICmpgt(Box<Instruction>, Box<Instruction>),
    // Compare less than
    ICmplt(Box<Instruction>, Box<Instruction>),
    // Compare greather than equal
    ICmpgte(Box<Instruction>, Box<Instruction>),
    // Compare less than equal
    ICmplte(Box<Instruction>, Box<Instruction>),

    // Compare equal
    FCmpe(Box<Instruction>, Box<Instruction>),
    // Compare not equal
    FCmpne(Box<Instruction>, Box<Instruction>),
    // Compare greater than
    FCmpgt(Box<Instruction>, Box<Instruction>),
    // Compare less than
    FCmplt(Box<Instruction>, Box<Instruction>),
    // Compare greather than equal
    FCmpgte(Box<Instruction>, Box<Instruction>),
    // Compare less than equal
    FCmplte(Box<Instruction>, Box<Instruction>),

    BranchZero(Box<Instruction>, Box<BasicBlock>),
    BranchNotZero(Box<Instruction>, Box<BasicBlock>),
}

pub struct BasicBlock {
    instructions: Vec<Instruction>,
    arguments: Vec<ast::TypeSignature>,
}

pub struct Function {
    blocks: Vec<BasicBlock>,
    arguments: Vec<ast::TypeSignature>,
}

pub struct VarDecl {
    name: String,
    type_: Vec<ast::TypeSignature>,
    assign: Box<Instruction>,
}
