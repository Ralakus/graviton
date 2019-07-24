
#[derive(Copy, Clone, Debug)]
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

#[derive(Copy, Clone, Debug)]
pub enum UnaryOperation {
    Negate,

    Not
}

#[derive(Debug, Clone)]
pub enum AST {
    Identifier(String),
    Number(f64),
    String(String),
    Binary(BinaryOperation, Box<AST>, Box<AST>),
    Unary(UnaryOperation, Box<AST>),
    Grouping(Box<AST>),
    List(Vec<Box<AST>>)
}