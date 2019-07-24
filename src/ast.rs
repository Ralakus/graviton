
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
pub enum Ast {
    Identifier(String),
    Number(f64),
    String(String),
    Binary(BinaryOperation, Box<Ast>, Box<Ast>),
    Unary(UnaryOperation, Box<Ast>),
    Grouping(Box<Ast>),
    List(Vec<Box<Ast>>)
}