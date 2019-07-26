

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

    // vector of expr
    Block(Vec<Ast>),

    // if cond, if expr, else if conds, else if exprs, optional else expr
    IfElse(Box<Ast>, Box<Ast>, Vec<(Box<Ast>, Box<Ast>)>, Option<Box<Ast>>),

    // while cond, while expr
    While(Box<Ast>, Box<Ast>),

    // variable name, mutable, optional value expr
    Let(String, bool, Option<Box<Ast>>)
}