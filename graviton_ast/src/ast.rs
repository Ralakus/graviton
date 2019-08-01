
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
    Equal,
    NotEqual,

    Assign
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum UnaryOperation {
    Negate,

    Not
}

#[derive(Hash, Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
pub enum PrimitiveType {
    Nil,
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64
}

#[derive(Hash, Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
pub enum TypeSignature {
    Primitive(PrimitiveType),
    Custom(String)
}

impl<'a> TypeSignature {
    pub fn new(t: &'a str) -> TypeSignature {
        match t {
            "nil"  => TypeSignature::Primitive(PrimitiveType::Nil),
            "bool" => TypeSignature::Primitive(PrimitiveType::Bool),
            "i8"   => TypeSignature::Primitive(PrimitiveType::I8),
            "i16"  => TypeSignature::Primitive(PrimitiveType::I16),
            "i32"  => TypeSignature::Primitive(PrimitiveType::I32),
            "i64"  => TypeSignature::Primitive(PrimitiveType::I64),
            "u8"   => TypeSignature::Primitive(PrimitiveType::U8),
            "u16"  => TypeSignature::Primitive(PrimitiveType::U16),
            "u32"  => TypeSignature::Primitive(PrimitiveType::U32),
            "u64"  => TypeSignature::Primitive(PrimitiveType::U64),
            _ => TypeSignature::Custom(t.to_string())
        }
    }
}

#[derive(Hash, Clone, Debug, Serialize, Deserialize)]
pub struct VariableSignature {
    pub name: String,
    pub type_sig: Option<TypeSignature>,
}

#[derive(Hash, Clone, Debug, Serialize, Deserialize)]
pub struct FunctionSignature {
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
    Statement(Box<AstNode>),

    // operator, left expr, right expr
    Binary(BinaryOperation, Box<AstNode>, Box<AstNode>),

    // operator, expr
    Unary(UnaryOperation, Box<AstNode>),

    // returned expression
    Return(Box<AstNode>),

    // vector of expr
    Block(Vec<AstNode>),

    // if cond, if expr, else if conds, else if exprs, optional else expr
    IfElse(Box<AstNode>, Box<AstNode>, Vec<(Box<AstNode>, Box<AstNode>)>, Option<Box<AstNode>>),

    // while cond, while expr
    While(Box<AstNode>, Box<AstNode>),

    // variable name, mutable, optional value expr
    Let(VariableSignature, bool, Option<Box<AstNode>>),

    // import file name, file's ast
    Import(String, Box<AstNode>),

    // optional function name, function parameters, return type, implementation
    FnDef(Option<String>, FunctionSignature, Box<AstNode>),

    // expression that evaluates to function, arguments
    FnCall(Box<AstNode>, Vec<AstNode>),
}

#[derive(Clone, Serialize, Deserialize)]
pub struct AstNode {
    pub node: Ast,

    #[cfg(feature = "node_code_pos")]
    pub pos: super::Position
}

impl std::fmt::Debug for AstNode {
    #[cfg(not(feature = "node_code_pos"))]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{}", format!("{:#?}", self.node))
        } else {
            write!(f, "{}", format!("{:?}", self.node))
        }
    }

    #[cfg(feature = "node_code_pos")]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{}", format!("[{},{}]: {:#?}", self.pos.line, self.pos.col, self.node))
        } else {
            write!(f, "{}", format!("[{},{}]: {:?}", self.pos.line, self.pos.col, self.node))
        }
    }
}