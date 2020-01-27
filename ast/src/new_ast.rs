use serde::{Deserialize, Serialize};

#[repr(u8)]
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

    And,
    Or,

    Assign,
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum UnaryOperation {
    Negate,

    Not,
}

#[repr(u8)]
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
    U64,
    F32,
    F64,
}

#[derive(Hash, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum TypeSignature {
    Primitive(PrimitiveType),
    Function(FunctionSignature),
    Custom(String),
}

impl<'a> TypeSignature {
    pub fn new(t: &'a str) -> TypeSignature {
        match t {
            "Nil" => TypeSignature::Primitive(PrimitiveType::Nil),
            "Bool" => TypeSignature::Primitive(PrimitiveType::Bool),
            "I8" => TypeSignature::Primitive(PrimitiveType::I8),
            "I16" => TypeSignature::Primitive(PrimitiveType::I16),
            "I32" => TypeSignature::Primitive(PrimitiveType::I32),
            "I64" => TypeSignature::Primitive(PrimitiveType::I64),
            "U8" => TypeSignature::Primitive(PrimitiveType::U8),
            "U16" => TypeSignature::Primitive(PrimitiveType::U16),
            "U32" => TypeSignature::Primitive(PrimitiveType::U32),
            "U64" => TypeSignature::Primitive(PrimitiveType::U64),
            "F32" => TypeSignature::Primitive(PrimitiveType::F32),
            "F64" => TypeSignature::Primitive(PrimitiveType::F64),
            _ => TypeSignature::Custom(t.to_string()),
        }
    }
    pub fn is_number(&self) -> bool {
        match self {
            TypeSignature::Primitive(p) => match p {
                PrimitiveType::Nil | PrimitiveType::Bool => false,
                _ => true,
            },
            _ => false,
        }
    }
    pub fn is_bool(&self) -> bool {
        match self {
            TypeSignature::Primitive(p) => match p {
                PrimitiveType::Bool => true,
                _ => false,
            },
            _ => false,
        }
    }
    pub fn is_nil(&self) -> bool {
        match self {
            TypeSignature::Primitive(p) => match p {
                PrimitiveType::Nil => true,
                _ => false,
            },
            _ => false,
        }
    }
    pub fn is_signed(&self) -> bool {
        match self {
            TypeSignature::Primitive(p) => match p {
                PrimitiveType::I8
                | PrimitiveType::I16
                | PrimitiveType::I32
                | PrimitiveType::I64 => true,
                _ => false,
            },
            _ => false,
        }
    }
    pub fn is_unsigned(&self) -> bool {
        match self {
            TypeSignature::Primitive(p) => match p {
                PrimitiveType::U8
                | PrimitiveType::U16
                | PrimitiveType::U32
                | PrimitiveType::U64 => true,
                _ => false,
            },
            _ => false,
        }
    }
    pub fn is_function(&self) -> bool {
        match self {
            TypeSignature::Function(_) => true,
            _ => false,
        }
    }
    pub fn is_custom(&self) -> bool {
        match self {
            TypeSignature::Custom(_) => true,
            _ => false,
        }
    }
    pub fn is_integer(&self) -> bool {
        match self {
            TypeSignature::Primitive(p) => match p {
                PrimitiveType::I8
                | PrimitiveType::I16
                | PrimitiveType::I32
                | PrimitiveType::I64
                | PrimitiveType::U8
                | PrimitiveType::U16
                | PrimitiveType::U32
                | PrimitiveType::U64 => true,
                _ => false,
            },
            _ => false,
        }
    }
    pub fn is_float(&self) -> bool {
        match self {
            TypeSignature::Primitive(p) => match p {
                PrimitiveType::F32 | PrimitiveType::F64 => true,
                _ => false,
            },
            _ => false,
        }
    }
    pub fn is_8bit(&self) -> bool {
        match self {
            TypeSignature::Primitive(p) => match p {
                PrimitiveType::I8 | PrimitiveType::U8 => true,
                _ => false,
            },
            _ => false,
        }
    }
    pub fn is_16bit(&self) -> bool {
        match self {
            TypeSignature::Primitive(p) => match p {
                PrimitiveType::I16 | PrimitiveType::U16 => true,
                _ => false,
            },
            _ => false,
        }
    }
    pub fn is_32bit(&self) -> bool {
        match self {
            TypeSignature::Primitive(p) => match p {
                PrimitiveType::I32 | PrimitiveType::U32 | PrimitiveType::F32 => true,
                _ => false,
            },
            _ => false,
        }
    }
    pub fn is_64bit(&self) -> bool {
        match self {
            TypeSignature::Primitive(p) => match p {
                PrimitiveType::I64 | PrimitiveType::U64 | PrimitiveType::F64 => true,
                _ => false,
            },
            _ => false,
        }
    }
}

impl std::fmt::Debug for TypeSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeSignature::Primitive(p) => match p {
                PrimitiveType::Nil => write!(f, "Nil"),
                PrimitiveType::Bool => write!(f, "Bool"),
                PrimitiveType::I8 => write!(f, "I8"),
                PrimitiveType::I16 => write!(f, "I16"),
                PrimitiveType::I32 => write!(f, "I32"),
                PrimitiveType::I64 => write!(f, "I64"),
                PrimitiveType::U8 => write!(f, "U8"),
                PrimitiveType::U16 => write!(f, "U16"),
                PrimitiveType::U32 => write!(f, "U32"),
                PrimitiveType::U64 => write!(f, "U64"),
                PrimitiveType::F32 => write!(f, "F32"),
                PrimitiveType::F64 => write!(f, "F64"),
            },
            TypeSignature::Function(func) => {
                if f.alternate() {
                    write!(f, "{:#?}", func)
                } else {
                    write!(f, "{:?}", func)
                }
            }
            TypeSignature::Custom(c) => write!(f, "{}", c),
        }
    }
}

#[derive(Hash, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct VariableSignature {
    pub mutable: bool,
    pub type_sig: Option<TypeSignature>,
}

impl std::fmt::Debug for VariableSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            if let Some(sig) = &self.type_sig {
                write!(f, "{}{:#?}", if self.mutable { "mut " } else { "" }, sig)
            } else {
                write!(f, "{}Untyped", if self.mutable { "mut " } else { "" })
            }
        } else if let Some(sig) = &self.type_sig {
            write!(f, "{}{:?}", if self.mutable { "mut " } else { "" }, sig)
        } else {
            write!(f, "{}Untyped", if self.mutable { "mut " } else { "" })
        }
    }
}

#[derive(Hash, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct FunctionSignature {
    pub params: Vec<VariableSignature>,
    pub return_type: Option<Box<TypeSignature>>,
}

impl std::fmt::Debug for FunctionSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "(")?;
            let len = self.params.len();
            let mut idx = 1;
            for p in &self.params {
                write!(f, "{:#?}{}", p, if idx < len { ", " } else { "" })?;
                idx += 1;
            }
            if let Some(rt) = &self.return_type {
                write!(f, ") -> {:#?}", rt)
            } else {
                write!(f, ") -> Untyped")
            }
        } else {
            write!(f, "(")?;
            let len = self.params.len();
            let mut idx = 1;
            for p in &self.params {
                write!(f, "{:?}{}", p, if idx < len { ", " } else { "" })?;
                idx += 1;
            }
            if let Some(rt) = &self.return_type {
                write!(f, ") -> {:?}", rt)
            } else {
                write!(f, ") -> Untyped")
            }
        }
    }
}

/*#[macro_export]
macro_rules! make_fn_sig {
    (( $( $type_:ident ),* ) -> $ret:ident) => {
        ast::FunctionSignature{ params: vec!(
            $(
                ast::VariableSignature { mutable: true, type_sig: Some(ast::TypeSignature::new(stringify!($type_))) },
            )*
        ), return_type: Some(Box::new(ast::TypeSignature::new(stringify!($ret)))) }
    };
}*/

// a file
#[derive(Clone, Serialize, Deserialize)]
pub struct Module {
    pub file: Option<String>,
    pub declarations: Vec<Declaration>,
}

// anything in a module's global scope or anything that declares an identifier
#[derive(Clone, Serialize, Deserialize)]
pub enum Declaration {
    // function definition fn_name :: (x: Type, y: Type) -> ReturnType { /* body */ }
    // function signature, param names, body
    Function(FunctionSignature, Vec<String>, Expression),

    // function extern fn_name :: extern (x: Type, y: Type) -> ReturnType;
    // function signature, function name
    ExternFunction(FunctionSignature, String),

    // struct definition struct_name :: struct { /* body */ }
    // name, containing types ( name , type signature)
    Struct(String, Vec<(String, TypeSignature)>),

    // variable declaration var_name: type = val_expr;
    // name, type signature, assign expression
    Variable(String, VariableSignature, Option<Expression>),

    // import delcaration import module_name
    // path (each directory), name
    Import(Vec<String>, String),
}

/*
// anything that changes state but doesn't return a result
#[derive(Clone, Serialize, Deserialize)]
pub enum Statement {

    Declaration()

}*/

#[derive(Clone, Serialize, Deserialize)]
pub enum Expression {
    // name
    Identifier(String),

    // any numeric integer in base 10
    // integer value
    Integer(i64),

    // floating point value
    Float(f64),

    // any raw string value, not formatted in any way as of now
    // string value
    String(String),

    // boolean value
    Bool(bool),

    // operator, left expr, right expr
    Binary(BinaryOperation, Box<Expression>, Box<Expression>),

    // operator, expr
    Unary(UnaryOperation, Box<Expression>),

    // returned expression
    Return(Box<Expression>),

    // vector of expr
    Block(Vec<Expression>),

    // if cond, if expr, else if conds, else if exprs, optional else expr
    IfElse(
        Box<Expression>,
        Box<Expression>,
        Vec<(Box<Expression>, Box<Expression>)>,
        Option<Box<Expression>>,
    ),

    // while cond, while expr
    While(Box<Expression>, Box<Expression>),

    // expression that evaluates to function, arguments
    FnCall(Box<Expression>, Vec<Expression>),

    // primitive cast to another type expr as Type
    // expression, type to cast to
    As(Box<Expression>, TypeSignature),
}