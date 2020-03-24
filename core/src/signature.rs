use serde::{Deserialize, Serialize};

/// Stores the type information of any data in Graviton
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeSignature {
    /// Here as a replacement to having Option<TypeSignature>, same as Option::None
    None,
    /// Type signature hasn't been typed by a typer yet
    Untyped,
    /// Primtive type
    Primitive(PrimitiveType),
    /// Struct type
    Struct(StructSignature),
    /// Function type
    Function(FunctionSignature),
}

/// Primtive types built into Graviton by default
#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum PrimitiveType {
    /// C's `void` equivalent in Graviton
    Nil,
    /// True of false value, can also be -1 and 0
    Boolean,
    /// An integer that has one bit dedicated to the signing (+/-) of the integer.
    /// `bitsize` must always be a power of two (2) on binary systems
    SignedInteger { bitsize: u8 },
    /// An always positive integer.
    /// `bitsize` must always be a power of two (2) on binary systems
    UnsignedInteger { bitsize: u8 },
    /// Floating point type.
    /// `bitsize` must be a power of two (2) on binary systems
    FloatingPoint { bitsize: u8 },
    /// Constant string type
    Str,
}

impl PrimitiveType {
    /// Parses a string into a primitive type
    pub fn new(type_string: &str) -> Self {
        match type_string {
            s if s.starts_with('I') => PrimitiveType::SignedInteger {
                bitsize: match s[1..].parse::<u8>() {
                    Ok(size) => size,
                    Err(_) => return PrimitiveType::Nil,
                },
            },
            s if s.starts_with('U') => PrimitiveType::UnsignedInteger {
                bitsize: match s[1..].parse::<u8>() {
                    Ok(size) => size,
                    Err(_) => return PrimitiveType::Nil,
                },
            },
            s if s.starts_with('F') => PrimitiveType::FloatingPoint {
                bitsize: match s[1..].parse::<u8>() {
                    Ok(size) => size,
                    Err(_) => return PrimitiveType::Nil,
                },
            },
            "Bool" => PrimitiveType::Boolean,
            "Str" => PrimitiveType::Str,
            _ => PrimitiveType::Nil,
        }
    }
}

/// Stores any data for a struct type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructSignature {
    /// Publicity (bool) and type signature of each struct field
    pub fields: Vec<(bool, TypeSignature)>,
}

/// Stores a functions name, parameters, and return type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionSignature {
    ///Type signature of each parameter
    pub parameters: Vec<TypeSignature>,
    /// Type of the return value
    pub return_type_signature: Box<TypeSignature>,
}

use fmt::{Display, Formatter};
use std::fmt;

impl Display for TypeSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypeSignature::None => write!(f, "None"),
            TypeSignature::Untyped => write!(f, "Untyped"),
            TypeSignature::Primitive(p) => write!(f, "{}", p),
            TypeSignature::Struct(s) => write!(f, "{}", s),
            TypeSignature::Function(func) => write!(f, "{}", func),
        }
    }
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self {
            PrimitiveType::Nil => write!(f, "Nil"),
            PrimitiveType::Boolean => write!(f, "Bool"),
            PrimitiveType::SignedInteger { bitsize } => write!(f, "I{}", bitsize),
            PrimitiveType::UnsignedInteger { bitsize } => write!(f, "U{}", bitsize),
            PrimitiveType::FloatingPoint { bitsize } => write!(f, "F{}", bitsize),
            PrimitiveType::Str => write!(f, "Str"),
        }
    }
}

impl Display for StructSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "struct {{ ")?;
        for field in &self.fields {
            write!(f, "{}{}, ", if field.0 { "pub " } else { "" }, field.1)?;
        }
        write!(f, "}}")
    }
}

impl Display for FunctionSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "fn ( ")?;
        for parameter in &self.parameters {
            write!(f, "{}, ", parameter)?;
        }
        write!(f, ") -> {}", self.return_type_signature)
    }
}
