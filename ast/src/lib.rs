use serde::{Deserialize, Serialize};

/// Stores the type information of any data in Graviton
#[derive(Clone, Serialize, Deserialize)]
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
#[derive(Copy, Clone, Serialize, Deserialize)]
pub enum PrimitiveType {
    /// An integer that has one bit dedicated to the signing (+/-) of the integer.
    /// `bitsize` must always be a power of two (2) on binary systems
    SignedInteger { bitsize: u8 },
    /// An always positive integer.
    /// `bitsize` must always be a power of two (2) on binary systems
    UnsignedInteger { bitsize: u8 },
    /// Floating point type.
    /// `bitsize` must be a power of two (2) on binary systems
    FloatingPoint { bitsize: u8 },
    /// True of false value, can also be -1 and 0
    Boolean,
    /// C's `void` equivalent in Graviton
    Nil,
}

/// Stores any data for a struct type
#[derive(Clone, Serialize, Deserialize)]
pub struct StructSignature {
    /// Publicity (bool) and type signature of each struct field
    pub fields: Vec<(bool, TypeSignature)>,
}

/// Stores a functions name, parameters, and return type
#[derive(Clone, Serialize, Deserialize)]
pub struct FunctionSignature {
    /// Mutability (bool) and type signature of each parameter
    pub parameters: Vec<(bool, TypeSignature)>,
}

/// An AST node type that doesn't store references to other nodes.
/// Has all the possible node Types
#[derive(Clone, Serialize, Deserialize)]
pub enum NodeType {
    /// Highest level of an AST.
    /// A struct that contains the name of the file, all statements in global
    Module {
        name: String,
        declarations: Vec<TypeSignature>,
    },

    // --------------------
    // Declarations
    // --------------------

    /// Declaration.
    /// A struct that contains a functions signature, name, parameter names, and body as defined in source
    Function {
        signature: FunctionSignature,
        name: String,
        parameter_names: Vec<String>,
        body: TypeSignature
    },

    

}
