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
    /// Type of the return value
    pub return_type_signature: Box<TypeSignature>,
}

/// The types of binary operations that can be performed
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

/// The types of unary operations that can be performed
#[repr(u8)]
#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum UnaryOperation {
    Negate,

    Not,
}

/// A vector wrapper that has a flag that states if the vector is completed grammar wise or not 
#[derive(Clone, Serialize, Deserialize)]
pub struct FinishVec<T> {
    pub finished: bool,
    pub v: Vec<T>
}

/// An AST node type that doesn't store references to other nodes.
/// Has all the possible node Types
#[derive(Clone, Serialize, Deserialize)]
pub enum NodeType {
    /// Highest level of an AST;
    /// A struct that contains the name of the file, all statements in global
    Module {
        name: String,
        declarations: FinishVec<TypeSignature>,
    },

    // --------------------
    // Declarations
    // --------------------
    //
    /// Declaration;
    /// A struct that contains a function's signature, name, parameter names, and body as defined in source
    Function {
        signature: FunctionSignature,
        name: String,
        parameter_names: Vec<String>,
        body: TypeSignature,
    },

    /// Declaration;
    /// A struct that contains an external's function name, parameter names, and signature
    ExternalFunction {
        signature: FunctionSignature,
        name: String,
        parameter_names: Vec<String>,
    },

    /// Declaration;
    /// A struct that contains a struct's name, field names, and signature
    Struct {
        signature: StructSignature,
        name: String,
        field_names: Vec<String>,
    },

    /// Declaration;
    /// A struct that contains the name of a module to import
    Import {
        /// Struct signature is because imports are treated like structs where you can access each declaration as if it were a struct.
        /// Similar to the Zig Programing Language
        signature: StructSignature,
        name: String,
    },

    // --------------------
    // Statements
    // --------------------
    //
    /// Statement;
    /// A struct that contains the while condition and body
    While {
        condition: TypeSignature,
        body: TypeSignature,
    },

    /// Statement;
    /// Contains an expression to return
    Return { expression: TypeSignature },

    // --------------------
    // Expressions
    // --------------------
    //
    /// Expression;
    /// A struct that contains a list of statements and an expression that returns a result
    Block {
        statements: FinishVec<TypeSignature>,
        end_expression: TypeSignature,
    },

    /// Expression;
    /// A struct that contains the if condition, the if body, and all else if's conditions and bodies, and the else body
    IfElse {
        condition: TypeSignature,
        body: TypeSignature,
        /// A vector of else if conditions (index 0) and else if bodies (index 1)
        else_ifs: FinishVec<(TypeSignature, TypeSignature)>,
        else_expression: TypeSignature,
    },

    /// Expression;
    /// Contains the expression that evalutates to a function, and the arguments provided to call said function
    FunctionCall {
        function: TypeSignature,
        arguments: FinishVec<TypeSignature>,
    },

    /// Expression;
    /// Contains a type signature that an expression is being casted to
    As {
        signature: TypeSignature,
        expression: TypeSignature,
    },

    /// Expression;
    /// Contains a binary operation and left and right expression
    Binary {
        op: BinaryOperation,
        left: TypeSignature,
        right: TypeSignature,
    },

    /// Expression;
    /// Contains a unary operator and an expression
    Unary {
        op: UnaryOperation,
        expression: TypeSignature
    },

    /// Expression;
    /// Contains an identifier name
    Identifier { name: String },

    /// Expression;
    /// A string value
    String { value: String },

    /// Expression;
    /// A boolean value
    Bool { value: bool },

    /// Expression;
    /// A floating point value
    Float { value: f64 },

    /// Expression;
    /// An integer value
    Integer { value: i64 },
}
