use serde::{Deserialize, Serialize};

use super::{itertools, Position};
use colored::Colorize;

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
}

impl PrimitiveType {
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FinishVec<T> {
    pub finished: bool,
    pub v: Vec<T>,
}

/// The ast that'll be passed between each stage
#[derive(Default, Debug)]
pub struct Ast {
    nodes: Vec<Node>,
    type_signatures: Vec<TypeSignature>,
    positions: Vec<Position>,
}

impl Ast {
    /// Creates a new empty `Ast`
    pub fn new() -> Self {
        Ast::default()
    }

    /// Pushses a new node to all of the vectors so each vector will be the same size
    pub fn push(&mut self, node: Node, type_signature: TypeSignature, pos: Position) {
        self.nodes.push(node);
        self.type_signatures.push(type_signature);
        self.positions.push(pos);
    }

    /// Safe mutable iterator for accessing the ast
    pub fn iter_mut(
        &mut self,
    ) -> impl Iterator<Item = (&mut Node, &mut TypeSignature, &mut Position)> {
        itertools::izip!(
            &mut self.nodes,
            &mut self.type_signatures,
            &mut self.positions
        )
    }

    /// Safe iterator for accessing the ast
    pub fn iter(&self) -> impl Iterator<Item = (&Node, &TypeSignature, &Position)> {
        itertools::izip!(&self.nodes, &self.type_signatures, &self.positions)
    }
}

/// An AST node that doesn't store references to other nodes.
/// Instead it stores the index of child nodes in the stack to make it memory efficient.
/// Has all the possible node types.
/// A match expression that matches all of the nodes
/// ```rust
/// match node {
///     core::ast::Node::Invalid => {}
///     core::ast::Node::Module { name, declarations } => {}
///     core::ast::Node::Function { signature, name, parameter_names, body } => {}
///     core::ast::Node::ExternalFunction { signature, name, parameter_names } => {}
///     core::ast::Node::Struct { signature, name, field_names } => {}
///     core::ast::Node::Import { signature, name } => {}
///     core::ast::Node::While { condition, body } => {}
///     core::ast::Node::Return { expression } => {}
///     core::ast::Node::Let { signature, name, assign, mutable } => {}
///     core::ast::Node::Block { statements, end_expression } => {}
///     core::ast::Node::IfElse { condition, body, else_ifs, else_expression } => {}
///     core::ast::Node::FunctionCall { function, arguments } => {}
///     core::ast::Node::As { signature, expression } => {}
///     core::ast::Node::Binary { op, left, right } => {}
///     core::ast::Node::Unary { op, expression } => {}
///     core::ast::Node::Identifier { name } => {}
///     core::ast::Node::String { value } => {}
///     core::ast::Node::Bool { value } => {}
///     core::ast::Node::Float { value } => {}
///     core::ast::Node::Integer { value } => {}
///     core::ast::Node::File { name } => {}
///     core::ast::Node::IdentifierData { name } => {}
///     core::ast::Node::Parameter { name } => {}
///     core::ast::Node::TypeSignature { signature } => {}
///     core::ast::Node::StructSignature { signature } => {}
///     core::ast::Node::FunctionSignature { signature } => {}
/// }
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Node {
    /// Invalid node;
    /// Used for errors
    Invalid,
    /// Highest level of an AST;
    /// A struct that contains the name of the file, all statements in global
    Module {
        name: usize,
        declarations: FinishVec<usize>,
    },

    // --------------------
    // Declarations
    // --------------------
    //
    /// Declaration;
    /// A struct that contains a function's signature, name, parameter names, and body as defined in source
    Function {
        /// Index to `FunctionSignature` node
        signature: usize,
        /// Index to `Identifier` node
        name: usize,
        /// Indexes to `Identifier` nodes
        parameter_names: Vec<usize>,
        /// Ofsset to any `Expression` node
        body: usize,
    },

    /// Declaration;
    /// A struct that contains an external's function name, parameter names, and signature
    ExternalFunction {
        /// Index to `FunctionSignature` node
        signature: usize,
        /// Index to `Identifier` nodes
        name: usize,
        /// Indexes to `Identifier` nodes
        parameter_names: Vec<usize>,
    },

    /// Declaration;
    /// A struct that contains a struct's name, field names, and signature
    Struct {
        /// Index to `StructSignature` nodes
        signature: usize,
        /// Index to `Identifier` node
        name: usize,
        /// Indexes to `Identifier` nodes
        field_names: Vec<usize>,
    },

    /// Declaration;
    /// A struct that contains the name of a module to import
    Import {
        /// Index to `StructSignature` node.
        /// Struct signature is because imports are treated like structs where you can access each declaration as if it were a struct.
        /// Similar to the Zig Programing Language
        signature: usize,
        /// Index to `Identifier` node
        name: usize,
    },

    // --------------------
    // Statements
    // --------------------
    //
    /// Statement;
    /// A struct that contains the while condition and body
    While {
        /// Index to `Expression` node
        condition: usize,
        /// Index to `Expression` node
        body: usize,
    },

    /// Statement;
    /// Contains an expression to return
    Return {
        /// Index to `Expression` node
        expression: usize,
    },

    /// Statement;
    /// Contains variable name and assign expression
    Let {
        /// Index to `TypeSignature` node
        signature: usize,
        /// Index to `Identifier` node
        name: usize,
        /// Index to `Expression` node
        assign: usize,
        /// Mutable or not
        mutable: bool,
    },

    // --------------------
    // Expressions
    // --------------------
    //
    /// Expression;
    /// A struct that contains a list of statements and an expression that returns a result
    Block {
        /// Indexes to `Statement` nodes
        statements: FinishVec<usize>,
        /// Indexes to `Expression` node
        end_expression: usize,
    },

    /// Expression;
    /// A struct that contains the if condition and the if expression and any children elseifs
    IfElse {
        /// Index to `Expression` node
        condition: usize,
        /// Index to `Expression` node
        body: usize,
        /// Indexes to `Expression` nodes.
        /// A vector of else if conditions (index 0) and else if bodies (index 1)
        else_ifs: FinishVec<(usize, usize)>,
        /// Index to `Expression` node
        else_expression: usize,
    },

    /// Expression;
    /// Contains the expression that evalutates to a function, and the arguments provided to call said function
    FunctionCall {
        /// Index to `Expression` node
        function: usize,
        /// Index to `Expression` node
        arguments: FinishVec<usize>,
    },

    /// Expression;
    /// Contains a type signature that an expression is being casted to
    As {
        /// Index to `TypeSignature` node
        signature: usize,
        /// Index to `Expression` node
        expression: usize,
    },

    /// Expression;
    /// Contains a binary operation and left and right expression
    Binary {
        op: BinaryOperation,
        /// Index to `Expression` node
        left: usize,
        /// Index to `Expression` node
        right: usize,
    },

    /// Expression;
    /// Contains a unary operator and an expression
    Unary {
        op: UnaryOperation,
        /// Index to `Expression` node
        expression: usize,
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

    // --------------------
    // Data nodes
    // --------------------
    //
    /// Data;
    /// A file name node
    File { name: String },

    /// Data;
    /// A identifier name node
    IdentifierData { name: String },

    /// Data;
    /// A parameter name node
    Parameter { name: String },

    // --------------------
    // Signature nodes
    // --------------------
    //
    /// Signature;
    /// A full type signature node
    TypeSignature { signature: TypeSignature },

    /// Signature;
    /// A full struct signature node
    StructSignature { signature: StructSignature },

    /// Signature;
    /// A function signature node
    FunctionSignature { signature: FunctionSignature },
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
        }
    }
}

impl Display for StructSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "struct {{ ")?;
        for field in &self.fields {
            write!(f, "{}{}, ", if field.0 { "mut " } else { "" }, field.1)?;
        }
        write!(f, "}}")
    }
}

impl Display for FunctionSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "fn ( ")?;
        for parameter in &self.parameters {
            write!(
                f,
                "{}{}, ",
                if parameter.0 { "mut " } else { "" },
                parameter.1
            )?;
        }
        write!(f, ") -> {}", self.return_type_signature)
    }
}

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        ast_formatter(f, 0, &self, self.iter().next().unwrap())
    }
}

/// AST printer's main node tab
const AST_FMT_DASH: char = '-';
/// AST printer's atom's and signature tab
const AST_FMT_SPACE: char = ' ';
/// AST print's tab length
const AST_FMT_TAB_LENGTH: usize = 4;

/// Repeats a character a certain amount of times
fn repeat_char(c: char, times: usize) -> String {
    std::iter::repeat(c).take(times).collect::<String>()
}

/// Tabs to the correct depth
fn ast_fmt_tab(f: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
    if depth != 0 {
        for _ in 0..depth - 1 {
            write!(f, "|{}", repeat_char(AST_FMT_SPACE, AST_FMT_TAB_LENGTH))?;
        }
        write!(f, "|{}", repeat_char(AST_FMT_DASH, AST_FMT_TAB_LENGTH))
    } else {
        Ok(())
    }
}

/// A recursive ast walker that prints a formatted ast
fn ast_formatter(
    f: &mut std::fmt::Formatter<'_>,
    depth: usize,
    ast: &Ast,
    node: (&Node, &TypeSignature, &Position),
) -> std::fmt::Result {
    ast_fmt_tab(f, depth)?;
    match node.0 {
        Node::Invalid => writeln!(
            f,
            "{} {}",
            "Invalid".color(colored::Color::Cyan),
            node.2.to_string().color(colored::Color::Red)
        ),
        Node::Module { name, declarations } => {
            writeln!(f, "{}", "Module".color(colored::Color::Cyan))?;
            let name_node = ast.iter().nth(*name).unwrap();
            ast_formatter(f, depth + 1, ast, name_node)?;
            for d in &declarations.v {
                let dnode = ast.iter().nth(*d).unwrap();
                ast_formatter(f, depth + 1, ast, dnode)?;
            }
            Ok(())
        }
        Node::Function {
            signature,
            name,
            parameter_names,
            body,
        } => {
            writeln!(
                f,
                "{} {}",
                "Function".color(colored::Color::Cyan),
                node.2.to_string().color(colored::Color::Red)
            )?;
            let signature_node = ast.iter().nth(*signature).unwrap();
            ast_formatter(f, depth + 1, ast, signature_node)?;
            let name_node = ast.iter().nth(*name).unwrap();
            ast_formatter(f, depth + 1, ast, name_node)?;
            for pn in parameter_names {
                let pnnode = ast.iter().nth(*pn).unwrap();
                ast_formatter(f, depth + 1, ast, pnnode)?;
            }
            let body_node = ast.iter().nth(*body).unwrap();
            ast_formatter(f, depth + 1, ast, body_node)
        }
        Node::ExternalFunction {
            signature,
            name,
            parameter_names,
        } => {
            writeln!(
                f,
                "{} {}",
                "ExternalFunction".color(colored::Color::Cyan),
                node.2.to_string().color(colored::Color::Red)
            )?;
            let signature_node = ast.iter().nth(*signature).unwrap();
            ast_formatter(f, depth + 1, ast, signature_node)?;
            let name_node = ast.iter().nth(*name).unwrap();
            ast_formatter(f, depth + 1, ast, name_node)?;
            for pn in parameter_names {
                let pnnode = ast.iter().nth(*pn).unwrap();
                ast_formatter(f, depth + 1, ast, pnnode)?;
            }
            Ok(())
        }
        Node::Struct {
            signature,
            name,
            field_names,
        } => {
            writeln!(
                f,
                "{} {}",
                "Struct".color(colored::Color::Cyan),
                node.2.to_string().color(colored::Color::Red)
            )?;
            let signature_node = ast.iter().nth(*signature).unwrap();
            ast_formatter(f, depth + 1, ast, signature_node)?;
            let name_node = ast.iter().nth(*name).unwrap();
            ast_formatter(f, depth + 1, ast, name_node)?;
            for fname in field_names {
                let fname_node = ast.iter().nth(*fname).unwrap();
                ast_formatter(f, depth + 1, ast, fname_node)?;
            }
            Ok(())
        }
        Node::Import { signature, name } => {
            writeln!(
                f,
                "{} {}",
                "Import".color(colored::Color::Cyan),
                node.2.to_string().color(colored::Color::Red)
            )?;
            let signature_node = ast.iter().nth(*signature).unwrap();
            ast_formatter(f, depth + 1, ast, signature_node)?;
            let name_node = ast.iter().nth(*name).unwrap();
            ast_formatter(f, depth + 1, ast, name_node)
        }
        Node::While { condition, body } => {
            writeln!(
                f,
                "{} {}",
                "While".color(colored::Color::Cyan),
                node.2.to_string().color(colored::Color::Red)
            )?;
            let condition_node = ast.iter().nth(*condition).unwrap();
            ast_formatter(f, depth + 1, ast, condition_node)?;
            let body_node = ast.iter().nth(*body).unwrap();
            ast_formatter(f, depth + 1, ast, body_node)
        }
        Node::Return { expression } => {
            writeln!(
                f,
                "{} {}",
                "Return".color(colored::Color::Cyan),
                node.2.to_string().color(colored::Color::Red)
            )?;
            let expression_node = ast.iter().nth(*expression).unwrap();
            ast_formatter(f, depth + 1, ast, expression_node)
        }
        Node::Let {
            signature,
            name,
            assign,
            mutable,
        } => {
            writeln!(
                f,
                "{} {}",
                "Let".color(colored::Color::Cyan),
                node.2.to_string().color(colored::Color::Red)
            )?;
            let signature_node = ast.iter().nth(*signature).unwrap();
            ast_formatter(f, depth + 1, ast, signature_node)?;
            let name_node = ast.iter().nth(*name).unwrap();
            ast_formatter(f, depth + 1, ast, name_node)?;
            let assign_node = ast.iter().nth(*assign).unwrap();
            ast_formatter(f, depth + 1, ast, assign_node)?;
            ast_fmt_tab(f, depth + 1)?;
            writeln!(
                f,
                "{} {}",
                "mutable:".color(colored::Color::Green),
                mutable.to_string().color(colored::Color::Yellow)
            )
        }
        Node::Block {
            statements,
            end_expression,
        } => {
            writeln!(
                f,
                "{} {}",
                "Block".color(colored::Color::Cyan),
                node.2.to_string().color(colored::Color::Red)
            )?;
            for s in &statements.v {
                let snode = ast.iter().nth(*s).unwrap();
                ast_formatter(f, depth + 1, ast, snode)?;
            }
            if *end_expression != 0 {
                let end_expression_node = ast.iter().nth(*end_expression).unwrap();
                ast_formatter(f, depth + 1, ast, end_expression_node)?;
            }
            Ok(())
        }
        Node::IfElse {
            condition,
            body,
            else_ifs,
            else_expression,
        } => {
            writeln!(
                f,
                "{} {}",
                "IfElse".color(colored::Color::Cyan),
                node.2.to_string().color(colored::Color::Red)
            )?;
            let condition_node = ast.iter().nth(*condition).unwrap();
            ast_formatter(f, depth + 1, ast, condition_node)?;
            let body_node = ast.iter().nth(*body).unwrap();
            ast_formatter(f, depth + 1, ast, body_node)?;
            for ef in &else_ifs.v {
                let ef0_node = ast.iter().nth(ef.0).unwrap();
                ast_formatter(f, depth + 1, ast, ef0_node)?;
                let ef1_node = ast.iter().nth(ef.1).unwrap();
                ast_formatter(f, depth + 1, ast, ef1_node)?;
            }
            if *else_expression != 0 {
                let else_expression_node = ast.iter().nth(*else_expression).unwrap();
                ast_formatter(f, depth + 1, ast, else_expression_node)?;
            }
            Ok(())
        }
        Node::FunctionCall {
            function,
            arguments,
        } => {
            writeln!(
                f,
                "{} {}",
                "FunctionCall".color(colored::Color::Cyan),
                node.2.to_string().color(colored::Color::Red)
            )?;
            let function_node = ast.iter().nth(*function).unwrap();
            ast_formatter(f, depth + 1, ast, function_node)?;
            for a in &arguments.v {
                let anode = ast.iter().nth(*a).unwrap();
                ast_formatter(f, depth + 1, ast, anode)?
            }
            Ok(())
        }
        Node::As {
            signature,
            expression,
        } => {
            writeln!(
                f,
                "{} {}",
                "As".color(colored::Color::Cyan),
                node.2.to_string().color(colored::Color::Red)
            )?;
            let signature_node = ast.iter().nth(*signature).unwrap();
            ast_formatter(f, depth + 1, ast, signature_node)?;
            let expression_node = ast.iter().nth(*expression).unwrap();
            ast_formatter(f, depth + 1, ast, expression_node)
        }
        Node::Binary { op, left, right } => {
            writeln!(
                f,
                "{} {}",
                format!("Binary {:?}", op).color(colored::Color::Cyan),
                node.2.to_string().color(colored::Color::Red)
            )?;
            let left_node = ast.iter().nth(*left).unwrap();
            ast_formatter(f, depth + 1, ast, left_node)?;
            let right_node = ast.iter().nth(*right).unwrap();
            ast_formatter(f, depth + 1, ast, right_node)
        }
        Node::Unary { op, expression } => {
            writeln!(
                f,
                "{} {}",
                format!("Unary {:?}", op).color(colored::Color::Cyan),
                node.2.to_string().color(colored::Color::Red)
            )?;
            let expression_node = ast.iter().nth(*expression).unwrap();
            ast_formatter(f, depth + 1, ast, expression_node)
        }
        Node::Identifier { name } => writeln!(
            f,
            "{} {} {}",
            "Identifier:".color(colored::Color::Cyan),
            name.to_string().color(colored::Color::Yellow),
            node.2.to_string().color(colored::Color::Red)
        ),
        Node::String { value } => writeln!(
            f,
            "{} {} {}",
            "String:".color(colored::Color::Cyan),
            value.color(colored::Color::Yellow),
            node.2.to_string().color(colored::Color::Red)
        ),
        Node::Bool { value } => writeln!(
            f,
            "{} {} {}",
            "Bool:".color(colored::Color::Cyan),
            value.to_string().color(colored::Color::Yellow),
            node.2.to_string().color(colored::Color::Red)
        ),
        Node::Float { value } => writeln!(
            f,
            "{} {} {}",
            "Float:".color(colored::Color::Cyan),
            value.to_string().color(colored::Color::Yellow),
            node.2.to_string().color(colored::Color::Red)
        ),
        Node::Integer { value } => writeln!(
            f,
            "{} {} {}",
            "Integer:".color(colored::Color::Cyan),
            value.to_string().color(colored::Color::Yellow),
            node.2.to_string().color(colored::Color::Red)
        ),
        Node::File { name } => writeln!(
            f,
            "{} \"{}\"",
            "file:".color(colored::Color::Green),
            name.to_string().color(colored::Color::Yellow)
        ),
        Node::IdentifierData { name } => writeln!(
            f,
            "{} \"{}\" {}",
            "identifier:".color(colored::Color::Green),
            name.to_string().color(colored::Color::Yellow),
            node.2.to_string().color(colored::Color::Red)
        ),
        Node::Parameter { name } => writeln!(
            f,
            "{} \"{}\" {}",
            "parameter:".color(colored::Color::Green),
            name.to_string().color(colored::Color::Yellow),
            node.2.to_string().color(colored::Color::Red)
        ),
        Node::TypeSignature { signature } => writeln!(
            f,
            "{} {} {}",
            "type signature:".color(colored::Color::Green),
            signature.to_string().color(colored::Color::Yellow),
            node.2.to_string().color(colored::Color::Red)
        ),
        Node::StructSignature { signature } => writeln!(
            f,
            "{} {} {}",
            "struct signature:".color(colored::Color::Green),
            signature.to_string().color(colored::Color::Yellow),
            node.2.to_string().color(colored::Color::Red)
        ),
        Node::FunctionSignature { signature } => writeln!(
            f,
            "{} {} {}",
            "function signature:".color(colored::Color::Green),
            signature.to_string().color(colored::Color::Yellow),
            node.2.to_string().color(colored::Color::Red)
        ),
    }
}
