use serde::{Deserialize, Serialize};

use super::{ansi, signature::TypeSignature, Position};

/// A full ir module that is never usually fully present in the pipeline
#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    /// The name of the module
    pub name: String,
    /// The ir instructions
    pub instructions: Vec<Instruction>,
    /// The signatures of each ir instruction
    pub signatures: Vec<TypeSignature>,
    /// The positions in code of each ir instruction
    pub positions: Vec<Position>,
}

impl Module {
    /// Creates an empty module
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Push an instruction into the module
    pub fn push(&mut self, pos: Position, sig: TypeSignature, ins: Instruction) {
        self.positions.push(pos);
        self.signatures.push(sig);
        self.instructions.push(ins);
    }
}

/// The ir that is being passed through the channels between each stage
#[derive(Debug, Clone)]
pub struct ChannelIr {
    /// Position in source code
    pub pos: Position,
    /// Type signature of instruction
    pub sig: TypeSignature,
    /// The instruction itself
    pub ins: Instruction,
}

/// Contains all of the instruction types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Instruction {
    /// An error instruction that the pipeline passes through when an error is detected
    Halt,

    Module(String),
    ModuleEnd,

    // --------------------
    // Branch structures
    // --------------------
    //
    /// If openening, expects the if condition ir afterwards
    If,
    /// If body, closes If opening, expects expression ir afterwards
    IfBody,
    /// If else opening, closes any preceeding if body or else if body, expects a condition ir afterwards
    IfElseIf,
    /// If else body, closes if else opening, epects condition ir afterwards
    IfElseIfBody,
    /// If else expression, closes any if or if else bodies, expects expression ir afterwards
    IfElse,
    /// If end, closes a full if expression
    IfEnd,

    /// While opening, expects the while condition ir afterwards
    While,
    /// While body, closes the while opening, expects expression ir afterwards
    WhileBody,
    /// While end, closes a full while expression
    WhileEnd,

    /// Loop opening, expects block afterwards
    Loop,
    /// Loop closing, closes a loop
    LoopEnd,

    /// Break expression
    Break,

    /// Break expression
    BreakExpression,

    /// Continue expression
    Continue,

    // --------------------
    // Declarations
    // --------------------
    //
    /// Function declaration, opens a function, expects parameters or expression ir afterwards
    Function,
    /// Function parameter, defines the name (String) for a parameter for the function, lines with the function signature
    FunctionParameter(String),
    /// Function end, closes a function
    FunctionEnd,

    /// Return a value from the stack
    Return,

    /// Let, declares an immutable variable with name (String), expects expression ir for assignment before
    Let(String),

    /// Let mut, declares a mutable variable with name (String), expects expression ir for assignment before
    LetMut(String),

    /// Let, declares an immutable variable with name (String)
    LetNoAssign(String),

    /// Let mut, declares a mutable variable with name (String)
    LetMutNoAssign(String),

    /// Declares a function with name (String), works like a normal function
    LetFunction(String),

    /// Declares a mutable function with name (String), works similar to a function pointer
    LetMutFunction(String),

    /// Declares a struct with name (String)
    LetStruct(String),

    // --------------------
    // Expressions
    // --------------------
    //
    /// Opens a block
    Block,
    /// Closes a block without an end expression
    BlockEnd,

    /// Closes a block with an end expression
    BlockEndExpression,

    /// Defines an external function, must be used inside let statement
    ExternFn,

    /// Defines a struct, expects field name ir afterwards
    Struct,
    /// Names a struct field from struct signature
    StructField(String),
    /// Names a public struct field from struct signature
    StructFieldPublic(String),
    /// Closes a struct
    StructEnd,

    /// Call with amount of arguments
    Call(u8),

    /// Cast expression to type
    As,

    /// Accesses a field from an expression
    FieldAccess(String),

    /// Identifier value (String)
    Identifier(String),
    /// String value (String), always `Str` type
    String(String),
    /// Boolean value (bool), always `Bool` type
    Bool(bool),
    /// Float type (f64), `F32` type by default
    Float(f64),
    // Integer type (isize), `I32` type by default
    Integer(isize),

    // --------------------
    // Binary operations
    // --------------------
    //
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

    // --------------------
    // Unary operations
    // --------------------
    //
    Negate,

    Not,

    // --------------------
    // Statement
    // --------------------
    //
    Statement,
}

/// Repeats a character a certain amount of times
fn repeat_char(c: char, times: usize) -> String {
    std::iter::repeat(c).take(times).collect::<String>()
}

/// Tabs to the correct depth
fn fmt_tab(f: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
    if depth != 0 {
        for _ in 0..depth {
            write!(f, "|{}", repeat_char(' ', super::TAB_WITDH))?;
        }
    }
    Ok(())
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[allow(clippy::enum_glob_use)]
        use Instruction::*;

        let mut depth = 0;
        for (ins, sig) in self.instructions.iter().zip(self.signatures.iter()) {
            match ins {
                Halt => {
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}HALT{}", ansi::Fg::BrightRed, ansi::Fg::Reset)?;
                }

                Module(name) => {
                    fmt_tab(f, depth)?;
                    depth += 1;
                    writeln!(
                        f,
                        "{}module{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        name,
                        ansi::Fg::Reset
                    )?;
                }
                ModuleEnd => {
                    depth -= 1;
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}module end{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }

                If => {
                    fmt_tab(f, depth)?;
                    depth += 1;
                    writeln!(f, "{}if{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }
                IfBody => {
                    fmt_tab(f, depth - 1)?;
                    writeln!(f, "{}if body{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }
                IfElseIf => {
                    fmt_tab(f, depth - 1)?;
                    writeln!(f, "{}if else{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }
                IfElseIfBody => {
                    fmt_tab(f, depth - 1)?;
                    writeln!(f, "{}if else body{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }
                IfElse => {
                    fmt_tab(f, depth - 1)?;
                    writeln!(f, "{}else{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }
                IfEnd => {
                    depth -= 1;
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}if end{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                While => {
                    fmt_tab(f, depth)?;
                    depth += 1;
                    writeln!(f, "{}while{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }
                WhileBody => {
                    fmt_tab(f, depth - 1)?;
                    writeln!(f, "{}while body{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }
                WhileEnd => {
                    depth -= 1;
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}while end{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }

                Loop => {
                    fmt_tab(f, depth)?;
                    depth += 1;
                    writeln!(f, "{}loop{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }

                LoopEnd => {
                    depth -= 1;
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}loop end{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                Break => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}break{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                BreakExpression => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}break{} expression {}{}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                Continue => {
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}continue{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }

                Function => {
                    fmt_tab(f, depth)?;
                    depth += 1;
                    writeln!(
                        f,
                        "{}function{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }
                FunctionParameter(name) => {
                    fmt_tab(f, depth - 1)?;
                    writeln!(
                        f,
                        "{}function parameter{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        name,
                        ansi::Fg::Reset
                    )?;
                }
                FunctionEnd => {
                    depth -= 1;
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}function end{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }

                Return => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}return{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                Let(name) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}let{} {} {}{} {}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        name,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                LetMut(name) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}let mut{} {} {}{} {}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        name,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                LetNoAssign(name) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}let {}no assign {} {}{} {}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        name,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                LetMutNoAssign(name) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}let mut {}no assign {} {}{} {}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        name,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                LetFunction(name) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}let {}function {} {}{} {}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        name,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                LetMutFunction(name) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}let mut {}function {} {}{} {}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        name,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                LetStruct(name) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}let {}struct {} {}{} {}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        name,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                Block => {
                    fmt_tab(f, depth)?;
                    depth += 1;
                    writeln!(f, "{}block{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }
                BlockEnd => {
                    depth -= 1;
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}block end{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }

                BlockEndExpression => {
                    depth -= 1;
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}block end {}expression{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        ansi::Fg::Reset
                    )?;
                }

                ExternFn => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}extern fn{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                Struct => {
                    fmt_tab(f, depth)?;
                    depth += 1;
                    writeln!(f, "{}struct{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }

                StructField(name) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}struct field {}{} {}{}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        name,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                StructFieldPublic(name) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}struct public field {}{} {}{}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        name,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                StructEnd => {
                    depth -= 1;
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}struct end{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                Call(arg_count) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}call{} {} {}{}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        arg_count,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                As => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}as{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                FieldAccess(name) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}field access{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        name,
                        ansi::Fg::Reset,
                    )?;
                }

                Identifier(value) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}identifier{} {}{} {}{}",
                        ansi::Fg::Green,
                        ansi::Fg::Yellow,
                        value,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }
                String(value) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}string{} {}{}",
                        ansi::Fg::Green,
                        ansi::Fg::Yellow,
                        value,
                        ansi::Fg::Reset
                    )?;
                }
                Bool(value) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}bool{} {}{}",
                        ansi::Fg::Green,
                        ansi::Fg::Yellow,
                        value,
                        ansi::Fg::Reset
                    )?;
                }
                Float(value) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}float{} {}{}",
                        ansi::Fg::Green,
                        ansi::Fg::Yellow,
                        value,
                        ansi::Fg::Reset
                    )?;
                }
                Integer(value) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}integer{} {}{}",
                        ansi::Fg::Green,
                        ansi::Fg::Yellow,
                        value,
                        ansi::Fg::Reset
                    )?;
                }

                Add => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}add{} {}{}",
                        ansi::Fg::Green,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }
                Subtract => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}sub{} {}{}",
                        ansi::Fg::Green,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }
                Multiply => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}mul{} {}{}",
                        ansi::Fg::Green,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }
                Divide => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}div{} {}{}",
                        ansi::Fg::Green,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }

                Less => {
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}less{}", ansi::Fg::Green, ansi::Fg::Reset)?;
                }
                LessEqual => {
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}less equal{}", ansi::Fg::Green, ansi::Fg::Reset)?;
                }
                Greater => {
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}greater{}", ansi::Fg::Green, ansi::Fg::Reset)?;
                }
                GreaterEqual => {
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}greater equal{}", ansi::Fg::Green, ansi::Fg::Reset)?;
                }
                Equal => {
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}equal{}", ansi::Fg::Green, ansi::Fg::Reset)?;
                }
                NotEqual => {
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}not equal{}", ansi::Fg::Green, ansi::Fg::Reset)?;
                }

                And => {
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}and{}", ansi::Fg::Green, ansi::Fg::Reset)?;
                }
                Or => {
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}or{}", ansi::Fg::Green, ansi::Fg::Reset)?;
                }

                Assign => {
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}assign{}", ansi::Fg::Green, ansi::Fg::Reset)?;
                }

                Negate => {
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}negate{}", ansi::Fg::Green, ansi::Fg::Reset)?;
                }

                Not => {
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}not{}", ansi::Fg::Green, ansi::Fg::Reset)?;
                }

                Statement => {
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}statement{}", ansi::Fg::Green, ansi::Fg::Reset)?;
                }
            }
        }
        Ok(())
    }
}
