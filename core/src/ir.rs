use serde::{Deserialize, Serialize};

use super::{ansi, signature::TypeSignature};

type SignatureIndex = usize;

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    /// The name of the module
    pub name: String,
    /// The ir instructions
    pub instructions: Vec<Instruction>,
    /// The signatures of each ir instruction
    pub signatures: Vec<TypeSignature>,
    /// The signatures that the IR refrences, like function and struct signatures
    pub ir_signatures: Vec<TypeSignature>,
}

impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, ins: Instruction) {
        self.instructions.push(ins);
        self.signatures.push(TypeSignature::Untyped);
    }

    pub fn push_signature(&mut self, sig: TypeSignature) -> usize {
        self.ir_signatures.push(sig);
        self.ir_signatures.len() - 1
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Instruction {
    Module(String),
    ModuleEnd,

    // --------------------
    // Branch structures
    // --------------------
    //
    If,
    IfBody,
    IfElseIf,
    IfElseIfBody,
    IfElse,
    IfEnd,

    While,
    WhileBody,
    WhileEnd,

    // --------------------
    // Declarations
    // --------------------
    //
    Function(SignatureIndex),
    FunctionParameter(String),
    FunctionEnd,

    Return,
    ReturnEnd,

    Let(String),
    LetEnd,

    LetMut(String),
    LetMutEnd,

    // --------------------
    // Expressions
    // --------------------
    //
    Block,
    BlockEnd,

    Import(SignatureIndex, String),

    ExternFn(SignatureIndex),

    Struct(SignatureIndex),

    Call(u8),

    As(SignatureIndex),

    Identifier(String),
    String(String),
    Bool(bool),
    Float(f64),
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
        Ok(())
    } else {
        Ok(())
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Instruction::*;
        let mut depth = 0;
        for (ins, sig) in self.instructions.iter().zip(self.signatures.iter()) {
            match ins {
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
                    writeln!(
                        f,
                        "{}if{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }
                IfBody => {
                    fmt_tab(f, depth - 1)?;
                    writeln!(
                        f,
                        "{}if body{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }
                IfElseIf => {
                    fmt_tab(f, depth - 1)?;
                    writeln!(
                        f,
                        "{}if else{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }
                IfElseIfBody => {
                    fmt_tab(f, depth - 1)?;
                    writeln!(
                        f,
                        "{}if else body{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }
                IfElse => {
                    fmt_tab(f, depth - 1)?;
                    writeln!(
                        f,
                        "{}else{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }
                IfEnd => {
                    depth -= 1;
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}if end{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
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

                Function(sig_idx) => {
                    fmt_tab(f, depth)?;
                    depth += 1;
                    writeln!(
                        f,
                        "{}function{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        self.ir_signatures[*sig_idx],
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
                    depth += 1;
                    writeln!(
                        f,
                        "{}return{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        sig,
                        ansi::Fg::Reset
                    )?;
                }
                ReturnEnd => {
                    depth -= 1;
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}return end{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }

                Let(name) => {
                    fmt_tab(f, depth)?;
                    depth += 1;
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
                LetEnd => {
                    depth -= 1;
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}let end{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
                }

                LetMut(name) => {
                    fmt_tab(f, depth)?;
                    depth += 1;
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
                LetMutEnd => {
                    depth -= 1;
                    fmt_tab(f, depth)?;
                    writeln!(f, "{}let mut end{}", ansi::Fg::Cyan, ansi::Fg::Reset)?;
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

                Import(sig_idx, name) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}import{} {} {}{}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        name,
                        ansi::Fg::Red,
                        self.ir_signatures[*sig_idx],
                        ansi::Fg::Reset
                    )?;
                }

                ExternFn(sig_idx) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}extern fn{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        self.ir_signatures[*sig_idx],
                        ansi::Fg::Reset
                    )?;
                }

                Struct(sig_idx) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}struct{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        self.ir_signatures[*sig_idx],
                        ansi::Fg::Reset
                    )?;
                }

                Call(arg_count) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}call{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Yellow,
                        arg_count,
                        ansi::Fg::Reset
                    )?;
                }

                As(sig_idx) => {
                    fmt_tab(f, depth)?;
                    writeln!(
                        f,
                        "{}as{} {}{}",
                        ansi::Fg::Cyan,
                        ansi::Fg::Red,
                        self.ir_signatures[*sig_idx],
                        ansi::Fg::Reset
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
