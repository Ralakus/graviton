use super::{
    ansi,
    ir::{ChannelIr, Instruction},
    notice::{Notice, NoticeLevel},
    signature::{PrimitiveType, TypeSignature},
    Position,
};

use mpsc::{Receiver, Sender};
use std::{collections::HashMap, sync::mpsc};

const PRIMITIVE_BOOL: TypeSignature = TypeSignature::Primitive(PrimitiveType::Boolean);
const PRIMITIVE_NIL: TypeSignature = TypeSignature::Primitive(PrimitiveType::Nil);

enum Variable {
    General(bool, TypeSignature),
    Struct(Vec<String>, TypeSignature),
}

struct Module {
    /// Variable map, immuatable
    variables: HashMap<String, Variable>,
    /// The submodules in the module
    sub_modules: HashMap<String, Module>,
}

enum Scope {
    Global {
        module: Module,
    },
    #[allow(dead_code)]
    Function {
        variables: HashMap<String, Variable>,
        identifier: String,
    },
    General {
        variables: HashMap<String, Variable>,
    },
}

#[derive(Clone)]
enum Value {
    /// General signature
    Signature(TypeSignature),
    /// Struct being constructed
    Struct(Vec<String>, TypeSignature),
}

impl Value {
    const fn sig(sig: TypeSignature) -> Self {
        Self::Signature(sig)
    }

    fn unwrap(&self) -> &TypeSignature {
        match self {
            Self::Signature(sig) => sig,
            Self::Struct(..) => panic!("Value not of signature"),
        }
    }

    fn unwrap_struct(&mut self) -> (&mut Vec<String>, &mut TypeSignature) {
        match self {
            Self::Struct(params, sig) => (params, sig),
            Self::Signature(_) => panic!("Value not of type struct"),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Signature(sig) => write!(f, "{sig}"),
            Self::Struct(params, sig) => write!(f, "{sig}: {params:?}"),
        }
    }
}

pub struct Analyzer {
    /// Notice stream
    notice_tx: Sender<Option<Notice>>,
    /// IR output stream
    ir_tx: Sender<Option<ChannelIr>>,
    /// IR receive stream
    ir_rx: Receiver<Option<ChannelIr>>,

    /// A name stack for the names of the modules it current is working with
    name_stack: Vec<String>,

    /// The work stack
    stack: Vec<(Value, Position)>,

    /// A vector of start stack locations and how many if branches, and if it has an else branch
    if_data: Vec<(usize, usize, bool)>,

    /// A vector of stack stack locations for loops
    loop_data: Vec<usize>,

    /// A stack for each working scope
    scopes: Vec<Scope>,
}

impl Analyzer {
    /// Creates a new analyzer
    fn new(
        notice_tx: Sender<Option<Notice>>,
        ir_tx: Sender<Option<ChannelIr>>,
        ir_rx: Receiver<Option<ChannelIr>>,
    ) -> Self {
        Self {
            notice_tx,
            ir_tx,
            ir_rx,
            name_stack: Vec::with_capacity(1),
            stack: Vec::with_capacity(32),
            if_data: Vec::with_capacity(4),
            loop_data: Vec::with_capacity(4),
            scopes: Vec::with_capacity(16),
        }
    }

    /// Prints the current state of the stacks in the analyzer
    pub fn print_stacks(&self) {
        print!("stack:     ");
        for (sig, _pos) in &self.stack {
            print!("[{}{}{}]", ansi::Fg::Yellow, sig, ansi::Fg::Reset);
        }
        print!("\nif data:   ");
        for (idx, branches, has_else) in &self.if_data {
            print!(
                "[{}{}, {}, {}{}]",
                ansi::Fg::Yellow,
                idx,
                branches,
                has_else,
                ansi::Fg::Reset
            );
        }
        print!("\nloop data: ");
        for idx in &self.loop_data {
            print!("[{}{}{}]", ansi::Fg::Yellow, idx, ansi::Fg::Reset);
        }
        print!("\nname data: ");
        for name in &self.name_stack {
            print!("[{}{}{}]", ansi::Fg::Yellow, name, ansi::Fg::Reset);
        }
        print!("\nscopes:    ");
        for scope in &self.scopes {
            print!(
                "[{}{}{}]",
                ansi::Fg::Yellow,
                match scope {
                    Scope::Global { .. } => "global",
                    Scope::Function { .. } => "function",
                    Scope::General { .. } => "general",
                },
                ansi::Fg::Reset
            );
        }
        println!();
    }

    fn send_end_signal(notice_tx: &Sender<Option<Notice>>, ir_tx: &Sender<Option<ChannelIr>>) {
        if let Err(e) = ir_tx.send(None) {
            eprintln!(
                "{}Analyzer ir send error: {}{}",
                ansi::Fg::BrightRed,
                e,
                ansi::Fg::Reset
            );
        }

        if let Err(e) = notice_tx.send(None) {
            eprintln!(
                "{}Analyzer notice send error: {}{}",
                ansi::Fg::BrightRed,
                e,
                ansi::Fg::Reset
            );
        }
    }

    fn emit_notice(&self, pos: Position, level: NoticeLevel, msg: String) {
        if level == NoticeLevel::Error {
            if let Err(e) = self.ir_tx.send(Some(ChannelIr {
                pos,
                sig: TypeSignature::None,
                ins: Instruction::Halt,
            })) {
                eprintln!(
                    "{}Analyzer notice send error: {}{}",
                    ansi::Fg::BrightRed,
                    e,
                    ansi::Fg::Reset
                );
            }
        }

        let notice = Notice::new(
            "Analyzer".to_string(),
            msg,
            pos,
            self.name_stack
                .last()
                .unwrap_or(&String::from("Analyzer"))
                .clone(),
            level,
        );

        if let Err(e) = self.notice_tx.send(Some(notice)) {
            eprintln!(
                "{}Analyzer notice send error: {}{}",
                ansi::Fg::BrightRed,
                e,
                ansi::Fg::Reset
            );
        }
    }

    fn emit_ir(&mut self, pos: Position, sig: TypeSignature, ins: Instruction) {
        let ir = ChannelIr { pos, sig, ins };

        if let Err(e) = self.ir_tx.send(Some(ir)) {
            eprintln!(
                "{}Analyzer ir send error: {}{}",
                ansi::Fg::BrightRed,
                e,
                ansi::Fg::Reset
            );
        }
    }

    fn make_variable(&mut self, name: String, var: Variable) {
        match self.scopes.last_mut().unwrap() {
            Scope::Global { module } => {
                module.variables.insert(name, var);
            }
            Scope::Function { variables, .. } | Scope::General { variables } => {
                variables.insert(name, var);
            }
        }
    }

    fn get_variable(&mut self, name: &str) -> Option<&Variable> {
        for scope in self.scopes.iter().rev() {
            match scope {
                Scope::Global { module } => {
                    if let Some(var) = module.variables.get(name) {
                        return Some(var);
                    }
                }
                Scope::Function { variables, .. } | Scope::General { variables, .. } => {
                    if let Some(var) = variables.get(name) {
                        return Some(var);
                    }
                }
            }
        }
        None
    }

    fn check_if_branch(&mut self, last_ir: &ChannelIr) {
        let (idx, branches, _) = self.if_data.last().unwrap();

        if last_ir.ins == Instruction::BlockEndExpression {
            if *branches > 1 {
                let (first_sig, _) = self.stack[*idx].clone();
                let (sig, pos) = self.stack.pop().unwrap();
                if self.stack.len() == *idx {
                    self.emit_notice(
                        pos,
                        NoticeLevel::Error,
                        format!("If branch expression expected no return expression, found {sig}"),
                    );
                }
                if sig.unwrap() != first_sig.unwrap() {
                    self.emit_notice(
                        pos,
                        NoticeLevel::Error,
                        format!(
                            "If branch expression returned a different type, found {sig}, expected {first_sig}"
                        ),
                    );
                }
            }
        } else if self.stack.len() != *idx {
            let (first_sig, _) = self.stack[*idx].clone();
            self.emit_notice(
                last_ir.pos,
                NoticeLevel::Error,
                format!("If branch lacks a return expression of type {first_sig}"),
            );
        }
    }

    /// Creates a new semantic analyzer instance to go after parser
    ///
    /// # Panics
    /// The code will panic if it fails to pop or push from the evaluation stack
    #[allow(clippy::unused_async)]
    pub async fn create(
        notice_tx: Sender<Option<Notice>>,
        ir_tx: Sender<Option<ChannelIr>>,
        ir_rx: Receiver<Option<ChannelIr>>,
    ) {
        #[allow(clippy::enum_glob_use)]
        use Instruction::*;

        let mut a = Self::new(notice_tx.clone(), ir_tx.clone(), ir_rx);

        let mut last_ir = ChannelIr {
            pos: Position::new(0, 0),
            sig: TypeSignature::None,
            ins: Instruction::Halt,
        };

        while let Ok(Some(ir)) = a.ir_rx.recv() {
            a.print_stacks();
            println!(
                "ins:       {}{:?}{}\n",
                ansi::Fg::Cyan,
                ir.ins,
                ansi::Fg::Reset
            );
            let sig = match &ir.ins {
                Halt => {
                    Self::send_end_signal(&notice_tx, &ir_tx);
                    TypeSignature::None
                }
                Module(name) => {
                    a.name_stack.push(name.clone());
                    a.scopes.push(Scope::Global {
                        module: crate::semantic::Module {
                            variables: HashMap::new(),
                            sub_modules: HashMap::new(),
                        },
                    });
                    TypeSignature::None
                }
                ModuleEnd => {
                    let name = a.name_stack.pop().unwrap();
                    let top_scope = a.scopes.pop().unwrap();
                    for scope in a.scopes.iter_mut().rev() {
                        if let Scope::Global { module, .. } = scope {
                            module.sub_modules.insert(
                                name,
                                match top_scope {
                                    Scope::Global { module } => module,
                                    Scope::Function { .. } | Scope::General { .. } => break,
                                },
                            );
                            break;
                        }
                    }
                    TypeSignature::None
                }

                If => {
                    a.if_data.push((a.stack.len(), 0, false));
                    TypeSignature::None
                }

                IfBody => {
                    let (sig, pos) = a.stack.pop().unwrap();
                    if !sig.unwrap().is_bool() {
                        a.emit_notice(
                            pos,
                            NoticeLevel::Error,
                            "Expression doesn't evaluate to bool".to_string(),
                        );
                    }
                    TypeSignature::None
                }

                IfElseIf => {
                    let (_, branches, _) = a.if_data.last_mut().unwrap();
                    *branches += 1;

                    a.check_if_branch(&last_ir);

                    TypeSignature::None
                }

                IfElseIfBody => {
                    let (sig, pos) = a.stack.pop().unwrap();
                    if !sig.unwrap().is_bool() {
                        a.emit_notice(
                            pos,
                            NoticeLevel::Error,
                            "Expression doesn't evaluate to bool".to_string(),
                        );
                    }
                    TypeSignature::None
                }

                IfElse => {
                    let (_, branches, has_else) = a.if_data.last_mut().unwrap();
                    *branches += 1;
                    *has_else = true;

                    a.check_if_branch(&last_ir);

                    TypeSignature::None
                }

                IfEnd => {
                    let (idx, branches, has_else) = a.if_data.pop().unwrap();

                    if last_ir.ins == BlockEndExpression {
                        let (first_sig, _) = a.stack[idx].clone();
                        if !has_else {
                            a.emit_notice(
                                ir.pos,
                                NoticeLevel::Error,
                                "If expression that returns a value doesn't have an else branch"
                                    .to_string(),
                            );
                        }
                        if branches > 0 {
                            let (sig, pos) = a.stack.pop().unwrap();
                            if a.stack.len() == idx {
                                a.emit_notice(pos, NoticeLevel::Error, format!("If branch expression expected no return expression, found {sig}"));
                            }
                            if sig.unwrap() != first_sig.unwrap() {
                                a.emit_notice(pos, NoticeLevel::Error, format!("If branch expression returned a different type, found {sig}, expected {first_sig}"));
                            }
                        }
                        first_sig.unwrap().clone()
                    } else if a.stack.len() != idx {
                        let (first_sig, _) = a.stack[idx].clone();
                        a.emit_notice(
                            last_ir.pos,
                            NoticeLevel::Error,
                            format!("If branch lacks a return expression of type {first_sig}"),
                        );
                        first_sig.unwrap().clone()
                    } else {
                        TypeSignature::None
                    }
                }

                While => {
                    a.loop_data.push(a.stack.len());
                    TypeSignature::None
                }

                WhileBody => {
                    let (sig, pos) = a.stack.pop().unwrap();
                    if !sig.unwrap().is_bool() {
                        a.emit_notice(
                            pos,
                            NoticeLevel::Error,
                            "Expression doesn't evaluate to bool".to_string(),
                        );
                    }
                    TypeSignature::None
                }

                WhileEnd => {
                    let stack_idx = a.loop_data.pop().unwrap();
                    if stack_idx != a.stack.len() {
                        a.emit_notice(
                            a.stack.last().unwrap().1,
                            NoticeLevel::Error,
                            "While expression blocks should not have an end expression".to_string(),
                        );
                        a.stack.pop();
                    }
                    a.stack.push((Value::sig(PRIMITIVE_NIL), ir.pos));
                    PRIMITIVE_NIL
                }

                Loop => {
                    a.loop_data.push(a.stack.len());
                    TypeSignature::None
                }

                LoopEnd => {
                    let stack_idx = a.loop_data.pop().unwrap();
                    if last_ir.ins == BlockEndExpression {
                        a.emit_notice(
                            a.stack.last().unwrap().1,
                            NoticeLevel::Error,
                            "Loop expression blocks should not have an end expression".to_string(),
                        );
                        a.stack.pop();
                    }
                    if stack_idx == a.stack.len() {
                        a.stack.push((Value::sig(PRIMITIVE_NIL), ir.pos));
                        PRIMITIVE_NIL
                    } else {
                        a.stack.last().unwrap().0.unwrap().clone()
                    }
                }

                Break => {
                    a.stack.push((Value::sig(PRIMITIVE_NIL), ir.pos));
                    PRIMITIVE_NIL
                }

                BreakExpression => a.stack.last().unwrap().0.unwrap().clone(),

                Continue => TypeSignature::None,

                Function => ir.sig.clone(),

                FunctionParameter(_) => ir.sig.clone(),

                FunctionEnd => TypeSignature::None,

                Return => a.stack.pop().unwrap().0.unwrap().clone(),

                Let(name) => {
                    let sig = if ir.sig == TypeSignature::Untyped {
                        a.stack.pop().unwrap().0.unwrap().clone()
                    } else {
                        a.stack.pop();
                        ir.sig.clone()
                    };
                    a.make_variable(name.clone(), Variable::General(false, sig.clone()));
                    sig
                }

                LetMut(name) => {
                    let sig = if ir.sig == TypeSignature::Untyped {
                        a.stack.pop().unwrap().0.unwrap().clone()
                    } else {
                        a.stack.pop();
                        ir.sig.clone()
                    };
                    a.make_variable(name.clone(), Variable::General(true, sig.clone()));
                    sig
                }

                LetNoAssign(name) => {
                    a.make_variable(name.clone(), Variable::General(false, ir.sig.clone()));
                    ir.sig.clone()
                }

                LetMutNoAssign(name) => {
                    a.make_variable(name.clone(), Variable::General(true, ir.sig.clone()));
                    ir.sig.clone()
                }

                LetFunction(name) => {
                    let sig = if ir.sig == TypeSignature::Untyped {
                        a.stack.pop().unwrap().0.unwrap().clone()
                    } else {
                        a.stack.pop();
                        ir.sig.clone()
                    };
                    a.make_variable(name.clone(), Variable::General(true, sig.clone()));
                    sig
                }

                LetMutFunction(name) => {
                    let sig = if ir.sig == TypeSignature::Untyped {
                        a.stack.pop().unwrap().0.unwrap().clone()
                    } else {
                        a.stack.pop();
                        ir.sig.clone()
                    };
                    a.make_variable(name.clone(), Variable::General(true, sig.clone()));
                    sig
                }

                LetStruct(name) => {
                    let mut value = a.stack.pop().unwrap();
                    let (params, sig) = value.0.unwrap_struct();
                    a.make_variable(name.clone(), Variable::Struct(params.clone(), sig.clone()));
                    ir.sig.clone()
                }

                Block => {
                    a.scopes.push(Scope::General {
                        variables: HashMap::new(),
                    });
                    TypeSignature::None
                }

                BlockEnd => {
                    a.scopes.pop();
                    PRIMITIVE_NIL
                }

                BlockEndExpression => {
                    a.scopes.pop();
                    a.stack.last().unwrap().0.unwrap().clone()
                }

                ExternFn => ir.sig.clone(),

                Struct => {
                    a.stack
                        .push((Value::Struct(Vec::new(), TypeSignature::Untyped), ir.pos));
                    TypeSignature::None
                }

                StructField(name) => {
                    a.stack
                        .last_mut()
                        .unwrap()
                        .0
                        .unwrap_struct()
                        .0
                        .push(name.clone());
                    ir.sig.clone()
                }

                StructFieldPublic(name) => {
                    a.stack
                        .last_mut()
                        .unwrap()
                        .0
                        .unwrap_struct()
                        .0
                        .push(name.clone());
                    ir.sig.clone()
                }

                StructEnd => {
                    *a.stack.last_mut().unwrap().0.unwrap_struct().1 = ir.sig.clone();
                    ir.sig.clone()
                }

                Call(params) => {
                    for _ in 0..*params {
                        a.stack.pop();
                    }
                    TypeSignature::Untyped
                }

                As => {
                    a.stack.last_mut().unwrap().0 = Value::sig(ir.sig.clone());
                    ir.sig.clone()
                }

                FieldAccess(_) => TypeSignature::Untyped,

                Identifier(name) => {
                    if let Some(var) = a.get_variable(name) {
                        let sig = match var {
                            Variable::General(_, sig) => sig.clone(),
                            Variable::Struct(_, sig) => sig.clone(),
                        };
                        a.stack.push((Value::sig(sig.clone()), ir.pos));
                        sig
                    } else {
                        a.emit_notice(ir.pos, NoticeLevel::Error, format!("{name} not defined"));
                        a.stack.push((Value::sig(PRIMITIVE_NIL), ir.pos));
                        TypeSignature::None
                    }
                }

                Statement => {
                    match last_ir.ins {
                        BreakExpression | Return => (),
                        _ => {
                            a.stack.pop();
                        }
                    }
                    TypeSignature::None
                }

                String(_) | Bool(_) | Float(_) | Integer(_) => {
                    a.stack.push((Value::sig(ir.sig.clone()), ir.pos));
                    ir.sig.clone()
                }

                Add | Subtract | Multiply | Divide => {
                    let (b_sig, b_pos) = a.stack.pop().unwrap();
                    let (a_sig, a_pos) = a.stack.last().unwrap();

                    if !a_sig.unwrap().clone().is_integer() {
                        a.emit_notice(*a_pos, NoticeLevel::Error, format!("Expression doesn't evaluate to integer; binary arithmetic requires integers; found {a_sig}"));
                    }

                    if !b_sig.unwrap().is_integer() {
                        a.emit_notice(b_pos, NoticeLevel::Error, format!("Expression doesn't evaluate to integer; binary arithmetic requires integers; found {b_sig}"));
                    }

                    a_sig.unwrap().clone()
                }

                Less | LessEqual | Greater | GreaterEqual | Equal | NotEqual => {
                    a.stack.pop();
                    a.stack.pop();
                    a.stack.push((Value::sig(PRIMITIVE_BOOL), ir.pos));
                    ir.sig.clone()
                }

                And | Or => {
                    let (sig, pos) = a.stack.pop().unwrap();
                    if !sig.unwrap().is_bool() {
                        a.emit_notice(
                            pos,
                            NoticeLevel::Error,
                            "Expression doesn't evaluate to bool".to_string(),
                        );
                    }
                    let (sig, pos) = a.stack.last().unwrap();
                    if !sig.unwrap().is_bool() {
                        a.emit_notice(
                            *pos,
                            NoticeLevel::Error,
                            "Expression doesn't evaluate to bool".to_string(),
                        );
                    }

                    PRIMITIVE_BOOL
                }

                Assign => PRIMITIVE_NIL,

                Not => {
                    let (sig, pos) = a.stack.last().unwrap();
                    if !sig.unwrap().is_bool() {
                        a.emit_notice(
                            *pos,
                            NoticeLevel::Error,
                            "Expression doesn't evaluate to bool".to_string(),
                        );
                    }
                    ir.sig.clone()
                }

                Negate => {
                    let (sig, pos) = a.stack.last().unwrap();
                    if !matches!(
                        sig.unwrap(),
                        TypeSignature::Primitive(PrimitiveType::SignedInteger { .. })
                    ) {
                        a.emit_notice(*pos, NoticeLevel::Error, "Expression doesn't evaluate to a signed integer; unary negate must be a signed integer".to_string());
                    }
                    ir.sig.clone()
                }
            };

            last_ir = ir.clone();

            a.emit_ir(ir.pos, sig, ir.ins);
        }

        a.print_stacks();

        Self::send_end_signal(&notice_tx, &ir_tx);
    }
}
