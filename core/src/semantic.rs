use super::{
    ansi,
    ir::{ChannelIr, Instruction},
    notice::{Notice, NoticeLevel},
    signature::{PrimitiveType, TypeSignature},
    Position,
};

use mpsc::{Receiver, Sender};
use std::{sync::mpsc, thread};

const PRIMITIVE_BOOL: TypeSignature = TypeSignature::Primitive(PrimitiveType::Boolean);
const PRIMITIVE_NIL: TypeSignature = TypeSignature::Primitive(PrimitiveType::Nil);

pub struct Analyzer {
    notice_tx: Sender<Option<Notice>>,
    ir_tx: Sender<Option<ChannelIr>>,
    ir_rx: Receiver<Option<ChannelIr>>,
    name_stack: Vec<String>,

    stack: Vec<(TypeSignature, Position)>,

    /// A vector of start stack locations and how many if branches, and if it has an else branch
    if_data: Vec<(usize, usize, bool)>,

    /// A vector of stack stack locations for loops
    loop_data: Vec<usize>,
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
            stack: Vec::with_capacity(64),
            if_data: Vec::with_capacity(4),
            loop_data: Vec::with_capacity(4),
        }
    }

    fn print_stacks(&self) {
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
        println!();
    }

    fn send_end_signal(notice_tx: Sender<Option<Notice>>, ir_tx: Sender<Option<ChannelIr>>) {
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

    fn check_if_branch(&mut self, last_ir: ChannelIr) {
        let (idx, branches, _) = self.if_data.last().unwrap();

        if last_ir.ins == Instruction::BlockEndExpression {
            if *branches > 1 {
                let (first_sig, _) = self.stack[*idx].clone();
                let (sig, pos) = self.stack.pop().unwrap();
                if self.stack.len() == *idx {
                    self.emit_notice(
                        pos,
                        NoticeLevel::Error,
                        format!(
                            "If branch expression expected no return expression, found {}",
                            sig
                        ),
                    );
                }
                if sig != first_sig {
                    self.emit_notice(
                        pos,
                        NoticeLevel::Error,
                        format!(
                            "If branch expression returned a different type, found {}, expected {}",
                            sig, first_sig
                        ),
                    );
                }
            }
        } else if self.stack.len() != *idx {
            let (first_sig, _) = self.stack[*idx].clone();
            self.emit_notice(
                last_ir.pos,
                NoticeLevel::Error,
                format!("If branch lacks a return expression of type {}", first_sig),
            );
        }
    }

    pub async fn create(
        notice_tx: Sender<Option<Notice>>,
        ir_tx: Sender<Option<ChannelIr>>,
        ir_rx: Receiver<Option<ChannelIr>>,
    ) {
        let mut a = Self::new(notice_tx.clone(), ir_tx.clone(), ir_rx);

        let mut last_ir = ChannelIr {
            pos: Position::new(0, 0),
            sig: TypeSignature::None,
            ins: Instruction::Halt,
        };

        while let Ok(Some(ir)) = a.ir_rx.recv() {
            // a.print_stacks();
            // println!("ins:       {}{:?}{}\n", ansi::Fg::Cyan, ir.ins, ansi::Fg::Reset);
            use Instruction::*;
            let sig = match &ir.ins {
                Halt => {
                    Self::send_end_signal(notice_tx.clone(), ir_tx.clone());
                    TypeSignature::None
                }
                Module(name) => {
                    a.name_stack.push(name.clone());
                    TypeSignature::None
                }
                ModuleEnd => {
                    a.name_stack.pop();
                    TypeSignature::None
                }

                If => {
                    a.if_data.push((a.stack.len(), 0, false));
                    TypeSignature::None
                }

                IfBody => {
                    let (sig, pos) = a.stack.pop().unwrap();
                    if sig != PRIMITIVE_BOOL {
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

                    a.check_if_branch(last_ir);

                    TypeSignature::None
                }

                IfElseIfBody => {
                    let (sig, pos) = a.stack.pop().unwrap();
                    if sig != PRIMITIVE_BOOL {
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

                    a.check_if_branch(last_ir);

                    TypeSignature::None
                }

                IfEnd => {
                    let (idx, branches, has_else) = a.if_data.pop().unwrap();

                    if last_ir.ins == BlockEndExpression {
                        let (first_sig, _) = a.stack[idx].clone();
                        if branches > 0 {
                            let (sig, pos) = a.stack.pop().unwrap();
                            if a.stack.len() == idx {
                                a.emit_notice(pos, NoticeLevel::Error, format!("If branch expression expected no return expression, found {}", sig));
                            }
                            if sig != first_sig {
                                a.emit_notice(pos, NoticeLevel::Error, format!("If branch expression returned a different type, found {}, expected {}", sig, first_sig));
                            }
                            if !has_else {
                                a.emit_notice(pos, NoticeLevel::Error, "If expression that returns a value doesn't have an else branch".to_string());
                            }
                        }
                        first_sig
                    } else if a.stack.len() != idx {
                        let (first_sig, _) = a.stack[idx].clone();
                        a.emit_notice(
                            last_ir.pos,
                            NoticeLevel::Error,
                            format!("If branch lacks a return expression of type {}", first_sig),
                        );
                        first_sig
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
                    if sig != PRIMITIVE_BOOL {
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
                    a.stack.push((PRIMITIVE_NIL, ir.pos));
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
                    if stack_idx != a.stack.len() {
                        a.stack.last().unwrap().0.clone()
                    } else {
                        a.stack.push((PRIMITIVE_NIL, ir.pos));
                        PRIMITIVE_NIL
                    }
                }

                Break => {
                    a.stack.push((PRIMITIVE_NIL, ir.pos));
                    PRIMITIVE_NIL
                }

                BreakExpression => a.stack.last().unwrap().0.clone(),

                Continue => TypeSignature::None,

                Function => ir.sig.clone(),

                FunctionParameter(_) => ir.sig.clone(),

                FunctionEnd => TypeSignature::None,

                Return => a.stack.pop().unwrap().0,

                Let(_) => a.stack.pop().unwrap().0,

                LetMut(_) => a.stack.pop().unwrap().0,

                LetNoAssign(_) => ir.sig.clone(),

                LetMutNoAssign(_) => ir.sig.clone(),

                LetFunction(_) => TypeSignature::None,

                LetMutFunction(_) => TypeSignature::None,

                LetStruct(_) => TypeSignature::None,

                Block => TypeSignature::None,

                BlockEnd => PRIMITIVE_NIL,

                BlockEndExpression => a.stack.last().unwrap().0.clone(),

                ExternFn => ir.sig.clone(),

                Struct => TypeSignature::None,

                StructField(_) => ir.sig.clone(),

                StructFieldPublic(_) => ir.sig.clone(),

                StructEnd => ir.sig.clone(),

                Call(params) => {
                    for _ in 0..*params {
                        a.stack.pop();
                    }
                    TypeSignature::Untyped
                }

                As => {
                    a.stack.last_mut().unwrap().0 = ir.sig.clone();
                    ir.sig.clone()
                }

                FieldAccess(_) => TypeSignature::Untyped,

                Identifier(_) => TypeSignature::Untyped,

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
                    a.stack.push((ir.sig.clone(), ir.pos));
                    ir.sig.clone()
                }

                Add | Subtract | Multiply | Divide => {
                    let (b_sig, b_pos) = a.stack.pop().unwrap();
                    let (a_sig, a_pos) = a.stack.last().unwrap();

                    if !matches!(a_sig, TypeSignature::Primitive(PrimitiveType::SignedInteger { .. }) | TypeSignature::Primitive(PrimitiveType::UnsignedInteger { .. }))
                    {
                        a.emit_notice(*a_pos, NoticeLevel::Error, "Expression doesn't evaluate to integer; binary arithmetic requires integers".to_string());
                    }

                    if !matches!(b_sig, TypeSignature::Primitive(PrimitiveType::SignedInteger { .. }) | TypeSignature::Primitive(PrimitiveType::UnsignedInteger { .. }))
                    {
                        a.emit_notice(b_pos, NoticeLevel::Error, "Expression doesn't evaluate to integer; binary arithmetic requires integers".to_string());
                    }

                    a_sig.clone()
                }

                Less | LessEqual | Greater | GreaterEqual | Equal | NotEqual => ir.sig.clone(),

                And | Or => {
                    let (sig, pos) = a.stack.pop().unwrap();
                    if sig != PRIMITIVE_BOOL {
                        a.emit_notice(
                            pos,
                            NoticeLevel::Error,
                            "Expression doesn't evaluate to bool".to_string(),
                        );
                    }
                    let (sig, pos) = a.stack.last().unwrap();
                    if *sig != PRIMITIVE_BOOL {
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
                    if *sig != PRIMITIVE_BOOL {
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
                        sig,
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

        // a.print_stacks();

        Self::send_end_signal(notice_tx, ir_tx);
    }
}
