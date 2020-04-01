use super::{
    ansi,
    ir::{ChannelIr, Instruction},
    notice::{Notice, NoticeLevel},
    signature::{PrimitiveType, TypeSignature},
    Position,
};

use mpsc::{Receiver, Sender};
use std::{sync::mpsc, thread};

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

macro_rules! check_expression {
    ($a:expr, $($type_:tt)*) => {
        {
            let result = $a.stack.last();
            if let Some(($($type_)*, _)) = result
            {
                true
            } else if let Some((_, pos)) = result {
                $a.emit_notice(
                    *pos,
                    NoticeLevel::Error,
                    format!("Expression doesn't evaluate to {}", $($type_)*),
                );
                false
            } else {
                $a.emit_notice(
                    Position::new(0, 0),
                    NoticeLevel::Error,
                    "Operation stack empty, should be unreachable".to_string(),
                );
                false
            }
        }
    };
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

    pub fn create(
        notice_tx: Sender<Option<Notice>>,
        ir_tx: Sender<Option<ChannelIr>>,
        ir_rx: Receiver<Option<ChannelIr>>,
    ) -> thread::JoinHandle<()> {
        thread::spawn(move || {
            let mut a = Self::new(notice_tx.clone(), ir_tx.clone(), ir_rx);

            a.emit_notice(
                Position::new(0, 0),
                NoticeLevel::Warning,
                "Not fully implemented yet".to_string(),
            );

            let mut last_ir = ChannelIr {
                pos: Position::new(0, 0),
                sig: TypeSignature::None,
                ins: Instruction::Halt,
            };

            while let Ok(Some(ir)) = a.ir_rx.recv() {
                /*print!("\nStack: ");
                for (sig, _pos) in &a.stack {
                    print!("[{}]", sig);
                }
                print!("\nIf data: ");
                for (idx, branches, has_else) in &a.if_data {
                    print!("[{}, {}, {}]", idx, branches, has_else);
                }
                print!("\nLoop data: ");
                for idx in &a.loop_data {
                    print!("[{}]", idx);
                }
                print!("\nName data: ");
                for name in &a.name_stack {
                    print!("[{}]", name);
                }
                println!("\n{:?}", ir.ins);*/
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
                        check_expression!(a, TypeSignature::Primitive(PrimitiveType::Boolean));
                        a.stack.pop();
                        TypeSignature::None
                    }

                    IfElseIf => {
                        let (_, branches, _) = a.if_data.last_mut().unwrap();
                        *branches += 1;

                        a.check_if_branch(last_ir);

                        TypeSignature::None
                    }

                    IfElseIfBody => {
                        check_expression!(a, TypeSignature::Primitive(PrimitiveType::Boolean));
                        a.stack.pop();
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
                                format!(
                                    "If branch lacks a return expression of type {}",
                                    first_sig
                                ),
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
                        check_expression!(a, TypeSignature::Primitive(PrimitiveType::Boolean));
                        a.stack.pop();
                        TypeSignature::None
                    }

                    WhileEnd => {
                        let stack_idx = a.loop_data.pop().unwrap();
                        if stack_idx != a.stack.len() {
                            a.emit_notice(
                                a.stack.last().unwrap().1,
                                NoticeLevel::Error,
                                "While expression blocks should not have an end expression"
                                    .to_string(),
                            );
                            a.stack.pop();
                        }
                        a.stack
                            .push((TypeSignature::Primitive(PrimitiveType::Nil), ir.pos));
                        TypeSignature::Primitive(PrimitiveType::Nil)
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
                                "Loop expression blocks should not have an end expression"
                                    .to_string(),
                            );
                            a.stack.pop();
                        }
                        if stack_idx != a.stack.len() {
                            a.stack.last().unwrap().0.clone()
                        } else {
                            a.stack
                                .push((TypeSignature::Primitive(PrimitiveType::Nil), ir.pos));
                            TypeSignature::Primitive(PrimitiveType::Nil)
                        }
                    }

                    Break => {
                        a.stack
                            .push((TypeSignature::Primitive(PrimitiveType::Nil), ir.pos));
                        TypeSignature::Primitive(PrimitiveType::Nil)
                    }

                    BreakExpression => a.stack.last().unwrap().0.clone(),

                    Statement => {
                        match last_ir.ins {
                            BreakExpression | Return => (),
                            _ => {
                                a.stack.pop();
                            }
                        }
                        TypeSignature::None
                    }

                    Bool(_) | Integer(_) | Float(_) => {
                        a.stack.push((ir.sig.clone(), ir.pos));
                        ir.sig.clone()
                    }

                    _ => TypeSignature::None,
                };

                last_ir = ir.clone();

                a.emit_ir(ir.pos, sig, ir.ins);
            }

            /*print!("\nStack: ");
            for (sig, _pos) in &a.stack {
                print!("[{}]", sig);
            }
            print!("\nIf data: ");
            for (idx, branches, has_else) in &a.if_data {
                print!("[{}, {}, {}]", idx, branches, has_else);
            }
            print!("\nLoop data: ");
            for idx in &a.loop_data {
                print!("[{}]", idx);
            }
            print!("\nName data: ");
            for name in &a.name_stack {
                print!("[{}]", name);
            }
            println!();*/

            Self::send_end_signal(notice_tx, ir_tx);
        })
    }
}
