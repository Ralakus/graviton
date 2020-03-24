use graviton::{
    core::{ir, Notice},
    frontend::parser::Parser,
};
use mpsc::{Receiver, Sender};
use std::sync::mpsc;

fn main() {
    let _source_old = "let a = 14 + 48;\n\
                  let add = (x: I32, y: I32) -> I32 { x + y };";
    let source = "let x = 5; x(5);\nlet z = { let y = (x: I32, y: I32) -> I32 { x + y }; y(x) } as F64;\nif z >= 5.0 { 14 } else { 48 };\nlet loop_result = loop{ let x = 5; break x; }; let pair = struct { pub x: F64, pub y: I32, };";

    let mut tir = ir::Module::new();

    let (ir_tx, ir_rx): (
        Sender<Option<ir::ChannelIr>>,
        Receiver<Option<ir::ChannelIr>>,
    ) = mpsc::channel();

    let (notice_tx, notice_rx): (Sender<Option<Notice>>, Receiver<Option<Notice>>) =
        mpsc::channel();

    let arc_source: std::sync::Arc<str> = std::sync::Arc::from(source);

    let parser = Parser::parse("main.grav".to_string(), arc_source, notice_tx, ir_tx);

    let (mut ir_done, mut notice_done) = (false, false);

    while !ir_done || !notice_done {
        match ir_rx.try_recv() {
            Ok(Some(ins)) => tir.push(ins.pos, ins.sig, ins.ins),
            Ok(None) => ir_done = true,
            Err(std::sync::mpsc::TryRecvError::Empty) => (),
            Err(std::sync::mpsc::TryRecvError::Disconnected) if ir_done => (),
            Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                panic!("ir_rx disconnected before sending end signal")
            }
        }

        match notice_rx.try_recv() {
            Ok(Some(notice)) => notice.report(Some(&source)),
            Ok(None) => notice_done = true,
            Err(std::sync::mpsc::TryRecvError::Empty) => (),
            Err(std::sync::mpsc::TryRecvError::Disconnected) if notice_done => (),
            Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                panic!("notice_rx disconnected before sending end signal")
            }
        }
    }

    parser.join().expect("Error joining parser thread");

    println!("Source:\n\n{}\n\n{}", source, tir);
}
