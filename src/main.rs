pub extern crate graviton_backend as backend;
pub extern crate graviton_core as core;
pub extern crate graviton_frontend as frontend;

use core::ir;

fn main() {
    let _source_old = "let a = 14 + 48;\n\
                  let add = (x: I32, y: I32) -> I32 { x + y };";
    let source = "!(14 * 48 == 672)";

    let mut tir = ir::Module::new();

    use mpsc::{Receiver, Sender};
    use std::sync::mpsc;
    let (ir_tx, ir_rx): (
        Sender<Option<ir::ChannelIr>>,
        Receiver<Option<ir::ChannelIr>>,
    ) = mpsc::channel();
    let (notice_tx, notice_rx): (Sender<Option<core::Notice>>, Receiver<Option<core::Notice>>) =
        mpsc::channel();
    let arc_source: std::sync::Arc<str> = std::sync::Arc::from(source);

    let parser =
        frontend::parser::Parser::parse("main.grav".to_string(), arc_source, notice_tx, ir_tx);

    let mut done = false;

    while !done {
        match ir_rx.try_recv() {
            Ok(Some(ins)) => tir.push(ins.pos, ins.sig, ins.ins),
            Ok(None) => done = true,
            Err(std::sync::mpsc::TryRecvError::Empty) => (),
            Err(std::sync::mpsc::TryRecvError::Disconnected) => (),
        }

        match notice_rx.try_recv() {
            Ok(Some(notice)) => notice.report(Some(&source)),
            Ok(None) => (),
            Err(std::sync::mpsc::TryRecvError::Empty) => (),
            Err(std::sync::mpsc::TryRecvError::Disconnected) => (),
        }
    }

    parser.join().expect("Error joining parser");

    println!("Source:\n\n{}\n\n{}", source, tir);
}
