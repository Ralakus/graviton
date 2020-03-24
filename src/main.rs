use graviton::{
    core::{ansi, ir, notice::Notice},
    frontend::parser::Parser,
};
use mpsc::{Receiver, Sender};
use std::sync::mpsc;

fn main() {
    let file_name = std::env::args().nth(1).expect("Expected one file argument");

    let file = match std::fs::File::open(file_name.as_str()) {
        Ok(f) => f,
        Err(e) => {
            eprintln!(
                "{}Error{}: {}: {}",
                ansi::Fg::Red,
                ansi::Fg::Reset,
                file_name,
                e
            );
            std::process::exit(1);
        }
    };

    let mapped_file = unsafe {
        match memmap::Mmap::map(&file) {
            Ok(f) => f,
            Err(e) => {
                eprintln!(
                    "{}Error{}: {}: {}",
                    ansi::Fg::Red,
                    ansi::Fg::Reset,
                    file_name,
                    e
                );
                std::process::exit(1);
            }
        }
    };

    let source = match std::str::from_utf8(&mapped_file[..]) {
        Ok(s) => s,
        Err(e) => {
            eprintln!(
                "{}Error{}: {}: {}",
                ansi::Fg::Red,
                ansi::Fg::Reset,
                file_name,
                e
            );
            std::process::exit(1);
        }
    };

    println!("Source:\n\n{}\n", source);

    let mut tir = ir::Module::new();

    let (ir_tx, ir_rx): (
        Sender<Option<ir::ChannelIr>>,
        Receiver<Option<ir::ChannelIr>>,
    ) = mpsc::channel();

    let (notice_tx, notice_rx): (Sender<Option<Notice>>, Receiver<Option<Notice>>) =
        mpsc::channel();

    let arc_source: std::sync::Arc<str> = std::sync::Arc::from(source);

    let parser = Parser::parse(file_name, arc_source, notice_tx, ir_tx);

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

    println!("\n{}", tir);
}
