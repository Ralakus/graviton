#![warn(clippy::nursery)]
use clap::{App, Arg};
use graviton::{
    core::{ansi, ir, notice::Notice, semantic::Analyzer},
    frontend::parser::Parser,
};
use std::{collections::HashMap, sync::mpsc};

const PROGRAM_NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHOR: &str = env!("CARGO_PKG_AUTHORS");
const DESCRIPTION: &str = env!("CARGO_PKG_DESCRIPTION");

fn main() {
    let args = App::new(PROGRAM_NAME)
        .version(VERSION)
        .about(DESCRIPTION)
        .author(AUTHOR)
        .arg(
            Arg::with_name("Input")
                .help("The input file to process")
                .required(true)
                .index(1),
        )
        .get_matches();

    let file_name = if let Some(input) = args.value_of("Input") {
        input
    } else {
        eprintln!("{}Expected an input file{}", ansi::Fg::Red, ansi::Fg::Reset,);
        std::process::exit(1);
    };

    let top_file = match std::fs::File::open(file_name) {
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

    let top_mapped_file = unsafe {
        match memmap::Mmap::map(&top_file) {
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

    let top_source = match std::str::from_utf8(&top_mapped_file[..]) {
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

    let mut mapped_files_map: HashMap<String, memmap::Mmap> = HashMap::new();

    println!("Source:\n\n{}\n", top_source);

    let mut tir = ir::Module::new();

    let (parser_ir_tx, analyzer_ir_rx) = mpsc::channel();
    let (analyzer_ir_tx, ir_rx) = mpsc::channel();

    let (notice_tx, notice_rx) = mpsc::channel();

    let (source_tx, source_rx) = mpsc::channel();

    let (source_request_tx, source_request_rx) = mpsc::channel();

    if let Err(e) = source_tx.send(Some(std::sync::Arc::from(top_source))) {
        eprintln!("{}Error: {}{}", ansi::Fg::BrightRed, e, ansi::Fg::Reset);
    }

    let parser = Parser::create(
        file_name.to_string(),
        source_rx,
        source_request_tx,
        notice_tx.clone(),
        parser_ir_tx,
    );

    let analyzer = Analyzer::create(notice_tx, analyzer_ir_tx, analyzer_ir_rx);

    let (mut ir_done, mut source_request_done) = (false, false);

    while !ir_done || !source_request_done {
        match ir_rx.try_recv() {
            Ok(Some(ins)) => tir.push(ins.pos, ins.sig, ins.ins),
            Ok(None) => ir_done = true,
            Err(std::sync::mpsc::TryRecvError::Empty) => (),
            Err(std::sync::mpsc::TryRecvError::Disconnected) if ir_done => (),
            Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                panic!("ir_rx disconnected before sending end signal")
            }
        }

        if let Ok(Some(notice)) = notice_rx.try_recv() {
            report_notice(notice, &mapped_files_map, file_name, top_source)
        }

        match source_request_rx.try_recv() {
            Ok(Some(file_name)) => {
                match std::fs::File::open(file_name.as_str()) {
                    Ok(file) => {
                        unsafe {
                            match memmap::Mmap::map(&file) {
                                Ok(map) => {
                                    mapped_files_map.insert(file_name.clone(), map);
                                    match std::str::from_utf8(&mapped_files_map[&file_name][..]) {
                                        Ok(s) => {
                                            if let Err(e) =
                                                source_tx.send(Some(std::sync::Arc::from(s)))
                                            {
                                                eprintln!(
                                                    "{}Error: {}{}",
                                                    ansi::Fg::BrightRed,
                                                    e,
                                                    ansi::Fg::Reset
                                                );
                                            }
                                        }
                                        Err(e) => {
                                            eprintln!(
                                                "{}Error{}: {}: {}",
                                                ansi::Fg::Red,
                                                ansi::Fg::Reset,
                                                file_name,
                                                e
                                            );
                                            if let Err(e) = source_tx.send(None) {
                                                eprintln!(
                                                    "{}Error: {}{}",
                                                    ansi::Fg::BrightRed,
                                                    e,
                                                    ansi::Fg::Reset
                                                );
                                            }
                                        }
                                    };
                                }
                                Err(e) => {
                                    eprintln!(
                                        "{}Error{}: {}: {}",
                                        ansi::Fg::Red,
                                        ansi::Fg::Reset,
                                        file_name,
                                        e
                                    );
                                    if let Err(e) = source_tx.send(None) {
                                        eprintln!(
                                            "{}Error: {}{}",
                                            ansi::Fg::BrightRed,
                                            e,
                                            ansi::Fg::Reset
                                        );
                                    }
                                }
                            }
                        };
                    }
                    Err(e) => {
                        eprintln!(
                            "{}Error{}: {}: {}",
                            ansi::Fg::Red,
                            ansi::Fg::Reset,
                            file_name,
                            e
                        );
                        if let Err(e) = source_tx.send(None) {
                            eprintln!("{}Error: {}{}", ansi::Fg::BrightRed, e, ansi::Fg::Reset);
                        }
                    }
                };
            }
            Ok(None) => source_request_done = true,
            Err(std::sync::mpsc::TryRecvError::Empty) => (),
            Err(std::sync::mpsc::TryRecvError::Disconnected) if source_request_done => (),
            Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                panic!("source_request_rx disconnected before sending end signal")
            }
        }
    }

    parser.join().expect("Error joining parser thread");
    analyzer.join().expect("Error joining analyzer thread");

    println!("\n{}", tir);
}

fn report_notice<'a>(
    notice: Notice,
    map: &HashMap<String, memmap::Mmap>,
    file_name: &str,
    top_source: &'a str,
) {
    let notice_file = notice.file.clone();
    if notice_file == *file_name {
        notice.report(Some(top_source));
    } else {
        notice.report(match map.get(&notice_file) {
            Some(map) => match std::str::from_utf8(&map[..]) {
                Ok(s) => Some(s),
                Err(_) => None,
            },
            None => None,
        })
    }
}
