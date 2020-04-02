use graviton::{
    core::{ansi, ir, notice::Notice, semantic::Analyzer},
    frontend::parser::Parser,
};
use std::{collections::HashMap, sync::mpsc};

pub struct Driver {
    mapped_files: HashMap<String, memmap::Mmap>,
}

impl Driver {
    fn new() -> Self {
        Self {
            mapped_files: HashMap::new(),
        }
    }

    pub async fn create(args: clap::ArgMatches<'_>) -> ir::Module {
        let mut tir = ir::Module::new();

        let d = std::sync::Mutex::new(Self::new());

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

        let (parser_ir_tx, analyzer_ir_rx) = mpsc::channel();
        let (analyzer_ir_tx, ir_rx) = mpsc::channel();

        let (notice_tx, notice_rx) = mpsc::channel();

        let (source_tx, source_rx) = mpsc::channel();

        let (source_request_tx, source_request_rx) = mpsc::channel();

        if let Err(e) = source_tx.send(Some(std::sync::Arc::from(top_source))) {
            eprintln!("{}Error: {}{}", ansi::Fg::BrightRed, e, ansi::Fg::Reset);
        }

        let parser_task = Parser::create(
            file_name.to_string(),
            source_rx,
            source_request_tx,
            notice_tx.clone(),
            parser_ir_tx,
        );

        let analyzer_task = Analyzer::create(notice_tx, analyzer_ir_tx, analyzer_ir_rx);

        let notice_task = async {
            loop {
                match notice_rx.recv() {
                    Ok(Some(notice)) => {
                        let mapped_files = &d.lock().unwrap().mapped_files;
                        Self::report_notice(notice, mapped_files, file_name, top_source)
                    }
                    Ok(None) => (),
                    _ => break,
                }
            }
        };

        let ir_task = async {
            while let Ok(Some(ir)) = ir_rx.recv() {
                tir.push(ir.pos, ir.sig, ir.ins);
            }
        };

        let source_request_task = async {
            while let Ok(Some(name)) = source_request_rx.recv() {
                let file = match std::fs::File::open(name.as_str()) {
                    Ok(f) => f,
                    Err(e) => {
                        eprintln!("{}Error: {}{}", ansi::Fg::BrightRed, e, ansi::Fg::Reset);
                        if let Err(esend) = source_tx.send(None) {
                            eprintln!("{}Error: {}{}", ansi::Fg::BrightRed, esend, ansi::Fg::Reset);
                        }
                        continue;
                    }
                };

                let mapped_file = unsafe {
                    match memmap::Mmap::map(&file) {
                        Ok(m) => m,
                        Err(e) => {
                            eprintln!("{}Error: {}{}", ansi::Fg::BrightRed, e, ansi::Fg::Reset);
                            if let Err(esend) = source_tx.send(None) {
                                eprintln!(
                                    "{}Error: {}{}",
                                    ansi::Fg::BrightRed,
                                    esend,
                                    ansi::Fg::Reset
                                );
                            }
                            continue;
                        }
                    }
                };

                let source = match std::str::from_utf8(&mapped_file[..]) {
                    Ok(s) => s,
                    Err(e) => {
                        eprintln!("{}Error: {}{}", ansi::Fg::BrightRed, e, ansi::Fg::Reset);
                        if let Err(esend) = source_tx.send(None) {
                            eprintln!("{}Error: {}{}", ansi::Fg::BrightRed, esend, ansi::Fg::Reset);
                        }
                        continue;
                    }
                };

                if let Err(esend) = source_tx.send(Some(std::sync::Arc::from(source))) {
                    eprintln!("{}Error: {}{}", ansi::Fg::BrightRed, esend, ansi::Fg::Reset);
                }

                let mapped_files = &mut d.lock().unwrap().mapped_files;
                mapped_files.insert(name, mapped_file);
            }
        };

        futures::join!(
            parser_task,
            analyzer_task,
            notice_task,
            ir_task,
            source_request_task
        );

        tir
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
}
