pub extern crate graviton_ast as ast;
pub extern crate graviton_core as core;
pub extern crate graviton_frontend as frontend;
pub extern crate graviton_backend as backend;

pub extern crate colored;
use colored::*;

pub fn report_notices<'a>(notices: &[core::Notice], source: Option<&'a str>) {
    for n in notices {
        n.report(source);
    }
}

pub fn parse_source<'a>(
    source: &'a str,
    filename: Option<&'a str>,
    debug_level: i32,
) -> Result<(ast::Module, Vec<core::Notice>), Vec<core::Notice>> {
    match frontend::parser::Parser::parse(source, filename) {
        Ok((mut module, parse_notices)) => {
            match analyze_module(
                if let Some(f) = filename {
                    Some(String::from(f))
                } else {
                    None
                },
                &mut module,
            ) {
                Ok(semantic_notices) => {
                    if debug_level >= 2 {
                        println!("{}: {:#?}", "Typed AST".cyan(), module);
                    }
                    Ok((
                        module,
                        parse_notices
                            .iter()
                            .cloned()
                            .chain(semantic_notices.iter().cloned())
                            .collect(),
                    ))
                }
                Err(e) => {
                    if debug_level >= 2 {
                        println!("{}: {:#?}", "Untyped AST".red(), module);
                    }
                    Err(e)
                }
            }
        }
        e => e,
    }
}

pub fn analyze_module(
    name: Option<String>,
    module: &mut ast::Module,
) -> Result<Vec<core::Notice>, Vec<core::Notice>> {
    match ast::semantic::SemanticAnalyzer::analyze(module, name, None) {
        Ok(notices) => Ok(notices),
        Err(e) => Err(e),
    }
}

pub fn compile_module(
    name: String,
    module: &ast::Module,
    debug_level: i32,
) -> Result<backend::native::NativeObject, Vec<core::Notice>> {
    match backend::native::Native::compile(name, module, debug_level) {
        Ok(obj) => Ok(obj),
        Err(e) => Err(e),
    }
}

pub fn compile_source<'a>(
    source: &'a str,
    filename: Option<&'a str>,
    debug_level: i32,
) -> Result<backend::native::NativeObject, Vec<core::Notice>> {
    let module = parse_source(source, filename, debug_level)?;
    compile_module(
        if let Some(f) = filename {
            String::from(f)
        } else {
            String::from("graviton")
        },
        &module.0,
        debug_level,
    )
}