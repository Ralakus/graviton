pub extern crate graviton_ast as ast;
pub extern crate graviton_backend as backend;
pub extern crate graviton_frontend as frontend;
pub extern crate graviton_ir as ir;

pub extern crate colored;
use colored::*;

pub mod errors;

#[derive(Debug, Clone)]
pub enum GravitonError {
    ParseError(Vec<frontend::parser::ParseError>),
    SemanticError(Vec<ast::semantic::SemanticError>),
    NativeError(Vec<backend::native::NativeError>),
}

impl<'a> GravitonError {
    pub fn report(&self, source: Option<&'a str>) {
        match self {
            GravitonError::ParseError(perrors) => {
                for perror in perrors {
                    errors::report_parser_error(perror, source)
                }
            }
            GravitonError::SemanticError(serrors) => {
                for serror in serrors {
                    errors::report_semantic_error(serror, source)
                }
            }
            GravitonError::NativeError(nerrors) => {
                for nerror in nerrors {
                    errors::report_native_error(nerror, source)
                }
            }
        }
    }
}

pub fn parse_source<'a>(
    source: &'a str,
    filename: Option<&'a str>,
    debug_level: i32,
) -> Result<ast::Module, GravitonError> {
    match frontend::parser::Parser::parse(source, filename) {
        Ok(mut module) => {
            match analyze_module(
                if let Some(f) = filename {
                    Some(String::from(f))
                } else {
                    None
                },
                &mut module,
            ) {
                Ok(_) => {
                    if debug_level >= 2 {
                        println!("{}: {:#?}", "Typed AST".cyan(), module);
                    }
                    Ok(module)
                }
                Err(e) => {
                    if debug_level >= 2 {
                        println!("{}: {:#?}", "Untyped AST".red(), module);
                    }
                    Err(e)
                }
            }
        }
        Err(e) => Err(GravitonError::ParseError(e)),
    }
}

pub fn analyze_module(name: Option<String>, module: &mut ast::Module) -> Result<(), GravitonError> {
    match ast::semantic::SemanticAnalyzer::analyze(module, name, None) {
        Ok(_) => Ok(()),
        Err(e) => Err(GravitonError::SemanticError(e)),
    }
}

pub fn compile_module(
    name: String,
    module: &ast::Module,
    debug_level: i32,
) -> Result<backend::native::NativeObject, GravitonError> {
    match backend::native::Native::compile(name, module, debug_level) {
        Ok(obj) => Ok(obj),
        Err(e) => Err(GravitonError::NativeError(e)),
    }
}

pub fn compile_source<'a>(
    source: &'a str,
    filename: Option<&'a str>,
    debug_level: i32,
) -> Result<backend::native::NativeObject, GravitonError> {
    let module = parse_source(source, filename, debug_level)?;
    compile_module(
        if let Some(f) = filename {
            String::from(f)
        } else {
            String::from("graviton")
        },
        &module,
        debug_level,
    )
}
