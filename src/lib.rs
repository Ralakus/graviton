pub extern crate graviton_ast as ast;
pub extern crate graviton_backend as backend;
pub extern crate graviton_frontend as frontend;

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
) -> Result<ast::AstNode, GravitonError> {
    match frontend::parser::Parser::parse(source, filename) {
        Ok(mut ast) => {
            match analyze_ast(
                if let Some(f) = filename {
                    Some(String::from(f))
                } else {
                    None
                },
                &mut ast,
            ) {
                Ok(_) => {
                    if debug_level >= 2 {
                        println!("{}: {:#?}", "Typed AST".cyan(), ast);
                    }
                    Ok(ast)
                }
                Err(e) => {
                    if debug_level >= 2 {
                        println!("{}: {:#?}", "Untyped AST".red(), ast);
                    }
                    Err(e)
                }
            }
        }
        Err(e) => Err(GravitonError::ParseError(e)),
    }
}

pub fn analyze_ast(name: Option<String>, ast: &mut ast::AstNode) -> Result<(), GravitonError> {
    match ast::semantic::SemanticAnalyzer::analyze(
        ast,
        name,
        Some(backend::native::stdlib::get_stdlib_signatures()),
    ) {
        Ok(_) => Ok(()),
        Err(e) => Err(GravitonError::SemanticError(e)),
    }
}

pub fn compile_ast(
    name: String,
    ast: &ast::AstNode,
    debug_level: i32,
) -> Result<backend::native::NativeObject, GravitonError> {
    match backend::native::Native::compile(name, ast, debug_level) {
        Ok(obj) => Ok(obj),
        Err(e) => Err(GravitonError::NativeError(e)),
    }
}

pub fn compile_source<'a>(
    source: &'a str,
    filename: Option<&'a str>,
    debug_level: i32,
) -> Result<backend::native::NativeObject, GravitonError> {
    let ast = parse_source(source, filename, debug_level)?;
    compile_ast(
        if let Some(f) = filename {
            String::from(f)
        } else {
            String::from("graviton")
        },
        &ast,
        debug_level,
    )
}
