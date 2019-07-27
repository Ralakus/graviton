
use super::ast;

use std::collections::HashMap;

pub struct SemanticError {
    pub msg: String
}

struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,

    variables: HashMap<String, ast::TypeSignature>,
    function: HashMap<String, ast::FunctionSignature>
}

pub struct SemanticAnalyzer<'a> {
    scopes: Vec<Scope<'a>>
}

impl<'a> SemanticAnalyzer<'a> {
    
    fn make_err(&self, msg: &'static str) -> SemanticError {
        SemanticError { msg: msg.to_string() }
    }

    pub fn analyze(ast: ast::Ast) -> Result<(), SemanticError> {

        let sa = SemanticAnalyzer {
            scopes: Vec::new()
        };

        Err(sa.make_err("not implemented yet"))
    }
}