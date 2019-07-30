
use super::ast;

use std::collections::HashMap;

const DEFAULT_NUM_SIGNATURE: ast::PrimitiveType = ast::PrimitiveType::I32;

#[derive(Clone)]
pub struct SemanticError {
    pub msg: String
}

#[derive(Clone)]
struct Scope {
    variables: HashMap<String, ast::TypeSignature>,
    functions: HashMap<String, ast::FunctionSignature>
}

pub struct SemanticAnalyzer {
    scopes: Vec<Scope>,
    errors: Vec<SemanticError>
}

impl<'a> SemanticAnalyzer {

    fn check_if_var_in_scopes(&self, var: &'a str) -> Option<&ast::TypeSignature> {
        for s in self.scopes.iter().rev() {
            match s.variables.get(&var.to_string()) {
                Some(ts) => return Some(ts),
                None => continue
            }
        }
        None
    }

    fn check_if_function_in_scopes(&self, func: &'a str) -> Option<&ast::FunctionSignature> {
        for s in self.scopes.iter().rev() {
            match s.functions.get(&func.to_string()) {
                Some(fs) => return Some(fs),
                None => continue
            }
        }
        None
    }
    
    fn make_err(&mut self, msg: &'a str) -> SemanticError {
        let e = SemanticError { msg: msg.to_string() };
        self.errors.push(e.clone());
        e
    }

    fn new_scope(&mut self) {
        self.scopes.push(Scope { variables: HashMap::new(), functions: HashMap::new() });
    }

    pub fn analyze(ast: ast::Ast) -> Result<(), Vec<SemanticError>> {

        let mut sa = SemanticAnalyzer {
            scopes: Vec::new(),
            errors: Vec::new()
        };

        analyze(&mut sa, ast);

        sa.make_err("not implemented yet");

        if sa.errors.len() > 0 {
            Err(sa.errors)
        } else {
            Ok(())
        }
    }
}

pub fn eval_expr_type(ast: ast::Ast) -> ast::TypeSignature {
    ast::TypeSignature::new("i32")
}

pub fn analyze(sa: &mut SemanticAnalyzer, ast: ast::Ast) {
    match ast {
        ast::Ast::Identifier(s) => {
            if let None = sa.check_if_var_in_scopes(s.as_str()) {
                sa.make_err(format!("Variable {} not defined", s).as_str());
            }
        },
        ast::Ast::Number(_) => (),
        ast::Ast::String(_) => {
            sa.make_err("Strings not supported yet");
        },
        ast::Ast::Bool(_) => (),
        ast::Ast::Statement(ast) => {
            analyze(sa, *ast);
        },
        ast::Ast::Binary(_, l, r) => {
            if eval_expr_type(*l) != eval_expr_type(*r) {
                sa.make_err("Invalid binary types");
            }
        },
        ast::Ast::Unary(_, _) => {

        },
        ast::Ast::Return(_) => {

        },
        ast::Ast::Block(_) => {

        }
        ast::Ast::IfElse(_, _, _, _) => {

        },
        ast::Ast::While(_, _) => {

        },
        ast::Ast::Let(_, _, _) => {

        },
        ast::Ast::Import(_) => {

        },
        ast::Ast::FnDef(_, _) => {

        },
        ast::Ast::FnCall(_, _) => {

        }
    };
}