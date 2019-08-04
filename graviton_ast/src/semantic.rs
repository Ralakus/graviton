
use super::ast;

use std::collections::HashMap;

const DEFAULT_NUM_PRIMITIVE_SIGNATURE: ast::PrimitiveType = ast::PrimitiveType::I32;
const DEFAULT_NUM_TYPE_SIGNATURE: ast::TypeSignature = ast::TypeSignature::Primitive(DEFAULT_NUM_PRIMITIVE_SIGNATURE);
const NIL_TYPE_SIGNATURE: ast::TypeSignature = ast::TypeSignature::Primitive(ast::PrimitiveType::Nil);
const BOOL_TYPE_SIGNATURE: ast::TypeSignature = ast::TypeSignature::Primitive(ast::PrimitiveType::Bool);

#[derive(Debug, Clone)]
pub struct SemanticError {
    pub msg: String,
    pub pos: super::Position,
}

#[derive(Debug, Clone)]
pub struct SemanticStdLib {
    variables: HashMap<String, (bool, ast::TypeSignature)>
}

impl SemanticStdLib {
    pub fn new() -> SemanticStdLib {
        SemanticStdLib {
            variables: HashMap::new()
        }
    }

    pub fn add_fn(&mut self, name: String, sig: ast::FunctionSignature) {
        self.variables.insert(name, (false, ast::TypeSignature::Function(sig)));
    }
}

#[derive(Debug, Clone)]
struct Scope {
    variables: HashMap<String, (bool, ast::TypeSignature)>,
}

pub struct SemanticAnalyzer {
    scopes: Vec<Scope>,
    errors: Vec<SemanticError>
}

impl<'a> SemanticAnalyzer {

    fn check_if_var_in_scopes(&self, var: &String) -> Option<&(bool, ast::TypeSignature)> {
        for s in self.scopes.iter().rev() {
            match s.variables.get(var) {
                Some(ts) => return Some(ts),
                None => continue
            }
        }
        None
    }
    
    fn make_err(&mut self, ast: &ast::AstNode, msg: String) -> SemanticError {
        let e = SemanticError { msg: msg, pos: ast.pos.clone() };
        self.errors.push(e.clone());
        e
    }

    fn new_scope(&mut self) {
        self.scopes.push(Scope { variables: HashMap::new() });
    }

    fn pop_scope(&mut self, ast: &ast::AstNode) {
        if self.scopes.len() == 1 {
            self.make_err(ast, format!("Cannot pop global scope"));
        } else {
            self.scopes.pop();
        }
    }

    fn last_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn check_if_type_is_defined(&mut self, type_: &ast::TypeSignature) -> Option<ast::TypeSignature> {
        match type_ {
            ast::TypeSignature::Primitive(_) => Some(type_.clone()),
            ast::TypeSignature::Function(_) => Some(type_.clone()),
            ast::TypeSignature::Custom(s) if s == "String" => Some(type_.clone()),
            _ => None
        }
    }

    pub fn analyze(ast: &ast::AstNode, stdlib: Option<SemanticStdLib>) -> Result<(), Vec<SemanticError>> {

        let mut sa = SemanticAnalyzer {
            scopes: vec!( Scope { variables: HashMap::new() } ),
            errors: Vec::new()
        };

        if let Some(lib) = stdlib {
            sa.last_scope().variables.extend(lib.variables);
        }

        analyze(&mut sa, ast);

        if sa.errors.len() > 0 {
            Err(sa.errors)
        } else {
            Ok(())
        }
    }
}

pub fn analyze(sa: &mut SemanticAnalyzer, ast: &ast::AstNode) -> ast::TypeSignature {
    match &ast.node {
        ast::Ast::Identifier(s) => {
            if let None = sa.check_if_var_in_scopes(&s) {
                sa.make_err(ast, format!("Variable {} not found in scope", s));
            }
            match sa.check_if_var_in_scopes(s) {
                Some(v) => v.1.clone(),
                None => NIL_TYPE_SIGNATURE.clone()
            }
        },
        ast::Ast::Number(_) => { DEFAULT_NUM_TYPE_SIGNATURE.clone() },
        ast::Ast::String(_) => {
            ast::TypeSignature::Custom(String::from("String"))
        },
        ast::Ast::Bool(_) => {
            BOOL_TYPE_SIGNATURE.clone()
        },
        ast::Ast::Statement(ast) => {
            analyze(sa, &*ast);
            NIL_TYPE_SIGNATURE.clone()
        },
        ast::Ast::Binary(op, l, r) => {
            match op {
                ast::BinaryOperation::Less |
                ast::BinaryOperation::LessEqual |
                ast::BinaryOperation::Greater |
                ast::BinaryOperation::GreaterEqual |
                ast::BinaryOperation::Equal |
                ast::BinaryOperation::NotEqual => {
                    if analyze(sa, &**l) != analyze(sa, &**r) {
                        sa.make_err(ast, format!("Binary operands are not the same type"));
                    }
                    BOOL_TYPE_SIGNATURE.clone()
                },
                ast::BinaryOperation::And => {
                    let ltype = analyze(sa, &**l);
                    if ltype != BOOL_TYPE_SIGNATURE {
                        sa.make_err(ast, format!("Left binary and operand not of type Bool; found {:?}", ltype));
                    }
                    let rtype = analyze(sa, &**r);
                    if rtype != BOOL_TYPE_SIGNATURE {
                        sa.make_err(ast, format!("Right binary and operand not of type Bool; found {:?}", rtype));
                    }
                    BOOL_TYPE_SIGNATURE.clone()
                }
                ast::BinaryOperation::Or => {
                    let ltype = analyze(sa, &**l);
                    if ltype != BOOL_TYPE_SIGNATURE {
                        sa.make_err(ast, format!("Left binary or operand not of type Bool; found {:?}", ltype));
                    }
                    let rtype = analyze(sa, &**r);
                    if rtype != BOOL_TYPE_SIGNATURE {
                        sa.make_err(ast, format!("Right binary or operand not of type Bool; found {:?}", rtype));
                    }
                    BOOL_TYPE_SIGNATURE.clone()
                },
                ast::BinaryOperation::Assign => {
                    let return_type = analyze(sa, &**l);
                    if return_type != analyze(sa, &**r) {
                        sa.make_err(ast, format!("Binary operands are not the same type"));
                    }
                    if let ast::Ast::Identifier(s) = &l.node {
                        if let None = sa.check_if_var_in_scopes(s) {
                            sa.make_err(l, format!("Variable {} not found in scope", s));
                        }
                        return_type
                    } else {
                        sa.make_err(ast, format!("Binary assign not assigning variable"));
                        NIL_TYPE_SIGNATURE.clone()
                    }
                },
                _ => {
                    let return_type = analyze(sa, &**l);
                    if return_type != analyze(sa, &**r) {
                        sa.make_err(ast, format!("Binary operands are not the same type"));
                    }
                    return_type
                }
            }
        },
        ast::Ast::Unary(_, expr) => {
            analyze(sa, &**expr)
        },
        ast::Ast::Return(_) => {
            sa.make_err(ast, format!("Returns may only be present within blocks"));
            NIL_TYPE_SIGNATURE.clone()
        },
        ast::Ast::Block(exprs) => {
            sa.new_scope();
            let mut idx = 1;
            let len = exprs.len();
            let mut return_type: Option<ast::TypeSignature> = None;
            for expr in exprs {
                match &expr.node {
                    ast::Ast::Statement(e) => {
                        if let ast::Ast::Return(rexpr) = &e.node {
                            let ret = analyze(sa, &rexpr);
                            if let Some(t) = &return_type {
                                if ret != *t {
                                    sa.make_err(&rexpr, format!("Not the same return type as previous returns"));
                                }
                            } else {
                                return_type = Some(ret);
                            }
                        } else {
                            analyze(sa, &expr);
                        }
                    },
                    e => {
                        if idx != len {
                            sa.make_err(expr, format!("Only the last element in a block can be an expression"));
                        }
                        if let ast::Ast::Return(rexpr) = e {
                            let ret = analyze(sa, &rexpr);
                            if let Some(t) = &return_type {
                                if ret != *t {
                                    sa.make_err(&rexpr, format!("Not the same return type as previous returns"));
                                }
                            } else {
                                return_type = Some(ret);
                            }
                        } else {
                            return_type = Some(analyze(sa, &expr));
                        }
                    }
                }
                idx += 1;
            }
            sa.pop_scope(ast);
            if let Some(r) = return_type {
                r
            } else {
                NIL_TYPE_SIGNATURE.clone()
            }
        }
        ast::Ast::IfElse(ifcond, ifexpr, elseifs, elseexpr) => {
            sa.new_scope();
            let ifcond_type = analyze(sa, &**ifcond);
            if ifcond_type != BOOL_TYPE_SIGNATURE {
                sa.make_err(ifcond, format!("If condition must evaluate to Bool; got {:?}", ifcond_type));
            }
            let expr_type = analyze(sa, &**ifexpr);
            sa.pop_scope(ast);
            for (eifc, eife) in elseifs {
                sa.new_scope();
                let eifc_type = analyze(sa, &**eifc);
                if eifc_type != BOOL_TYPE_SIGNATURE {
                    sa.make_err(eifc, format!("Else if condition must evaluate to Bool; got {:?}", eifc_type));
                }
                let branch_type = analyze(sa, &**eife);
                if branch_type != expr_type {
                    sa.make_err(eife, format!("If branch doesn't have the same return type; expected {:?} but got {:?}", expr_type, branch_type));
                }
                sa.pop_scope(ast);
            }
            if let Some(eexpr) = elseexpr {
                let branch_type = analyze(sa, &**eexpr);
                if branch_type != expr_type {
                    sa.make_err(eexpr, format!("If branch doesn't have the same return type; expected {:?} but got {:?}", expr_type, branch_type));
                }
            }
            expr_type
        },
        ast::Ast::While(cond, expr) => {
            sa.new_scope();
            let cond_type = analyze(sa, &**cond);
            if cond_type != BOOL_TYPE_SIGNATURE {
                sa.make_err(cond, format!("While condition must evaluate to Bool; got {:?}", cond_type));
            }
            let return_type = analyze(sa, &**expr);
            sa.pop_scope(ast);
            return_type
        },
        ast::Ast::Let(name, sig, expr) => {
            if let None = sa.check_if_var_in_scopes(&name) {
                let mut return_type = NIL_TYPE_SIGNATURE.clone();
                if let Some(type_sig) = &sig.type_sig {
                    if let Some(e) = expr {
                        let e_type = analyze(sa, &*e);
                        if *type_sig != e_type {
                            sa.make_err(ast, format!("Variable type and assign type do not match; expected {:?} but got {:?}", *type_sig, e_type));
                        }
                    }
                    sa.last_scope().variables.insert(name.clone(), (sig.mutable, type_sig.clone()));
                    return_type = type_sig.clone();
                    // println!("Variable {} of type {:?}, stated", sig.name, type_sig);
                } else {
                    if let Some(e) = expr {
                        let expr_type = analyze(sa, &*e);
                        // println!("Variable {} of type {:?}, infer", sig.name, expr_type);
                        return_type = expr_type.clone();
                        sa.last_scope().variables.insert(name.clone(), (sig.mutable, expr_type));
                    } else {
                        sa.make_err(ast, format!("Cannot infer type without an assign expression"));
                        sa.last_scope().variables.insert(name.clone(), (sig.mutable, NIL_TYPE_SIGNATURE.clone()));
                    }
                }
                return_type
            } else {
                sa.make_err(ast, format!("Variable {} already defined", name));
                NIL_TYPE_SIGNATURE.clone()
            }
        },
        ast::Ast::Import(_, expr) => {
            analyze(sa, &**expr)
        },
        ast::Ast::FnDef(sig, param_names, expr) => {
            let mut fn_return_type = sig.clone();
            sa.new_scope();
            for (var, name) in sig.params.iter().zip(param_names.iter()) {
                if let Some(type_sig) = &var.type_sig {
                    if let None = sa.check_if_type_is_defined(type_sig) {
                        sa.make_err(ast, format!("Type {:?} is not defined", type_sig));
                    }
                    sa.last_scope().variables.insert(name.clone(), (var.mutable, type_sig.clone()));
                } else {
                    sa.make_err(ast, format!("Function parameter types cannot be infered; Parameter: {}", name));
                    sa.last_scope().variables.insert(name.clone(), (var.mutable, NIL_TYPE_SIGNATURE.clone()));
                }
            }
            if let Some(type_sig) = &sig.return_type {
                if let None = sa.check_if_type_is_defined(type_sig) {
                    sa.make_err(ast, format!("Type {:?} is not defined", type_sig));
                }
                let expr_type = analyze(sa, &**expr);
                if **type_sig != expr_type {
                    sa.make_err(ast, format!("Return types not the same; expected {:?} but got {:?}", **type_sig, expr_type));
                }
                fn_return_type.return_type = Some(Box::new((**type_sig).clone()));
            } else {
                fn_return_type.return_type = Some(Box::new(analyze(sa, &**expr)));
            }
            sa.pop_scope(ast);
            ast::TypeSignature::Function(fn_return_type)
        },
        ast::Ast::FnCall(callee, args) => {
            let mut return_type: ast::TypeSignature;
            if let ast::TypeSignature::Function(sig) = analyze(sa, &**callee) {
                if sig.params.len() == args.len() {
                    for (param, arg) in sig.params.iter().zip(args.iter()) {
                        if let Some(type_sig) = &param.type_sig {
                            let arg_return_type = analyze(sa, arg);
                            if *type_sig != arg_return_type {
                                sa.make_err(&arg, format!("Expected type {:?} but got type {:?}", type_sig, arg_return_type));
                            }
                        } else {
                            unreachable!();
                        }
                    }
                    return_type = *sig.return_type.unwrap();
                } else {
                    sa.make_err(&**callee, format!("Function expected {} arguments but got {} arguments", sig.params.len(), args.len()));
                    return_type = NIL_TYPE_SIGNATURE.clone();
                }
            } else {
                sa.make_err(&**callee, format!("Not a callable expression"));
                return_type = NIL_TYPE_SIGNATURE.clone();
            }
            return_type
        },
        ast::Ast::As(expr, type_) => {
            let expr_type = analyze(sa, &**expr);
            if let None = sa.check_if_type_is_defined(type_) {
                sa.make_err(ast, format!("Type {:?} is not defined", type_));
            }
            match (&expr_type, &type_) {
                (ast::TypeSignature::Primitive(_), ast::TypeSignature::Primitive(_)) => {
                    type_.clone()
                },
                (_, ast::TypeSignature::Primitive(_)) => {
                    sa.make_err(expr, format!("Can only cast between primtive types; Left of as is of type {:?}", expr_type));
                    expr_type
                },
                (ast::TypeSignature::Primitive(_), _) => {
                    sa.make_err(ast, format!("Can only cast between primtive types; Right of as is of type {:?}", type_));
                    expr_type
                },
                _ => {
                    sa.make_err(expr, format!("Can only cast between primtive types; {:?} and {:?} are not primitives", expr_type, type_));
                    expr_type
                }
            }
        }
    }
}