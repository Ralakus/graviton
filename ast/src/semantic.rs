use super::{ast, Notice, NoticeLevel};

use std::collections::HashMap;

const DEFAULT_NUM_PRIMITIVE_SIGNATURE: ast::PrimitiveType = ast::PrimitiveType::I32;
const DEFAULT_NUM_TYPE_SIGNATURE: ast::TypeSignature =
    ast::TypeSignature::Primitive(DEFAULT_NUM_PRIMITIVE_SIGNATURE);
const NIL_TYPE_SIGNATURE: ast::TypeSignature =
    ast::TypeSignature::Primitive(ast::PrimitiveType::Nil);
const BOOL_TYPE_SIGNATURE: ast::TypeSignature =
    ast::TypeSignature::Primitive(ast::PrimitiveType::Bool);
const FLOAT_TYPE_SIGNATURE: ast::TypeSignature =
    ast::TypeSignature::Primitive(ast::PrimitiveType::F32);

#[derive(Debug, Clone, Default)]
pub struct SemanticStdLib {
    variables: HashMap<String, (bool, ast::TypeSignature)>,
}

impl SemanticStdLib {
    pub fn add_fn(&mut self, name: String, sig: ast::FunctionSignature) {
        self.variables
            .insert(name, (false, ast::TypeSignature::Function(sig)));
    }
}

#[derive(Debug, Clone)]
struct Scope {
    variables: HashMap<String, (bool, ast::TypeSignature)>,
}

pub struct SemanticAnalyzer {
    scopes: Vec<Scope>,
    notices: Vec<Notice>,
    in_function_block: bool,
    file: Option<String>,
    supress_errors: bool,
    current_fn: (String, ast::TypeSignature),
}

impl<'a> SemanticAnalyzer {
    fn check_if_var_in_scopes(&self, var: &str) -> Option<&(bool, ast::TypeSignature)> {
        for s in self.scopes.iter().rev() {
            match s.variables.get(var) {
                Some(ts) => return Some(ts),
                None => continue,
            }
        }
        None
    }

    fn make_notice(&mut self, pos: super::Position, msg: String) -> Notice {
        let e = Notice {
            level: NoticeLevel::Notice,
            msg,
            pos,
            file: self.file.clone(),
            from: "Semantic".to_string(),
        };
        if !self.supress_errors {
            self.notices.push(e.clone());
        }
        e
    }

    fn make_err(&mut self, pos: super::Position, msg: String) -> Notice {
        let e = Notice {
            level: NoticeLevel::Error,
            msg,
            pos,
            file: self.file.clone(),
            from: "Semantic".to_string(),
        };
        if !self.supress_errors {
            self.notices.push(e.clone());
        }
        e
    }

    fn new_scope(&mut self) {
        self.scopes.push(Scope {
            variables: HashMap::new(),
        });
    }

    fn pop_scope(&mut self, pos: super::Position) {
        if self.scopes.len() == 1 {
            self.make_err(pos, "Cannot pop global scope".to_string());
        } else {
            self.scopes.pop();
        }
    }

    fn last_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn check_if_type_is_defined(
        &mut self,
        type_: &ast::TypeSignature,
    ) -> Option<ast::TypeSignature> {
        match type_ {
            ast::TypeSignature::Primitive(_) => Some(type_.clone()),
            ast::TypeSignature::Function(_) => Some(type_.clone()),
            ast::TypeSignature::Custom(s) if s == "String" => Some(type_.clone()),
            _ => None,
        }
    }

    pub fn analyze(
        module: &mut ast::Module,
        filename: Option<String>,
        stdlib: Option<SemanticStdLib>,
    ) -> Result<Vec<Notice>, Vec<Notice>> {
        let mut sa = SemanticAnalyzer {
            scopes: vec![Scope {
                variables: HashMap::new(),
            }],
            notices: Vec::new(),
            in_function_block: false,
            file: filename,
            supress_errors: false,
            current_fn: (String::new(), NIL_TYPE_SIGNATURE.clone()),
        };

        if let Some(lib) = stdlib {
            sa.last_scope().variables.extend(lib.variables);
        }

        analyze_module(&mut sa, module);

        if core::contains_errors(&sa.notices) {
            Err(sa.notices)
        } else {
            Ok(sa.notices)
        }
    }
}

fn analyze_module(sa: &mut SemanticAnalyzer, module: &mut ast::Module) -> ast::TypeSignature {
    let module_type = {
        let mut idx = 1;
        let len = module.expressions.len();
        let mut return_type: Option<ast::TypeSignature> = None;
        for expr in (&mut *module.expressions).iter_mut() {
            let mut e_error = false;
            match expr.node {
                ast::Ast::Statement(_) => {
                    analyze(sa, expr);
                }
                _ => {
                    if idx != len {
                        e_error = true;
                    }
                    return_type = Some(analyze(sa, expr));
                }
            }
            if e_error {
                sa.make_err(
                    expr.pos,
                    "Only the last element in a module can be an expression; consider adding a semicolon \';\' to the end of the expression".to_string(),
                );
            }
            idx += 1;
        }
        if let Some(r) = return_type {
            if r != DEFAULT_NUM_TYPE_SIGNATURE && r != NIL_TYPE_SIGNATURE {
                sa.make_err(
                    super::Position { line: 0, col: 0 },
                    format!("Modules may only return I32 or Nil; found {:?}", r),
                );
            }
            r
        } else {
            NIL_TYPE_SIGNATURE.clone()
        }
    };
    module.type_sig = Some(module_type.clone());
    module_type
}

fn analyze(sa: &mut SemanticAnalyzer, ast: &mut ast::AstNode) -> ast::TypeSignature {
    let node_type = match ast.node {
        ast::Ast::Identifier(ref s) => {
            if sa.check_if_var_in_scopes(&s).is_none() {
                if sa.current_fn.0 == **s {
                    if let ast::TypeSignature::Function(ast::FunctionSignature {
                        return_type: None,
                        ..
                    }) = sa.current_fn.1
                    {
                        sa.make_err(
                            ast.pos,
                            format!(
                                "Recursive function {} must have an explicit return type",
                                sa.current_fn.0
                            ),
                        );
                    } else {
                        return sa.current_fn.1.clone();
                    }
                } else {
                    sa.make_err(ast.pos, format!("Variable {} not found in scope", s));
                }
            }
            match sa.check_if_var_in_scopes(&s) {
                Some(v) => v.1.clone(),
                None => NIL_TYPE_SIGNATURE.clone(),
            }
        }
        ast::Ast::Integer(_) => DEFAULT_NUM_TYPE_SIGNATURE.clone(),
        ast::Ast::Float(_) => FLOAT_TYPE_SIGNATURE.clone(),
        ast::Ast::String(_) => ast::TypeSignature::Custom(String::from("String")),
        ast::Ast::Bool(_) => BOOL_TYPE_SIGNATURE.clone(),
        ast::Ast::Statement(ref mut ast) => {
            analyze(sa, &mut **ast);
            NIL_TYPE_SIGNATURE.clone()
        }
        ast::Ast::Binary(ref op, ref mut l, ref mut r) => match op {
            ast::BinaryOperation::Less
            | ast::BinaryOperation::LessEqual
            | ast::BinaryOperation::Greater
            | ast::BinaryOperation::GreaterEqual
            | ast::BinaryOperation::Equal
            | ast::BinaryOperation::NotEqual => {
                let ltype = analyze(sa, &mut **l);
                let rtype = analyze(sa, &mut **r);
                if ltype != rtype {
                    sa.make_err(
                        ast.pos,
                        format!(
                            "Binary operands are not the same type; {:?} != {:?}",
                            ltype, rtype
                        ),
                    );
                }
                BOOL_TYPE_SIGNATURE.clone()
            }
            ast::BinaryOperation::And => {
                let ltype = analyze(sa, &mut **l);
                if ltype != BOOL_TYPE_SIGNATURE {
                    sa.make_err(
                        l.pos,
                        format!(
                            "Left binary and operand not of type Bool; found {:?}",
                            ltype
                        ),
                    );
                }
                let rtype = analyze(sa, &mut **r);
                if rtype != BOOL_TYPE_SIGNATURE {
                    sa.make_err(
                        r.pos,
                        format!(
                            "Right binary and operand not of type Bool; found {:?}",
                            rtype
                        ),
                    );
                }
                BOOL_TYPE_SIGNATURE.clone()
            }
            ast::BinaryOperation::Or => {
                let ltype = analyze(sa, &mut **l);
                if ltype != BOOL_TYPE_SIGNATURE {
                    sa.make_err(
                        l.pos,
                        format!("Left binary or operand not of type Bool; found {:?}", ltype),
                    );
                }
                let rtype = analyze(sa, &mut **r);
                if rtype != BOOL_TYPE_SIGNATURE {
                    sa.make_err(
                        r.pos,
                        format!(
                            "Right binary or operand not of type Bool; found {:?}",
                            rtype
                        ),
                    );
                }
                BOOL_TYPE_SIGNATURE.clone()
            }
            ast::BinaryOperation::Assign => {
                let return_type = analyze(sa, &mut **l);
                if return_type != analyze(sa, &mut **r) {
                    sa.make_err(r.pos, "Binary operands are not the same type".to_string());
                }
                if let ast::Ast::Identifier(s) = &l.node {
                    if let Some(v) = sa.check_if_var_in_scopes(s) {
                        if !v.0 {
                            sa.make_err(l.pos, format!("Variable {} not mutable", s));
                        }
                    } else {
                        sa.make_err(l.pos, format!("Variable {} not found in scope", s));
                    }
                    return_type
                } else {
                    sa.make_err(l.pos, "Binary assign not assigning variable".to_string());
                    NIL_TYPE_SIGNATURE.clone()
                }
            }
            node => {
                let ltype = analyze(sa, &mut **l);
                let rtype = analyze(sa, &mut **r);
                if ltype.is_nil() || ltype.is_bool() {
                    sa.make_err(ast.pos, format!("Bool and Nil are not valid binary math operand types; Got type {:?} on the left", ltype));
                }
                if rtype.is_nil() || rtype.is_bool() {
                    sa.make_err(ast.pos, format!("Bool and Nil are not valid binary math operand types; Got type {:?} on the right", rtype));
                }
                if ltype != rtype {
                    sa.make_err(
                        ast.pos,
                        format!(
                            "Binary operands are not the same type; {:?} != {:?}",
                            ltype, rtype
                        ),
                    );
                }

                if let ast::BinaryOperation::Divide = node {
                    if rtype.is_integer() && ltype.is_integer() {
                        sa.make_notice(
                            ast.pos,
                            "Binary divide on two integers truncates any decimal results"
                                .to_string(),
                        );
                    }
                }

                ltype
            }
        },
        ast::Ast::Unary(ref op, ref mut expr) => {
            let expr_type = analyze(sa, &mut **expr);
            match op {
                ast::UnaryOperation::Not => {
                    if expr_type != BOOL_TYPE_SIGNATURE {
                        sa.make_err(
                            expr.pos,
                            "Unary not expression must evaluate to Bool".to_string(),
                        );
                    }
                    BOOL_TYPE_SIGNATURE.clone()
                }
                ast::UnaryOperation::Negate => {
                    if !expr_type.is_number() {
                        sa.make_err(
                            expr.pos,
                            "Negate only supports primitive number types".to_string(),
                        );
                    }
                    expr_type
                }
            }
        }
        ast::Ast::Return(_) => {
            sa.make_err(
                ast.pos,
                "Returns may only be present within function blocks".to_string(),
            );
            NIL_TYPE_SIGNATURE.clone()
        }
        ast::Ast::Block(ref mut exprs) => {
            sa.new_scope();
            let mut idx = 1;
            let len = exprs.len();
            let mut return_type: Option<ast::TypeSignature> = None;
            for expr in (&mut **exprs).iter_mut() {
                let mut e_error = false;
                match expr.node {
                    ast::Ast::Statement(ref mut e) => {
                        if let ast::Ast::Return(ref mut rexpr) = &mut e.node {
                            let ret = analyze(sa, rexpr);
                            if let Some(t) = &return_type {
                                if ret != *t {
                                    sa.make_err(
                                        rexpr.pos,
                                        "Not the same return type as previous returns".to_string(),
                                    );
                                }
                            } else {
                                return_type = Some(ret);
                            }
                            if !sa.in_function_block {
                                sa.make_err(
                                    rexpr.pos,
                                    "Returns not allowed outside of function blocks".to_string(),
                                );
                            }
                        } else {
                            analyze(sa, expr);
                        }
                    }
                    ref mut e => {
                        if idx != len {
                            e_error = true;
                        }
                        if let ast::Ast::Return(ref mut rexpr) = e {
                            let ret = analyze(sa, rexpr);
                            if let Some(t) = &return_type {
                                if ret != *t {
                                    sa.make_err(
                                        rexpr.pos,
                                        "Not the same return type as previous returns".to_string(),
                                    );
                                }
                            } else {
                                return_type = Some(ret);
                            }
                            if !sa.in_function_block {
                                sa.make_err(
                                    rexpr.pos,
                                    "Returns not allowed outside of function blocks".to_string(),
                                );
                            }
                        } else {
                            return_type = Some(analyze(sa, expr));
                        }
                    }
                }
                if e_error {
                    sa.make_err(
                        expr.pos,
                        "Only the last element in a block can be an expression".to_string(),
                    );
                }
                idx += 1;
            }
            sa.pop_scope(ast.pos);
            if let Some(r) = return_type {
                r
            } else {
                NIL_TYPE_SIGNATURE.clone()
            }
        }
        ast::Ast::IfElse(ref mut ifcond, ref mut ifexpr, ref mut elseifs, ref mut elseexpr) => {
            sa.new_scope();
            let ifcond_type = analyze(sa, &mut **ifcond);
            if ifcond_type != BOOL_TYPE_SIGNATURE {
                sa.make_err(
                    ifcond.pos,
                    format!("If condition must evaluate to Bool; got {:?}", ifcond_type),
                );
            }
            let expr_type = analyze(sa, &mut **ifexpr);
            if expr_type != NIL_TYPE_SIGNATURE && elseexpr.is_none() {
                sa.make_err(
                    ifcond.pos,
                    "If condition must have an else branch if a value is returned".to_string(),
                );
            }
            sa.pop_scope(ifexpr.pos);
            for (eifc, eife) in elseifs {
                sa.new_scope();
                let eifc_type = analyze(sa, &mut **eifc);
                if eifc_type != BOOL_TYPE_SIGNATURE {
                    sa.make_err(
                        eifc.pos,
                        format!(
                            "Else if condition must evaluate to Bool; got {:?}",
                            eifc_type
                        ),
                    );
                }
                let branch_type = analyze(sa, &mut **eife);
                if branch_type != expr_type {
                    sa.make_err(eife.pos, format!("If branch doesn't have the same return type; expected {:?} but got {:?}", expr_type, branch_type));
                }
                sa.pop_scope(eife.pos);
            }
            if let Some(eexpr) = elseexpr {
                let branch_type = analyze(sa, &mut **eexpr);
                if branch_type != expr_type {
                    sa.make_err(eexpr.pos, format!("If branch doesn't have the same return type; expected {:?} but got {:?}", expr_type, branch_type));
                }
            }
            expr_type
        }
        ast::Ast::While(ref mut cond, ref mut expr) => {
            sa.new_scope();
            let cond_type = analyze(sa, &mut **cond);
            if cond_type != BOOL_TYPE_SIGNATURE {
                sa.make_err(
                    cond.pos,
                    format!("While condition must evaluate to Bool; got {:?}", cond_type),
                );
            }
            let return_type = analyze(sa, &mut **expr);
            sa.pop_scope(ast.pos);
            return_type
        }
        ast::Ast::VarDecl(ref name, ref mut sig, ref mut expr) => {
            if sa.check_if_var_in_scopes(&name).is_none() {
                let mut return_type = NIL_TYPE_SIGNATURE.clone();
                if let Some(type_sig) = &sig.type_sig {
                    if let Some(e) = expr {
                        if let ast::Ast::FnDef(sig, _, _) = &e.node {
                            sa.current_fn =
                                (name.clone(), ast::TypeSignature::Function(sig.clone()));
                        }
                        let e_type = analyze(sa, &mut *e);
                        if *type_sig != e_type {
                            sa.make_err(ast.pos, format!("Variable type and assign type do not match; expected {:?} but got {:?}", *type_sig, e_type));
                        }
                    }
                    sa.last_scope()
                        .variables
                        .insert(name.clone(), (sig.mutable, type_sig.clone()));
                    return_type = type_sig.clone();
                } else if let Some(e) = expr {
                    if let ast::Ast::FnDef(sig, _, _) = &e.node {
                        sa.current_fn = (name.clone(), ast::TypeSignature::Function(sig.clone()));
                    }
                    let expr_type = analyze(sa, &mut *e);
                    // println!("Variable {} of type {:?}, infer", sig.name, expr_type);
                    sig.type_sig = Some(expr_type.clone());
                    return_type = expr_type.clone();
                    sa.last_scope()
                        .variables
                        .insert(name.clone(), (sig.mutable, expr_type));
                } else {
                    sa.make_err(
                        ast.pos,
                        "Cannot infer type without an assign expression".to_string(),
                    );
                    sa.last_scope()
                        .variables
                        .insert(name.clone(), (sig.mutable, NIL_TYPE_SIGNATURE.clone()));
                }
                sa.current_fn = (String::new(), NIL_TYPE_SIGNATURE.clone());
                return_type
            } else {
                sa.current_fn = (String::new(), NIL_TYPE_SIGNATURE.clone());
                sa.make_err(ast.pos, format!("Variable {} already defined", name));
                NIL_TYPE_SIGNATURE.clone()
            }
        }
        ast::Ast::Import(ref mut module) => {
            let old_file = sa.file.clone();
            sa.file = module.file.clone();
            let r = analyze_module(sa, &mut *module);
            sa.file = old_file;
            r
        }
        ast::Ast::FnDef(ref mut sig, ref param_names, ref mut expr) => {
            sa.new_scope();
            for (var, name) in sig.params.iter().zip(param_names.iter()) {
                if let Some(type_sig) = &var.type_sig {
                    if sa.check_if_type_is_defined(type_sig).is_none() {
                        sa.make_err(ast.pos, format!("Type {:?} is not defined", type_sig));
                    }
                    sa.last_scope()
                        .variables
                        .insert(name.clone(), (var.mutable, type_sig.clone()));
                } else {
                    sa.make_err(
                        ast.pos,
                        format!(
                            "Function parameter types cannot be infered; Parameter: {}",
                            name
                        ),
                    );
                    sa.last_scope()
                        .variables
                        .insert(name.clone(), (var.mutable, NIL_TYPE_SIGNATURE.clone()));
                }
            }
            if let Some(type_sig) = &sig.return_type {
                if sa.check_if_type_is_defined(type_sig).is_none() {
                    sa.make_err(ast.pos, format!("Type {:?} is not defined", type_sig));
                }
                if let ast::Ast::Block(_) = &expr.node {
                    sa.in_function_block = true;
                }
                let expr_type = analyze(sa, &mut **expr);
                sa.in_function_block = false;
                if **type_sig != expr_type {
                    sa.make_err(
                        expr.pos,
                        format!(
                            "Return types not the same; expected {:?} but got {:?}",
                            **type_sig, expr_type
                        ),
                    );
                }
            } else {
                sig.return_type = Some(Box::new(analyze(sa, &mut **expr)));
            }
            sa.pop_scope(ast.pos);
            ast::TypeSignature::Function(sig.clone())
        }
        ast::Ast::FnExtern(ref sig, ref _name) => ast::TypeSignature::Function(sig.clone()),
        ast::Ast::FnCall(ref mut callee, ref mut args) => {
            let mut return_type: ast::TypeSignature = NIL_TYPE_SIGNATURE.clone();
            let callee_type = analyze(sa, &mut **callee);
            if let ast::TypeSignature::Function(sig) = callee_type {
                if sig.params.len() == args.len() {
                    for (param, arg) in sig.params.iter().zip(args.iter_mut()) {
                        if let Some(type_sig) = &param.type_sig {
                            let arg_return_type = analyze(sa, arg);
                            if *type_sig != arg_return_type {
                                sa.make_err(
                                    arg.pos,
                                    format!(
                                        "Expected type {:?} but got type {:?}",
                                        type_sig, arg_return_type
                                    ),
                                );
                            }
                        } else {
                            unreachable!();
                        }
                    }
                    return_type = *sig.return_type.unwrap();
                } else {
                    sa.make_err(
                        callee.pos,
                        format!(
                            "Function expected {} arguments but got {} arguments",
                            sig.params.len(),
                            args.len()
                        ),
                    );
                    return_type = NIL_TYPE_SIGNATURE.clone();
                }
            } else {
                sa.make_err(callee.pos, "Not a callable expression".to_string());
            }
            return_type
        }
        ast::Ast::As(ref mut expr, ref type_) => {
            let expr_type = analyze(sa, &mut **expr);
            if sa.check_if_type_is_defined(&type_).is_none() {
                sa.make_err(ast.pos, format!("Type {:?} is not defined", type_));
            }
            match (&expr_type, &type_) {
                (ast::TypeSignature::Primitive(_), ast::TypeSignature::Primitive(_)) => {
                    type_.clone()
                }
                (_, ast::TypeSignature::Primitive(_)) => {
                    sa.make_err(
                        expr.pos,
                        format!(
                            "Can only cast between primtive types; Left of as is of type {:?}",
                            expr_type
                        ),
                    );
                    expr_type
                }
                (ast::TypeSignature::Primitive(_), _) => {
                    sa.make_err(
                        ast.pos,
                        format!(
                            "Can only cast between primtive types; Right of as is of type {:?}",
                            type_
                        ),
                    );
                    expr_type
                }
                _ => {
                    sa.make_err(ast.pos, format!("Can only cast between primtive types; {:?} and {:?} are not primitives", expr_type, type_));
                    expr_type
                }
            }
        }
    };
    ast.type_sig = Some(node_type.clone());
    node_type
}
