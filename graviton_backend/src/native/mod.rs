use super::ast;

use cranelift::codegen::ir::Value;
use cranelift::frontend::*;
use cranelift::prelude::*;
use cranelift_faerie::{FaerieBackend, FaerieBuilder, FaerieTrapCollection};
use cranelift_module::{DataContext, Linkage, Module};
use std::collections::HashMap;
use std::error::Error;

pub mod stdlib;
pub mod gravtypes;

#[derive(Debug, Clone)]
pub struct NativeError {
    pub msg: String,
    pub pos: ast::Position,
    pub file: Option<String>,
}

pub struct NativeObject {
    artifact: cranelift_faerie::FaerieProduct,
}

impl NativeObject {
    pub fn write_file(&self, filename: &String) -> Result<(), NativeError> {
        let file = match std::fs::File::create(filename) {
            Ok(f) => f,
            Err(e) => {
                return Err(NativeError {
                    msg: String::from(e.description()),
                    pos: ast::Position { line: -1, col: -1 },
                    file: None,
                })
            }
        };
        match self.artifact.artifact.write(file) {
            Ok(_) => {}
            Err(e) => {
                return Err(NativeError {
                    msg: format!("{}", e),
                    pos: ast::Position { line: -1, col: -1 },
                    file: None,
                })
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct Scope {
    variables: HashMap<String, Variable>,
}

pub struct Native {
    context: codegen::Context,
    module: Module<FaerieBackend>,
    builder_ctx: FunctionBuilderContext,
    data_ctx: DataContext,
    scopes: Vec<Scope>,
    errors: Vec<NativeError>,
}

impl Native {
    pub fn compile(
        name: String,
        ast: &ast::AstNode,
        debug_level: i32,
    ) -> Result<NativeObject, Vec<NativeError>> {
        let mut flag_builder = settings::builder();
        flag_builder.enable("is_pic").unwrap();

        let isa_builder = cranelift_native::builder().unwrap();
        let isa = isa_builder.finish(settings::Flags::new(flag_builder));

        let backend_builder = FaerieBuilder::new(
            isa,
            name,
            FaerieTrapCollection::Disabled,
            cranelift_module::default_libcall_names(),
        )
        .unwrap();

        let module: Module<FaerieBackend> = Module::new(backend_builder);

        let mut ntv = Native {
            context: module.make_context(),
            module,
            builder_ctx: FunctionBuilderContext::new(),
            data_ctx: DataContext::new(),
            scopes: vec![Scope {
                variables: HashMap::new(),
            }],
            errors: Vec::new(),
        };

        ntv.context
            .func
            .signature
            .returns
            .push(AbiParam::new(types::I32));

        let mut builder = FunctionBuilder::new(&mut ntv.context.func, &mut ntv.builder_ctx);

        let main_ebb = builder.create_ebb();
        builder.append_ebb_params_for_function_params(main_ebb);

        builder.switch_to_block(main_ebb);
        builder.seal_block(main_ebb);

        let mut translator = AstTranslator {
            builder: &mut builder,
            scopes: &mut ntv.scopes,
            module: &mut ntv.module,
            data_ctx: &mut ntv.data_ctx,
            errors: &mut ntv.errors,
        };

        let tmp = match translator.ast_to_cranelift(ast) {
            Ok(v) => v,
            Err(e) => {
                translator.errors.push(e);
                translator.builder.ins().iconst(types::I32, 14)
            }
        };

        // let tmp = translator.builder.ins().iconst(types::I32, 0);
        translator.builder.ins().return_(&[tmp]);

        translator.builder.finalize();

        let main_id = match ntv.module.declare_function(
            "graviton_main",
            Linkage::Export,
            &ntv.context.func.signature,
        ) {
            Ok(id) => id,
            Err(e) => {
                ntv.errors.push(NativeError {
                    msg: format!("{}", e),
                    pos: ast::Position { line: -1, col: -1 },
                    file: None,
                });
                return Err(ntv.errors);
            }
        };

        match ntv.module.define_function(main_id, &mut ntv.context) {
            Ok(_) => {}
            Err(e) => {
                ntv.errors.push(NativeError {
                    msg: format!("{}", e),
                    pos: ast::Position { line: -1, col: -1 },
                    file: None,
                });
                return Err(ntv.errors);
            }
        }

        if debug_level >= 3 {
            println!("{}", ntv.context.func.display(None));
        }

        let result = ntv.module.finish();

        if ntv.errors.len() > 0 {
            Err(ntv.errors)
        } else {
            Ok(NativeObject { artifact: result })
        }
    }
}

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

fn hash<T: Hash>(obj: T) -> u64 {
    let mut hasher = DefaultHasher::new();
    obj.hash(&mut hasher);
    hasher.finish()
}

struct AstTranslator<'a> {
    builder: &'a mut FunctionBuilder<'a>,
    scopes: &'a mut Vec<Scope>,
    module: &'a mut Module<FaerieBackend>,
    data_ctx: &'a mut DataContext,
    errors: &'a mut Vec<NativeError>,
}

impl<'a> AstTranslator<'a> {
    fn make_error(&mut self, pos: &ast::Position, msg: String) -> NativeError {
        let ne = NativeError {
            msg,
            pos: pos.clone(),
            file: None,
        };
        self.errors.push(ne.clone());
        ne
    }

    fn check_if_var_in_scopes(scopes: &'a Vec<Scope>, var: &String) -> Option<&'a Variable> {
        for s in scopes.iter().rev() {
            match s.variables.get(var) {
                Some(ts) => return Some(ts),
                None => continue,
            }
        }
        None
    }

    /*fn new_scope(&mut self) {
        self.scopes.push(Scope {
            variables: HashMap::new(),
        });
    }

    fn pop_scope(&mut self, pos: &ast::Position) {
        if self.scopes.len() == 1 {
            self.make_error(pos, format!("Cannot pop global scope"));
        } else {
            self.scopes.pop();
        }
    }*/

    fn last_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn make_data(
        &mut self,
        name: &String,
        contents: Vec<u8>,
        writeable: bool,
    ) -> Result<cranelift_module::DataId, NativeError> {
        self.data_ctx.define(contents.into_boxed_slice());
        let id = match self
            .module
            .declare_data(name, Linkage::Export, writeable, None)
        {
            Ok(id) => id,
            Err(e) => {
                return Err(
                    self.make_error(&ast::Position { line: -1, col: -1 }, format!("{:?}", e))
                )
            }
        };
        match self.module.define_data(id, &self.data_ctx) {
            Ok(_) => {}
            Err(e) => {
                return Err(
                    self.make_error(&ast::Position { line: -1, col: -1 }, format!("{:?}", e))
                )
            }
        };
        self.data_ctx.clear();
        self.module.finalize_definitions();
        Ok(id)
    }
    fn ast_to_cranelift(&mut self, ast: &ast::AstNode) -> Result<Value, NativeError> {
        match &ast.node {
            ast::Ast::Identifier(ident) => {
                let var = match AstTranslator::check_if_var_in_scopes(self.scopes, ident) {
                    Some(var) => var,
                    None => {
                        return Err(self.make_error(
                            &ast.pos,
                            format!("Variable {} not defined in scope", ident),
                        ))
                    }
                };
                Ok(self.builder.use_var(*var))
            }
            ast::Ast::Number(n) => Ok(self.builder.ins().iconst(types::I32, *n as i64)),
            ast::Ast::String(s) => {
                let id = self.make_data(
                    &format!("s{}", hash(ast)),
                    format!("{}\0", s).as_bytes().to_vec(),
                    false,
                )?;
                let local_id = self.module.declare_data_in_func(id, &mut self.builder.func);
                let type_ = gravtypes::type_to_cranelift(&ast.type_sig, &self.module);
                Ok(self.builder.ins().symbol_value(type_, local_id))
            }
            ast::Ast::Bool(b) => Ok(self.builder.ins().bconst(types::B1, *b)),
            ast::Ast::Statement(expr) => {
                self.ast_to_cranelift(expr)?;
                Ok(self.builder.ins().iconst(types::I32, 0))
            }
            ast::Ast::Binary(op, l, r) => {
                let lval = self.ast_to_cranelift(&*l)?;
                let rval = self.ast_to_cranelift(&*r)?;
                match op {
                    ast::BinaryOperation::Add => {
                        Ok(self.builder.ins().iadd(lval, rval))
                    }
                    ast::BinaryOperation::Subtract => { 
                        Ok(self.builder.ins().isub(lval, rval)) 
                    }
                    ast::BinaryOperation::Multiply => { 
                        Ok(self.builder.ins().imul(lval, rval)) 
                    }
                    ast::BinaryOperation::Divide => { 
                        if ast.type_sig.as_ref().unwrap().is_unsigned() {
                            Ok(self.builder.ins().udiv(lval, rval))
                        } else {
                            Ok(self.builder.ins().sdiv(lval, rval))
                        }
                    }

                    ast::BinaryOperation::Less => {
                        Ok(if ast.type_sig.as_ref().unwrap().is_unsigned() {
                            self.builder.ins().icmp(IntCC::UnsignedLessThan, lval, rval)
                        } else {
                            self.builder.ins().icmp(IntCC::SignedLessThan, lval, rval)
                        })
                    }
                    ast::BinaryOperation::LessEqual => {
                        Ok(if ast.type_sig.as_ref().unwrap().is_unsigned() {
                            self.builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, lval, rval)
                        } else {
                            self.builder.ins().icmp(IntCC::SignedLessThanOrEqual, lval, rval)
                        })
                    }
                    ast::BinaryOperation::Greater => {
                        Ok(if ast.type_sig.as_ref().unwrap().is_unsigned() {
                            self.builder.ins().icmp(IntCC::UnsignedGreaterThan, lval, rval)
                        } else {
                            self.builder.ins().icmp(IntCC::SignedGreaterThan, lval, rval)
                        })
                    }
                    ast::BinaryOperation::GreaterEqual => {
                        Ok(if ast.type_sig.as_ref().unwrap().is_unsigned() {
                            self.builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, lval, rval)
                        } else {
                            self.builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, lval, rval)
                        })
                    }
                    ast::BinaryOperation::Equal => {
                        Ok(self.builder.ins().icmp(IntCC::Equal, lval, rval))
                    }
                    ast::BinaryOperation::NotEqual => {
                        Ok(self.builder.ins().icmp(IntCC::NotEqual, lval, rval))
                    }

                    ast::BinaryOperation::And => Ok(self.builder.ins().band(lval, rval)),
                    ast::BinaryOperation::Or => Ok(self.builder.ins().bor(lval, rval)),

                    ast::BinaryOperation::Assign => {
                        if let ast::Ast::Identifier(ident) = &l.node {
                            let var =
                                match AstTranslator::check_if_var_in_scopes(self.scopes, ident) {
                                    Some(var) => var,
                                    None => {
                                        return Err(self.make_error(
                                            &ast.pos,
                                            format!("Variable {} not defined in scope", ident),
                                        ))
                                    }
                                };
                            self.builder.def_var(*var, rval);
                            Ok(self.builder.use_var(*var))
                        } else {
                            Err(self.make_error(&ast.pos, format!("Not implemented")))
                        }
                    }
                }
            }
            ast::Ast::Unary(op, expr) => {
                let e = self.ast_to_cranelift(expr)?;
                match op {
                    ast::UnaryOperation::Not => Ok(self.builder.ins().bnot(e)),
                    ast::UnaryOperation::Negate => Ok(self.builder.ins().irsub_imm(e, 0)),
                }
            }
            ast::Ast::Return(_expr) => Err(self.make_error(&ast.pos, format!("Not implemented"))),
            ast::Ast::Block(exprs) => {
                let mut last_ins: Option<Value> = None;
                for e in exprs {
                    last_ins = Some(self.ast_to_cranelift(e)?);
                }
                if let Some(ast::TypeSignature::Primitive(ast::PrimitiveType::Nil)) = ast.type_sig {
                    Ok(self.builder.ins().iconst(types::I32, 0))
                } else {
                    if let Some(ins) = last_ins {
                        Ok(ins)
                    } else {
                        Ok(self.builder.ins().iconst(types::I32, 0))
                    }
                }
            }
            ast::Ast::IfElse(ifcond, ifexpr, elseifs, elseexpr) => {
                let exitebb = self.builder.create_ebb();
                self.builder.append_ebb_param(exitebb, gravtypes::type_to_cranelift(&ast.type_sig, &self.module));

                let mut elifebb = if elseifs.len() > 0 {
                    Some(self.builder.create_ebb())
                } else {
                    None
                };

                let elseebb = if let Some(_) = elseexpr {
                    Some(self.builder.create_ebb())
                } else {
                    None
                };

                let ifcondval = self.ast_to_cranelift(&*ifcond)?;

                if let Some(eiebb) = elifebb {
                    self.builder.ins().brz(ifcondval, eiebb, &[]);
                } else if let Some(eebb) = elseebb {
                    self.builder.ins().brz(ifcondval, eebb, &[]);
                } else {
                    let zero = self.builder.ins().iconst(gravtypes::type_to_cranelift(&ast.type_sig, &self.module), 0);
                    self.builder.ins().brz(ifcondval, exitebb, &[zero]);
                }

                let ifreturn = self.ast_to_cranelift(&*ifexpr)?;
                self.builder.ins().jump(exitebb, &[ifreturn]);

                let len = elseifs.len();
                let mut idx = 1;
                for (elifc, elife) in elseifs {
                    if let Some(ebb) = elifebb {
                        self.builder.switch_to_block(ebb);
                        self.builder.seal_block(ebb);

                        let elifcondval = self.ast_to_cranelift(&*elifc)?;
                        if idx < len {
                            let tebb = self.builder.create_ebb();
                            self.builder.ins().brz(elifcondval, tebb, &[]);
                            elifebb = Some(tebb);
                        } else if let Some(eebb) = elseebb {
                            self.builder.ins().brz(elifcondval, eebb, &[]);
                        } else {
                            let zero = self.builder.ins().iconst(gravtypes::type_to_cranelift(&ast.type_sig, &self.module), 0);
                            self.builder.ins().brz(elifcondval, exitebb, &[zero]);
                        }
                        let elifreturn = self.ast_to_cranelift(&*elife)?;
                        self.builder.ins().jump(exitebb, &[elifreturn]);
                    }

                    idx += 1;
                }

                if let (Some(eebb), Some(eexpr)) = (elseebb, elseexpr) {
                    self.builder.switch_to_block(eebb);
                    self.builder.seal_block(eebb);

                    let else_return = self.ast_to_cranelift(&*eexpr)?;
                    self.builder.ins().jump(exitebb, &[else_return]);
                }

                self.builder.switch_to_block(exitebb);
                self.builder.seal_block(exitebb);

                Ok(self.builder.ebb_params(exitebb)[0])
            }
            ast::Ast::While(cond, expr) => {
                let body_block = self.builder.create_ebb();
                let exit_block = self.builder.create_ebb();
                self.builder.ins().jump(body_block, &[]);
                self.builder.switch_to_block(body_block);

                let condition_value = self.ast_to_cranelift(&*cond)?;
                self.builder.ins().brz(condition_value, exit_block, &[]);

                self.ast_to_cranelift(&*expr)?;

                self.builder.ins().jump(body_block, &[]);

                self.builder.switch_to_block(exit_block);

                self.builder.seal_block(body_block);
                self.builder.seal_block(exit_block);

                Ok(self.builder.ins().iconst(types::I32, 0))
            }
            ast::Ast::Let(name, var_sig, set_expr) => {
                let var = Variable::new(self.last_scope().variables.len());

                self.builder.declare_var(var, gravtypes::type_to_cranelift(&var_sig.type_sig, &self.module));

                self.last_scope().variables.insert(name.clone(), var);

                let set = if let Some(expr) = set_expr {
                    self.ast_to_cranelift(expr)?
                } else {
                    self.builder.ins().iconst(gravtypes::type_to_cranelift(&var_sig.type_sig, &self.module), 0)
                };

                self.builder.def_var(var, set);

                Ok(self.builder.use_var(var))
            }
            ast::Ast::Import(_import_name, expr) => self.ast_to_cranelift(&*expr),
            ast::Ast::FnDef(_sig, _param_names, _return_type) => {
                Err(self.make_error(&ast.pos, format!("Not implemented")))
            }
            ast::Ast::FnCall(callee, args) => {
                if let ast::Ast::Identifier(name) = &callee.node {
                    let mut sig = self.module.make_signature();
                    for arg in args {
                        sig.params.push(AbiParam::new(gravtypes::type_to_cranelift(&arg.type_sig, &self.module)));
                    }

                    sig.returns.push(AbiParam::new(gravtypes::type_to_cranelift(&ast.type_sig, &self.module)));
                    
                    let id = self
                        .module
                        .declare_function(&name, Linkage::Import, &sig)
                        .unwrap();
                    let local_id = self.module.declare_func_in_func(id, &mut self.builder.func);

                    let mut arg_values = Vec::new();
                    for arg in args {
                        arg_values.push(self.ast_to_cranelift(&*arg)?);
                    }

                    let call = self.builder.ins().call(local_id, &arg_values);
                    Ok(self.builder.inst_results(call)[0])
                } else {
                    Err(self.make_error(&ast.pos, format!("Not implemented")))
                }
            }
            ast::Ast::As(castee, _cast_type) => {
                self.ast_to_cranelift(&*castee)
            }
        }
    }
}
