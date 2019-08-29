use super::ast;
use super::ir;

#[derive(Debug, Clone)]
pub struct IRError {
    pub msg: String,
    pub pos: ast::Position,
    pub file: Option<String>,
}

pub struct IRTranslator {
    file: Option<String>,
    errors: Vec<IRError>,
}

impl IRTranslator {
    fn make_error(&mut self, pos: &ast::Position, msg: String) {
        self.errors.push(IRError {
            msg,
            pos: pos.clone(),
            file: self.file.clone(),
        });
    }

    fn module_to_ir(&self, module: &ast::Module) -> Result<ir::Module, ()> {
        let mut irm = ir::Module {
            functions: Vec::new(),
            globals: Vec::new(),
        };

        let module_fn = irm.new_function(Vec::new(), &module.type_sig.as_ref().unwrap_or(&ast::TypeSignature::Primitive(ast::PrimitiveType::Nil)));
        let main_block = module_fn.new_block();

        for expr in &module.expressions {
            match &expr.node {
                ast::Ast::Let(name, sig, value) => {
                    if let Some(val) = value {
                        match &val.node {
                            ast::Ast::FnDef(sig, params, body) => {

                            }
                            _ => {}
                        }
                    } else {

                    }
                }
                _ => {}
            }
        }

        Ok(irm)
    }

    pub fn translate_module(module: &ast::Module) -> Result<ir::Module, Vec<IRError>> {
        let irt = IRTranslator {
            file: module.file.clone(),
            errors: Vec::new(),
        };

        if let Ok(module) = irt.module_to_ir(module) {
            Ok(module)
        } else {
            Err(irt.errors)
        }
    }
}
