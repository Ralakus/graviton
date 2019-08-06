use super::ast;

use cranelift::frontend::*;
use cranelift::prelude::*;
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_faerie::{FaerieBackend, FaerieBuilder, FaerieTrapCollection};
use std::error::Error;

#[derive(Debug, Clone)]
pub struct NativeError {
   msg: String,
   pos: ast::Position 
}

pub struct Native {
    context: codegen::Context,
    module: Module<FaerieBackend>,
    builder_ctx: FunctionBuilderContext,
    errors: Vec<NativeError>
}

impl Native {
    pub fn compile(name: String, ast: &ast::AstNode) -> Result<cranelift_faerie::FaerieProduct, Vec<NativeError>> {
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
            errors: Vec::new()
        };

        ntv.context.func.signature.returns.push(AbiParam::new(types::I32));

        let mut builder = FunctionBuilder::new(&mut ntv.context.func, &mut ntv.builder_ctx);

        let main_ebb = builder.create_ebb();
        builder.append_ebb_params_for_function_params(main_ebb);

        builder.switch_to_block(main_ebb);

        ast_to_cranelift(ast);

        builder.seal_block(main_ebb);

        let tmp = builder.ins().iconst(types::I32, 14);
        builder.ins().return_(&[tmp]);

        builder.finalize();

        let main_id = match ntv.module.declare_function("main", Linkage::Export, &ntv.context.func.signature) {
                Ok(id) => id,
                Err(e) => {
                    ntv.errors.push(NativeError { msg: format!("{:?}", e), pos: ast::Position { line: -1, col: -1 } });
                    return Err(ntv.errors);
                }
            };

        match ntv.module.define_function(main_id, &mut ntv.context) {
            Ok(_) => {},
            Err(e) => {
                ntv.errors.push(NativeError { msg: format!("{:?}", e), pos: ast::Position { line: -1, col: -1 } });
                return Err(ntv.errors);
            }
        }

        println!("{}", ntv.context.func.display(None));

        let result = ntv.module.finish();

        if ntv.errors.len() > 0 {
            Err(ntv.errors)
        } else {
            Ok(result)
        }

    }
}

fn ast_to_cranelift(ast: &ast::AstNode) {
    match &ast.node {
        ast::Ast::Identifier(ident) => {}
        ast::Ast::Number(n) => {}
        ast::Ast::String(s) => {}
        ast::Ast::Bool(b) => {}
        ast::Ast::Statement(expr) => {}
        ast::Ast::Binary(op, l, r) => {}
        ast::Ast::Unary(op, expr) => {}
        ast::Ast::Return(expr) => {}
        ast::Ast::Block(exprs) => {}
        ast::Ast::IfElse(ifcond, ifexpr, elseifs, elseexpr) => {}
        ast::Ast::While(cond, expr) => {}
        ast::Ast::Let(name, var_sig, set_expr) => {}
        ast::Ast::Import(_import_name, expr) => {}
        ast::Ast::FnDef(_sig, _param_names, _return_type) => {}
        ast::Ast::FnCall(_callee, _args) => {}
        ast::Ast::As(castee, cast_type) => {}
    }
}
