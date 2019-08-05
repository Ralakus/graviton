
use super::ast;

use cranelift::prelude::*;
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
use cranelift::frontend::*;

pub struct JitError {
    msg: String,
    pos: ast::Position,
}

pub struct Jit {
    builder_context: FunctionBuilderContext,

    ctx: codegen::Context,

    data_ctx: DataContext,

    module: Module<SimpleJITBackend>
}

impl Jit {

    pub fn new() -> Jit {
        if cfg!(windows) {
            unimplemented!();
        }

        let builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());
        let module = Module::new(builder);

        Jit {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module
        }
    }

    pub fn compile_ast(&mut self, ast: &ast::AstNode) -> Result<*const u8, JitError> {
        Err(JitError { msg: "Not implemented yet".to_string(), pos: ast.pos.clone() })
    }

}

struct FunctionTranslator<'a> {
    int: types::Type,
    builder: FunctionBuilder<'a>,
    module: &'a mut Module<SimpleJITBackend>,
}

impl<'a> FunctionTranslator<'a> {
    
}