
use super::ir;
use super::ast;

pub struct IRTranslator {

}

impl IRTranslator {
    fn module_to_ir(&self, ast: &ast::AstNode) -> ir::Module {
        ir::Module {
            functions: Vec::new(),
            globals: Vec::new(),
        }
    }

    fn new_block(&self) -> ir::BasicBlock {
        ir::BasicBlock {
            instructions: Vec::new(),
            arguments: Vec::new(),
        }
    }

    fn new_function(&self, arguments: Vec<ast::TypeSignature>) -> ir::Function {
        ir::Function {
            name: String::new(),
            blocks: Vec::new(),
            signature: ast::make_fn_sig!(() -> Nil),
            variables: Vec::new(),
        }
    }

    pub fn translate_module(ast: &ast::AstNode) -> ir::Module {
        let irt = IRTranslator {};
        irt.module_to_ir(ast)
    }
}