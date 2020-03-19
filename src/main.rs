pub extern crate graviton_backend as backend;
pub extern crate graviton_core as core;
pub extern crate graviton_frontend as frontend;

use core::{ir, signature};

fn main() {
    let source = "let a = 14 + 48;\n\
                  let add = (x: I32, y: I32) -> I32 { x + y }";

    let mut tir = ir::Module::new();

    tir.push(ir::Instruction::Module("main.grav".to_string()));

    tir.push(ir::Instruction::Let("a".to_string()));

    tir.push(ir::Instruction::Integer(14));
    tir.push(ir::Instruction::Integer(48));
    tir.push(ir::Instruction::Add);

    tir.push(ir::Instruction::LetEnd);
    tir.push(ir::Instruction::Statement);

    tir.push(ir::Instruction::Let("add".to_string()));

    let sig_idx = tir.push_signature(signature::TypeSignature::Function(
        core::signature::FunctionSignature {
            parameters: vec![
                (
                    false,
                    signature::TypeSignature::Primitive(signature::PrimitiveType::new("I32")),
                ),
                (
                    false,
                    signature::TypeSignature::Primitive(signature::PrimitiveType::new("I32")),
                ),
            ],
            return_type_signature: Box::new(core::signature::TypeSignature::Primitive(
                signature::PrimitiveType::new("I32"),
            )),
        },
    ));
    tir.push(ir::Instruction::Function(sig_idx));
    tir.push(ir::Instruction::FunctionParameter("x".to_string()));
    tir.push(ir::Instruction::FunctionParameter("y".to_string()));

    tir.push(ir::Instruction::Block);

    tir.push(ir::Instruction::Identifier("x".to_string()));
    tir.push(ir::Instruction::Identifier("y".to_string()));
    tir.push(ir::Instruction::Add);

    tir.push(ir::Instruction::BlockEnd);

    tir.push(ir::Instruction::FunctionEnd);

    tir.push(ir::Instruction::LetEnd);

    tir.push(ir::Instruction::ModuleEnd);

    println!("{}\n<Parser>\n{}\n<Backend>", source, tir);
}
