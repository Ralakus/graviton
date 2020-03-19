pub extern crate graviton_backend as backend;
pub extern crate graviton_core as core;
pub extern crate graviton_frontend as frontend;

fn main() {
    let source = "let a = 14 + 48;\n\
                   fn add(x: I32, y: I32) -> I32 { x + y }";

    let mut ast = core::ast::Ast::new();

    ast.push(
        core::ast::Node::Module {
            name: 1,
            declarations: core::ast::FinishVec::<usize> {
                v: vec![2, 8],
                finished: true,
            },
        },
        core::signature::TypeSignature::Struct(core::signature::StructSignature {
            fields: vec![(
                false,
                core::signature::TypeSignature::Function(core::signature::FunctionSignature {
                    parameters: vec![
                        (
                            false,
                            core::signature::TypeSignature::Primitive(
                                core::signature::PrimitiveType::SignedInteger { bitsize: 32 },
                            ),
                        ),
                        (
                            false,
                            core::signature::TypeSignature::Primitive(
                                core::signature::PrimitiveType::SignedInteger { bitsize: 32 },
                            ),
                        ),
                    ],
                    return_type_signature: Box::new(core::signature::TypeSignature::Primitive(
                        core::signature::PrimitiveType::SignedInteger { bitsize: 32 },
                    )),
                }),
            )],
        }),
        core::Position::new(0, 0),
    );

    ast.push(
        core::ast::Node::File {
            name: "main.grav".to_string(),
        },
        core::signature::TypeSignature::Primitive(core::signature::PrimitiveType::Nil),
        core::Position::new(0, 0),
    );

    ast.push(
        core::ast::Node::Let {
            signature: 3,
            name: 4,
            assign: 5,
            mutable: false,
        },
        core::signature::TypeSignature::Primitive(core::signature::PrimitiveType::Nil),
        core::Position::new(1, 1),
    );

    ast.push(
        core::ast::Node::TypeSignature {
            signature: core::signature::TypeSignature::Primitive(
                core::signature::PrimitiveType::SignedInteger { bitsize: 32 },
            ),
        },
        core::signature::TypeSignature::Primitive(core::signature::PrimitiveType::Nil),
        core::Position::new(1, 7),
    );

    ast.push(
        core::ast::Node::IdentifierData {
            name: "a".to_string(),
        },
        core::signature::TypeSignature::Primitive(core::signature::PrimitiveType::Nil),
        core::Position::new(1, 5),
    );

    ast.push(
        core::ast::Node::Binary {
            op: core::ast::BinaryOperation::Add,
            left: 6,
            right: 7,
        },
        core::signature::TypeSignature::Primitive(core::signature::PrimitiveType::SignedInteger {
            bitsize: 32,
        }),
        core::Position::new(1, 12),
    );

    ast.push(
        core::ast::Node::Integer { value: 14 },
        core::signature::TypeSignature::Primitive(core::signature::PrimitiveType::SignedInteger {
            bitsize: 32,
        }),
        core::Position::new(1, 9),
    );

    ast.push(
        core::ast::Node::Integer { value: 48 },
        core::signature::TypeSignature::Primitive(core::signature::PrimitiveType::SignedInteger {
            bitsize: 32,
        }),
        core::Position::new(1, 14),
    );

    ast.push(
        core::ast::Node::Function {
            signature: 9,
            name: 10,
            parameter_names: vec![11, 12],
            body: 13,
        },
        core::signature::TypeSignature::Function(core::signature::FunctionSignature {
            parameters: vec![
                (
                    false,
                    core::signature::TypeSignature::Primitive(
                        core::signature::PrimitiveType::SignedInteger { bitsize: 32 },
                    ),
                ),
                (
                    false,
                    core::signature::TypeSignature::Primitive(
                        core::signature::PrimitiveType::SignedInteger { bitsize: 32 },
                    ),
                ),
            ],
            return_type_signature: Box::new(core::signature::TypeSignature::Primitive(
                core::signature::PrimitiveType::SignedInteger { bitsize: 32 },
            )),
        }),
        core::Position::new(2, 1),
    );

    ast.push(
        core::ast::Node::FunctionSignature {
            signature: core::signature::FunctionSignature {
                parameters: vec![
                    (
                        false,
                        core::signature::TypeSignature::Primitive(
                            core::signature::PrimitiveType::SignedInteger { bitsize: 32 },
                        ),
                    ),
                    (
                        false,
                        core::signature::TypeSignature::Primitive(
                            core::signature::PrimitiveType::SignedInteger { bitsize: 32 },
                        ),
                    ),
                ],
                return_type_signature: Box::new(core::signature::TypeSignature::Primitive(
                    core::signature::PrimitiveType::SignedInteger { bitsize: 32 },
                )),
            },
        },
        core::signature::TypeSignature::Primitive(core::signature::PrimitiveType::Nil),
        core::Position::new(2, 7),
    );

    ast.push(
        core::ast::Node::IdentifierData {
            name: "add".to_string(),
        },
        core::signature::TypeSignature::Primitive(core::signature::PrimitiveType::Nil),
        core::Position::new(2, 4),
    );

    ast.push(
        core::ast::Node::Parameter {
            name: "x".to_string(),
        },
        core::signature::TypeSignature::Primitive(core::signature::PrimitiveType::Nil),
        core::Position::new(2, 8),
    );

    ast.push(
        core::ast::Node::Parameter {
            name: "y".to_string(),
        },
        core::signature::TypeSignature::Primitive(core::signature::PrimitiveType::Nil),
        core::Position::new(2, 16),
    );

    ast.push(
        core::ast::Node::Block {
            statements: core::ast::FinishVec::<usize> {
                v: vec![],
                finished: true,
            },
            end_expression: 14,
        },
        core::signature::TypeSignature::Primitive(core::signature::PrimitiveType::SignedInteger {
            bitsize: 32,
        }),
        core::Position::new(1, 31),
    );

    ast.push(
        core::ast::Node::Binary {
            op: core::ast::BinaryOperation::Add,
            left: 15,
            right: 16,
        },
        core::signature::TypeSignature::Primitive(core::signature::PrimitiveType::SignedInteger {
            bitsize: 32,
        }),
        core::Position::new(2, 35),
    );

    ast.push(
        core::ast::Node::Identifier {
            name: "x".to_string(),
        },
        core::signature::TypeSignature::Primitive(core::signature::PrimitiveType::SignedInteger {
            bitsize: 32,
        }),
        core::Position::new(2, 33),
    );

    ast.push(
        core::ast::Node::Identifier {
            name: "y".to_string(),
        },
        core::signature::TypeSignature::Primitive(core::signature::PrimitiveType::SignedInteger {
            bitsize: 32,
        }),
        core::Position::new(2, 37),
    );

    println!("Source:\n\n{}\n\n{}", source, ast);
}
