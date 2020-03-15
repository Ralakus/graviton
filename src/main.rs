pub extern crate graviton_backend as backend;
pub extern crate graviton_core as core;
pub extern crate graviton_frontend as frontend;

fn main() {
    let source = "let a = 14 + 48;\n\
                   fn add(x: I32, y: I32) -> I32 { x + y }";

    let mut interface = core::interface::Interface::new();

    interface.ast.push(
        core::ast::Node::Module {
            name: 1,
            declarations: core::ast::FinishVec::<usize> {
                v: vec![2, 8],
                finished: true,
            },
        },
        core::ast::TypeSignature::Struct(core::ast::StructSignature {
            fields: vec![(
                false,
                core::ast::TypeSignature::Function(core::ast::FunctionSignature {
                    parameters: vec![
                        (
                            false,
                            core::ast::TypeSignature::Primitive(
                                core::ast::PrimitiveType::SignedInteger { bitsize: 32 },
                            ),
                        ),
                        (
                            false,
                            core::ast::TypeSignature::Primitive(
                                core::ast::PrimitiveType::SignedInteger { bitsize: 32 },
                            ),
                        ),
                    ],
                    return_type_signature: Box::new(core::ast::TypeSignature::Primitive(
                        core::ast::PrimitiveType::SignedInteger { bitsize: 32 },
                    )),
                }),
            )],
        }),
        core::Position::new(0, 0),
    );

    interface.ast.push(
        core::ast::Node::File {
            name: "main.grav".to_string(),
        },
        core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::Nil),
        core::Position::new(0, 0),
    );

    interface.ast.push(
        core::ast::Node::Let {
            signature: 3,
            name: 4,
            assign: 5,
            mutable: false,
        },
        core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::Nil),
        core::Position::new(1, 1),
    );

    interface.ast.push(
        core::ast::Node::TypeSignature {
            signature: core::ast::TypeSignature::Primitive(
                core::ast::PrimitiveType::SignedInteger { bitsize: 32 },
            ),
        },
        core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::Nil),
        core::Position::new(1, 7),
    );

    interface.ast.push(
        core::ast::Node::IdentifierData {
            name: "a".to_string(),
        },
        core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::Nil),
        core::Position::new(1, 5),
    );

    interface.ast.push(
        core::ast::Node::Binary {
            op: core::ast::BinaryOperation::Add,
            left: 6,
            right: 7,
        },
        core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::SignedInteger {
            bitsize: 32,
        }),
        core::Position::new(1, 12),
    );

    interface.ast.push(
        core::ast::Node::Integer { value: 14 },
        core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::SignedInteger {
            bitsize: 32,
        }),
        core::Position::new(1, 9),
    );

    interface.ast.push(
        core::ast::Node::Integer { value: 48 },
        core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::SignedInteger {
            bitsize: 32,
        }),
        core::Position::new(1, 14),
    );

    interface.ast.push(
        core::ast::Node::Function {
            signature: 9,
            name: 10,
            parameter_names: vec![11, 12],
            body: 13,
        },
        core::ast::TypeSignature::Function(core::ast::FunctionSignature {
            parameters: vec![
                (
                    false,
                    core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::SignedInteger {
                        bitsize: 32,
                    }),
                ),
                (
                    false,
                    core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::SignedInteger {
                        bitsize: 32,
                    }),
                ),
            ],
            return_type_signature: Box::new(core::ast::TypeSignature::Primitive(
                core::ast::PrimitiveType::SignedInteger { bitsize: 32 },
            )),
        }),
        core::Position::new(2, 1),
    );

    interface.ast.push(
        core::ast::Node::FunctionSignature {
            signature: core::ast::FunctionSignature {
                parameters: vec![
                    (
                        false,
                        core::ast::TypeSignature::Primitive(
                            core::ast::PrimitiveType::SignedInteger { bitsize: 32 },
                        ),
                    ),
                    (
                        false,
                        core::ast::TypeSignature::Primitive(
                            core::ast::PrimitiveType::SignedInteger { bitsize: 32 },
                        ),
                    ),
                ],
                return_type_signature: Box::new(core::ast::TypeSignature::Primitive(
                    core::ast::PrimitiveType::SignedInteger { bitsize: 32 },
                )),
            },
        },
        core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::Nil),
        core::Position::new(2, 7),
    );

    interface.ast.push(
        core::ast::Node::IdentifierData {
            name: "add".to_string(),
        },
        core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::Nil),
        core::Position::new(2, 4),
    );

    interface.ast.push(
        core::ast::Node::Parameter {
            name: "x".to_string(),
        },
        core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::Nil),
        core::Position::new(2, 8),
    );

    interface.ast.push(
        core::ast::Node::Parameter {
            name: "y".to_string(),
        },
        core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::Nil),
        core::Position::new(2, 16),
    );

    interface.ast.push(
        core::ast::Node::Block {
            statements: core::ast::FinishVec::<usize> {
                v: vec![],
                finished: true,
            },
            end_expression: 14,
        },
        core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::SignedInteger {
            bitsize: 32,
        }),
        core::Position::new(1, 31),
    );

    interface.ast.push(
        core::ast::Node::Binary {
            op: core::ast::BinaryOperation::Add,
            left: 15,
            right: 16,
        },
        core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::SignedInteger {
            bitsize: 32,
        }),
        core::Position::new(2, 35),
    );

    interface.ast.push(
        core::ast::Node::Identifier {
            name: "x".to_string(),
        },
        core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::SignedInteger {
            bitsize: 32,
        }),
        core::Position::new(2, 33),
    );

    interface.ast.push(
        core::ast::Node::Identifier {
            name: "y".to_string(),
        },
        core::ast::TypeSignature::Primitive(core::ast::PrimitiveType::SignedInteger {
            bitsize: 32,
        }),
        core::Position::new(2, 37),
    );

    println!("Source:\n\n{}\n\n{}", source, interface.ast);
}
