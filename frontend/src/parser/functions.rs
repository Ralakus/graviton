use crate::{
    ir::Instruction,
    parser::{rules, DeclarationType, LoopType, Parser},
    signature::{FunctionSignature, PrimitiveType, StructSignature, TypeSignature},
    {
        token::{TokenData, TokenType},
        NoticeLevel, Position,
    },
};

/// A funciton that should never be called that serves as a null pointer in the lookup table
pub(crate) fn nil_func<'a>(_p: &mut Parser<'a>) -> Result<(), ()> {
    unreachable!("Somehow `nil_func` was called, this shouldn't happen");
}

/// Parse a module
pub(crate) fn module<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    p.emit_ir_previous(TypeSignature::None, Instruction::Module(p.name.clone()));
    let mut was_error = false;
    while !p.check(TokenType::Eof) {
        if declaration_or_statement(p).is_err() {
            was_error = true;
        }
    }
    p.emit_ir_previous(TypeSignature::None, Instruction::ModuleEnd);
    if was_error {
        Err(())
    } else {
        Ok(())
    }
}

/// Checks to see if the token is a declaration or statement starter and parses
pub(crate) fn declaration_or_statement<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    match p.current().type_ {
        TokenType::KwModule => declaration(p),
        _ => statement(p),
    }
}

/// Parses a declaration
pub(crate) fn declaration<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    match p.current().type_ {
        TokenType::KwModule => {
            p.consume(
                TokenType::KwModule,
                "Expected keyword `module` for opening a module declaration",
            )?;

            let module_pos = p.current().pos;

            let mut segments = Vec::new();

            if let Ok(s) = p.consume(TokenType::Identifier, "Expected identifer ") {
                match s {
                    TokenData::Str(s) => segments.push(s.to_string()),
                    _ => {
                        p.emit_notice_previous(
                            NoticeLevel::Error,
                            "Failed to extract string data from string token".to_string(),
                        );
                        p.synchronize(&[]);
                        return Err(());
                    }
                }
            } else {
                return Err(());
            }

            while !p.check_consume(TokenType::Semicolon) {
                p.consume(TokenType::Dot, "Expected `.` or `;` after identifier")?;

                if let Ok(s) = p.consume(
                    TokenType::Identifier,
                    "Expected identifer for module statement",
                ) {
                    match s {
                        TokenData::Str(s) => segments.push(s.to_string()),
                        _ => {
                            p.emit_notice_previous(
                                NoticeLevel::Error,
                                "Failed to extract string data from string token".to_string(),
                            );
                            p.synchronize(&[]);
                            return Err(());
                        }
                    }
                } else {
                    return Err(());
                }
            }

            let module_name = if segments[0] == "std" {
                let environment = std::env::current_dir().unwrap();
                format!("{}/{}", environment.to_str().unwrap(), segments.join("/"))
            } else {
                segments.join("/")
            };

            if let Err(_) = p.parse_imported_module(format!("{}.grav", module_name)) {
                p.emit_notice(
                    module_pos,
                    NoticeLevel::Error,
                    format!("Failed to load module {}", segments.join(".")),
                );
                return Err(());
            }
        }
        _ => {
            p.emit_notice_current(NoticeLevel::Error, "Expected a declaration".to_string());
            p.synchronize(&[]);
            return Err(());
        }
    }
    Ok(())
}

/// Parses a statement
pub(crate) fn statement<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    match p.current().type_ {
        TokenType::KwLet => let_statement(p)?,
        _ => expression_statement(p)?,
    };
    Ok(())
}

/// Parses a let statement
pub(crate) fn let_statement<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    let mut was_error = p
        .consume(
            TokenType::KwLet,
            "Expected keyword `let` for opening a let statement",
        )
        .is_err();
    let mutable = p.check_consume(TokenType::KwMut);
    let mutable_keyword_pos = if mutable {
        p.previous().pos
    } else {
        Position::new(0, 0)
    };
    let name = match p.consume(
        TokenType::Identifier,
        "Expected identifier for let statement",
    )? {
        TokenData::Str(s) => (*s).to_string(),
        _ => {
            p.emit_notice_previous(
                NoticeLevel::Error,
                "Failed to extract string data from identifier token".to_string(),
            );
            p.synchronize(&[]);
            was_error = true;
            String::from("ERROR")
        }
    };

    let (signature, is_untyped) = if p.check_consume(TokenType::Colon) {
        (type_(p)?, false)
    } else {
        (TypeSignature::Untyped, true)
    };

    let is_assigned = if p.check_consume(TokenType::Equal) {
        if expression(p).is_err() {
            was_error = true;
        }
        true
    } else if is_untyped {
        p.emit_notice_previous(
            NoticeLevel::Error,
            "Let statement must have an explicit type if a value isn't assigned on declaration"
                .to_string(),
        );
        was_error = true;
        false
    } else {
        false
    };

    if p.consume(
        TokenType::Semicolon,
        "Expected closing `;` for let statement",
    )
    .is_err()
    {
        was_error = true;
    }

    if is_assigned {
        if p.last_declaration == DeclarationType::Function {
            p.emit_ir_previous(
                signature,
                if mutable {
                    Instruction::LetMutFunction(name)
                } else {
                    Instruction::LetFunction(name)
                },
            );
        } else if p.last_declaration == DeclarationType::Struct {
            if mutable {
                p.emit_notice(mutable_keyword_pos, NoticeLevel::Warning, "Let statement for structs cannot be mutable, defaulting to immuatble, please remove \"mut\"".to_string());
            }
            p.emit_ir_previous(signature, Instruction::LetStruct(name));
        } else {
            p.emit_ir_previous(
                signature,
                if mutable {
                    Instruction::LetMut(name)
                } else {
                    Instruction::Let(name)
                },
            );
        }
    } else {
        p.emit_ir_previous(
            signature,
            if mutable {
                Instruction::LetMutNoAssign(name)
            } else {
                Instruction::LetNoAssign(name)
            },
        );
    }

    if was_error {
        Err(())
    } else {
        Ok(())
    }
}

/// Parses an expression statement
pub(crate) fn expression_statement<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    expression(p)?;
    p.consume(TokenType::Semicolon, "Expected closing `;`")?;
    Ok(())
}

/// Parse an expression with a precedence greater than or equal to `precedence`
pub(crate) fn parse_precedence<'a>(p: &mut Parser<'a>, precedence: rules::Prec) -> Result<(), ()> {
    let prefix_rule = rules::get_rule(p.current().type_).prefix;
    if prefix_rule as usize == nil_func as usize {
        p.emit_notice_current(NoticeLevel::Error, "Expected prefix expression".to_string());
        p.synchronize(&[]);
        return Err(());
    }

    if prefix_rule(p).is_err() {
        p.synchronize(&[]);
        return Err(());
    }

    while precedence <= rules::get_rule(p.current().type_).precedence {
        let infix_rule = rules::get_rule(p.current().type_).infix;
        if infix_rule as usize == nil_func as usize {
            p.emit_notice_current(NoticeLevel::Error, "Expected infix expression".to_string());
            p.synchronize(&[]);
            return Err(());
        }

        if infix_rule(p).is_err() {
            p.synchronize(&[]);
            return Err(());
        }
    }

    Ok(())
}

/// Parse an expression with a precedence greater than Assignment
#[inline]
pub(crate) fn expression<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    parse_precedence(p, rules::Prec::Assignment)
}

/// Parse a literal expression
pub(crate) fn literal<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    p.advance();
    match (p.previous().type_, p.previous().data.clone()) {
        (TokenType::Number, TokenData::Integer(n)) => p.emit_ir_previous(
            TypeSignature::Primitive(PrimitiveType::new("I32")),
            Instruction::Integer(n),
        ),
        (TokenType::Number, TokenData::Float(n)) => p.emit_ir_previous(
            TypeSignature::Primitive(PrimitiveType::new("F32")),
            Instruction::Float(n),
        ),
        (TokenType::Number, _) => p.emit_notice_previous(
            NoticeLevel::Error,
            "Failed to extract float or integer data from number token".to_string(),
        ),
        (TokenType::String, TokenData::Str(s)) => p.emit_ir_previous(
            TypeSignature::Primitive(PrimitiveType::new("Str")),
            Instruction::String(s.to_string()),
        ),
        (TokenType::String, _) => p.emit_notice_previous(
            NoticeLevel::Error,
            "Failed to extract string data from string token".to_string(),
        ),
        (TokenType::KwTrue, _) => p.emit_ir_previous(
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            Instruction::Bool(true),
        ),
        (TokenType::KwFalse, _) => p.emit_ir_previous(
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            Instruction::Bool(false),
        ),
        (TokenType::Identifier, TokenData::Str(s)) => p.emit_ir_previous(
            TypeSignature::Untyped,
            Instruction::Identifier(s.to_string()),
        ),
        (TokenType::Identifier, _) => p.emit_notice_previous(
            NoticeLevel::Error,
            "Failed to extract string data from identifier token".to_string(),
        ),
        _ => {
            p.emit_notice_previous(
                NoticeLevel::Error,
                "Unreachable branch in `literal` parse function".to_string(),
            );
            p.synchronize(&[]);
            return Err(());
        }
    }

    Ok(())
}

/// Parse a unary expression
pub(crate) fn unary<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    p.advance();
    let start_pos = p.previous().pos;
    let op_tok = p.previous().type_;
    parse_precedence(p, rules::Prec::Unary)?;
    let (sig, ins) = match op_tok {
        TokenType::Minus => (TypeSignature::Untyped, Instruction::Negate),
        TokenType::Bang => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            Instruction::Not,
        ),
        _ => {
            p.emit_notice_previous(
                NoticeLevel::Error,
                "Unreachable branch in `unary` parse function".to_string(),
            );
            p.synchronize(&[]);
            return Err(());
        }
    };
    p.emit_ir(start_pos, sig, ins);
    Ok(())
}

/// Parse a binary expression
pub(crate) fn binary<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    p.advance();
    let start_pos = p.previous().pos;
    let op_tok = p.previous().type_;
    parse_precedence(p, rules::get_rule(op_tok).precedence)?;
    let (sig, ins) = match op_tok {
        TokenType::Plus => (TypeSignature::Untyped, Instruction::Add),
        TokenType::Minus => (TypeSignature::Untyped, Instruction::Subtract),
        TokenType::Star => (TypeSignature::Untyped, Instruction::Multiply),
        TokenType::Slash => (TypeSignature::Untyped, Instruction::Divide),

        TokenType::Less => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            Instruction::Less,
        ),
        TokenType::LessEqual => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            Instruction::LessEqual,
        ),
        TokenType::Greater => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            Instruction::Greater,
        ),
        TokenType::GreaterEqual => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            Instruction::GreaterEqual,
        ),
        TokenType::EqualEqual => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            Instruction::Equal,
        ),
        TokenType::BangEqual => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            Instruction::NotEqual,
        ),

        TokenType::KwAnd => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            Instruction::And,
        ),
        TokenType::KwOr => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            Instruction::Or,
        ),

        TokenType::Equal => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            Instruction::Assign,
        ),
        _ => {
            p.emit_notice_previous(
                NoticeLevel::Error,
                "Unreachable branch in `binary` parse function".to_string(),
            );
            p.synchronize(&[]);
            return Err(());
        }
    };
    p.emit_ir(start_pos, sig, ins);
    Ok(())
}

/// Parse a grouping or function
pub(crate) fn grouping_or_fn<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    let start_pos = p.current().pos;
    let mut was_error = p
        .consume(
            TokenType::LParen,
            "Expected opening `(` for function/grouping",
        )
        .is_err();

    let is_function = p.lookahead().type_ == TokenType::Colon
        || (p.check(TokenType::RParen) && p.lookahead().type_ == TokenType::RArrow);

    if is_function {
        let (mut params_sigs, mut param_names): (Vec<TypeSignature>, Vec<String>) =
            (Vec::new(), Vec::new());
        while !p.check_consume(TokenType::RParen) {
            let param_pos = p.current().pos;
            let name = match p.consume(
                TokenType::Identifier,
                "Expected identifier for function parameter",
            ) {
                Ok(TokenData::Str(s)) => (*s).to_string(),
                _ => {
                    p.emit_notice_previous(
                        NoticeLevel::Error,
                        "Failed to extract string data from identifier token".to_string(),
                    );
                    was_error = true;
                    p.synchronize(&[TokenType::LCurly, TokenType::RArrow]);
                    break;
                }
            };

            param_names.push(name);

            if p.consume(TokenType::Colon, "Expected `:` after parameter name")
                .is_err()
            {
                p.synchronize(&[TokenType::LCurly, TokenType::RArrow]);
                was_error = true;
                break;
            }

            params_sigs.push(type_(p)?);

            if !p.check_consume(TokenType::Comma) && !p.check(TokenType::RParen) {
                p.emit_notice(param_pos, NoticeLevel::Error, "Must have a comma `,` after every parameter in the function declaration unless it's the last parameter".to_string());
                p.synchronize(&[TokenType::LCurly, TokenType::RArrow]);
                return Err(());
            }
        }

        if p.consume(
            TokenType::RArrow,
            "Expect `->` after function parameters then return type",
        )
        .is_err()
        {
            p.synchronize(&[TokenType::LCurly]);
            was_error = true;
        }

        let return_type = if let Ok(t) = type_(p) {
            t
        } else {
            p.synchronize(&[TokenType::LCurly]);
            TypeSignature::None
        };

        p.emit_ir(
            start_pos,
            TypeSignature::Function(FunctionSignature {
                parameters: params_sigs,
                return_type_signature: Box::new(return_type),
            }),
            Instruction::Function,
        );

        param_names.iter().for_each(|name| {
            p.emit_ir(
                start_pos,
                TypeSignature::Untyped,
                Instruction::FunctionParameter(name.clone()),
            )
        });

        p.nested_fn_count += 1;
        if block(p).is_err() {
            p.synchronize(&[]);
            was_error = true;
        }
        p.nested_fn_count -= 1;
        p.emit_ir_previous(TypeSignature::None, Instruction::FunctionEnd);
    } else {
        expression(p)?;
        p.consume(TokenType::RParen, "Expected closing ')' for grouping")?;
    }

    if was_error {
        Err(())
    } else {
        Ok(())
    }
}

/// Parses a block expression
pub(crate) fn block<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    p.consume(
        TokenType::LCurly,
        "Expected opening `{` for block expression",
    )?;

    let mut is_expression_block = false;

    p.emit_ir_previous(TypeSignature::Untyped, Instruction::Block);
    while !p.check_consume(TokenType::RCurly) {
        let start_pos = p.current().pos;
        if p.check(TokenType::KwLet) {
            let_statement(p)?;
        } else {
            expression(p)?;

            if p.check_consume(TokenType::Semicolon) {
                p.emit_ir_previous(TypeSignature::Untyped, Instruction::Statement);
            } else if !p.check(TokenType::RCurly) {
                p.emit_notice(start_pos, NoticeLevel::Error, "Only the last element in a block can be an expression, all the rest must be statements and end with a `;`".to_string());
                p.synchronize(&[]);
                return Err(());
            } else {
                is_expression_block = true;
            }
        }

        if p.check(TokenType::Eof) {
            p.emit_notice_current(
                NoticeLevel::Error,
                "Expected a closing `}` for block expression".to_string(),
            );
            return Err(());
        }
    }

    p.emit_ir_previous(
        TypeSignature::Untyped,
        if is_expression_block {
            Instruction::BlockEndExpression
        } else {
            Instruction::BlockEnd
        },
    );

    Ok(())
}

/// Parses a struct expression
pub(crate) fn struct_<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    p.consume(
        TokenType::KwStruct,
        "Expected keyword `struct` for struct expression",
    )?;

    let start_pos = p.current().pos;

    p.emit_ir(start_pos, TypeSignature::Untyped, Instruction::Struct);

    p.consume(TokenType::LCurly, "Expected opening `{` for struct")?;

    let mut struct_signature: Vec<(bool, TypeSignature)> = Vec::new();

    while !p.check_consume(TokenType::RCurly) {
        let element_pos = p.current().pos;
        let is_public = p.check_consume(TokenType::KwPub);
        let name = match p.consume(TokenType::Identifier, "Expected a struct field name")? {
            TokenData::Str(s) => (*s).to_string(),
            _ => {
                p.emit_notice_previous(
                    NoticeLevel::Error,
                    "Failed to extract string value from identifier token".to_string(),
                );
                p.synchronize(&[]);
                return Err(());
            }
        };

        p.consume(TokenType::Colon, "Expected `:` then type afterwards")?;

        let signature = type_(p)?;
        struct_signature.push((is_public, signature.clone()));

        p.emit_ir(element_pos, signature, Instruction::StructField(name));

        if !p.check_consume(TokenType::Comma) && !p.check(TokenType::RCurly) {
            p.emit_notice(
                element_pos,
                NoticeLevel::Error,
                "Must have a comma `,` after every field in the struct unless it's the last field"
                    .to_string(),
            );
            p.synchronize(&[]);
            return Err(());
        }
    }

    p.emit_ir_previous(
        TypeSignature::Struct(StructSignature {
            fields: struct_signature,
        }),
        Instruction::StructEnd,
    );

    Ok(())
}

/// Parses a call expression
pub(crate) fn call<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    let mut was_error = p
        .consume(
            TokenType::LParen,
            "Expected `(` for call expression opening",
        )
        .is_err();
    let start_pos = p.current().pos;
    let mut arg_count = 0;
    while !p.check_consume(TokenType::RParen) {
        if expression(p).is_err() {
            p.synchronize(&[TokenType::Comma]);
            was_error = true;
        }
        if !p.check_consume(TokenType::Comma) && !p.check(TokenType::RParen) {
            p.emit_notice_previous(
                NoticeLevel::Error,
                "Expected a `,` in between call expressions".to_string(),
            );
            p.synchronize(&[]);
            return Err(());
        }
        arg_count += 1;
    }
    p.emit_ir(
        start_pos,
        TypeSignature::Untyped,
        Instruction::Call(arg_count),
    );
    if was_error {
        Err(())
    } else {
        Ok(())
    }
}

/// Parses an if expression
pub(crate) fn if_<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    p.consume(
        TokenType::KwIf,
        "Expected keyword `if` for opening an if expression",
    )?;

    p.emit_ir_previous(TypeSignature::Untyped, Instruction::If);
    if expression(p).is_err() {
        p.synchronize(&[TokenType::LCurly]);
    }
    p.emit_ir_previous(TypeSignature::Untyped, Instruction::IfBody);
    if block(p).is_err() {
        p.synchronize(&[TokenType::KwElse]);
    }

    while p.check_consume(TokenType::KwElse) {
        if p.check_consume(TokenType::KwIf) {
            p.emit_ir_previous(TypeSignature::Untyped, Instruction::IfElseIf);
            if expression(p).is_err() {
                p.synchronize(&[TokenType::LCurly]);
            }
            p.emit_ir_previous(TypeSignature::Untyped, Instruction::IfElseIfBody);
            block(p)?;
        } else {
            p.emit_ir_previous(TypeSignature::Untyped, Instruction::IfElse);
            if block(p).is_err() {
                p.synchronize(&[]);
            }
        }
    }

    p.emit_ir_previous(TypeSignature::Untyped, Instruction::IfEnd);
    Ok(())
}

/// Parses a field access expression
pub(crate) fn field_access<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    p.consume(TokenType::Dot, "Expected `.` for field access expression")?;
    let name = match p.consume(
        TokenType::Identifier,
        "Expected identifier after field access",
    )? {
        TokenData::Str(s) => (*s).to_string(),
        _ => {
            p.emit_notice_previous(
                NoticeLevel::Error,
                "Failed to extract string data from identifier token".to_string(),
            );
            p.synchronize(&[]);
            return Err(());
        }
    };

    p.emit_ir_previous(TypeSignature::Untyped, Instruction::FieldAccess(name));

    Ok(())
}

/// Parses an as expression
pub(crate) fn as_<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    p.consume(TokenType::KwAs, "Expected keyword `as` for as expression")?;
    let pos = p.current().pos;
    let type_ = type_(p)?;

    p.emit_ir(pos, type_, Instruction::As);

    Ok(())
}

/// Parses a while expression
pub(crate) fn while_<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    let mut was_error = p
        .consume(
            TokenType::KwWhile,
            "Expected keyword `while` for opening a while loop",
        )
        .is_err();
    p.emit_ir_previous(
        TypeSignature::Primitive(PrimitiveType::new("Nil")),
        Instruction::While,
    );
    if expression(p).is_err() {
        p.synchronize(&[TokenType::LCurly]);
        was_error = true;
    }
    p.emit_ir_current(
        TypeSignature::Primitive(PrimitiveType::new("Bool")),
        Instruction::WhileBody,
    );
    p.loop_stack.push(LoopType::While);
    if block(p).is_err() {
        p.synchronize(&[]);
        was_error = true;
    }
    p.loop_stack.pop();
    p.emit_ir_previous(
        TypeSignature::Primitive(PrimitiveType::new("Nil")),
        Instruction::WhileEnd,
    );
    if was_error {
        Err(())
    } else {
        Ok(())
    }
}

/// Parses a loop expression
pub(crate) fn loop_<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    let mut was_error = p
        .consume(
            TokenType::KwLoop,
            "Expected keyword `loop` for opening a loop",
        )
        .is_err();
    p.emit_ir_current(TypeSignature::Untyped, Instruction::Loop);
    p.loop_stack.push(LoopType::Loop);
    if block(p).is_err() {
        p.synchronize(&[]);
        was_error = true;
    }
    p.loop_stack.pop();
    p.emit_ir_previous(TypeSignature::Untyped, Instruction::LoopEnd);
    if was_error {
        Err(())
    } else {
        Ok(())
    }
}

/// Parses a break expression
pub(crate) fn break_<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    if p.loop_stack.is_empty() {
        p.emit_notice_current(
            NoticeLevel::Error,
            "Can only break inside a loop".to_string(),
        );
        p.synchronize(&[]);
        return Err(());
    }

    p.consume(
        TokenType::KwBreak,
        "Expected keyword `break` for break expression",
    )?;

    if let Some(LoopType::Loop) = p.loop_stack.last() {
        expression(p)?;
        p.emit_ir_previous(TypeSignature::Untyped, Instruction::Break);
    } else {
        if !p.check(TokenType::Semicolon) {
            p.emit_notice_previous(
                NoticeLevel::Error,
                "Only `break` expressions inside `loop` expressions may return a value".to_string(),
            );
            p.synchronize(&[]);
            return Err(());
        }
        p.emit_ir_previous(
            TypeSignature::Primitive(PrimitiveType::new("Nil")),
            Instruction::Break,
        );
    }

    Ok(())
}

/// Parses a continue expression
pub(crate) fn continue_<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    if p.loop_stack.is_empty() {
        p.emit_notice_current(
            NoticeLevel::Error,
            "Can only continue inside a loop".to_string(),
        );
        p.synchronize(&[]);
        return Err(());
    }

    p.consume(
        TokenType::KwContinue,
        "Expected keyword `continue` for continue expression",
    )?;

    p.emit_ir_previous(TypeSignature::None, Instruction::Continue);

    Ok(())
}

/// Parses an extern expression
pub(crate) fn extern_function<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    p.consume(
        TokenType::KwExtern,
        "Expected keyword `extern` for extern function",
    )?;

    let start_pos = p.previous().pos;

    p.consume(
        TokenType::LParen,
        "Expected `(` for opening extern function parameters",
    )?;

    let mut param_sigs = Vec::new();

    while !p.check_consume(TokenType::RParen) {
        let param_pos = p.current().pos;

        if p.consume(
            TokenType::Identifier,
            "Expected identifier for parameter name",
        )
        .is_err()
        {
            p.synchronize(&[TokenType::RArrow]);
            break;
        }

        if p.consume(TokenType::Colon, "Expected `:` after parameter name")
            .is_err()
        {
            p.synchronize(&[TokenType::RArrow]);
            break;
        }

        param_sigs.push(type_(p)?);

        if !p.check_consume(TokenType::Comma) && !p.check(TokenType::RParen) {
            p.emit_notice(param_pos, NoticeLevel::Error, "Must have a comma `,` after every parameter in the extern function declaration unless it's the last parameter".to_string());
            p.synchronize(&[TokenType::RArrow]);
            return Err(());
        }
    }

    p.consume(
        TokenType::RArrow,
        "Expected `->` for extern function return type",
    )?;

    let return_type = type_(p)?;

    p.emit_ir(
        start_pos,
        TypeSignature::Function(FunctionSignature {
            parameters: param_sigs,
            return_type_signature: Box::new(return_type),
        }),
        Instruction::ExternFn,
    );

    Ok(())
}

/// Parses a return expression
pub(crate) fn return_<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    if p.nested_fn_count > 0 {
        let start_pos = p.current().pos;
        p.consume(
            TokenType::KwReturn,
            "Expected keyword `return` for return expression",
        )?;
        expression(p)?;
        p.emit_ir(start_pos, TypeSignature::Untyped, Instruction::Return);
        Ok(())
    } else {
        p.emit_notice_current(
            NoticeLevel::Error,
            "`return` expressions may only be inside a function".to_string(),
        );
        Err(())
    }
}

/// Parses a type signature
fn type_<'a>(p: &mut Parser<'a>) -> Result<TypeSignature, ()> {
    let ret = match (&p.current().type_, &p.current().data) {
        (TokenType::Identifier, TokenData::Str(s)) => {
            Ok(TypeSignature::Primitive(PrimitiveType::new(s)))
        }
        _ => {
            p.emit_notice_current(NoticeLevel::Error, "Expected a valid type".to_string());
            Err(())
        }
    };

    if ret.is_ok() {
        p.advance();
        ret
    } else {
        Err(())
    }
}
