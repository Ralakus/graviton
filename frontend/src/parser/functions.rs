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
pub fn nil_func(_p: &mut Parser<'_>) -> Result<(), ()> {
    unreachable!("Somehow `nil_func` was called, this shouldn't happen");
}

/// Parse a module
pub fn module(p: &mut Parser<'_>) -> Result<(), ()> {
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
pub fn declaration_or_statement(p: &mut Parser<'_>) -> Result<(), ()> {
    match p.current().type_ {
        TokenType::KwModule => declaration(p),
        _ => statement(p),
    }
}

/// Parses a declaration
pub fn declaration(p: &mut Parser<'_>) -> Result<(), ()> {
    if p.current().type_ == TokenType::KwModule {
        p.consume(
            TokenType::KwModule,
            "Expected keyword `module` for opening a module declaration",
        )?;

        let module_pos = p.current().pos;

        let mut segments = Vec::new();

        if let Ok(s) = p.consume(TokenType::Identifier, "Expected identifer ") {
            if let TokenData::Str(s) = s {
                segments.push((*s).to_string());
            } else {
                p.emit_notice_previous(
                    NoticeLevel::Error,
                    "Failed to extract string data from string token".to_string(),
                );
                p.synchronize(&[]);
                return Err(());
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
                if let TokenData::Str(s) = s {
                    segments.push((*s).to_string());
                } else {
                    p.emit_notice_previous(
                        NoticeLevel::Error,
                        "Failed to extract string data from string token".to_string(),
                    );
                    p.synchronize(&[]);
                    return Err(());
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

        if p.parse_imported_module(format!("{module_name}.grav"))
            .is_err()
        {
            p.emit_notice(
                module_pos,
                NoticeLevel::Error,
                format!("Failed to load module {}", segments.join(".")),
            );
            return Err(());
        }
    } else {
        p.emit_notice_current(NoticeLevel::Error, "Expected a declaration".to_string());
        p.synchronize(&[]);
        return Err(());
    }
    Ok(())
}

/// Parses a statement
pub fn statement(p: &mut Parser<'_>) -> Result<(), ()> {
    match p.current().type_ {
        TokenType::KwLet => let_statement(p)?,
        _ => expression_statement(p)?,
    };
    Ok(())
}

/// Parses a let statement
pub fn let_statement(p: &mut Parser<'_>) -> Result<(), ()> {
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
    let name = if let TokenData::Str(s) = p.consume(
        TokenType::Identifier,
        "Expected identifier for let statement",
    )? {
        (*s).to_string()
    } else {
        p.emit_notice_previous(
            NoticeLevel::Error,
            "Failed to extract string data from identifier token".to_string(),
        );
        p.synchronize(&[]);
        was_error = true;
        String::from("ERROR")
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
pub fn expression_statement(p: &mut Parser<'_>) -> Result<(), ()> {
    expression(p)?;
    p.consume(TokenType::Semicolon, "Expected closing `;`")?;
    p.emit_ir_previous(TypeSignature::None, Instruction::Statement);
    Ok(())
}

/// Parse an expression with a precedence greater than or equal to `precedence`
pub fn parse_precedence(p: &mut Parser<'_>, precedence: rules::Prec) -> Result<(), ()> {
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
pub fn expression(p: &mut Parser<'_>) -> Result<(), ()> {
    parse_precedence(p, rules::Prec::Assignment)
}

/// Parse a literal expression
pub fn literal(p: &mut Parser<'_>) -> Result<(), ()> {
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
            TypeSignature::Primitive(PrimitiveType::Str),
            Instruction::String(s.to_string()),
        ),
        (TokenType::String, _) => p.emit_notice_previous(
            NoticeLevel::Error,
            "Failed to extract string data from string token".to_string(),
        ),
        (TokenType::KwTrue, _) => p.emit_ir_previous(
            TypeSignature::Primitive(PrimitiveType::Boolean),
            Instruction::Bool(true),
        ),
        (TokenType::KwFalse, _) => p.emit_ir_previous(
            TypeSignature::Primitive(PrimitiveType::Boolean),
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
pub fn unary(p: &mut Parser<'_>) -> Result<(), ()> {
    p.advance();
    let start_pos = p.previous().pos;
    let op_tok = p.previous().type_;
    parse_precedence(p, rules::Prec::Unary)?;
    let (sig, ins) = match op_tok {
        TokenType::Minus => (TypeSignature::Untyped, Instruction::Negate),
        TokenType::Bang => (
            TypeSignature::Primitive(PrimitiveType::Boolean),
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
pub fn binary(p: &mut Parser<'_>) -> Result<(), ()> {
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
            TypeSignature::Primitive(PrimitiveType::Boolean),
            Instruction::Less,
        ),
        TokenType::LessEqual => (
            TypeSignature::Primitive(PrimitiveType::Boolean),
            Instruction::LessEqual,
        ),
        TokenType::Greater => (
            TypeSignature::Primitive(PrimitiveType::Boolean),
            Instruction::Greater,
        ),
        TokenType::GreaterEqual => (
            TypeSignature::Primitive(PrimitiveType::Boolean),
            Instruction::GreaterEqual,
        ),
        TokenType::EqualEqual => (
            TypeSignature::Primitive(PrimitiveType::Boolean),
            Instruction::Equal,
        ),
        TokenType::BangEqual => (
            TypeSignature::Primitive(PrimitiveType::Boolean),
            Instruction::NotEqual,
        ),

        TokenType::KwAnd => (
            TypeSignature::Primitive(PrimitiveType::Boolean),
            Instruction::And,
        ),
        TokenType::KwOr => (
            TypeSignature::Primitive(PrimitiveType::Boolean),
            Instruction::Or,
        ),

        TokenType::Equal => (
            TypeSignature::Primitive(PrimitiveType::Nil),
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
pub fn grouping_or_fn(p: &mut Parser<'_>) -> Result<(), ()> {
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
            let name = if let TokenData::Str(s) = p.consume(
                TokenType::Identifier,
                "Expected identifier for function parameter",
            )? {
                (*s).to_string()
            } else {
                p.emit_notice_previous(
                    NoticeLevel::Error,
                    "Failed to extract string data from identifier token".to_string(),
                );
                was_error = true;
                p.synchronize(&[TokenType::LCurly, TokenType::RArrow]);
                break;
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

        let return_type = type_(p).map_or_else(
            |()| {
                p.synchronize(&[TokenType::LCurly]);
                TypeSignature::None
            },
            |t| t,
        );

        p.emit_ir(
            start_pos,
            TypeSignature::Function(FunctionSignature {
                parameters: params_sigs,
                return_type_signature: Box::new(return_type),
            }),
            Instruction::Function,
        );

        for name in &param_names {
            p.emit_ir(
                start_pos,
                TypeSignature::Untyped,
                Instruction::FunctionParameter(name.clone()),
            );
        }

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
pub fn block(p: &mut Parser<'_>) -> Result<(), ()> {
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
pub fn struct_(p: &mut Parser<'_>) -> Result<(), ()> {
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
        let name = if let TokenData::Str(s) =
            p.consume(TokenType::Identifier, "Expected a struct field name")?
        {
            (*s).to_string()
        } else {
            p.emit_notice_previous(
                NoticeLevel::Error,
                "Failed to extract string value from identifier token".to_string(),
            );
            p.synchronize(&[]);
            return Err(());
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
pub fn call(p: &mut Parser<'_>) -> Result<(), ()> {
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
pub fn if_(p: &mut Parser<'_>) -> Result<(), ()> {
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
pub fn field_access(p: &mut Parser<'_>) -> Result<(), ()> {
    p.consume(TokenType::Dot, "Expected `.` for field access expression")?;
    let name = if let TokenData::Str(s) = p.consume(
        TokenType::Identifier,
        "Expected identifier after field access",
    )? {
        (*s).to_string()
    } else {
        p.emit_notice_previous(
            NoticeLevel::Error,
            "Failed to extract string data from identifier token".to_string(),
        );
        p.synchronize(&[]);
        return Err(());
    };

    p.emit_ir_previous(TypeSignature::Untyped, Instruction::FieldAccess(name));

    Ok(())
}

/// Parses an as expression
pub fn as_(p: &mut Parser<'_>) -> Result<(), ()> {
    p.consume(TokenType::KwAs, "Expected keyword `as` for as expression")?;
    let pos = p.current().pos;
    let type_ = type_(p)?;

    p.emit_ir(pos, type_, Instruction::As);

    Ok(())
}

/// Parses a while expression
pub fn while_(p: &mut Parser<'_>) -> Result<(), ()> {
    let mut was_error = p
        .consume(
            TokenType::KwWhile,
            "Expected keyword `while` for opening a while loop",
        )
        .is_err();
    p.emit_ir_previous(
        TypeSignature::Primitive(PrimitiveType::Nil),
        Instruction::While,
    );
    if expression(p).is_err() {
        p.synchronize(&[TokenType::LCurly]);
        was_error = true;
    }
    p.emit_ir_current(
        TypeSignature::Primitive(PrimitiveType::Boolean),
        Instruction::WhileBody,
    );
    p.loop_stack.push(LoopType::While);
    if block(p).is_err() {
        p.synchronize(&[]);
        was_error = true;
    }
    p.loop_stack.pop();
    p.emit_ir_previous(
        TypeSignature::Primitive(PrimitiveType::Nil),
        Instruction::WhileEnd,
    );
    if was_error {
        Err(())
    } else {
        Ok(())
    }
}

/// Parses a loop expression
pub fn loop_(p: &mut Parser<'_>) -> Result<(), ()> {
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
pub fn break_(p: &mut Parser<'_>) -> Result<(), ()> {
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

    if p.loop_stack.last() == Some(&LoopType::Loop) {
        if p.check(TokenType::Semicolon) {
            p.emit_ir_previous(
                TypeSignature::Primitive(PrimitiveType::Nil),
                Instruction::Break,
            );
        } else {
            expression(p)?;
            p.emit_ir_previous(TypeSignature::Untyped, Instruction::BreakExpression);
        }
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
            TypeSignature::Primitive(PrimitiveType::Nil),
            Instruction::Break,
        );
    }

    Ok(())
}

/// Parses a continue expression
pub fn continue_(p: &mut Parser<'_>) -> Result<(), ()> {
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
pub fn extern_function(p: &mut Parser<'_>) -> Result<(), ()> {
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
pub fn return_(p: &mut Parser<'_>) -> Result<(), ()> {
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
fn type_(p: &mut Parser<'_>) -> Result<TypeSignature, ()> {
    let ret = if let (TokenType::Identifier, TokenData::Str(s)) =
        (&p.current().type_, &p.current().data)
    {
        Ok(TypeSignature::Primitive(PrimitiveType::new(s)))
    } else {
        p.emit_notice_current(NoticeLevel::Error, "Expected a valid type".to_string());
        Err(())
    };

    if ret.is_ok() {
        p.advance();
        ret
    } else {
        Err(())
    }
}
