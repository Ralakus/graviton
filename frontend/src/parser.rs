use super::{
    ir::{ChannelIr, Instruction},
    signature::{FunctionSignature, PrimitiveType, StructSignature, TypeSignature},
    {
        lexer::Lexer,
        token::{Token, TokenData, TokenType},
        Notice, NoticeLevel, Position,
    },
};

use mpsc::Sender;
use std::{sync::mpsc, thread};

/// Precedence used in Pratt parsing
#[repr(u8)]
#[derive(Clone, Copy, Eq, PartialEq, PartialOrd)]
enum Prec {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . () []
                // Primary
}

/// A grammatical parse function
type ParseFn = fn(&mut Parser) -> Result<(), ()>;

/// A parse rule for a token
#[derive(Clone)]
struct ParseRule {
    /// When it's found as a prefix expression
    prefix: ParseFn,
    /// When it's found an infix expression
    infix: ParseFn,
    /// It's precedence
    precedence: Prec,
}

/// A lookup table for the expression rules for each token when encountered as an expression
const PARSER_RULE_TABLE: [ParseRule; TokenType::Eof as usize + 1] = [
    ParseRule {
        prefix: grouping_or_fn,
        infix: call,
        precedence: Prec::Call,
    }, // TokenType::LParen
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::RParen
    ParseRule {
        prefix: block,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::LCurly
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::RCurly
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::LBracket
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::RBracket
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::Comma
    ParseRule {
        prefix: nil_func,
        infix: field_access,
        precedence: Prec::Call,
    }, // TokenType::Dot
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::Semicolon
    ParseRule {
        prefix: nil_func,
        infix: binary,
        precedence: Prec::Term,
    }, // TokenType::Plus
    ParseRule {
        prefix: unary,
        infix: binary,
        precedence: Prec::Unary,
    }, // TokenType::Minus
    ParseRule {
        prefix: nil_func,
        infix: binary,
        precedence: Prec::Factor,
    }, // TokenType::Star
    ParseRule {
        prefix: nil_func,
        infix: binary,
        precedence: Prec::Factor,
    }, // TokenType::Slash
    ParseRule {
        prefix: unary,
        infix: nil_func,
        precedence: Prec::Unary,
    }, // TokenType::Bang
    ParseRule {
        prefix: nil_func,
        infix: binary,
        precedence: Prec::Comparison,
    }, // TokenType::BangEqual
    ParseRule {
        prefix: nil_func,
        infix: binary,
        precedence: Prec::Assignment,
    }, // TokenType::Equal
    ParseRule {
        prefix: nil_func,
        infix: binary,
        precedence: Prec::Equality,
    }, // TokenType::EqualEqual
    ParseRule {
        prefix: nil_func,
        infix: binary,
        precedence: Prec::Comparison,
    }, // TokenType::Greater
    ParseRule {
        prefix: nil_func,
        infix: binary,
        precedence: Prec::Comparison,
    }, // TokenType::GreaterEqual
    ParseRule {
        prefix: nil_func,
        infix: binary,
        precedence: Prec::Comparison,
    }, // TokenType::Less
    ParseRule {
        prefix: nil_func,
        infix: binary,
        precedence: Prec::Comparison,
    }, // TokenType::LessEqual
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::Colon
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::RArrow
    ParseRule {
        prefix: literal,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::Identifier
    ParseRule {
        prefix: literal,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::String
    ParseRule {
        prefix: literal,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::Number
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::And,
    }, // TokenType::KwAnd
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::Or,
    }, // TokenType::KwOr
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwSelf
    ParseRule {
        prefix: struct_,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwStruct
    ParseRule {
        prefix: return_,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwReturn
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwImport
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwLet
    ParseRule {
        prefix: extern_function,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwExtern
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwMut
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwPub
    ParseRule {
        prefix: if_,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwIf
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwElse
    ParseRule {
        prefix: loop_,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwLoop
    ParseRule {
        prefix: while_,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwWhile
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwFor
    ParseRule {
        prefix: break_,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwBreak
    ParseRule {
        prefix: continue_,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwContinue
    ParseRule {
        prefix: literal,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwTrue
    ParseRule {
        prefix: literal,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwFalse
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwNil
    ParseRule {
        prefix: nil_func,
        infix: as_,
        precedence: Prec::Call,
    }, // TokenType::KwAs
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::Err
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::Eof
];

/// A wrapper function to get a rule from the loopup table
#[inline]
fn get_rule(type_: TokenType) -> &'static ParseRule {
    &PARSER_RULE_TABLE[type_ as usize]
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum LoopType {
    While,
    Loop,
}

/// The index for the previos token in the parser
const PREVIOUS: usize = 0;
/// The index for the current token in the parser
const CURRENT: usize = 1;

/// An array of generally safe tokens to synchronize to for error handling
const SAFE_TOKENS: [TokenType; 6] = [
    TokenType::RCurly,
    TokenType::Semicolon,
    TokenType::KwIf,
    TokenType::KwLet,
    TokenType::KwStruct,
    TokenType::Eof,
];

/// An array of closing tokens, synchronize will consume these tokens
const CLOSING_TOKENS: [TokenType; 4] = [
    TokenType::RBracket,
    TokenType::RCurly,
    TokenType::RParen,
    TokenType::Semicolon,
];

/// An enum stating a declaration type for a let statement
#[derive(PartialEq)]
enum DeclarationType {
    Struct,
    Function,
    Other,
}

/// The parser struct, contains all of the data necessary to parse
pub struct Parser<'a> {
    /// Name of the current module being parsed
    name: String,

    /// Lexer
    lex: Lexer<'a>,

    /// Channel to send notices though
    notice_tx: Sender<Option<Notice>>,
    /// Channel to send ir through
    ir_tx: Sender<Option<ChannelIr>>,

    /// Token stack, 0 is previous, 1 is the current token, 2 is the first look ahead, 3 is the second look ahead
    tokens: [Token<'a>; 3],

    /// A stack to see what type of loop the parser is in
    loop_stack: Vec<LoopType>,

    /// An integer that keeps track of how many function's it's in
    nested_fn_count: u8,

    /// Last declaration type, used for let statements to find the correct instruction to output
    last_declaration: DeclarationType,
}

impl<'a> Parser<'a> {
    /// Creates a new parser
    fn new(
        name: String,
        source: &'a str,
        notice_tx: Sender<Option<Notice>>,
        ir_tx: Sender<Option<ChannelIr>>,
    ) -> Self {
        Parser {
            name,
            lex: Lexer::new(source),
            notice_tx,
            ir_tx,
            tokens: [Token::default(), Token::default(), Token::default()],
            loop_stack: Vec::with_capacity(8),
            nested_fn_count: 0,
            last_declaration: DeclarationType::Other,
        }
    }

    /// Advances the lexer to the next token
    #[inline]
    fn advance(&mut self) {
        self.tokens[0] = self.tokens[1].clone();
        self.tokens[1] = self.tokens[2].clone();

        if let Some(tok) = self.lex.next() {
            self.tokens[2] = tok;
        } else {
            self.tokens[2] = Token::new(TokenType::Eof, TokenData::None, Position::new(0, 0));
        }
    }

    /// Returns the lookahead token
    #[inline]
    fn lookahead(&self) -> &Token<'a> {
        &self.tokens[2]
    }

    /// Returns the current token the parser is on
    #[inline]
    fn current(&self) -> &Token<'a> {
        &self.tokens[CURRENT]
    }

    /// Returns the previous token that was already passed
    #[inline]
    fn previous(&self) -> &Token<'a> {
        &self.tokens[PREVIOUS]
    }

    /// Check if the current token is of type, returns true if so and vice versa
    #[inline]
    fn check(&mut self, type_: TokenType) -> bool {
        self.current().type_ == type_
    }

    /// Check if the current token is of type, returns true and comsumes the token if so and false and does nothing otherwise
    #[inline]
    fn check_consume(&mut self, type_: TokenType) -> bool {
        if self.current().type_ == type_ {
            self.advance();
            true
        } else {
            false
        }
    }

    /// If the current token matches `type_`, advances and returns the token's data, otherwise return an error
    #[inline]
    fn consume(&mut self, type_: TokenType, err_msg: &'static str) -> Result<&TokenData, ()> {
        if self.current().type_ == type_ {
            self.advance();
            Ok(&self.previous().data)
        } else {
            self.emit_notice_current(
                NoticeLevel::Error,
                format!(
                    "{} => Found {:?}, expected {:?}",
                    err_msg,
                    self.current().type_,
                    type_
                ),
            );
            Err(())
        }
    }

    /// Makes an IR instruction with a custom position and sends it through the channel
    #[inline]
    fn emit_ir(&mut self, pos: Position, sig: TypeSignature, ins: Instruction) {
        if ins == Instruction::FunctionEnd || ins == Instruction::ExternFn {
            self.last_declaration = DeclarationType::Function;
        } else if ins == Instruction::StructEnd {
            self.last_declaration = DeclarationType::Struct;
        } else {
            self.last_declaration = DeclarationType::Other;
        }

        let ir = ChannelIr { pos, sig, ins };

        if let Err(e) = self.ir_tx.send(Some(ir)) {
            eprintln!(
                "{}Parser ir send error: {}{}",
                core::ansi::Fg::BrightRed,
                e,
                core::ansi::Fg::Reset
            );
        }
    }

    /// Makes an IR instruction and sends it through the channel
    #[inline]
    fn emit_ir_previous(&mut self, sig: TypeSignature, ins: Instruction) {
        self.emit_ir(self.previous().pos, sig, ins)
    }

    /// Makes an IR instruction and sends it through the channel
    #[inline]
    fn emit_ir_current(&mut self, sig: TypeSignature, ins: Instruction) {
        self.emit_ir(self.current().pos, sig, ins)
    }

    /// Makes a notice at a position
    #[inline]
    fn emit_notice(&mut self, pos: Position, level: NoticeLevel, msg: String) {
        if level == NoticeLevel::Error {
            if let Err(e) = self.ir_tx.send(Some(ChannelIr {
                pos,
                sig: TypeSignature::None,
                ins: Instruction::Halt,
            })) {
                eprintln!(
                    "{}Parser notice send error: {}{}",
                    core::ansi::Fg::BrightRed,
                    e,
                    core::ansi::Fg::Reset
                );
            }
        }

        let notice = Notice::new("Parser".to_string(), msg, pos, self.name.clone(), level);

        if let Err(e) = self.notice_tx.send(Some(notice)) {
            eprintln!(
                "{}Parser notice send error: {}{}",
                core::ansi::Fg::BrightRed,
                e,
                core::ansi::Fg::Reset
            );
        }
    }

    /// Makes a notice at previous position
    #[inline]
    fn emit_notice_previous(&mut self, level: NoticeLevel, msg: String) {
        self.emit_notice(self.previous().pos, level, msg)
    }

    /// Makes a notice at current position
    #[inline]
    fn emit_notice_current(&mut self, level: NoticeLevel, msg: String) {
        self.emit_notice(self.current().pos, level, msg)
    }

    /// For errror recovery, skips until the next generally 'safe' token and any other tokens specified
    fn synchronize(&mut self, tokens: &[TokenType]) {
        while tokens.iter().filter(|t| self.check(**t)).count()
            + SAFE_TOKENS.iter().filter(|t| self.check(**t)).count()
            == 0
        {
            self.advance();
        }
        if tokens
            .iter()
            .filter(|t| CLOSING_TOKENS.contains(t) && self.check(**t))
            .count()
            == 1
            || self.current().type_ == TokenType::RCurly
            || self.current().type_ == TokenType::Semicolon
        {
            self.advance();
        }
    }

    /// Creates the parser thread
    pub fn parse(
        name: String,
        source: std::sync::Arc<str>,
        notice_tx: Sender<Option<Notice>>,
        ir_tx: Sender<Option<ChannelIr>>,
    ) -> thread::JoinHandle<()> {
        thread::spawn(move || {
            let source_arc = source.clone();
            let mut p = Parser::new(name, &*source_arc, notice_tx, ir_tx);

            // Fill the look ahead with tokens
            while p.check(TokenType::Eof) {
                p.advance();
            }

            if module(&mut p).is_ok() {}

            if let Err(e) = p.ir_tx.send(None) {
                eprintln!(
                    "{}Parser ir send error: {}{}",
                    core::ansi::Fg::BrightRed,
                    e,
                    core::ansi::Fg::Reset
                );
            }

            if let Err(e) = p.notice_tx.send(None) {
                eprintln!(
                    "{}Parser notice send error: {}{}",
                    core::ansi::Fg::BrightRed,
                    e,
                    core::ansi::Fg::Reset
                );
            }
        })
    }
}

/// A funciton that should never be called that serves as a null pointer in the lookup table
fn nil_func<'a>(_p: &mut Parser<'a>) -> Result<(), ()> {
    unreachable!("Somehow `nil_func` was called, this shouldn't happen");
}

/// Parse a module
fn module<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
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

fn declaration_or_statement<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    match p.current().type_ {
        TokenType::KwImport => declaration(p),
        _ => statement(p),
    }
}

fn declaration<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    match p.current().type_ {
        TokenType::KwImport => {
            p.synchronize(&[]);
            p.emit_notice_previous(
                NoticeLevel::Warning,
                "Imports not supported yet".to_string(),
            );
        }
        _ => {
            p.emit_notice_current(NoticeLevel::Error, "Expected a declaration".to_string());
            p.synchronize(&[]);
            return Err(());
        }
    }
    Ok(())
}

fn statement<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    match p.current().type_ {
        TokenType::KwLet => let_statement(p)?,
        _ => expression_statement(p)?,
    };
    Ok(())
}

fn let_statement<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
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

fn expression_statement<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    expression(p)?;
    p.consume(TokenType::Semicolon, "Expected closing `;`")?;
    Ok(())
}

/// Parse an expression with a precedence greater than or equal to `precedence`
fn parse_precedence<'a>(p: &mut Parser<'a>, precedence: Prec) -> Result<(), ()> {
    let prefix_rule = get_rule(p.current().type_).prefix;
    if prefix_rule as usize == nil_func as usize {
        p.emit_notice_current(NoticeLevel::Error, "Expected prefix expression".to_string());
        p.synchronize(&[]);
        return Err(());
    }

    if prefix_rule(p).is_err() {
        p.synchronize(&[]);
        return Err(());
    }

    while precedence <= get_rule(p.current().type_).precedence {
        let infix_rule = get_rule(p.current().type_).infix;
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
fn expression<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    parse_precedence(p, Prec::Assignment)
}

/// Parse a literal expression
fn literal<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
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
fn unary<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    p.advance();
    let start_pos = p.previous().pos;
    let op_tok = p.previous().type_;
    parse_precedence(p, Prec::Unary)?;
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
fn binary<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    p.advance();
    let start_pos = p.previous().pos;
    let op_tok = p.previous().type_;
    parse_precedence(p, get_rule(op_tok).precedence)?;
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
fn grouping_or_fn<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
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

fn block<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
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

fn struct_<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
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

fn call<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
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

fn if_<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
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

fn field_access<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
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

fn as_<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    p.consume(TokenType::KwAs, "Expected keyword `as` for as expression")?;
    let pos = p.current().pos;
    let type_ = type_(p)?;

    p.emit_ir(pos, type_, Instruction::As);

    Ok(())
}

fn while_<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
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

fn loop_<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
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

fn break_<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
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

fn continue_<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
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

fn extern_function<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
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

fn return_<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
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
