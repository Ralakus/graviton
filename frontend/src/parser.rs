use super::{
    ir,
    signature::{PrimitiveType, TypeSignature},
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
        infix: nil_func,
        precedence: Prec::Call,
    }, // TokenType::LParen
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::RParen
    ParseRule {
        prefix: nil_func,
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
        infix: nil_func,
        precedence: Prec::None,
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
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwStruct
    ParseRule {
        prefix: nil_func,
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
        prefix: nil_func,
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
    }, // TokenType::KwIf
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwElse
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwWhile
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwFor
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwBreak
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
        infix: nil_func,
        precedence: Prec::Factor,
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

/// The index for the previos token in the parser
const PREVIOUS: usize = 0;
/// The index for the current token in the parser
const CURRENT: usize = 1;

/// The parser struct, contains all of the data necessary to parse
pub struct Parser<'a> {
    /// Name of the current module being parsed
    name: String,

    /// Lexer
    lex: Lexer<'a>,

    /// Channel to send notices though
    notice_tx: Sender<Option<Notice>>,
    /// Channel to send ir through
    ir_tx: Sender<Option<ir::ChannelIr>>,

    /// Token stack, 0 is previous, 1 is the current token, 2 is the first look ahead, 3 is the second look ahead
    tokens: [Token<'a>; 4],
}

impl<'a> Parser<'a> {
    /// Creates a new parser
    fn new(
        name: String,
        source: &'a str,
        notice_tx: Sender<Option<Notice>>,
        ir_tx: Sender<Option<ir::ChannelIr>>,
    ) -> Self {
        Parser {
            name,
            lex: Lexer::new(source),
            notice_tx,
            ir_tx,
            tokens: [
                Token::default(),
                Token::default(),
                Token::default(),
                Token::default(),
            ],
        }
    }

    /// Advances the lexer to the next token
    #[inline]
    fn advance(&mut self) {
        unsafe {
            std::ptr::copy(
                self.tokens.as_mut_ptr().offset(1),
                self.tokens.as_mut_ptr(),
                3,
            );
        }

        if let Some(tok) = self.lex.next() {
            self.tokens[3] = tok;
        } else {
            self.tokens[3] = Token::new(TokenType::Eof, TokenData::None, Position::new(0, 0));
        }
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

    /// If the current token matches `type_`, advance, otherwise return an error
    #[inline]
    fn consume(&mut self, type_: TokenType, err_msg: &'static str) -> Result<(), ()> {
        if self.current().type_ == type_ {
            self.advance();
            Ok(())
        } else {
            self.make_notice(
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
    fn make_ir_with_pos(&mut self, pos: Position, sig: TypeSignature, ins: ir::Instruction) {
        let ir = ir::ChannelIr { pos, sig, ins };

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
    fn make_ir(&mut self, sig: TypeSignature, ins: ir::Instruction) {
        self.make_ir_with_pos(self.previous().pos, sig, ins)
    }

    /// Makes an error at current position
    #[inline]
    fn make_notice(&mut self, level: NoticeLevel, msg: String) {
        let notice = Notice::new(
            "Parser".to_string(),
            msg,
            self.previous().pos,
            self.name.clone(),
            level,
        );
        if let Err(e) = self.notice_tx.send(Some(notice)) {
            eprintln!(
                "{}Parser notice send error: {}{}",
                core::ansi::Fg::BrightRed,
                e,
                core::ansi::Fg::Reset
            );
        }
    }

    /// For errror recovery, skips until the next 'safe' token
    fn synchronize(&mut self) {}

    /// Creates the parser thread
    pub fn parse(
        name: String,
        source: std::sync::Arc<str>,
        notice_tx: Sender<Option<Notice>>,
        ir_tx: Sender<Option<ir::ChannelIr>>,
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

/// A funciton that should never be called that serves as a placeholder in the lookup table
fn nil_func<'a>(_p: &mut Parser<'a>) -> Result<(), ()> {
    unreachable!("Wot, somehow, `nil_func` was called, this shouldn't happen");
}

/// Parse a module
fn module<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    p.make_ir(TypeSignature::None, ir::Instruction::Module(p.name.clone()));
    expression(p)?;
    p.make_ir(TypeSignature::None, ir::Instruction::ModuleEnd);
    Ok(())
}

/// Parse an expression with a precedence greater than or equal to `precedence`
fn parse_precedence<'a>(p: &mut Parser<'a>, precedence: Prec) -> Result<(), ()> {
    p.advance();

    let prefix_rule = get_rule(p.previous().type_).prefix;
    if prefix_rule as usize == nil_func as usize {
        p.make_notice(NoticeLevel::Error, "Expected prefix expression".to_string());
        return Err(());
    }

    if prefix_rule(p).is_err() {
        p.synchronize();
        return Err(());
    }

    while precedence <= get_rule(p.current().type_).precedence {
        p.advance();

        let infix_rule = get_rule(p.previous().type_).infix;
        if infix_rule as usize == nil_func as usize {
            p.make_notice(NoticeLevel::Error, "Expected infix expression".to_string());
            return Err(());
        }

        if infix_rule(p).is_err() {
            p.synchronize();
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
    match (p.previous().type_, p.previous().data.clone()) {
        (TokenType::Number, TokenData::Integer(n)) => p.make_ir(
            TypeSignature::Primitive(PrimitiveType::new("I32")),
            ir::Instruction::Integer(n),
        ),
        (TokenType::Number, TokenData::Float(n)) => p.make_ir(
            TypeSignature::Primitive(PrimitiveType::new("F32")),
            ir::Instruction::Float(n),
        ),
        (TokenType::Number, _) => p.make_notice(
            NoticeLevel::Error,
            "Failed to extract float or integer data from number token".to_string(),
        ),
        (TokenType::String, TokenData::Str(s)) => p.make_ir(
            TypeSignature::Primitive(PrimitiveType::new("Str")),
            ir::Instruction::String(s.to_string()),
        ),
        (TokenType::String, _) => p.make_notice(
            NoticeLevel::Error,
            "Failed to extract string data from string token".to_string(),
        ),
        (TokenType::KwTrue, _) => p.make_ir(
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            ir::Instruction::Bool(true),
        ),
        (TokenType::KwFalse, _) => p.make_ir(
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            ir::Instruction::Bool(false),
        ),
        (TokenType::Identifier, TokenData::Str(s)) => p.make_ir(
            TypeSignature::Untyped,
            ir::Instruction::Identifier(s.to_string()),
        ),
        (TokenType::Identifier, _) => p.make_notice(
            NoticeLevel::Error,
            "Failed to extract string data from identifier token".to_string(),
        ),
        _ => {
            p.make_notice(
                NoticeLevel::Error,
                "Unreachable branch in `unary` parse function".to_string(),
            );
            return Err(());
        }
    }

    Ok(())
}

/// Parse a unary expression
fn unary<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    let start_pos = p.previous().pos;
    let op_tok = p.previous().type_;
    parse_precedence(p, Prec::Unary)?;
    let (sig, ins) = match op_tok {
        TokenType::Minus => (TypeSignature::Untyped, ir::Instruction::Negate),
        TokenType::Bang => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            ir::Instruction::Not,
        ),
        _ => {
            p.make_notice(
                NoticeLevel::Error,
                "Unreachable branch in `unary` parse function".to_string(),
            );
            return Err(());
        }
    };
    p.make_ir_with_pos(start_pos, sig, ins);
    Ok(())
}

/// Parse a binary expression
fn binary<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    let start_pos = p.previous().pos;
    let op_tok = p.previous().type_;
    parse_precedence(p, get_rule(op_tok).precedence)?;
    let (sig, ins) = match op_tok {
        TokenType::Plus => (TypeSignature::Untyped, ir::Instruction::Add),
        TokenType::Minus => (TypeSignature::Untyped, ir::Instruction::Subtract),
        TokenType::Star => (TypeSignature::Untyped, ir::Instruction::Multiply),
        TokenType::Slash => (TypeSignature::Untyped, ir::Instruction::Divide),

        TokenType::Less => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            ir::Instruction::Less,
        ),
        TokenType::LessEqual => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            ir::Instruction::LessEqual,
        ),
        TokenType::Greater => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            ir::Instruction::Greater,
        ),
        TokenType::GreaterEqual => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            ir::Instruction::GreaterEqual,
        ),
        TokenType::EqualEqual => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            ir::Instruction::Equal,
        ),
        TokenType::BangEqual => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            ir::Instruction::NotEqual,
        ),

        TokenType::KwAnd => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            ir::Instruction::And,
        ),
        TokenType::KwOr => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            ir::Instruction::Or,
        ),

        TokenType::Equal => (
            TypeSignature::Primitive(PrimitiveType::new("Bool")),
            ir::Instruction::Assign,
        ),
        _ => {
            p.make_notice(
                NoticeLevel::Error,
                "Unreachable branch in `binary` parse function".to_string(),
            );
            return Err(());
        }
    };
    p.make_ir_with_pos(start_pos, sig, ins);
    Ok(())
}

/// Parse a grouping or function (function parsing not implemented yet)
fn grouping_or_fn<'a>(p: &mut Parser<'a>) -> Result<(), ()> {
    expression(p)?;
    p.consume(TokenType::RParen, "Expected closing ')'")?;
    Ok(())
}

/*fn maybe_statement_else_expression<'a>(_p: &mut Parser<'a>) -> Result<(), ()> {
    Ok(())
}*/
