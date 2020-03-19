#![allow(dead_code)]

use super::{
    ir,
    signature::{FunctionSignature, TypeSignature},
    {
        lexer::Lexer,
        token::{Token, TokenData, TokenType},
        Notice, NoticeLevel, Position,
    },
};

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

type ParseFn = fn(&mut Parser) -> Result<(), ()>;

#[derive(Clone)]
struct ParseRule {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Prec,
}

const PARSER_RULE_TABLE: [ParseRule; TokenType::Eof as usize + 1] = [
    ParseRule {
        prefix: nil_func,
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
        infix: nil_func,
        precedence: Prec::Term,
    }, // TokenType::Plus
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::Term,
    }, // TokenType::Minus
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::Factor,
    }, // TokenType::Star
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::Factor,
    }, // TokenType::Slash
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::Bang
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::Comparison,
    }, // TokenType::BangEqual
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::Assignment,
    }, // TokenType::Equal
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::Equality,
    }, // TokenType::EqualEqual
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::Comparison,
    }, // TokenType::Greater
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::Comparison,
    }, // TokenType::GreaterEqual
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::Comparison,
    }, // TokenType::Less
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
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
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::Identifier
    ParseRule {
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::String
    ParseRule {
        prefix: nil_func,
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
        prefix: nil_func,
        infix: nil_func,
        precedence: Prec::None,
    }, // TokenType::KwTrue
    ParseRule {
        prefix: nil_func,
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
    /// Contains the notices that arise during compilation
    notices: Vec<Notice>,

    /// Lexer
    lex: Lexer<'a>,

    /// Reference to the ir it's generating
    ir: &'a ir::Module,

    /// Token stack, 0 is previous, 1 is the current token, 2 is the first look ahead, 3 is the second look ahead
    tokens: [Token<'a>; 4],
}

impl<'a> Parser<'a> {
    fn new(source: &'a str, ir: &'a mut ir::Module) -> Self {
        Parser {
            notices: Vec::new(),
            lex: Lexer::new(source),
            ir,
            tokens: [
                Token::default(),
                Token::default(),
                Token::default(),
                Token::default(),
            ],
        }
    }

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

    #[inline]
    fn current(&self) -> &Token<'a> {
        &self.tokens[CURRENT]
    }

    #[inline]
    fn previous(&self) -> &Token<'a> {
        &self.tokens[PREVIOUS]
    }

    #[inline]
    fn check(&mut self, type_: TokenType) -> bool {
        self.current().type_ == type_
    }

    #[inline]
    fn consume(&mut self, type_: TokenType, err_msg: &'static str) -> Result<(), ()> {
        if self.current().type_ == type_ {
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

    #[inline]
    fn make_notice(&mut self, level: NoticeLevel, msg: String) {
        self.notices.push(Notice::new(
            "Parser".to_string(),
            msg,
            self.previous().pos,
            "".to_string(),
            level,
        ));
    }

    fn synchronize(&mut self) {}

    fn parse(source: &'a str) -> Result<(ir::Module, Vec<Notice>), (ir::Module, Vec<Notice>)> {
        let mut ir = ir::Module::new();
        let mut p = Parser::new(source, &mut ir);

        while !p.check(TokenType::Eof) {
            p.advance();
        }

        let notices = p.notices;

        Ok((ir, notices))
    }
}

fn nil_func<'a>(_p: &mut Parser<'a>) -> Result<(), ()> {
    unreachable!("Wot, somehow, `nil_func` was called, this shouldn't happen");
}

fn module<'a>(_p: &mut Parser<'a>) -> Result<(), ()> {
    Ok(())
}

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

fn maybe_statement_else_expression<'a>(_p: &mut Parser<'a>) -> Result<(), ()> {
    Ok(())
}
