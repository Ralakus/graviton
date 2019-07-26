
use super::lexer::Lexer;
use super::tokens::{TokenType, Token, TokenData, Position};
use super::ast::{Ast, UnaryOperation, BinaryOperation};

#[repr(u8)]
#[derive(Clone, Copy)]
enum Prec {
	None,
	Assignment,  // =
	Or,          // or
	And,         // and
	Equality,    // == !=
	Comparison,  // < > <= >=
	Term,        // + -
	Factor,      // * /
	Unary,       // ! -
	Call,        // . () []
	Primary
}

type ParseFn = fn(&mut Parser) -> Result<Ast, String>;

#[derive(Clone)]
struct ParseRule {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Prec
}

const PARSER_RULE_TABLE: [ParseRule; 43] = [
ParseRule{prefix: grouping, infix: nil_func, precedence: Prec::Call       }, // TokenType::LParen
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::RParen
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::LCurly
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::RCurly
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::LBracket
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::RBracket
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::Comma
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::Dot
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::Semicolon
ParseRule{prefix: nil_func, infix: binary,   precedence: Prec::Term       }, // TokenType::Plus
ParseRule{prefix: unary,    infix: binary,   precedence: Prec::Term       }, // TokenType::Minus
ParseRule{prefix: nil_func, infix: binary,   precedence: Prec::Factor     }, // TokenType::Star
ParseRule{prefix: nil_func, infix: binary,   precedence: Prec::Factor     }, // TokenType::Slash
ParseRule{prefix: nil_func, infix: binary,   precedence: Prec::None       }, // TokenType::Bang
ParseRule{prefix: nil_func, infix: binary,   precedence: Prec::Comparison }, // TokenType::BangEqual
ParseRule{prefix: nil_func, infix: binary,   precedence: Prec::Assignment }, // TokenType::Equal
ParseRule{prefix: nil_func, infix: binary,   precedence: Prec::Equality   }, // TokenType::EqualEqual
ParseRule{prefix: nil_func, infix: binary,   precedence: Prec::Comparison }, // TokenType::Greater
ParseRule{prefix: nil_func, infix: binary,   precedence: Prec::Comparison }, // TokenType::GreaterEqual
ParseRule{prefix: nil_func, infix: binary,   precedence: Prec::Comparison }, // TokenType::Less
ParseRule{prefix: nil_func, infix: binary,   precedence: Prec::Comparison }, // TokenType::LessEqual
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::Colon
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::Identifier
ParseRule{prefix: literal,  infix: nil_func, precedence: Prec::None       }, // TokenType::String
ParseRule{prefix: literal,  infix: nil_func, precedence: Prec::None       }, // TokenType::Number
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::KwAnd
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::KwOr
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::KwSelf
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::KwStruct
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::KwReturn
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::KwImport
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::KwLet
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::KwDef
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::KwIf
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::KwElse
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::KwWhile
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::KwFor
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::KwBreak
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::KwTrue
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::KwFalse
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::KwNil
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::Err
ParseRule{prefix: nil_func, infix: nil_func, precedence: Prec::None       }, // TokenType::Eof
];

fn get_rule(token_type: TokenType) -> &'static ParseRule {
    &PARSER_RULE_TABLE[token_type as usize]
}


pub struct Parser<'a> {
    lex: Lexer<'a>,
    current: Token,
    previous: Token,

    prefix_node: Ast,
}

impl<'a> Parser<'a> {

    fn advance(&mut self) {
        self.previous = self.current.clone();

        if let Some(t) = self.lex.next() {
            self.current = t;
        } else {
            self.current = Token::new(TokenType::Eof, TokenData::None, Position{line: -1, col:-1});
        }
    }

    fn consume(&mut self, token_type: TokenType, err_msg: &'static str) -> Result<(), String> {
        if self.current.token_type == token_type {
            Ok(self.advance())
        } else {
            Err(self.make_error(err_msg))
        }
    }

    pub fn parse(source: &'a str) -> Result<Ast, String> {
        let mut p = Parser {
            lex: Lexer::new(source),
            current: Token::new(TokenType::Eof, TokenData::None, Position{line: -1, col:-1}),
            previous: Token::new(TokenType::Eof, TokenData::None, Position{line: -1, col:-1}),

            prefix_node: Ast::List(Vec::new())
        };

        p.advance();
        let ast = expression(&mut p)?;
        p.consume(TokenType::Eof, "Expected EOF")?;

        Ok(ast)
    }

    fn make_error(&mut self, msg: &'static str) -> String {
        format!("Line: {}, Col: {}, {}", self.previous.pos.line, self.previous.pos.col, msg)
    }

}

fn nil_func<'a>(p: &mut Parser<'a>) -> Result<Ast, String> {
    Err(p.make_error("Invalid parser function call!"))
}

fn parse_precedence<'a>(p: &mut Parser<'a>, precedence: Prec) -> Result<Ast, String> {
    p.advance();

    let prefix_rule = get_rule(p.previous.token_type.clone()).prefix;
    if prefix_rule as usize == nil_func as usize {
        return Err(p.make_error("Expected prefix expression"));
    }

    p.prefix_node = prefix_rule(p)?;

    while precedence as u8 <= get_rule(p.current.token_type.clone()).precedence as u8 {
        p.advance();
        let infix_rule = get_rule(p.previous.token_type.clone()).infix;
        if infix_rule as usize == nil_func as usize {
            return Err(p.make_error("Expected infix expression"));
        }

        p.prefix_node = infix_rule(p)?;
    }

    Ok(p.prefix_node.clone())
}

fn expression<'a>(p: &mut Parser<'a>)  -> Result<Ast, String> {
    parse_precedence(p, Prec::Assignment)
}

fn literal<'a>(p: &mut Parser<'a>)  -> Result<Ast, String> {
    match p.previous.token_type {
        TokenType::Number => Ok(Ast::Number(if let TokenData::Number(n) = &p.previous.data { n.clone() } else { 0.0 })),
        TokenType::String => Ok(Ast::String(if let TokenData::String(s) = &p.previous.data { s.clone() } else { "Error".to_string() })),
        _ => Err(p.make_error("Unreachable error for literal()"))
    }
}

fn unary<'a>(p: &mut Parser<'a>)  -> Result<Ast, String> {
    let op = p.previous.token_type.clone();

    let expr = parse_precedence(p, Prec::Unary)?;

    Ok(Ast::Unary(match op {
        TokenType::Minus => UnaryOperation::Negate,
        TokenType::Bang => UnaryOperation::Not,
        _ => return Err(p.make_error("Invalid unary operator"))
    }, Box::new(expr)))
}

fn binary<'a>(p: &mut Parser<'a>)  -> Result<Ast, String> {
    let left = p.prefix_node.clone();
    let op = p.previous.token_type.clone();
    let right = parse_precedence(p, get_rule(op.clone()).precedence)?;
    Ok(Ast::Binary(match op {
        TokenType::Plus => BinaryOperation::Add,
        TokenType::Minus => BinaryOperation::Subtract, 
        TokenType::Star => BinaryOperation::Multiply,
        TokenType::Slash => BinaryOperation::Divide,

        TokenType::Less => BinaryOperation::Less,
        TokenType::LessEqual => BinaryOperation::LessEqual,
        TokenType::Greater => BinaryOperation::Greater,
        TokenType::GreaterEqual => BinaryOperation::GreaterEqual,
        TokenType::EqualEqual => BinaryOperation::Equals,
        TokenType::Equal => BinaryOperation::Assign,

        _ => return Err(p.make_error("Invalid binary operator"))
    }, Box::new(left), Box::new(right)))
}

fn grouping<'a>(p: &mut Parser<'a>)  -> Result<Ast, String> {
    let expr = expression(p)?;
    p.consume(TokenType::RParen, "Expected right parenthesis")?;
    Ok(Ast::Grouping(Box::new(expr)))
}