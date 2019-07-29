
use super::lexer::Lexer;
use super::tokens::{TokenType, Token, TokenData, Position};
use super::ast::{Ast, UnaryOperation, BinaryOperation, TypeSignature, VariableSignature, FunctionSignature};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub pos: Position,
    pub msg: String,
}

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

type ParseFn = fn(&mut Parser) -> Result<Ast, ParseError>;

#[derive(Clone)]
struct ParseRule {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Prec
}

const PARSER_RULE_TABLE: [ParseRule; 44] = [
ParseRule{prefix: grouping,   infix: nil_func, precedence: Prec::Call       }, // TokenType::LParen
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::RParen
ParseRule{prefix: block,      infix: nil_func, precedence: Prec::None       }, // TokenType::LCurly
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::RCurly
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::LBracket
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::RBracket
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::Comma
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::Dot
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::Semicolon
ParseRule{prefix: nil_func,   infix: binary,   precedence: Prec::Term       }, // TokenType::Plus
ParseRule{prefix: unary,      infix: binary,   precedence: Prec::Term       }, // TokenType::Minus
ParseRule{prefix: nil_func,   infix: binary,   precedence: Prec::Factor     }, // TokenType::Star
ParseRule{prefix: nil_func,   infix: binary,   precedence: Prec::Factor     }, // TokenType::Slash
ParseRule{prefix: nil_func,   infix: binary,   precedence: Prec::None       }, // TokenType::Bang
ParseRule{prefix: nil_func,   infix: binary,   precedence: Prec::Comparison }, // TokenType::BangEqual
ParseRule{prefix: nil_func,   infix: binary,   precedence: Prec::Assignment }, // TokenType::Equal
ParseRule{prefix: nil_func,   infix: binary,   precedence: Prec::Equality   }, // TokenType::EqualEqual
ParseRule{prefix: nil_func,   infix: binary,   precedence: Prec::Comparison }, // TokenType::Greater
ParseRule{prefix: nil_func,   infix: binary,   precedence: Prec::Comparison }, // TokenType::GreaterEqual
ParseRule{prefix: nil_func,   infix: binary,   precedence: Prec::Comparison }, // TokenType::Less
ParseRule{prefix: nil_func,   infix: binary,   precedence: Prec::Comparison }, // TokenType::LessEqual
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::Colon
ParseRule{prefix: identifier, infix: nil_func, precedence: Prec::None       }, // TokenType::Identifier
ParseRule{prefix: literal,    infix: nil_func, precedence: Prec::None       }, // TokenType::String
ParseRule{prefix: literal,    infix: nil_func, precedence: Prec::None       }, // TokenType::Number
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::KwAnd
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::KwOr
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::KwSelf
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::KwStruct
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::KwReturn
ParseRule{prefix: import,     infix: nil_func, precedence: Prec::None       }, // TokenType::KwImport
ParseRule{prefix: let_,       infix: nil_func, precedence: Prec::None       }, // TokenType::KwLet
ParseRule{prefix: fn_,        infix: nil_func, precedence: Prec::None       }, // TokenType::KwFn
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::KwMut
ParseRule{prefix: if_else,    infix: nil_func, precedence: Prec::None       }, // TokenType::KwIf
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::KwElse
ParseRule{prefix: while_,     infix: nil_func, precedence: Prec::None       }, // TokenType::KwWhile
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::KwFor
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::KwBreak
ParseRule{prefix: literal,    infix: nil_func, precedence: Prec::None       }, // TokenType::KwTrue
ParseRule{prefix: literal,    infix: nil_func, precedence: Prec::None       }, // TokenType::KwFalse
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::KwNil
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::Err
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::Eof
];

fn get_rule(type_: TokenType) -> &'static ParseRule {
    &PARSER_RULE_TABLE[type_ as usize]
}


pub struct Parser<'a> {
    lex: Lexer<'a>,
    current: Token,
    previous: Token,
    errors: Vec<ParseError>,

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

    fn consume(&mut self, type_: TokenType, err_msg: &'static str) -> Result<(), ParseError> {
        if self.current.type_ == type_ {
            Ok(self.advance())
        } else {
            Err(self.make_error(err_msg))
        }
    }

    fn check(&mut self, type_: TokenType) -> bool {
        self.current.type_ == type_
    }

    pub fn parse(source: &'a str) -> Result<Ast, Vec<ParseError>> {
        let mut p = Parser {
            lex: Lexer::new(source),
            current: Token::new(TokenType::Eof, TokenData::None, Position{line: -1, col:-1}),
            previous: Token::new(TokenType::Eof, TokenData::None, Position{line: -1, col:-1}),
            errors: Vec::new(),

            prefix_node: Ast::Block(Vec::new())
        };

        p.advance();
        let mut exprs: Vec<Ast> = Vec::new();
        while !p.check(TokenType::Eof) {
            match expression(&mut p) {
                Ok(ast) => exprs.push(ast),
                _ => (),
            }
        }
        let _ = p.consume(TokenType::Eof, "Expected EOF");

        if p.errors.len() > 0 {
            return Err(p.errors);
        }

        Ok(Ast::Block(exprs))
    }

    fn make_error(&mut self, msg: &'static str) -> ParseError {
        let error = ParseError {
            pos: self.previous.pos.clone(),
            msg: msg.to_string()
        };
        self.errors.push(error.clone());
        error
    }

    // For error recovery 
    fn synchronize(&mut self) {
        while !(self.check(TokenType::RCurly) ||
                self.check(TokenType::KwFn) ||
                self.check(TokenType::KwStruct) ||
                self.check(TokenType::KwLet) || 
                self.check(TokenType::KwWhile) ||
                self.check(TokenType::KwReturn) || 
                self.check(TokenType::Eof)) {
            self.advance();
            if self.check(TokenType::Semicolon) {
                self.advance();
                break;
            }
        }
    }

}

fn get_variable_signature<'a>(p: &mut Parser<'a>) -> Result<VariableSignature, ParseError> {
    p.consume(TokenType::Identifier, "Expected identifier for name")?;
    let name = match &p.previous.data {
            TokenData::String(s) => s.clone(),
            _ => return Err(p.make_error("Could not read identifier name from token"))
        };
    
    let type_sig: Option<TypeSignature> = 
    if p.check(TokenType::Colon) {
        p.advance();
        p.consume(TokenType::Identifier, "Expected identifier for variable type")?;
        Some(TypeSignature { name: match &p.previous.data {
            TokenData::String(s) => s.clone(),
            _ => return Err(p.make_error("Could not read identifier name from token"))
            }})
    } else {
        None
    };

    Ok(VariableSignature{name, type_sig})
}

fn nil_func<'a>(p: &mut Parser<'a>) -> Result<Ast, ParseError> {
    Err(p.make_error("Invalid parser function call!"))
}

fn parse_precedence<'a>(p: &mut Parser<'a>, precedence: Prec) -> Result<Ast, ParseError> {
    p.advance();

    let prefix_rule = get_rule(p.previous.type_.clone()).prefix;
    if prefix_rule as usize == nil_func as usize {
        return Err(p.make_error("Expected prefix expression"));
    }

    let prefix_node = prefix_rule(p);
    match prefix_node {
        Ok(ast) => p.prefix_node = ast,
        Err(_) => p.synchronize()
    }

    while precedence as u8 <= get_rule(p.current.type_.clone()).precedence as u8 {
        p.advance();
        let infix_rule = get_rule(p.previous.type_.clone()).infix;
        if infix_rule as usize == nil_func as usize {
            return Err(p.make_error("Expected infix expression"));
        }

        let infix_node = infix_rule(p);
        match infix_node {
            Ok(ast) => p.prefix_node = ast,
            Err(_) => p.synchronize()
        }
    }

    Ok(p.prefix_node.clone())
}

fn expression<'a>(p: &mut Parser<'a>)  -> Result<Ast, ParseError> {
    parse_precedence(p, Prec::Assignment)
}

fn literal<'a>(p: &mut Parser<'a>)  -> Result<Ast, ParseError> {
    match p.previous.type_ {
        TokenType::Number => Ok(Ast::Number(if let TokenData::Number(n) = &p.previous.data { n.clone() } else { 0.0 })),
        TokenType::String => Ok(Ast::String(if let TokenData::String(s) = &p.previous.data { s.clone() } else { "err".to_string() })),
        TokenType::KwTrue => Ok(Ast::Bool(true)),
        TokenType::KwFalse => Ok(Ast::Bool(false)),
        _ => Err(p.make_error("Unreachable error for literal()"))
    }
}

fn identifier<'a>(p: &mut Parser<'a>)  -> Result<Ast, ParseError> {
    let name = match p.previous.type_ {
            TokenType::Identifier => if let TokenData::String(s) = &p.previous.data { s.clone() } else { "err".to_string() },
            _ => return Err(p.make_error("Unreachable error for identifier()"))
        };
    if p.check(TokenType::LParen) {
        p.advance();

        
        let mut args: Vec<Ast> = Vec::new();
        while !p.check(TokenType::RParen) {
            args.push(expression(p)?);

            if p.check(TokenType::Comma) {
                p.advance();
            } else {
                break;
            }
        }

        p.consume(TokenType::RParen, "Expected closing right parenthesis")?;

        Ok(Ast::FnCall(name, args))
    } else {
        Ok(Ast::Identifier(name))
    }
}

fn unary<'a>(p: &mut Parser<'a>)  -> Result<Ast, ParseError> {
    let op = p.previous.type_.clone();

    let expr = parse_precedence(p, Prec::Unary)?;

    Ok(Ast::Unary(match op {
        TokenType::Minus => UnaryOperation::Negate,
        TokenType::Bang => UnaryOperation::Not,
        _ => return Err(p.make_error("Invalid unary operator"))
    }, Box::new(expr)))
}

fn binary<'a>(p: &mut Parser<'a>)  -> Result<Ast, ParseError> {
    let left = p.prefix_node.clone();
    let op = p.previous.type_.clone();
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

fn grouping<'a>(p: &mut Parser<'a>)  -> Result<Ast, ParseError> {
    let expr = expression(p)?;
    p.consume(TokenType::RParen, "Expected closing right parenthesis")?;
    Ok(expr)
}

fn block<'a>(p: &mut Parser<'a>)  -> Result<Ast, ParseError> {
    let mut expr_vec: Vec<Ast> = Vec::new();
    while !p.check(TokenType::RCurly) {
        expr_vec.push(expression(p)?);
    }
    p.consume(TokenType::RCurly, "Expected closing right curly bracket")?;
    Ok(Ast::Block(expr_vec))
}

fn if_else<'a>(p: &mut Parser<'a>)  -> Result<Ast, ParseError> {
    let if_cond = expression(p)?;
    let if_block = expression(p)?;
    
    let mut else_if_exprs: Vec<(Box<Ast>, Box<Ast>)> = Vec::new();
    let mut else_block: Option<Box<Ast>> = None;

    while p.check(TokenType::KwElse) {
        p.advance();
        if p.check(TokenType::KwIf) {
            p.advance();
            else_if_exprs.push((Box::new(expression(p)?), Box::new(expression(p)?)));
        } else {
            else_block = Some(Box::new(expression(p)?));
            break;
        }
    }

    Ok(Ast::IfElse(Box::new(if_cond), Box::new(if_block), else_if_exprs, else_block))
}

fn while_<'a>(p: &mut Parser<'a>)  -> Result<Ast, ParseError> {
    Ok(Ast::While(Box::new(expression(p)?), Box::new(expression(p)?)))
}

fn let_<'a>(p: &mut Parser<'a>)  -> Result<Ast, ParseError> {
    let mut mutable = false;
    if p.check(TokenType::KwMut) {
        p.advance();
        mutable = true;
    }

    let var_sig = get_variable_signature(p)?;
    
    let val_expr: Option<Box<Ast>> = 
        if p.check(TokenType::Equal) {
            p.advance();
            Some(Box::new(expression(p)?))
        } else {
            None
        };

    if p.check(TokenType::Semicolon) {
        p.advance();
        return Ok(Ast::Statement(Box::new(Ast::Let(var_sig, mutable, val_expr))));
    }

    Ok(Ast::Let(var_sig, mutable, val_expr))

}

fn import<'a>(p: &mut Parser<'a>)  -> Result<Ast, ParseError> {
    p.consume(TokenType::String, "Expected string for import file")?;
    let name = match &p.previous.data {
            TokenData::String(s) => s.clone(),
            _ => return Err(p.make_error("Could not read identifier name from token"))
        };
    if p.check(TokenType::Semicolon) {
        p.advance();
        return Ok(Ast::Statement(Box::new(Ast::Import(name))));
    }

    Ok(Ast::Import(name))
}

fn fn_<'a>(p: &mut Parser<'a>)  -> Result<Ast, ParseError> {
    let name: Option<String> =
        if p.check(TokenType::Identifier) {
            p.advance();
            match &p.previous.data {
                TokenData::String(s) => Some(s.clone()),
                _ => return Err(p.make_error("Could not read identifier name from token"))
            }
        } else {
            None
        };

    p.consume(TokenType::LParen, "Expected left parenthesis for function params")?;

    let mut params: Vec<VariableSignature> = Vec::new();
    while !p.check(TokenType::RParen) {
        params.push(get_variable_signature(p)?);
        
        if p.check(TokenType::Comma) {
            p.advance();
        }
    }

    p.consume(TokenType::RParen, "Expected right parenthesis to close function params")?;

    let type_sig: Option<TypeSignature> = 
        if p.check(TokenType::Colon) {
            p.advance();
            p.consume(TokenType::Identifier, "Expected type identifier")?;
            Some(TypeSignature { name: match &p.previous.data {
                    TokenData::String(s) => s.clone(),
                    _ => return Err(p.make_error("Could not read identifier name from token"))
                }})
        } else {
            None
        };


    Ok(Ast::FnDef(FunctionSignature { name, params, return_type: type_sig }, Box::new(expression(p)?)))
}