
use super::lexer::Lexer;
use super::tokens::{TokenType, Token, TokenData, Position};
use super::ast::{Ast, AstNode, UnaryOperation, BinaryOperation, TypeSignature, VariableSignature, FunctionSignature};
use memmap::Mmap;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub pos: Position,
    pub msg: String,
    pub file: Option<String>
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

type ParseFn = fn(&mut Parser) -> Result<AstNode, ParseError>;

#[derive(Clone)]
struct ParseRule {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Prec
}

const PARSER_RULE_TABLE: [ParseRule; 44] = [
ParseRule{prefix: grouping,   infix: call,     precedence: Prec::Call       }, // TokenType::LParen
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
ParseRule{prefix: return_,    infix: nil_func, precedence: Prec::None       }, // TokenType::KwReturn
ParseRule{prefix: import,     infix: nil_func, precedence: Prec::None       }, // TokenType::KwImport
ParseRule{prefix: let_,       infix: nil_func, precedence: Prec::None       }, // TokenType::KwLet
ParseRule{prefix: fn_,        infix: nil_func, precedence: Prec::None       }, // TokenType::KwFn
ParseRule{prefix: nil_func,   infix: nil_func, precedence: Prec::None       }, // TokenType::KwMut
ParseRule{prefix: if_,        infix: nil_func, precedence: Prec::None       }, // TokenType::KwIf
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

    file_name: Option<&'a str>,

    prefix_node: AstNode,
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
            Err(self.make_error_with_string(format!("{}; found {:?}", err_msg, self.current.type_)))
        }
    }

    fn check(&mut self, type_: TokenType) -> bool {
        self.current.type_ == type_
    }

    pub fn parse(source: &'a str, file_name: Option<&'a str>) -> Result<AstNode, Vec<ParseError>> {
        let mut p = Parser {
            lex: Lexer::new(source),
            current: Token::new(TokenType::Eof, TokenData::None, Position{line: -1, col:-1}),
            previous: Token::new(TokenType::Eof, TokenData::None, Position{line: -1, col:-1}),
            errors: Vec::new(),

            file_name,

            prefix_node: AstNode {
                node: Ast::Block(Vec::new()),

                #[cfg(feature = "node_code_pos")]
                pos: Position { line: 1, col: 1 }
            }
        };

        p.advance();
        let mut exprs: Vec<AstNode> = Vec::new();
        while !p.check(TokenType::Eof) {
            match maybe_statement_else_expression(&mut p) {
                Ok(ast) => exprs.push(ast),
                _ => (),
            }
        }
        let _ = p.consume(TokenType::Eof, "Expected EOF");

        if p.errors.len() > 0 {
            return Err(p.errors);
        }

        Ok(p.new_node(Ast::Block(exprs)))
    }

    fn make_error(&mut self, msg: &'static str) -> ParseError {
        let error = ParseError {
            pos: self.previous.pos.clone(),
            msg: msg.to_string(),
            file: if let Some(name) = self.file_name { Some(name.to_string()) } else { None }
        };
        self.errors.push(error.clone());
        error
    }

    fn make_error_with_string(&mut self, msg: String) -> ParseError {
        let error = ParseError {
            pos: self.previous.pos.clone(),
            msg: msg,
            file: if let Some(name) = self.file_name { Some(name.to_string()) } else { None }
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

    fn new_node(&self, ast: Ast) -> AstNode {
        AstNode {
            node: ast,

            #[cfg(feature = "node_code_pos")]
            pos: self.previous.pos.clone()
        }
    }

}

fn nil_func<'a>(p: &mut Parser<'a>) -> Result<AstNode, ParseError> {
    Err(p.make_error("Invalid parser function call!"))
}

fn variable_signature<'a>(p: &mut Parser<'a>) -> Result<VariableSignature, ParseError> {
    p.consume(TokenType::Identifier, "Expected identifier for name")?;
    let name = match &p.previous.data {
            TokenData::String(s) => s.clone(),
            _ => return Err(p.make_error("Could not read identifier name from token"))
        };
    
    let type_sig: Option<TypeSignature> = 
    if p.check(TokenType::Colon) {
        p.advance();
        p.consume(TokenType::Identifier, "Expected identifier for type")?;
        Some(TypeSignature::new(match &p.previous.data {
                TokenData::String(s) => s.as_str(),
                _ => return Err(p.make_error("Could not read identifier name from token"))
            }))
    } else {
        None
    };

    Ok(VariableSignature{name, type_sig})
}

fn parse_precedence<'a>(p: &mut Parser<'a>, precedence: Prec) -> Result<AstNode, ParseError> {
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

fn maybe_statement_else_expression<'a>(p: &mut Parser<'a>) -> Result<AstNode, ParseError> {

    let expr = expression(p)?;
    if p.check(TokenType::Semicolon) {
        p.advance();
        Ok(p.new_node(Ast::Statement(Box::new(expr))))
    } else {
        Ok(expr)
    }
}

/*fn statement<'a>(p: &mut Parser<'a>) -> Result<AstNode, ParseError> {
    let is_return = if p.check(TokenType::KwReturn) {
        p.advance();
        true
    } else {
        false
    };

    let expr = expression(p)?;
    p.consume(TokenType::Semicolon, "Expected \';\' to end expression to make into statement")?;
    if !is_return { Ok(p.new_node(Ast::Statement(Box::new(expr)))) } else { Ok(p.new_node(Ast::Return(Box::new(expr)))) }
}*/

fn expression<'a>(p: &mut Parser<'a>) -> Result<AstNode, ParseError> {
    parse_precedence(p, Prec::Assignment)
}

fn literal<'a>(p: &mut Parser<'a>) -> Result<AstNode, ParseError> {
    match p.previous.type_ {
        TokenType::Number => Ok(p.new_node(Ast::Number(if let TokenData::Number(n) = &p.previous.data { n.clone() } else { 0.0 }))),
        TokenType::String => Ok(p.new_node(Ast::String(if let TokenData::String(s) = &p.previous.data { s.clone() } else { "err".to_string() }))),
        TokenType::KwTrue => Ok(p.new_node(Ast::Bool(true))),
        TokenType::KwFalse => Ok(p.new_node(Ast::Bool(false))),
        _ => Err(p.make_error("Unreachable error for literal()"))
    }
}

fn identifier<'a>(p: &mut Parser<'a>) -> Result<AstNode, ParseError> {
    let name = match p.previous.type_ {
            TokenType::Identifier => if let TokenData::String(s) = &p.previous.data { s.clone() } else { "err".to_string() },
            _ => return Err(p.make_error("Unreachable error for identifier()"))
        };
    Ok(p.new_node(Ast::Identifier(name)))
}

fn unary<'a>(p: &mut Parser<'a>) -> Result<AstNode, ParseError> {
    let op = p.previous.type_.clone();

    let expr = parse_precedence(p, Prec::Unary)?;

    Ok(p.new_node(Ast::Unary(match op {
        TokenType::Minus => UnaryOperation::Negate,
        TokenType::Bang => UnaryOperation::Not,
        _ => return Err(p.make_error("Invalid unary operator"))
    }, Box::new(expr))))
}

fn binary<'a>(p: &mut Parser<'a>) -> Result<AstNode, ParseError> {
    let left = p.prefix_node.clone();
    let op = p.previous.type_.clone();
    let right = parse_precedence(p, get_rule(op.clone()).precedence)?;
    Ok(p.new_node(Ast::Binary(match op {
        TokenType::Plus => BinaryOperation::Add,
        TokenType::Minus => BinaryOperation::Subtract, 
        TokenType::Star => BinaryOperation::Multiply,
        TokenType::Slash => BinaryOperation::Divide,

        TokenType::Less => BinaryOperation::Less,
        TokenType::LessEqual => BinaryOperation::LessEqual,
        TokenType::Greater => BinaryOperation::Greater,
        TokenType::GreaterEqual => BinaryOperation::GreaterEqual,
        TokenType::EqualEqual => BinaryOperation::Equal,
        TokenType::BangEqual => BinaryOperation::NotEqual,
        TokenType::Equal => BinaryOperation::Assign,

        _ => return Err(p.make_error("Invalid binary operator"))
    }, Box::new(left), Box::new(right))))
}

fn grouping<'a>(p: &mut Parser<'a>) -> Result<AstNode, ParseError> {
    let expr = expression(p)?;
    p.consume(TokenType::RParen, "Expected closing right parenthesis")?;
    Ok(expr)
}

fn return_<'a>(p: &mut Parser<'a>) -> Result<AstNode, ParseError> {
    let expr = expression(p)?;
    Ok(p.new_node(Ast::Return(Box::new(expr))))
}

fn block<'a>(p: &mut Parser<'a>) -> Result<AstNode, ParseError> {
    let mut expr_vec: Vec<AstNode> = Vec::new();
    while !p.check(TokenType::RCurly) {
        expr_vec.push(maybe_statement_else_expression(p)?);
    }
    p.consume(TokenType::RCurly, "Expected closing right curly bracket")?;
    Ok(p.new_node(Ast::Block(expr_vec)))
}

fn if_<'a>(p: &mut Parser<'a>)  -> Result<AstNode, ParseError> {
    let if_cond = expression(p)?;
    let if_block = expression(p)?;
    
    let mut else_if_exprs: Vec<(Box<AstNode>, Box<AstNode>)> = Vec::new();
    let mut else_block: Option<Box<AstNode>> = None;

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

    Ok(p.new_node(Ast::IfElse(Box::new(if_cond), Box::new(if_block), else_if_exprs, else_block)))
}

fn while_<'a>(p: &mut Parser<'a>) -> Result<AstNode, ParseError> {
    let cond = expression(p)?;
    let body = expression(p)?;
    Ok(p.new_node(Ast::While(Box::new(cond), Box::new(body))))
}

fn let_<'a>(p: &mut Parser<'a>) -> Result<AstNode, ParseError> {
    let mut mutable = false;
    if p.check(TokenType::KwMut) {
        p.advance();
        mutable = true;
    }

    let var_sig = variable_signature(p)?;
    
    let val_expr: Option<Box<AstNode>> = 
        if p.check(TokenType::Equal) {
            p.advance();
            Some(Box::new(expression(p)?))
        } else {
            None
        };

    Ok(p.new_node(Ast::Let(var_sig, mutable, val_expr)))

}

fn import<'a>(p: &mut Parser<'a>) -> Result<AstNode, ParseError> {
    p.consume(TokenType::String, "Expected string for import file")?;
    let name = match &p.previous.data {
            TokenData::String(s) => s.clone(),
            _ => return Err(p.make_error("Could not read identifier name from token"))
        };

    let mapped_file: memmap::Mmap;

    let file = if let Ok(f) = std::fs::File::open(name.clone()) { f } else { return Err(p.make_error_with_string(format!("Failed to open file {}", name))) };
    mapped_file = unsafe { if let Ok(mf) = Mmap::map(&file) { mf } else { return Err(p.make_error_with_string(format!("Failed to map file {}", name))) } };

    let result = Parser::parse(
        if let Ok(s) = std::str::from_utf8(&mapped_file[..]) { &s }
        else {
            return Err(p.make_error_with_string(format!("Failed to convert file {} to utf8", name)));
        }, Some(&*name));

    match result {
        Ok(ast) => Ok(p.new_node(Ast::Import(name, Box::new(ast)))),
        Err(errors) => {
            let mut e = errors.clone();
            p.errors.append(&mut e);
            Err(p.make_error_with_string(format!("Failed to parse file {}", name)))
        }
    }
}

fn fn_<'a>(p: &mut Parser<'a>) -> Result<AstNode, ParseError> {
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

    if !p.check(TokenType::RParen) {

        params.push(variable_signature(p)?);

        while p.check(TokenType::Comma) {
            p.advance();
            params.push(variable_signature(p)?);
        }

    }

    p.consume(TokenType::RParen, "Expected right parenthesis to close function params")?;

    let type_sig: Option<TypeSignature> = 
        if p.check(TokenType::Colon) {
            p.advance();
            p.consume(TokenType::Identifier, "Expected type identifier")?;
            Some(TypeSignature::new(match &p.previous.data {
                    TokenData::String(s) => s.as_str(),
                    _ => return Err(p.make_error("Could not read identifier name from token"))
                }))
        } else {
            None
        };

    let body = expression(p)?;

    Ok(p.new_node(Ast::FnDef(name, FunctionSignature { params, return_type: type_sig }, Box::new(body))))
}

fn call<'a>(p: &mut Parser<'a>) -> Result<AstNode, ParseError> {

    let callee = p.prefix_node.clone();

    let mut args: Vec<AstNode> = Vec::new();
    if !p.check(TokenType::RParen) {
        args.push(expression(p)?);

        while p.check(TokenType::Comma) {
            p.advance();
            args.push(expression(p)?);
        }
    }

    p.consume(TokenType::RParen, "Expected right parenthesis to close function call arguments")?;

    Ok(p.new_node(Ast::FnCall(Box::new(callee), args)))

}