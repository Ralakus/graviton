use super::Position;

#[repr(u8)]
#[derive(Debug, PartialEq, Clone, Copy, Hash, Eq)]
pub enum TokenType {
    LParen,
    RParen,
    LCurly,
    RCurly,
    LBracket,
    RBracket,

    Comma,
    Dot,
    Semicolon,

    Plus,
    Minus,
    Star,
    Slash,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Colon,
    RArrow,

    Identifier,
    String,
    Number,

    KwAnd,
    KwOr,

    KwSelf,
    KwStruct,
    KwReturn,
    KwImport,
    KwLet,
    KwExtern,
    KwMut,

    KwIf,
    KwElse,
    KwWhile,
    KwFor,
    KwBreak,

    KwTrue,
    KwFalse,
    KwNil,

    KwAs,

    Err,
    Eof,
}

#[derive(Debug, Clone)]
pub enum TokenData<'a> {
    None,
    Integer(i64),
    Float(f64),
    Str(&'a str),
    String(String),
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub type_: TokenType,
    pub data: TokenData<'a>,
    pub pos: Position,
}

impl<'a> Token<'a> {
    pub fn new(type_: TokenType, data: TokenData<'a>, pos: Position) -> Self {
        Token { type_, data, pos }
    }
}

impl<'a> Default for Token<'a> {
    fn default() -> Self {
        Token {
            type_: TokenType::Eof,
            data: TokenData::None,
            pos: Position::new(0, 0),
        }
    }
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}, {:?}, line: {}, col: {}",
            self.type_, self.data, self.pos.line, self.pos.col
        )
    }
}
