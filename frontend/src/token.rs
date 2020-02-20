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
    String(String),
    Integer(i64),
    Float(f64),
    Str(&'a str),
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

impl<'a> ToString for Token<'a> {
    fn to_string(&self) -> String {
        format!(
            "{:?}, {:?}, line: {}, col: {}",
            self.type_, self.data, self.pos.line, self.pos.col
        )
    }
}
