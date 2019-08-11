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
    //KwFn,
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

pub use ast::Position;

#[derive(Debug, Clone)]
pub enum TokenData {
    None,
    String(String),
    Number(f64),
    Str(&'static str),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub type_: TokenType,
    pub data: TokenData,
    pub pos: Position,
}

impl Token {
    pub fn new(type_: TokenType, data: TokenData, pos: Position) -> Self {
        Token { type_, data, pos }
    }
}

impl ToString for Token {
    fn to_string(&self) -> String {
        format!(
            "{:?}, {:?}, line: {}, col: {}",
            self.type_, self.data, self.pos.line, self.pos.col
        )
    }
}
