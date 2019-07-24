

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
    KwDef,

    KwIf,
    KwElse,
    KwWhile,
    KwFor,
    KwBreak,

    KwTrue,
    KwFalse,
    KwNil,

    Err,
    Eof
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Position {
    pub line: i32,
    pub col: i32
}

#[derive(Debug, Clone)]
pub enum TokenData {
    None,
    String(String),
    Number(f64),
    Str(&'static str)
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub data: TokenData,
    pub pos: Position
}


impl Token {
    pub fn new(token_type: TokenType, data: TokenData, pos: Position) -> Self {
        Token {
            token_type,
            data,
            pos
        }
    }
}

impl ToString for Token {
    fn to_string(&self) -> String { 
        format!("{:?}, {:?}, line: {}, col: {}", self.token_type, self.data, self.pos.line, self.pos.col)
    }
}