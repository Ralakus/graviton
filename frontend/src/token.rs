use super::Position;

/// An enum with the type of token it is, it's not a varient since any data can be attached to any token type
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
    KwModule,
    KwLet,
    KwExtern,
    KwMut,
    KwPub,

    KwIf,
    KwElse,
    KwLoop,
    KwWhile,
    KwFor,
    KwBreak,
    KwContinue,

    KwTrue,
    KwFalse,
    KwNil,

    KwAs,

    Err,
    Eof,
}

/// A data packet for a token that the lexer attaches to it
#[derive(Debug, Clone)]
pub enum TokenData<'a> {
    None,
    Integer(isize),
    Float(f64),
    Str(&'a str),
    String(String),
}

/// The token struct, contains the token type, any attached data, and the code position it was found
#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub type_: TokenType,
    pub data: TokenData<'a>,
    pub pos: Position,
}

impl<'a> Token<'a> {
    /// Creates a new token
    #[must_use]
    pub const fn new(type_: TokenType, data: TokenData<'a>, pos: Position) -> Self {
        Self { type_, data, pos }
    }
}

impl<'a> Default for Token<'a> {
    /// Default empty token, Eof with no data
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
