use super::{
    token::{Token, TokenData, TokenType},
    Position,
};

use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    static ref IDENT_MAP: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("and", TokenType::KwAnd);
        m.insert("or", TokenType::KwOr);

        m.insert("self", TokenType::KwSelf);
        m.insert("struct", TokenType::KwStruct);
        m.insert("return", TokenType::KwReturn);
        m.insert("import", TokenType::KwImport);
        m.insert("extern", TokenType::KwExtern);
        m.insert("mut", TokenType::KwMut);

        m.insert("if", TokenType::KwIf);
        m.insert("else", TokenType::KwElse);
        m.insert("while", TokenType::KwWhile);
        m.insert("for", TokenType::KwFor);
        m.insert("break", TokenType::KwBreak);

        m.insert("true", TokenType::KwTrue);
        m.insert("false", TokenType::KwFalse);

        m.insert("nil", TokenType::KwNil);
        m.insert("let", TokenType::KwLet);

        m.insert("as", TokenType::KwAs);

        m
    };
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    full_source: &'a str,
    source: Option<&'a str>,
    start_pos: Position,
    pos: Position,
    idx: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            full_source: source,
            source: Some(source),
            start_pos: Position::new(1, 1),
            pos: Position::new(1, 1),
            idx: 0,
        }
    }

    fn advance(&mut self) -> Option<char> {
        match self.source {
            Some(src) => {
                let mut chars = src.chars();
                let c = chars.next();
                self.source = Some(chars.as_str());
                self.idx += 1;
                self.pos.col += 1;
                c
            }
            None => None,
        }
    }

    fn peek(&self) -> Option<char> {
        match self.source {
            Some(src) => src.chars().next(),
            None => None,
        }
    }

    fn skip_white_space(&mut self) {
        while match self.peek() {
            Some(c) if c == '\n' => {
                self.pos.line += 1;
                self.pos.col = 0;
                true
            }
            Some(c) if c.is_whitespace() => true,
            _ => false,
        } {
            self.advance();
        }
        self.start_pos = self.pos;
    }

    fn check_ident(ident: &str) -> TokenType {
        match IDENT_MAP.get(ident) {
            Some(token_type) => *token_type,
            None => TokenType::Identifier,
        }
    }

    pub fn get_tok(&mut self) -> Option<Token<'a>> {
        self.skip_white_space();
        match self.advance() {
            Some(c) => match c {
                '(' => Some(Token::new(
                    TokenType::LParen,
                    TokenData::None,
                    self.start_pos,
                )),
                ')' => Some(Token::new(
                    TokenType::RParen,
                    TokenData::None,
                    self.start_pos,
                )),
                '{' => Some(Token::new(
                    TokenType::LCurly,
                    TokenData::None,
                    self.start_pos,
                )),
                '}' => Some(Token::new(
                    TokenType::RCurly,
                    TokenData::None,
                    self.start_pos,
                )),
                '[' => Some(Token::new(
                    TokenType::RBracket,
                    TokenData::None,
                    self.start_pos,
                )),
                ']' => Some(Token::new(
                    TokenType::LBracket,
                    TokenData::None,
                    self.start_pos,
                )),

                ',' => Some(Token::new(
                    TokenType::Comma,
                    TokenData::None,
                    self.start_pos,
                )),
                '.' => Some(Token::new(TokenType::Dot, TokenData::None, self.start_pos)),
                ';' => Some(Token::new(
                    TokenType::Semicolon,
                    TokenData::None,
                    self.start_pos,
                )),
                /*':' => match self.peek() {
                    Some('=') => {
                        self.advance();
                        Some(Token::new(
                            TokenType::ColonEqual,
                            TokenData::None,
                            self.start_pos,
                        ))
                    }
                    _ => Some(Token::new(
                        TokenType::Colon,
                        TokenData::None,
                        self.start_pos,
                    )),
                },*/
                ':' => Some(Token::new(
                    TokenType::Colon,
                    TokenData::None,
                    self.start_pos,
                )),

                '+' => Some(Token::new(TokenType::Plus, TokenData::None, self.start_pos)),
                '-' => match self.peek() {
                    Some('>') => {
                        self.advance();
                        Some(Token::new(
                            TokenType::RArrow,
                            TokenData::None,
                            self.start_pos,
                        ))
                    }
                    _ => Some(Token::new(
                        TokenType::Minus,
                        TokenData::None,
                        self.start_pos,
                    )),
                },
                '*' => Some(Token::new(TokenType::Star, TokenData::None, self.start_pos)),
                '/' => match self.peek() {
                    Some('/') => {
                        self.advance();
                        while let Some(c) = self.peek() {
                            if c == '\n' {
                                break;
                            } else {
                                self.advance();
                            }
                        }
                        self.get_tok()
                    }
                    _ => Some(Token::new(
                        TokenType::Slash,
                        TokenData::None,
                        self.start_pos,
                    )),
                },

                '!' => match self.peek() {
                    Some('=') => {
                        self.advance();
                        Some(Token::new(
                            TokenType::BangEqual,
                            TokenData::None,
                            self.start_pos,
                        ))
                    }
                    _ => Some(Token::new(TokenType::Bang, TokenData::None, self.start_pos)),
                },
                '=' => match self.peek() {
                    Some('=') => {
                        self.advance();
                        Some(Token::new(
                            TokenType::EqualEqual,
                            TokenData::None,
                            self.start_pos,
                        ))
                    }
                    _ => Some(Token::new(
                        TokenType::Equal,
                        TokenData::None,
                        self.start_pos,
                    )),
                },
                '<' => match self.peek() {
                    Some('=') => {
                        self.advance();
                        Some(Token::new(
                            TokenType::LessEqual,
                            TokenData::None,
                            self.start_pos,
                        ))
                    }
                    _ => Some(Token::new(TokenType::Less, TokenData::None, self.start_pos)),
                },
                '>' => match self.peek() {
                    Some('=') => {
                        self.advance();
                        Some(Token::new(
                            TokenType::GreaterEqual,
                            TokenData::None,
                            self.start_pos,
                        ))
                    }
                    _ => Some(Token::new(
                        TokenType::Greater,
                        TokenData::None,
                        self.start_pos,
                    )),
                },

                '\"' => {
                    let start_idx = self.idx;
                    while let Some(c) = self.peek() {
                        if c != '\"' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    self.advance();

                    if self.peek().is_none() {
                        return Some(Token::new(
                            TokenType::Err,
                            TokenData::String("Unterminated string".to_string()),
                            self.start_pos,
                        ));
                    }

                    let slice = match self.full_source.get(start_idx..self.idx - 1) {
                        Some(s) => s,
                        None => {
                            return Some(Token::new(
                                TokenType::Err,
                                TokenData::String("Failed to index into source".to_string()),
                                self.start_pos,
                            ))
                        }
                    };

                    Some(Token::new(
                        TokenType::String,
                        TokenData::Str(slice),
                        self.start_pos,
                    ))
                }

                l if l.is_alphabetic() || l == '_' => {
                    let start_idx = self.idx;
                    while let Some(c) = self.peek() {
                        if c.is_alphabetic() || c.is_digit(10) || c == '_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    let slice = match self.full_source.get(start_idx - 1..self.idx) {
                        Some(s) => s,
                        None => {
                            return Some(Token::new(
                                TokenType::Err,
                                TokenData::String("Failed to index into source".to_string()),
                                self.start_pos,
                            ))
                        }
                    };

                    let ident_type = Lexer::check_ident(slice);
                    if ident_type == TokenType::Identifier {
                        Some(Token::new(
                            TokenType::Identifier,
                            TokenData::Str(slice),
                            self.start_pos,
                        ))
                    } else {
                        Some(Token::new(ident_type, TokenData::None, self.start_pos))
                    }
                }

                d if d.is_digit(10) => {
                    let start_idx = self.idx;
                    let mut is_float = false;
                    while let Some(c) = self.peek() {
                        if c == '.' {
                            is_float = true;
                            self.advance();
                        } else if c.is_digit(10) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    let slice = match self.full_source.get(start_idx - 1..self.idx) {
                        Some(s) => s,
                        None => {
                            return Some(Token::new(
                                TokenType::Err,
                                TokenData::String("Failed to index into source".to_string()),
                                self.start_pos,
                            ))
                        }
                    };
                    Some(Token::new(
                        TokenType::Number,
                        if is_float {
                            TokenData::Float(match slice.parse::<f64>() {
                                Ok(f) => f,
                                Err(e) => {
                                    return Some(Token::new(
                                        TokenType::Err,
                                        TokenData::String(format!(
                                            "Failed parse float form source: {}",
                                            e
                                        )),
                                        self.start_pos,
                                    ))
                                }
                            })
                        } else {
                            TokenData::Integer(match slice.parse::<i64>() {
                                Ok(f) => f,
                                Err(e) => {
                                    return Some(Token::new(
                                        TokenType::Err,
                                        TokenData::String(format!(
                                            "Failed parse integer form source: {}",
                                            e
                                        )),
                                        self.start_pos,
                                    ))
                                }
                            })
                        },
                        self.start_pos,
                    ))
                }
                _ => Some(Token::new(
                    TokenType::Err,
                    TokenData::Str("Unexpected character"),
                    self.start_pos,
                )),
            },
            None => None,
        }
    }
}
