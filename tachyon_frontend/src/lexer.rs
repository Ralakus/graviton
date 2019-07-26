use super::tokens::{TokenType, Token, TokenData, Position};
use std::iter::Peekable;
use std::str::Chars;


pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    start_pos: Position,
    pos: Position
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source: source.chars().peekable(),
            start_pos: Position{line: 1, col: 1},
            pos: Position{line: 1, col: 1}
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.pos.col += 1;
        self.source.next()
    }

    fn peek(&mut self) -> Option<char> {
        match self.source.peek() {
            Some(&c) => Some(c),
            None => None
        }
    }

    fn skip_whitespace(&mut self) {
        while match self.peek() {
            Some(c) if c == '\n' => {
                self.pos.line += 1;
                self.pos.col = 1;
                true
            }
            Some(c) if c.is_whitespace() => {
                true
            }
            _ => false
        } {
            self.advance();
        }
        self.start_pos = self.pos;
    }

    fn check_ident(ident: &String) -> TokenType {
        match ident.as_str() {
            "and" => TokenType::KwAnd,
            "or" => TokenType::KwOr,
            "self" => TokenType::KwSelf,
            "struct" => TokenType::KwStruct,
            "return" => TokenType::KwReturn,
            "import" => TokenType::KwImport,
            "let" => TokenType::KwLet,
            "def" => TokenType::KwDef,

            "if" => TokenType::KwIf,
            "else" => TokenType::KwElse,
            "while" => TokenType::KwWhile,
            "for" => TokenType::KwFor,
            "break" => TokenType::KwBreak,

            "true" => TokenType::KwTrue,
            "false" => TokenType::KwFalse,

            "nil" => TokenType::KwNil,
            
            _ => TokenType::Identifier
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.skip_whitespace();
        match self.advance() {
            None => None,
            Some('(') => Some(Token::new(TokenType::LParen, TokenData::None, self.start_pos)),
            Some(')') => Some(Token::new(TokenType::RParen, TokenData::None, self.start_pos)),
            Some('{') => Some(Token::new(TokenType::LCurly, TokenData::None, self.start_pos)),
            Some('}') => Some(Token::new(TokenType::RCurly, TokenData::None, self.start_pos)),
            Some('[') => Some(Token::new(TokenType::RBracket, TokenData::None, self.start_pos)),
            Some(']') => Some(Token::new(TokenType::LBracket, TokenData::None, self.start_pos)),

            Some(',') => Some(Token::new(TokenType::Comma, TokenData::None, self.start_pos)),
            Some('.') => Some(Token::new(TokenType::Dot, TokenData::None, self.start_pos)),
            Some(';') => Some(Token::new(TokenType::Semicolon, TokenData::None, self.start_pos)),
            Some(':') => Some(Token::new(TokenType::Colon, TokenData::None, self.start_pos)),

            Some('+') => Some(Token::new(TokenType::Plus, TokenData::None, self.start_pos)),
            Some('-') => Some(Token::new(TokenType::Minus, TokenData::None, self.start_pos)),
            Some('*') => Some(Token::new(TokenType::Star, TokenData::None, self.start_pos)),
            Some('/') => Some(Token::new(TokenType::Slash, TokenData::None, self.start_pos)),

            Some('!') => match self.peek() {
                Some('=') => {
                    self.advance();
                    Some(Token::new(TokenType::BangEqual, TokenData::None, self.start_pos))
                }
                _ => Some(Token::new(TokenType::Bang, TokenData::None, self.start_pos))
            }
            Some('=') => match self.peek() {
                Some('=') => {
                    self.advance();
                    Some(Token::new(TokenType::EqualEqual, TokenData::None, self.start_pos))
                }
                _ => Some(Token::new(TokenType::Equal, TokenData::None, self.start_pos))
            }
            Some('<') => match self.peek() {
                Some('=') => {
                    self.advance();
                    Some(Token::new(TokenType::LessEqual, TokenData::None, self.start_pos))
                }
                _ => Some(Token::new(TokenType::Less, TokenData::None, self.start_pos))
            }
            Some('>') => match self.peek() {
                Some('=') => {
                    self.advance();
                    Some(Token::new(TokenType::GreaterEqual, TokenData::None, self.start_pos))
                }
                _ => Some(Token::new(TokenType::Greater, TokenData::None, self.start_pos))
            }

            Some('\"') => {
                let mut literal = String::new();
                while let Some(c) = self.advance() {
                    if c == '\"' {
                        self.advance();
                        return Some(Token::new(TokenType::String, TokenData::String(literal), self.start_pos));
                    } else {
                        literal.push(c);
                    }
                }
                Some(Token::new(TokenType::Err, TokenData::Str("Unterminated String"), self.start_pos))
            }

            Some(c) if c.is_digit(10) => {
                let mut literal = c.to_string();
                while let Some(c) = self.peek() {
                    if c.is_digit(10) {
                        literal.push(c);
                    } else {
                        break;
                    }
                    self.advance();
                }
                Some(Token::new(TokenType::Number, TokenData::Number(literal.parse::<f64>().unwrap()), self.start_pos))
            },
            Some(c) if c.is_alphabetic() => {
                let mut literal = c.to_string();
                while let Some(c) = self.peek() {
                    if c.is_alphabetic() || c.is_digit(10){
                        literal.push(c);
                    } else {
                        break;
                    }
                    self.advance();
                }
                let ident_type = Lexer::check_ident(&literal);
                if ident_type == TokenType::Identifier {
                    Some(Token::new(ident_type, TokenData::String(literal), self.start_pos))
                } else {
                    Some(Token::new(ident_type, TokenData::None, self.start_pos))
                }
            },
            _ => Some(Token::new(TokenType::Err, TokenData::Str("Unexpected character"), self.start_pos))
        }
    }
}