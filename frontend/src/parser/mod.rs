use super::{
    ir::{ChannelIr, Instruction},
    signature::TypeSignature,
    {
        lexer::Lexer,
        token::{Token, TokenData, TokenType},
        Notice, NoticeLevel, Position,
    },
};

use mpsc::{Receiver, Sender};
use std::sync::{mpsc, Arc};

/// The subfunctions for parsing
mod functions;
/// The rule table for parsing
mod rules;

/// The amount in seconds the parser waits for source data before it throws an error
const PARSER_SOURCE_REQUEST_TIMEOUT: u64 = 10;

/// The index for the previos token in the parser
const PREVIOUS: usize = 0;
/// The index for the current token in the parser
const CURRENT: usize = 1;
/// The index for the lookahead token in the parser
const LOOKAHEAD: usize = 2;

/// An array of generally safe tokens to synchronize to for error handling
const SAFE_TOKENS: [TokenType; 6] = [
    TokenType::RCurly,
    TokenType::Semicolon,
    TokenType::KwIf,
    TokenType::KwLet,
    TokenType::KwStruct,
    TokenType::Eof,
];

/// An array of closing tokens, synchronize will consume these tokens
const CLOSING_TOKENS: [TokenType; 4] = [
    TokenType::RBracket,
    TokenType::RCurly,
    TokenType::RParen,
    TokenType::Semicolon,
];

/// An enum stating a declaration type for a let statement
#[derive(PartialEq)]
pub(crate) enum DeclarationType {
    Struct,
    Function,
    Other,
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum LoopType {
    While,
    Loop,
}

/// The parser struct, contains all of the data necessary to parse
pub struct Parser<'a> {
    /// Name of the current module being parsed
    pub(crate) name: String,

    /// Lexer
    pub(crate) lex: Lexer<'a>,

    /// Channel to send notices though
    pub(crate) notice_tx: Sender<Option<Notice>>,

    /// Channel to send ir through
    pub(crate) ir_tx: Sender<Option<ChannelIr>>,

    /// Token stack, 0 is previous, 1 is the current token, 2 is the first look ahead, 3 is the second look ahead
    pub(crate) tokens: [Token<'a>; 3],

    /// A stack to see what type of loop the parser is in
    pub(crate) loop_stack: Vec<LoopType>,

    /// An integer that keeps track of how many function's it's in
    pub(crate) nested_fn_count: u8,

    /// Last declaration type, used for let statements to find the correct instruction to output
    pub(crate) last_declaration: DeclarationType,

    /// Recives source data
    pub(crate) source_rx: &'a Receiver<Option<Arc<str>>>,

    /// Sends request for a source file
    pub(crate) source_request_tx: Sender<Option<String>>,
}

impl<'a> Parser<'a> {
    /// Creates a new parser
    pub(crate) fn new(
        name: String,
        source: &'a str,
        source_rx: &'a Receiver<Option<Arc<str>>>,
        source_request_tx: Sender<Option<String>>,
        notice_tx: Sender<Option<Notice>>,
        ir_tx: Sender<Option<ChannelIr>>,
    ) -> Self {
        Parser {
            name,
            lex: Lexer::new(source),
            notice_tx,
            ir_tx,
            tokens: [Token::default(), Token::default(), Token::default()],
            loop_stack: Vec::with_capacity(8),
            nested_fn_count: 0,
            last_declaration: DeclarationType::Other,
            source_rx,
            source_request_tx,
        }
    }

    /// Advances the lexer to the next token
    #[inline]
    pub(crate) fn advance(&mut self) {
        self.tokens[PREVIOUS] = self.tokens[CURRENT].clone();
        self.tokens[CURRENT] = self.tokens[LOOKAHEAD].clone();

        if let Some(tok) = self.lex.next() {
            self.tokens[LOOKAHEAD] = tok;
        } else {
            self.tokens[LOOKAHEAD] =
                Token::new(TokenType::Eof, TokenData::None, Position::new(0, 0));
        }
    }

    /// Returns the lookahead token
    #[inline]
    pub(crate) const fn lookahead(&self) -> &Token<'a> {
        &self.tokens[2]
    }

    /// Returns the current token the parser is on
    #[inline]
    pub(crate) const fn current(&self) -> &Token<'a> {
        &self.tokens[CURRENT]
    }

    /// Returns the previous token that was already passed
    #[inline]
    pub(crate) const fn previous(&self) -> &Token<'a> {
        &self.tokens[PREVIOUS]
    }

    /// Check if the current token is of type, returns true if so and vice versa
    #[inline]
    pub(crate) fn check(&mut self, type_: TokenType) -> bool {
        self.current().type_ == type_
    }

    /// Check if the current token is of type, returns true and comsumes the token if so and false and does nothing otherwise
    #[inline]
    pub(crate) fn check_consume(&mut self, type_: TokenType) -> bool {
        if self.current().type_ == type_ {
            self.advance();
            true
        } else {
            false
        }
    }

    /// If the current token matches `type_`, advances and returns the token's data, otherwise return an error
    #[inline]
    pub(crate) fn consume(
        &mut self,
        type_: TokenType,
        err_msg: &'static str,
    ) -> Result<&TokenData, ()> {
        if self.current().type_ == type_ {
            self.advance();
            Ok(&self.previous().data)
        } else {
            self.emit_notice_current(
                NoticeLevel::Error,
                format!(
                    "{} => Found {:?}, expected {:?}",
                    err_msg,
                    self.current().type_,
                    type_
                ),
            );
            Err(())
        }
    }

    /// Makes an IR instruction with a custom position and sends it through the channel
    #[inline]
    pub(crate) fn emit_ir(&mut self, pos: Position, sig: TypeSignature, ins: Instruction) {
        if ins == Instruction::FunctionEnd || ins == Instruction::ExternFn {
            self.last_declaration = DeclarationType::Function;
        } else if ins == Instruction::StructEnd {
            self.last_declaration = DeclarationType::Struct;
        } else {
            self.last_declaration = DeclarationType::Other;
        }

        let ir = ChannelIr { pos, sig, ins };

        if let Err(e) = self.ir_tx.send(Some(ir)) {
            eprintln!(
                "{}Parser ir send error: {}{}",
                common::ansi::Fg::BrightRed,
                e,
                common::ansi::Fg::Reset
            );
        }
    }

    /// Makes an IR instruction and sends it through the channel
    #[inline]
    pub(crate) fn emit_ir_previous(&mut self, sig: TypeSignature, ins: Instruction) {
        self.emit_ir(self.previous().pos, sig, ins)
    }

    /// Makes an IR instruction and sends it through the channel
    #[inline]
    pub(crate) fn emit_ir_current(&mut self, sig: TypeSignature, ins: Instruction) {
        self.emit_ir(self.current().pos, sig, ins)
    }

    /// Makes a notice at a position
    #[inline]
    pub(crate) fn emit_notice(&mut self, pos: Position, level: NoticeLevel, msg: String) {
        if level == NoticeLevel::Error {
            if let Err(e) = self.ir_tx.send(Some(ChannelIr {
                pos,
                sig: TypeSignature::None,
                ins: Instruction::Halt,
            })) {
                eprintln!(
                    "{}Parser notice send error: {}{}",
                    common::ansi::Fg::BrightRed,
                    e,
                    common::ansi::Fg::Reset
                );
            }
        }

        let notice = Notice::new("Parser".to_string(), msg, pos, self.name.clone(), level);

        if let Err(e) = self.notice_tx.send(Some(notice)) {
            eprintln!(
                "{}Parser notice send error: {}{}",
                common::ansi::Fg::BrightRed,
                e,
                common::ansi::Fg::Reset
            );
        }
    }

    /// Makes a notice at previous position
    #[inline]
    pub(crate) fn emit_notice_previous(&mut self, level: NoticeLevel, msg: String) {
        self.emit_notice(self.previous().pos, level, msg)
    }

    /// Makes a notice at current position
    #[inline]
    pub(crate) fn emit_notice_current(&mut self, level: NoticeLevel, msg: String) {
        self.emit_notice(self.current().pos, level, msg)
    }

    /// For errror recovery, skips until the next generally 'safe' token and any other tokens specified
    pub(crate) fn synchronize(&mut self, tokens: &[TokenType]) {
        while tokens.iter().filter(|t| self.check(**t)).count()
            + SAFE_TOKENS.iter().filter(|t| self.check(**t)).count()
            == 0
        {
            self.advance();
        }
        if tokens
            .iter()
            .filter(|t| CLOSING_TOKENS.contains(t) && self.check(**t))
            .count()
            == 1
            || self.current().type_ == TokenType::RCurly
            || self.current().type_ == TokenType::Semicolon
        {
            self.advance();
        }
    }

    fn send_end_signal(
        notice_tx: Sender<Option<Notice>>,
        ir_tx: Sender<Option<ChannelIr>>,
        source_request_tx: Sender<Option<String>>,
    ) {
        if let Err(e) = ir_tx.send(None) {
            eprintln!(
                "{}Parser ir send error: {}{}",
                common::ansi::Fg::BrightRed,
                e,
                common::ansi::Fg::Reset
            );
        }

        if let Err(e) = notice_tx.send(None) {
            eprintln!(
                "{}Parser notice send error: {}{}",
                common::ansi::Fg::BrightRed,
                e,
                common::ansi::Fg::Reset
            );
        }

        if let Err(e) = source_request_tx.send(None) {
            eprintln!(
                "{}Parser source request send error: {}{}",
                common::ansi::Fg::BrightRed,
                e,
                common::ansi::Fg::Reset
            );
        }
    }

    fn recieve_source(source_rx: &'a Receiver<Option<Arc<str>>>) -> Result<Arc<str>, ()> {
        match source_rx.recv_timeout(std::time::Duration::from_secs(
            PARSER_SOURCE_REQUEST_TIMEOUT,
        )) {
            Ok(Some(source)) => Ok(source),
            Ok(None) => Err(()),
            Err(e) => {
                eprintln!(
                    "{}Parser failed to receive source data within {} seconds: {}{}",
                    common::ansi::Fg::BrightRed,
                    PARSER_SOURCE_REQUEST_TIMEOUT,
                    e,
                    common::ansi::Fg::Reset
                );
                Err(())
            }
        }
    }

    fn request_source(&self, name: String) -> Result<Arc<str>, ()> {
        if let Err(e) = self.source_request_tx.send(Some(name)) {
            eprintln!(
                "{}Parser failed to send source request: {}{}",
                common::ansi::Fg::BrightRed,
                e,
                common::ansi::Fg::Reset
            );
        }

        Parser::recieve_source(self.source_rx)
    }

    pub(crate) fn parse_imported_module(&mut self, file_name: String) -> Result<(), ()> {
        let source = self.request_source(file_name.clone())?;

        let mut p = Parser::new(
            file_name,
            &source,
            self.source_rx,
            self.source_request_tx.clone(),
            self.notice_tx.clone(),
            self.ir_tx.clone(),
        );

        // Fill the look ahead with tokens
        p.advance();
        p.advance();

        functions::module(&mut p)
    }

    /// Creates the parser thread from source data
    pub async fn create(
        name: String,
        source_rx: Receiver<Option<Arc<str>>>,
        source_request_tx: Sender<Option<String>>,
        notice_tx: Sender<Option<Notice>>,
        ir_tx: Sender<Option<ChannelIr>>,
    ) {
        let source_rx = source_rx;

        let source = match Parser::recieve_source(&source_rx) {
            Ok(s) => s,
            Err(_) => {
                Parser::send_end_signal(notice_tx, ir_tx, source_request_tx);
                return;
            }
        };

        let mut p = Parser::new(
            name,
            &source,
            &source_rx,
            source_request_tx.clone(),
            notice_tx.clone(),
            ir_tx.clone(),
        );

        // Fill the look ahead with tokens
        p.advance();
        p.advance();

        if functions::module(&mut p).is_ok() {}

        Parser::send_end_signal(notice_tx, ir_tx, source_request_tx);
    }
}
