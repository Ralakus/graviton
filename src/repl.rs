
use std::io::{BufRead,Write};
use tachyon::frontend::tokens::{Token, TokenType, TokenData, Position};
use super::errors;

use super::tachyon;

use super::colored::*;

pub fn repl(debug_level_in: i32) -> Result<(), String> {

    let mut debug_level = debug_level_in;

    let mut source = String::from("");
    let mut lex: tachyon::frontend::lexer::Lexer;

    'repl: loop {
        source.clear();
        print!("> "); std::io::stdout().flush().unwrap();
        std::io::stdin().lock().read_line(&mut source).expect("Error reading input");

        lex = tachyon::frontend::lexer::Lexer::new(source.as_str());

        if let Some(tok) = lex.next() {
            if tok.type_ == TokenType::Colon {
                if let Some(t) = lex.next() {
                    match t.data {
                        TokenData::String(s) => match s.as_str() {
                            "exit" => return Ok(()),
                            "debug" => {
                                let val = lex.next().unwrap_or(Token { type_: TokenType::Number, data: TokenData::Number(0.0), pos: Position { line: -1, col: -1} });
                                if let TokenData::Number(n) = val.data {
                                    debug_level = n as i32;
                                } else {
                                    debug_level = 0;
                                }
                            }
                            _ => println!("Invalid command {}", s),
                        },
                        _ => println!("Invalid token type {:?}", t.type_)
                    }
                } else {
                    println!("{}", "Expected argument after \":\"".red());
                }
                continue 'repl;
            }
        }

        let ast = tachyon::frontend::parser::Parser::parse(source.as_str());

        match ast {
            Ok(a) => {
                if debug_level >= 2 {
                    println!("{}\n{:#?}", "AST:".cyan(), a);
                }
            },
            Err(errors) => {
                for e in errors {
                    errors::report_error(&e, Some(source.as_str()), None);
                }
            },
        };
    }
}