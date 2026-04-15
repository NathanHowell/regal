//! Track line and column information for each word token.
//!
//! Usage:
//!     cargo run --example extras -- <path/to/file>

use regal::{IncrementalError, TokenCache};
use regal_macros::RegalLexer;
use std::env;
use std::fs;

const MAX_TOKENS: usize = 16 * 1024;

#[derive(RegalLexer, Copy, Clone, Debug, PartialEq, Eq, Default)]
enum TokenKind {
    #[default]
    #[regex(r"[ \t]+", skip)]
    Trivia,
    #[token("\n")]
    Newline,
    #[regex(r"\w+")]
    Word,
}

fn lex(source: &str) -> Result<Vec<(TokenKind, core::ops::Range<usize>)>, String> {
    let compiled = TokenKind::lexer();
    let mut cache: TokenCache<TokenKind, MAX_TOKENS> = TokenCache::new();
    match cache.rebuild(compiled, source) {
        Ok(Some(_partial)) => Err("unterminated token at EOF".into()),
        Ok(None) => Ok(cache
            .tokens()
            .iter()
            .filter(|record| !record.skipped)
            .map(|record| {
                (
                    record.token,
                    core::ops::Range {
                        start: record.start,
                        end: record.end,
                    },
                )
            })
            .collect()),
        Err(IncrementalError::TokenOverflow) => Err("input file produced too many tokens".into()),
        Err(other) => Err(format!("lexing failed: {other:?}")),
    }
}

fn main() {
    let path = env::args().nth(1).expect("usage: extras <path/to/file>");
    let source = fs::read_to_string(&path).expect("failed to read file");

    let tokens = match lex(&source) {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("error: {err}");
            std::process::exit(1);
        }
    };

    let mut line = 1usize;
    let mut line_start = 0usize;

    for (token, span) in tokens {
        match token {
            TokenKind::Newline => {
                line += 1;
                line_start = span.end;
            }
            TokenKind::Word => {
                let column = span.start - line_start;
                let lexeme = &source[span.clone()];
                println!("Word '{lexeme}' at line {line}, column {column}");
            }
            TokenKind::Trivia => {}
        }
    }
}
