//! Demonstrate custom error handling with Regal.

use regal::{IncrementalError, TokenCache};
use regal_macros::RegalLexer;
use std::num::ParseIntError;

const MAX_TOKENS: usize = 512;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
enum LexingError {
    InvalidInteger(String),
    NonAsciiCharacter(char),
    #[default]
    Other,
}

impl From<ParseIntError> for LexingError {
    fn from(err: ParseIntError) -> Self {
        use std::num::IntErrorKind::*;
        match err.kind() {
            PosOverflow | NegOverflow => LexingError::InvalidInteger("overflow error".to_owned()),
            _ => LexingError::InvalidInteger("other error".to_owned()),
        }
    }
}

#[derive(RegalLexer, Copy, Clone, Debug, PartialEq, Eq, Default)]
enum TokenKind {
    #[default]
    #[regex(r"[ \t]+", skip)]
    Trivia,
    #[regex(r"[A-Za-z]+")]
    Word,
    #[regex(r"[0-9]+")]
    Integer,
    #[regex(r"[^\x00-\x7F]")]
    NonAscii,
}

#[derive(Debug, PartialEq)]
enum Token {
    Word,
    Integer(u8),
}

fn lex(input: &str) -> Result<Vec<Result<Token, LexingError>>, LexingError> {
    let compiled = TokenKind::lexer();
    let mut cache: TokenCache<TokenKind, MAX_TOKENS> = TokenCache::new();
    match cache.rebuild(compiled, input) {
        Ok(Some(_partial)) => return Err(LexingError::Other),
        Ok(None) => {}
        Err(IncrementalError::TokenOverflow) => return Err(LexingError::Other),
        Err(_) => return Err(LexingError::Other),
    }

    let mut output = Vec::new();
    for record in cache.tokens() {
        if record.skipped {
            continue;
        }
        let slice = &input[record.start..record.end];
        match record.token {
            TokenKind::Word => output.push(Ok(Token::Word)),
            TokenKind::Integer => {
                let value = slice.parse::<u8>().map_err(LexingError::from)?;
                output.push(Ok(Token::Integer(value)));
            }
            TokenKind::NonAscii => {
                if let Some(ch) = slice.chars().next() {
                    output.push(Err(LexingError::NonAsciiCharacter(ch)));
                } else {
                    output.push(Err(LexingError::Other));
                }
            }
            TokenKind::Trivia => {}
        }
    }
    Ok(output)
}

fn main() {
    let stream = lex("Hello 256 Jérome").expect("lexing should succeed");
    let mut iter = stream.into_iter();

    assert_eq!(iter.next(), Some(Ok(Token::Word)));
    assert_eq!(
        iter.next(),
        Some(Err(LexingError::InvalidInteger(
            "overflow error".to_owned()
        )))
    );
    assert_eq!(iter.next(), Some(Ok(Token::Word)));
    assert_eq!(iter.next(), Some(Err(LexingError::NonAsciiCharacter('é'))));
    assert_eq!(iter.next(), Some(Ok(Token::Word)));
    assert_eq!(iter.next(), None);

    println!("custom error demonstration passed");
}
