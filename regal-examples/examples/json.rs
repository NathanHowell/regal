//! JSON parser example built on Regal tokens.
//!
//! Usage (from workspace root):
//!     cargo run -p regal-examples --example json -- regal-examples/examples/example.json

use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use regal::{IncrementalError, TokenCache};
use regal_macros::RegalLexer;
use std::collections::HashMap;
use std::env;
use std::fmt;
use std::fs;
use std::ops::Range;

const MAX_TOKENS: usize = 16 * 1024;
type Span = Range<usize>;
type Error = (String, Span);
type Result<T> = std::result::Result<T, Error>;

#[derive(RegalLexer, Copy, Clone, Debug, PartialEq, Eq, Default)]
enum TokenKind {
    #[default]
    #[regex(r"[ \t\r\n\f]+", skip)]
    Trivia,
    #[token("false")]
    False,
    #[token("true")]
    True,
    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("null")]
    Null,
    #[regex(r"-?[0-9]+")]
    #[regex(r"-?[0-9]+\.[0-9]+")]
    #[regex(r"-?[0-9]+[eE][+-]?[0-9]+")]
    #[regex(r"-?[0-9]+\.[0-9]+[eE][+-]?[0-9]+")]
    Number,
    #[regex(r#""[^"\\]*""#)]
    String,
}

#[derive(Debug, Clone)]
enum Token {
    Bool(bool),
    BraceOpen,
    BraceClose,
    BracketOpen,
    BracketClose,
    Colon,
    Comma,
    Null,
    Number(f64),
    String(String),
}

#[derive(Debug)]
enum Value {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => f.write_str("null"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Array(items) => {
                f.write_str("[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                f.write_str("]")
            }
            Value::Object(map) => {
                f.write_str("{")?;
                for (i, (k, v)) in map.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "\"{}\": {}", k, v)?;
                }
                f.write_str("}")
            }
        }
    }
}

#[derive(Debug, Clone)]
struct SpannedToken {
    token: Token,
    span: Span,
}

struct TokenCursor<'src> {
    tokens: &'src [SpannedToken],
    index: usize,
    source_len: usize,
    last_span: Span,
}

impl<'src> TokenCursor<'src> {
    fn new(tokens: &'src [SpannedToken], source_len: usize) -> Self {
        Self {
            tokens,
            index: 0,
            source_len,
            last_span: 0..0,
        }
    }

    fn next(&mut self) -> Option<&'src SpannedToken> {
        let token = self.tokens.get(self.index)?;
        self.last_span = token.span.clone();
        self.index += 1;
        Some(token)
    }

    fn peek(&self) -> Option<&'src SpannedToken> {
        self.tokens.get(self.index)
    }

    fn span(&self) -> Span {
        if self.index == 0 {
            self.source_len..self.source_len
        } else {
            self.last_span.clone()
        }
    }
}

fn lex(source: &str) -> Result<Vec<SpannedToken>> {
    let compiled = TokenKind::lexer();
    let mut cache: TokenCache<TokenKind, MAX_TOKENS> = TokenCache::new();
    match cache.rebuild(compiled, source) {
        Ok(Some(partial)) => Err((
            "unterminated token".into(),
            partial.start..partial.start + partial.fragment.len(),
        )),
        Ok(None) => {
            let mut output = Vec::new();
            for record in cache.tokens() {
                if record.skipped {
                    continue;
                }
                let span = record.start..record.end;
                let slice = &source[span.clone()];
                let token = match record.token {
                    TokenKind::False => Token::Bool(false),
                    TokenKind::True => Token::Bool(true),
                    TokenKind::BraceOpen => Token::BraceOpen,
                    TokenKind::BraceClose => Token::BraceClose,
                    TokenKind::BracketOpen => Token::BracketOpen,
                    TokenKind::BracketClose => Token::BracketClose,
                    TokenKind::Colon => Token::Colon,
                    TokenKind::Comma => Token::Comma,
                    TokenKind::Null => Token::Null,
                    TokenKind::Number => {
                        let value = slice.parse::<f64>().map_err(|_| {
                            (format!("invalid number literal: {slice}"), span.clone())
                        })?;
                        Token::Number(value)
                    }
                    TokenKind::String => Token::String(slice.to_owned()),
                    TokenKind::Trivia => continue,
                };
                output.push(SpannedToken { token, span });
            }
            Ok(output)
        }
        Err(IncrementalError::Reject { offset, span }) => Err((
            format!(
                "unexpected token starting at byte {offset}: {:?}",
                &source[offset..offset + span]
            ),
            offset..offset + span,
        )),
        Err(IncrementalError::TokenOverflow) => Err((
            "input produced too many tokens".into(),
            source.len()..source.len(),
        )),
        Err(other) => Err((
            format!("lexing failed: {other:?}"),
            source.len()..source.len(),
        )),
    }
}

fn parse_value(cursor: &mut TokenCursor<'_>) -> Result<Value> {
    if let Some(span_token) = cursor.next() {
        match &span_token.token {
            Token::Bool(value) => Ok(Value::Bool(*value)),
            Token::BraceOpen => parse_object(cursor),
            Token::BracketOpen => parse_array(cursor),
            Token::Null => Ok(Value::Null),
            Token::Number(num) => Ok(Value::Number(*num)),
            Token::String(s) => Ok(Value::String(s.clone())),
            _ => Err((
                "unexpected token (expected value)".into(),
                span_token.span.clone(),
            )),
        }
    } else {
        Err(("expected value".into(), cursor.span()))
    }
}

fn parse_array(cursor: &mut TokenCursor<'_>) -> Result<Value> {
    let mut array = Vec::new();
    let open_span = cursor.span();
    let mut expect_value = true;

    while let Some(token) = cursor.peek() {
        match &token.token {
            Token::BracketClose if !expect_value => {
                cursor.next();
                return Ok(Value::Array(array));
            }
            Token::Comma if !expect_value => {
                cursor.next();
                expect_value = true;
            }
            _ if expect_value => {
                let value = parse_value(cursor)?;
                array.push(value);
                expect_value = false;
            }
            _ => {
                return Err((
                    "unexpected token (array context)".into(),
                    token.span.clone(),
                ));
            }
        }
    }

    Err(("unmatched '['".into(), open_span))
}

fn parse_object(cursor: &mut TokenCursor<'_>) -> Result<Value> {
    let mut map = HashMap::new();
    let open_span = cursor.span();
    let mut expect_key = true;

    while let Some(token) = cursor.peek() {
        match &token.token {
            Token::BraceClose if !expect_key => {
                cursor.next();
                return Ok(Value::Object(map));
            }
            Token::Comma if !expect_key => {
                cursor.next();
                expect_key = true;
            }
            Token::String(key) if expect_key => {
                cursor.next(); // consume key
                match cursor.next() {
                    Some(next) if matches!(next.token, Token::Colon) => {}
                    Some(other) => {
                        return Err(("expected ':' after key".into(), other.span.clone()));
                    }
                    None => return Err(("expected ':' after key".into(), cursor.span())),
                }
                let value = parse_value(cursor)?;
                map.insert(key.trim_matches('"').to_owned(), value);
                expect_key = false;
            }
            _ => {
                return Err((
                    "unexpected token (object context)".into(),
                    token.span.clone(),
                ));
            }
        }
    }

    Err(("unmatched '{'".into(), open_span))
}

fn main() {
    let filename = env::args().nth(1).expect("usage: json <path/to/file>");
    let source = fs::read_to_string(&filename).expect("failed to read file");

    let tokens = match lex(&source) {
        Ok(tokens) => tokens,
        Err((msg, span)) => {
            report(&filename, &source, (msg, span));
            std::process::exit(1);
        }
    };

    let mut cursor = TokenCursor::new(&tokens, source.len());
    match parse_value(&mut cursor) {
        Ok(value) => {
            if let Some(extra) = cursor.peek() {
                report(
                    &filename,
                    &source,
                    ("unexpected trailing token".into(), extra.span.clone()),
                );
                std::process::exit(1);
            } else {
                println!("{}", value);
            }
        }
        Err((msg, span)) => {
            report(&filename, &source, (msg, span));
            std::process::exit(1);
        }
    }
}

fn report(filename: &str, source: &str, (message, span): Error) {
    let mut colors = ColorGenerator::new();
    let color = colors.next();
    Report::build(ReportKind::Error, filename, span.start)
        .with_message("invalid JSON input")
        .with_label(
            Label::new((filename, span.clone()))
                .with_message(message)
                .with_color(color),
        )
        .finish()
        .eprint((filename, Source::from(source.to_owned())))
        .unwrap();
}
