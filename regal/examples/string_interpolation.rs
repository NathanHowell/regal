//! Simple string interpolation using Regal-derived lexers.

use regal::{IncrementalError, TokenCache};
use regal_macros::RegalLexer;
use std::collections::HashMap;

const MAX_TOKENS: usize = 2048;

#[derive(RegalLexer, Copy, Clone, Debug, PartialEq, Eq, Default)]
enum DefToken {
    #[default]
    #[regex(r"[ \t]+", skip)]
    Trivia,
    #[token("\n")]
    Newline,
    #[regex(r"[A-Za-z][A-Za-z0-9_]*")]
    Identifier,
    #[token("=")]
    Equals,
    #[token("'")]
    Quote,
    #[regex(r"[^'\n]+")]
    Body,
}

#[derive(Debug)]
struct SpannedToken {
    kind: DefToken,
    span: core::ops::Range<usize>,
}

fn lex(source: &str) -> Result<Vec<SpannedToken>, String> {
    let compiled = DefToken::lexer();
    let mut cache: TokenCache<DefToken, MAX_TOKENS> = TokenCache::new();
    match cache.rebuild(compiled, source) {
        Ok(Some(partial)) => Err(format!(
            "unterminated token starting at byte {}",
            partial.start
        )),
        Ok(None) => Ok(cache
            .tokens()
            .iter()
            .filter(|record| !record.skipped)
            .map(|record| SpannedToken {
                kind: record.token,
                span: record.start..record.end,
            })
            .collect()),
        Err(IncrementalError::TokenOverflow) => Err("script produced too many tokens".into()),
        Err(other) => Err(format!("lexing failed: {other:?}")),
    }
}

fn interpolate(raw: &str, symbols: &HashMap<String, String>) -> Result<String, String> {
    let mut result = String::new();
    let mut chars = raw.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '$' {
            match chars.peek() {
                Some('{') => {
                    chars.next();
                    let mut ident = String::new();
                    while let Some(next) = chars.next() {
                        if next == '}' {
                            break;
                        }
                        ident.push(next);
                    }
                    let name = ident.trim();
                    if name.is_empty() {
                        return Err("empty interpolation".into());
                    }
                    match symbols.get(name) {
                        Some(value) => result.push_str(value),
                        None => return Err(format!("unknown variable '{name}'")),
                    }
                }
                _ => result.push('$'),
            }
        } else {
            result.push(ch);
        }
    }
    Ok(result)
}

fn evaluate(source: &str) -> Result<HashMap<String, String>, String> {
    let tokens = lex(source)?;
    let mut index = 0usize;
    let mut symbols = HashMap::new();

    while index < tokens.len() {
        match tokens[index].kind {
            DefToken::Newline => {
                index += 1;
            }
            DefToken::Identifier => {
                let name_span = tokens[index].span.clone();
                let name = source[name_span].to_string();
                index += 1;

                if index >= tokens.len() || tokens[index].kind != DefToken::Equals {
                    return Err(format!("expected '=' after identifier '{name}'"));
                }
                index += 1;

                if index >= tokens.len() || tokens[index].kind != DefToken::Quote {
                    return Err(format!("expected opening quote after '{name}'"));
                }
                index += 1;

                if index >= tokens.len() || tokens[index].kind != DefToken::Body {
                    return Err(format!("expected string body for '{name}'"));
                }
                let body_span = tokens[index].span.clone();
                let body = &source[body_span];
                index += 1;

                if index >= tokens.len() || tokens[index].kind != DefToken::Quote {
                    return Err(format!("expected closing quote after '{name}'"));
                }
                index += 1;

                let value = interpolate(body, &symbols)?;
                symbols.insert(name, value);
            }
            other => {
                return Err(format!(
                    "unexpected token {:?} at byte {}",
                    other, tokens[index].span.start
                ));
            }
        }
    }

    Ok(symbols)
}

fn main() {
    let script = "\
name = 'Mark'\n\
greeting = 'Hi ${name}!'\n\
surname = 'Scott'\n\
greeting2 = 'Hi ${name} ${surname}!'\n\
";

    match evaluate(script) {
        Ok(symbols) => {
            for (key, value) in symbols {
                println!("{key} = {value}");
            }
        }
        Err(err) => {
            eprintln!("error: {err}");
            std::process::exit(1);
        }
    }
}
