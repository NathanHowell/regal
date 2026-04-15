//! Brainfuck interpreter powered by Regal.
//!
//! Usage:
//!     cargo run --example brainfuck -- examples/hello_world.bf

use regal::{IncrementalError, TokenCache};
use regal_macros::RegalLexer;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{self, Read};

const MAX_TOKENS: usize = 64 * 1024;

#[derive(RegalLexer, Copy, Clone, Debug, PartialEq, Eq, Default)]
#[allow(clippy::upper_case_acronyms)]
enum Op {
    #[default]
    #[regex(r"[^\[\]\+\-<>\.,]+", skip)]
    Trivia,
    #[token(">")]
    IncPointer,
    #[token("<")]
    DecPointer,
    #[token("+")]
    IncData,
    #[token("-")]
    DecData,
    #[token(".")]
    OutData,
    #[token(",")]
    InpData,
    #[token("[")]
    CondJumpForward,
    #[token("]")]
    CondJumpBackward,
}

fn lex_ops(source: &str) -> Result<Vec<Op>, String> {
    let compiled = Op::lexer();
    let mut cache: TokenCache<Op, MAX_TOKENS> = TokenCache::new();
    match cache.rebuild(compiled, source) {
        Ok(Some(partial)) => Err(format!(
            "unterminated token starting at byte {} (fragment {:?})",
            partial.start, partial.fragment
        )),
        Ok(None) => Ok(cache
            .tokens()
            .iter()
            .filter(|record| !record.skipped)
            .map(|record| record.token)
            .collect()),
        Err(IncrementalError::TokenOverflow) => {
            Err("program is too large for example buffer".into())
        }
        Err(other) => Err(format!("failed to lex Brainfuck source: {other:?}")),
    }
}

fn execute(code: &str) -> Result<(), String> {
    let ops = lex_ops(code)?;

    let mut data = [0u8; 30_000];
    let mut pointer: usize = 0;

    let mut forward = HashMap::new();
    let mut backward = HashMap::new();
    let mut stack = Vec::new();

    for (i, op) in ops.iter().enumerate() {
        match op {
            Op::CondJumpForward => stack.push(i),
            Op::CondJumpBackward => {
                let start = stack
                    .pop()
                    .ok_or_else(|| format!("unmatched ] operator at instruction {i}"))?;
                forward.insert(start, i);
                backward.insert(i, start);
            }
            _ => {}
        }
    }

    if !stack.is_empty() {
        return Err(format!("unmatched [ operator(s) at instructions {stack:?}"));
    }

    let mut pc = 0usize;
    while pc < ops.len() {
        match ops[pc] {
            Op::IncPointer => {
                pointer = pointer
                    .checked_add(1)
                    .filter(|idx| *idx < data.len())
                    .ok_or_else(|| "pointer moved past end of tape".to_owned())?;
            }
            Op::DecPointer => {
                if pointer == 0 {
                    return Err("pointer moved before start of tape".into());
                }
                pointer -= 1;
            }
            Op::IncData => data[pointer] = data[pointer].wrapping_add(1),
            Op::DecData => data[pointer] = data[pointer].wrapping_sub(1),
            Op::OutData => print!("{}", data[pointer] as char),
            Op::InpData => data[pointer] = read_byte()?,
            Op::CondJumpForward => {
                if data[pointer] == 0 {
                    pc = *forward
                        .get(&pc)
                        .ok_or_else(|| format!("missing ] to match [ at instruction {pc}"))?;
                }
            }
            Op::CondJumpBackward => {
                if data[pointer] != 0 {
                    pc = *backward
                        .get(&pc)
                        .ok_or_else(|| format!("missing [ to match ] at instruction {pc}"))?;
                }
            }
            Op::Trivia => {}
        }
        pc += 1;
    }

    Ok(())
}

fn read_byte() -> Result<u8, String> {
    let mut buf = [0u8; 1];
    io::stdin()
        .read_exact(&mut buf)
        .map_err(|err| format!("failed to read byte from stdin: {err}"))?;
    Ok(buf[0])
}

fn main() {
    let path = env::args().nth(1).expect("usage: brainfuck <path/to/file>");
    let program = fs::read_to_string(&path).expect("failed to read source file");
    if let Err(err) = execute(&program) {
        eprintln!("error: {err}");
        std::process::exit(1);
    }
}
