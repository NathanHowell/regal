//! Expression calculator using Regal for lexing and Chumsky for parsing.
//!
//! Usage (from workspace root):
//!     cargo run -p regal-examples --example calculator -- "1 + 7 * (3 - 4) / 2"

use chumsky::prelude::*;
use regal::{IncrementalError, TokenCache};
use regal_macros::RegalLexer;
use std::env;
use std::fmt;

const MAX_TOKENS: usize = 2048;

#[derive(RegalLexer, Copy, Clone, Debug, PartialEq, Eq, Default)]
enum TokenKind {
    #[default]
    #[regex(r"[ \t\n]+", skip)]
    Whitespace,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[regex(r"[0-9]+")]
    Integer,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,
    Integer(isize),
}

#[derive(Debug)]
enum LexError {
    TokenOverflow,
    UnexpectedEnd,
    InvalidInteger(String),
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexError::TokenOverflow => f.write_str("token cache overflow"),
            LexError::UnexpectedEnd => f.write_str("unexpected end of input"),
            LexError::InvalidInteger(msg) => write!(f, "invalid integer: {}", msg),
        }
    }
}

fn lex(input: &str) -> Result<Vec<Token>, LexError> {
    let compiled = TokenKind::lexer();
    let mut cache: TokenCache<TokenKind, MAX_TOKENS> = TokenCache::new();
    match cache.rebuild(compiled, input) {
        Ok(Some(_partial)) => Err(LexError::UnexpectedEnd),
        Ok(None) => {
            let mut tokens = Vec::new();
            for record in cache.tokens() {
                if record.skipped {
                    continue;
                }
                let slice = &input[record.start..record.end];
                let token = match record.token {
                    TokenKind::Plus => Token::Plus,
                    TokenKind::Minus => Token::Minus,
                    TokenKind::Multiply => Token::Multiply,
                    TokenKind::Divide => Token::Divide,
                    TokenKind::LParen => Token::LParen,
                    TokenKind::RParen => Token::RParen,
                    TokenKind::Integer => slice
                        .parse::<isize>()
                        .map(Token::Integer)
                        .map_err(|err| LexError::InvalidInteger(err.to_string()))?,
                    TokenKind::Whitespace => continue,
                };
                tokens.push(token);
            }
            Ok(tokens)
        }
        Err(IncrementalError::TokenOverflow) => Err(LexError::TokenOverflow),
        Err(_) => Err(LexError::UnexpectedEnd),
    }
}

#[derive(Debug)]
enum Expr {
    Int(isize),
    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
}

impl Expr {
    fn eval(&self) -> isize {
        match self {
            Expr::Int(n) => *n,
            Expr::Neg(expr) => -expr.eval(),
            Expr::Add(lhs, rhs) => lhs.eval() + rhs.eval(),
            Expr::Sub(lhs, rhs) => lhs.eval() - rhs.eval(),
            Expr::Mul(lhs, rhs) => lhs.eval() * rhs.eval(),
            Expr::Div(lhs, rhs) => lhs.eval() / rhs.eval(),
        }
    }
}

fn parser<'src>()
-> impl Parser<'src, &'src [Token], Expr, chumsky::extra::Err<chumsky::error::Simple<'src, Token>>>
{
    recursive(|expr| {
        let atom = {
            let int = select! {
                Token::Integer(value) => Expr::Int(value),
            };
            let parenthesized = expr
                .clone()
                .delimited_by(just(Token::LParen), just(Token::RParen));
            int.or(parenthesized)
        };

        let unary = just(Token::Minus)
            .repeated()
            .foldr(atom.clone(), |_op, rhs| Expr::Neg(Box::new(rhs)));

        let product = unary.clone().foldl(
            just(Token::Multiply)
                .or(just(Token::Divide))
                .then(unary.clone())
                .repeated(),
            |lhs, (op, rhs)| match op {
                Token::Multiply => Expr::Mul(Box::new(lhs), Box::new(rhs)),
                Token::Divide => Expr::Div(Box::new(lhs), Box::new(rhs)),
                _ => unreachable!("unhandled multiplicative operator"),
            },
        );

        product.clone().foldl(
            just(Token::Plus)
                .or(just(Token::Minus))
                .then(product)
                .repeated(),
            |lhs, (op, rhs)| match op {
                Token::Plus => Expr::Add(Box::new(lhs), Box::new(rhs)),
                Token::Minus => Expr::Sub(Box::new(lhs), Box::new(rhs)),
                _ => unreachable!("unhandled additive operator"),
            },
        )
    })
}

fn main() {
    let input = env::args().nth(1).expect("usage: calculator <expression>");

    let tokens = match lex(&input) {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("lex error: {err}");
            std::process::exit(1);
        }
    };

    match parser().parse(tokens.as_slice()).into_result() {
        Ok(expr) => {
            println!("[AST]\n{:#?}", expr);
            println!("\n[result]\n{}", expr.eval());
        }
        Err(errors) => {
            eprintln!("parse error: {errors:#?}");
            std::process::exit(1);
        }
    }
}
