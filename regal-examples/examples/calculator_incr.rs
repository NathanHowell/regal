//! Incremental calculator REPL demonstrating Regal's streaming lexer.
//!
//! The UI is built with `cursive` so each keystroke updates the token cache,
//! the token list, and the expression tree immediately. Type characters to
//! extend the buffer, use Backspace/Ctrl-B to delete, Ctrl-R to reset, and
//! Esc/Ctrl-C to quit.

use cursive::{
    Cursive, CursiveRunnable,
    backends::crossterm::Backend,
    event::{Event, Key},
    view::{Nameable, Resizable},
    views::{DummyView, LinearLayout, Panel, TextView},
};
use regal::{IncrementalError, PartialToken, TextEdit, TokenCache};
use regal_macros::RegalLexer;
use std::fmt::Write as FmtWrite;

const MAX_TOKENS: usize = 2048;
const INSTRUCTIONS: &str = "\
Controls:
  - type characters to append
  - Backspace or Ctrl-B removes the last character
  - Ctrl-R resets, Enter redraws, Ctrl-C/Esc exits";

#[derive(RegalLexer, Copy, Clone, Debug, PartialEq, Eq, Default)]
enum TokenKind {
    #[default]
    #[regex(r"[ \t]+", skip)]
    Whitespace,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[regex(r"[0-9]+")]
    Integer,
}

struct AppState {
    buffer: String,
    cache: TokenCache<TokenKind, MAX_TOKENS>,
    compiled: &'static TokenKindCompiledLexer,
    status: Option<String>,
}

struct ViewSnapshot {
    buffer: String,
    tokens: String,
    partial: String,
    ast: String,
    status: String,
}

#[derive(Clone)]
struct LexToken<'src> {
    kind: TokenKind,
    lexeme: &'src str,
    span: std::ops::Range<usize>,
}

#[derive(Debug)]
struct ParseError {
    message: String,
    span: std::ops::Range<usize>,
}

#[derive(Debug)]
struct Expr<'src> {
    span: std::ops::Range<usize>,
    value: Option<isize>,
    kind: ExprKind<'src>,
}

#[derive(Debug)]
enum ExprKind<'src> {
    Int {
        literal: &'src str,
        parsed: isize,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr<'src>>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr<'src>>,
        right: Box<Expr<'src>>,
    },
}

#[derive(Debug)]
enum UnaryOp {
    Neg,
}

#[derive(Debug)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

fn main() {
    let mut siv = CursiveRunnable::new(|| Backend::init());
    let compiled = TokenKind::lexer();

    siv.add_fullscreen_layer(
        LinearLayout::vertical()
            .child(TextView::new("Incremental calculator REPL").center())
            .child(DummyView.fixed_height(1))
            .child(TextView::new(INSTRUCTIONS))
            .child(DummyView.fixed_height(1))
            .child(
                Panel::new(TextView::new("").with_name("buffer"))
                    .title("Buffer")
                    .full_width(),
            )
            .child(DummyView.fixed_height(1))
            .child(
                Panel::new(TextView::new("").with_name("tokens"))
                    .title("Tokens")
                    .full_width(),
            )
            .child(DummyView.fixed_height(1))
            .child(
                Panel::new(TextView::new("").with_name("partial"))
                    .title("Partial Token")
                    .full_width(),
            )
            .child(DummyView.fixed_height(1))
            .child(
                Panel::new(TextView::new("").with_name("ast"))
                    .title("AST")
                    .full_width(),
            )
            .child(DummyView.fixed_height(1))
            .child(
                Panel::new(TextView::new("").with_name("status"))
                    .title("Status")
                    .full_width(),
            ),
    );

    siv.set_user_data(AppState {
        buffer: String::new(),
        cache: TokenCache::new(),
        compiled,
        status: None,
    });

    let initial_snapshot = {
        let mut snap = None;
        siv.with_user_data(|state: &mut AppState| {
            snap = Some(snapshot_with_strings(state, None, None));
        });
        snap.expect("snapshot")
    };
    apply_snapshot(&mut siv, initial_snapshot);

    siv.add_global_callback(Event::CtrlChar('c'), |s| s.quit());
    siv.add_global_callback(Event::Key(Key::Esc), |s| s.quit());

    siv.add_global_callback(Event::CtrlChar('r'), reset_buffer);
    siv.add_global_callback(Event::Key(Key::Enter), |s| {
        let snap = {
            let mut result = None;
            s.with_user_data(|state: &mut AppState| {
                result = Some(snapshot_with_strings(state, None, None));
            });
            result.expect("snapshot")
        };
        apply_snapshot(s, snap);
    });
    siv.add_global_callback(Event::Key(Key::Backspace), backspace);
    siv.add_global_callback(Event::CtrlChar('b'), backspace);

    for ch in [' ', '-', '+', '*', '/', '(', ')'] {
        let capture = ch;
        siv.add_global_callback(Event::Char(capture), move |s| insert_char(s, capture));
    }

    for digit in '0'..='9' {
        let capture = digit;
        siv.add_global_callback(Event::Char(capture), move |s| insert_char(s, capture));
    }

    siv.run();
}

fn insert_char(siv: &mut Cursive, ch: char) {
    let snapshot = {
        let mut result: Option<ViewSnapshot> = None;
        siv.with_user_data(|state: &mut AppState| {
            let mut next = state.buffer.clone();
            next.push(ch);
            let edit = TextEdit {
                range: state.buffer.len()..state.buffer.len(),
                replacement_len: ch.len_utf8(),
            };
            match state.cache.apply_edit(state.compiled, next.as_str(), edit) {
                Ok(partial) => {
                    let partial_text = partial.as_ref().map(format_partial);
                    state.buffer = next;
                    state.status = None;
                    result = Some(snapshot_with_strings(state, partial_text, None));
                }
                Err(err) => {
                    let message = describe_incremental_err(err, state.buffer.as_str());
                    state.status = Some(message.clone());
                    result = Some(snapshot_with_strings(state, None, Some(message)));
                }
            }
        });
        result.expect("snapshot")
    };
    apply_snapshot(siv, snapshot);
}

fn backspace(siv: &mut Cursive) {
    let snapshot = {
        let mut result: Option<ViewSnapshot> = None;
        siv.with_user_data(|state: &mut AppState| {
            if let Some(last) = state.buffer.chars().next_back() {
                let len = last.len_utf8();
                let start = state.buffer.len() - len;
                let end = state.buffer.len();
                let mut next = state.buffer.clone();
                next.truncate(start);
                let edit = TextEdit {
                    range: start..end,
                    replacement_len: 0,
                };
                match state.cache.apply_edit(state.compiled, next.as_str(), edit) {
                    Ok(partial) => {
                        let partial_text = partial.as_ref().map(format_partial);
                        state.buffer = next;
                        state.status = None;
                        result = Some(snapshot_with_strings(state, partial_text, None));
                    }
                    Err(err) => {
                        let message = describe_incremental_err(err, state.buffer.as_str());
                        state.status = Some(message.clone());
                        result = Some(snapshot_with_strings(state, None, Some(message)));
                    }
                }
            } else {
                let message = "buffer is already empty".to_owned();
                state.status = Some(message.clone());
                result = Some(snapshot_with_strings(state, None, Some(message)));
            }
        });
        result.expect("snapshot")
    };
    apply_snapshot(siv, snapshot);
}

fn reset_buffer(siv: &mut Cursive) {
    let snapshot = {
        let mut result: Option<ViewSnapshot> = None;
        siv.with_user_data(|state: &mut AppState| {
            state.buffer.clear();
            state.cache.clear();
            let message = "buffer cleared".to_owned();
            state.status = Some(message.clone());
            result = Some(snapshot_with_strings(state, None, Some(message)));
        });
        result.expect("snapshot")
    };
    apply_snapshot(siv, snapshot);
}

fn snapshot_with_strings(
    state: &AppState,
    partial_text: Option<String>,
    status_override: Option<String>,
) -> ViewSnapshot {
    let tokens = collect_tokens(state.buffer.as_str(), &state.cache);

    let tokens_text = if tokens.is_empty() {
        "<empty>".to_owned()
    } else {
        let mut text = String::new();
        write_tokens(&mut text, &tokens);
        text
    };

    let partial_text = partial_text.unwrap_or_else(|| "None".to_owned());

    let ast_text = if tokens.is_empty() {
        "<empty>".to_owned()
    } else {
        let mut parser = Parser::new(&tokens);
        match parser.parse_expression() {
            Ok(expr) => {
                if let Some(next) = parser.peek() {
                    format!("Unexpected token {:?} at span {:?}", next.kind, next.span)
                } else {
                    let mut text = String::new();
                    write_expr(&mut text, &expr, 0);
                    text
                }
            }
            Err(err) => format!("Error at bytes {:?}: {}", err.span, err.message),
        }
    };

    let status_text = status_override
        .or_else(|| state.status.clone())
        .unwrap_or_else(|| "Ready".to_owned());

    ViewSnapshot {
        buffer: state.buffer.clone(),
        tokens: tokens_text,
        partial: partial_text,
        ast: ast_text,
        status: status_text,
    }
}

fn apply_snapshot(siv: &mut Cursive, snapshot: ViewSnapshot) {
    let ViewSnapshot {
        buffer,
        tokens,
        partial,
        ast,
        status,
    } = snapshot;
    siv.call_on_name("buffer", |view: &mut TextView| view.set_content(buffer));
    siv.call_on_name("tokens", |view: &mut TextView| view.set_content(tokens));
    siv.call_on_name("partial", |view: &mut TextView| view.set_content(partial));
    siv.call_on_name("ast", |view: &mut TextView| view.set_content(ast));
    siv.call_on_name("status", |view: &mut TextView| view.set_content(status));
}

fn describe_incremental_err(err: IncrementalError, source: &str) -> String {
    match err {
        IncrementalError::TokenOverflow => "token cache capacity exceeded".to_owned(),
        IncrementalError::Reject { offset, span } => {
            let end = core::cmp::min(source.len(), offset.saturating_add(span));
            let fragment = if offset < end {
                &source[offset..end]
            } else {
                ""
            };
            format!("invalid token near byte {offset}: {:?}", fragment)
        }
        IncrementalError::UnknownToken(id) => format!("unknown token id {id}"),
        IncrementalError::InvalidEdit => "invalid edit range".to_owned(),
        IncrementalError::LengthMismatch { expected, actual } => {
            format!("length mismatch after edit (expected {expected}, saw {actual})")
        }
    }
}

fn collect_tokens<'src>(
    source: &'src str,
    cache: &TokenCache<TokenKind, MAX_TOKENS>,
) -> Vec<LexToken<'src>> {
    let mut tokens = Vec::new();
    for record in cache.tokens() {
        if record.skipped {
            continue;
        }
        tokens.push(LexToken {
            kind: record.token,
            lexeme: &source[record.start..record.end],
            span: record.start..record.end,
        });
    }
    tokens
}

fn write_tokens(output: &mut String, tokens: &[LexToken<'_>]) {
    output.push_str("[index] Kind span lexeme\n");
    for (index, token) in tokens.iter().enumerate() {
        writeln!(
            output,
            "[{index:02}] {:?} {:?} {:?}",
            token.kind, token.span, token.lexeme
        )
        .unwrap();
    }
}

fn format_partial(partial: &PartialToken<'_, TokenKind, TOKEN_KIND_TOKEN_COUNT>) -> String {
    let mut text = String::new();
    write_partial(&mut text, partial);
    text
}

fn write_partial(
    output: &mut String,
    partial: &PartialToken<'_, TokenKind, TOKEN_KIND_TOKEN_COUNT>,
) {
    writeln!(
        output,
        "start={} fragment={:?}",
        partial.start, partial.fragment
    )
    .unwrap();
    if let Some(primary) = partial.primary {
        writeln!(
            output,
            "primary: {:?} (priority {}, skip={})",
            primary.token, primary.priority, primary.skipped
        )
        .unwrap();
    }
    if !partial.candidates.is_empty() {
        output.push_str("candidates:\n");
        for cand in partial.candidates.iter() {
            writeln!(
                output,
                "  {:?} (priority {}, skip={})",
                cand.token, cand.priority, cand.skipped
            )
            .unwrap();
        }
    }
}

struct Parser<'src> {
    tokens: &'src [LexToken<'src>],
    index: usize,
}

impl<'src> Parser<'src> {
    fn new(tokens: &'src [LexToken<'src>]) -> Self {
        Self { tokens, index: 0 }
    }

    fn parse_expression(&mut self) -> Result<Expr<'src>, ParseError> {
        let mut expr = self.parse_term()?;
        loop {
            match self.peek_kind() {
                Some(TokenKind::Plus) => {
                    self.next();
                    let rhs = self.parse_term()?;
                    expr = Expr::binary(BinaryOp::Add, expr, rhs);
                }
                Some(TokenKind::Minus) => {
                    self.next();
                    let rhs = self.parse_term()?;
                    expr = Expr::binary(BinaryOp::Sub, expr, rhs);
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr<'src>, ParseError> {
        let mut expr = self.parse_factor()?;
        loop {
            match self.peek_kind() {
                Some(TokenKind::Star) => {
                    self.next();
                    let rhs = self.parse_factor()?;
                    expr = Expr::binary(BinaryOp::Mul, expr, rhs);
                }
                Some(TokenKind::Slash) => {
                    self.next();
                    let rhs = self.parse_factor()?;
                    expr = Expr::binary(BinaryOp::Div, expr, rhs);
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr<'src>, ParseError> {
        if matches!(self.peek_kind(), Some(TokenKind::Minus)) {
            let op = self.next().unwrap();
            let expr = self.parse_factor()?;
            let span = op.span.start..expr.span.end;
            let value = expr.value.map(|v| -v);
            return Ok(Expr {
                span,
                value,
                kind: ExprKind::Unary {
                    op: UnaryOp::Neg,
                    expr: Box::new(expr),
                },
            });
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr<'src>, ParseError> {
        if let Some(token) = self.next() {
            match token.kind {
                TokenKind::Integer => {
                    let parsed = token.lexeme.parse::<isize>().map_err(|err| ParseError {
                        message: format!("invalid integer literal: {err}"),
                        span: token.span.clone(),
                    })?;
                    Ok(Expr {
                        span: token.span.clone(),
                        value: Some(parsed),
                        kind: ExprKind::Int {
                            literal: token.lexeme,
                            parsed,
                        },
                    })
                }
                TokenKind::LParen => {
                    let expr = self.parse_expression()?;
                    match self.next() {
                        Some(close) if matches!(close.kind, TokenKind::RParen) => {
                            let span = token.span.start..close.span.end;
                            Ok(Expr {
                                span,
                                value: expr.value,
                                kind: expr.kind,
                            })
                        }
                        other => Err(ParseError {
                            message: "expected ')'".to_owned(),
                            span: other.map(|t| t.span.clone()).unwrap_or(expr.span.clone()),
                        }),
                    }
                }
                _ => Err(ParseError {
                    message: format!("unexpected token {:?}", token.kind),
                    span: token.span.clone(),
                }),
            }
        } else {
            Err(ParseError {
                message: "unexpected end of input".to_owned(),
                span: self.tokens.last().map(|t| t.span.clone()).unwrap_or(0..0),
            })
        }
    }

    fn peek_kind(&self) -> Option<TokenKind> {
        self.tokens.get(self.index).map(|token| token.kind)
    }

    fn next(&mut self) -> Option<&'src LexToken<'src>> {
        let token = self.tokens.get(self.index)?;
        self.index += 1;
        Some(token)
    }

    fn peek(&self) -> Option<&'src LexToken<'src>> {
        self.tokens.get(self.index)
    }
}

impl<'src> Expr<'src> {
    fn binary(op: BinaryOp, left: Expr<'src>, right: Expr<'src>) -> Self {
        let span = left.span.start..right.span.end;
        let value = match (left.value, right.value, &op) {
            (Some(lhs), Some(rhs), BinaryOp::Add) => Some(lhs + rhs),
            (Some(lhs), Some(rhs), BinaryOp::Sub) => Some(lhs - rhs),
            (Some(lhs), Some(rhs), BinaryOp::Mul) => Some(lhs * rhs),
            (Some(lhs), Some(rhs), BinaryOp::Div) => {
                if rhs == 0 {
                    None
                } else {
                    Some(lhs / rhs)
                }
            }
            _ => None,
        };
        Expr {
            span,
            value,
            kind: ExprKind::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            },
        }
    }
}

fn write_expr(output: &mut String, expr: &Expr<'_>, indent: usize) {
    let pad = "  ".repeat(indent);
    let value = match expr.value {
        Some(v) => format!("{v}"),
        None => "invalid".to_owned(),
    };
    match &expr.kind {
        ExprKind::Int { literal, parsed } => {
            writeln!(
                output,
                "{pad}Int literal={literal:?} value={parsed} span={:?}",
                expr.span
            )
            .unwrap();
        }
        ExprKind::Unary {
            op: UnaryOp::Neg,
            expr: inner,
        } => {
            writeln!(output, "{pad}Unary(-) value={value} span={:?}", expr.span).unwrap();
            write_expr(output, inner, indent + 1);
        }
        ExprKind::Binary { op, left, right } => {
            let op_name = match op {
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Mul => "*",
                BinaryOp::Div => "/",
            };
            writeln!(
                output,
                "{pad}Binary({op_name}) value={value} span={:?}",
                expr.span
            )
            .unwrap();
            write_expr(output, left, indent + 1);
            write_expr(output, right, indent + 1);
        }
    }
}
