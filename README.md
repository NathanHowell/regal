# Regal

Regal is a `no_std`, allocation-free lexer engine that compiles regular-expression patterns into a minimal DFA at build time. It supports incremental lexing, cursor-aware queries, and deterministic token streams suitable for editors or embedded parsers.

## Getting Started

Add both crates to your `Cargo.toml`. The core library holds the runtime DFA interpreter, while `regal-macros` provides the derive macro that emits compiled tables during compilation.

```toml
[dependencies]
regal = { path = "path/to/regal" }
regal-macros = { path = "path/to/regal/regal-macros" }
```

The library is `#![no_std]` by default and requires no allocator. Patterns are compiled on the host via the proc macro, and the generated tables can be embedded in firmware or other constrained environments.

## Defining a Lexer with `#[derive(RegalLexer)]`

Create an enum whose variants correspond to token kinds. Attach `#[token]` or `#[regex]` attributes to supply literal or regular-expression patterns. Optional `priority = …` and `skip` flags control disambiguation and trivia handling, similar to Logos.

```rust
use regal::{TextEdit, TokenCache};
use regal_macros::RegalLexer;

#[derive(RegalLexer, Copy, Clone, Debug, PartialEq, Eq, Default)]
enum TokenKind {
    #[default]
    #[regex(r"[ \t\n]+", skip)]
    Whitespace,

    #[regex(r"[A-Za-z_][A-Za-z0-9_]*", priority = 5)]
    Identifier,

    #[regex(r"[0-9]+", priority = 5)]
    Number,

    #[token("=")]
    Equals,
}

fn main() {
    // Compiled tables are emitted as a &'static CompiledLexer.
    let compiled = TokenKind::lexer();

    // TokenCache drives incremental lexing with minimal re-scans.
    let mut cache: TokenCache<TokenKind, 32> = TokenCache::new();
    cache.rebuild(compiled, "let answer = 42").unwrap();

    // Tokens are deterministic and trivia can be skipped via the flag above.
    for record in cache.tokens().iter().filter(|t| !t.skipped) {
        println!("{:?} => {:?}", record.token, record.span(compiled));
    }

    // Incrementally update after an edit.
    cache
        .apply_edit(
            compiled,
            "let answer = 100",
            TextEdit {
                range: 12..14,
                replacement_len: 3,
            },
        )
        .unwrap();
}
```

### Attribute Reference

| Attribute            | Description                                                                                  |
| -------------------- | -------------------------------------------------------------------------------------------- |
| `#[token("...")]`    | Match an exact literal. The macro escapes the literal to avoid regex metacharacters.        |
| `#[regex("...")]`    | Match a regular-expression pattern using `regex-syntax`.                                     |
| `skip`               | Mark the token as trivia; skipped tokens are not emitted by default.                         |
| `priority = <int>`   | Override the default priority (lower values win). Helps resolve ambiguities deterministically. |

Variant-level `#[skip]` and `#[priority = …]` attributes act as defaults if you want to apply the same policy to multiple patterns.

## Incremental and Cursor-Aware Lexing

The core crate exposes incremental utilities that keep a token cache synced with text edits. When an edit arrives, Regal identifies the smallest affected span, replays from a saved DFA checkpoint, and produces either a completed token stream or a partial token (for cursor completions).

```rust
let (checkpoint, partial) = cache.rebuild(compiled, source)?;
if let Some(token) = partial {
    // Handle in-progress token for completion.
}

let partial = cache.apply_edit(compiled, new_source, TextEdit { range, replacement_len })?;
let cursor_view = cache.cursor_view(cursor_position)?;
```

The incremental layer is stack-allocated and uses `heapless::Vec` so that it works in `no_std` contexts.

## Capacity Planning

Patterns are compiled into const-generic tables. When using the runtime `compile` API instead of the macro, you must pick conservative bounds for:

- `TOKENS`: number of token definitions.
- `NFA_STATES`, `NFA_TRANSITIONS`, `NFA_EPSILONS`: Thompson NFA limits.
- `DFA_STATES`, `DFA_TRANSITIONS`: minimized DFA capacities.
- `MAX_BOUNDARIES`: workspace for character partitioning (roughly twice the transition count).

The derive macro infers the necessary sizes automatically and emits packed arrays that can be embedded in ROM.

## Testing

The repository includes `tests/macro_basic.rs`, which demonstrates the derive macro, incremental cache, and deterministic rebuilds. Run the suite with:

```bash
CARGO_NET_OFFLINE=true cargo test
```

This command works in offline/sandboxed environments and exercises both the macro crate and the incremental APIs.

## License

See the project’s `Cargo.toml` for licensing information.
