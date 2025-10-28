use regal::TokenCache;
use regal_macros::RegalLexer;

#[derive(RegalLexer, Copy, Clone, Debug, PartialEq, Eq, Default)]
enum TokenKind {
    #[default]
    #[regex(r"[ \t\n]+", skip)]
    Whitespace,
    #[token("let", priority = 1)]
    Let,
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*")]
    Ident,
}

fn main() {
    let compiled = TokenKind::lexer();
    let mut cache: TokenCache<TokenKind, 8> = TokenCache::new();
    let _ = cache.rebuild(compiled, "let answer").unwrap();
    let cursor = cache.cursor(4);
    assert!(cursor.containing.is_some());
}
