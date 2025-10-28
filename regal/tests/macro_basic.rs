use regal::{TextEdit, TokenCache};
use regal_macros::RegalLexer;

#[derive(RegalLexer, Copy, Clone, Debug, PartialEq, Eq, Default)]
enum TokenKind {
    #[default]
    #[regex(r"[ \t\n]+", skip)]
    Whitespace,
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*")]
    Identifier,
    #[regex(r"[0-9]+")]
    Number,
    #[token("=")]
    Equals,
}

#[test]
fn derive_macro_produces_static_lexer() {
    let compiled = TokenKind::lexer();
    let mut cache: TokenCache<TokenKind, 16> = TokenCache::new();
    let source = "let answer = 42";
    let partial = cache.rebuild(compiled, source).expect("rebuild to succeed");
    assert!(partial.is_none(), "no partial token for complete input");

    let collected: Vec<(TokenKind, &str)> = cache
        .tokens()
        .iter()
        .filter(|record| !record.skipped)
        .map(|record| (record.token, &source[record.start..record.end]))
        .collect();

    assert_eq!(
        collected,
        vec![
            (TokenKind::Identifier, "let"),
            (TokenKind::Identifier, "answer"),
            (TokenKind::Equals, "="),
            (TokenKind::Number, "42"),
        ]
    );

    // Re-running the cache with modified input should still emit deterministic tokens.
    let partial = cache
        .apply_edit(
            compiled,
            "let answer = 100",
            TextEdit {
                range: 12..14,
                replacement_len: 3,
            },
        )
        .expect("apply_edit succeeds");
    assert!(partial.is_none());

    let collected_after: Vec<(TokenKind, &str)> = cache
        .tokens()
        .iter()
        .filter(|record| !record.skipped)
        .map(|record| (record.token, &"let answer = 100"[record.start..record.end]))
        .collect();

    assert_eq!(
        collected_after,
        vec![
            (TokenKind::Identifier, "let"),
            (TokenKind::Identifier, "answer"),
            (TokenKind::Equals, "="),
            (TokenKind::Number, "100"),
        ]
    );
}
