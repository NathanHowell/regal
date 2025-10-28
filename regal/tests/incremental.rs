use regal::{
    CharCategory, ClassAtom, CompiledLexer, CursorView, IncrementalError, Pattern, PatternNode,
    TextEdit, TokenCache, TokenRecord, TokenSpec, compile,
};
use std::vec::Vec;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Default)]
enum Tok {
    #[default]
    Unknown,
    Ident,
    Number,
    Reboot,
    Reset,
    Whitespace,
}

const TOKENS: usize = 4;
const NFA_STATES: usize = 64;
const NFA_TRANSITIONS: usize = 128;
const NFA_EPSILONS: usize = 128;
const DFA_STATES: usize = 64;
const DFA_TRANSITIONS: usize = 128;
const MAX_BOUNDARIES: usize = 256;
const CACHE: usize = 32;
const MAX_DENSE: usize = 256;

const REBOOT_NODE: PatternNode<'static> = PatternNode::Literal(b"reboot");
const REBOOT_PATTERN: Pattern<'static> = Pattern::new(&REBOOT_NODE);

const WS_CLASS: [ClassAtom; 1] = [ClassAtom::Category(CharCategory::Whitespace)];
const WS_NODE: PatternNode<'static> = PatternNode::Class(&WS_CLASS);
const WS_REPEAT_NODE: PatternNode<'static> = PatternNode::Repeat {
    inner: Pattern::new(&WS_NODE),
    min: 1,
    max: None,
};
const WS_PATTERN: Pattern<'static> = Pattern::new(&WS_REPEAT_NODE);

const IDENT_CLASS: [ClassAtom; 1] = [ClassAtom::Range {
    start: b'a' as u32,
    end: b'z' as u32,
}];
const IDENT_NODE: PatternNode<'static> = PatternNode::Class(&IDENT_CLASS);
const IDENT_REPEAT_NODE: PatternNode<'static> = PatternNode::Repeat {
    inner: Pattern::new(&IDENT_NODE),
    min: 1,
    max: None,
};
const IDENT_PATTERN: Pattern<'static> = Pattern::new(&IDENT_REPEAT_NODE);

const DIGIT_CLASS: [ClassAtom; 1] = [ClassAtom::Range {
    start: b'0' as u32,
    end: b'9' as u32,
}];
const DIGIT_NODE: PatternNode<'static> = PatternNode::Class(&DIGIT_CLASS);
const DIGIT_REPEAT_NODE: PatternNode<'static> = PatternNode::Repeat {
    inner: Pattern::new(&DIGIT_NODE),
    min: 1,
    max: None,
};
const NUMBER_PATTERN: Pattern<'static> = Pattern::new(&DIGIT_REPEAT_NODE);

const FULL_SPECS: [TokenSpec<'static, Tok>; 4] = [
    TokenSpec {
        pattern: WS_PATTERN,
        token: Tok::Whitespace,
        priority: 10,
        skip: true,
    },
    TokenSpec {
        pattern: REBOOT_PATTERN,
        token: Tok::Reboot,
        priority: 0,
        skip: false,
    },
    TokenSpec {
        pattern: IDENT_PATTERN,
        token: Tok::Ident,
        priority: 5,
        skip: false,
    },
    TokenSpec {
        pattern: NUMBER_PATTERN,
        token: Tok::Number,
        priority: 5,
        skip: false,
    },
];

const KEYWORD_SPECS: [TokenSpec<'static, Tok>; 2] = [
    TokenSpec {
        pattern: WS_PATTERN,
        token: Tok::Whitespace,
        priority: 10,
        skip: true,
    },
    TokenSpec {
        pattern: REBOOT_PATTERN,
        token: Tok::Reboot,
        priority: 0,
        skip: false,
    },
];

const KEYWORD_MULTI_SPECS: [TokenSpec<'static, Tok>; 3] = [
    TokenSpec {
        pattern: WS_PATTERN,
        token: Tok::Whitespace,
        priority: 10,
        skip: true,
    },
    TokenSpec {
        pattern: REBOOT_PATTERN,
        token: Tok::Reboot,
        priority: 0,
        skip: false,
    },
    TokenSpec {
        pattern: Pattern::new(&PatternNode::Literal(b"reset")),
        token: Tok::Reset,
        priority: 0,
        skip: false,
    },
];

fn compile_full() -> CompiledLexer<Tok, TOKENS, DFA_STATES, DFA_TRANSITIONS, MAX_DENSE> {
    compile::<
        Tok,
        TOKENS,
        NFA_STATES,
        NFA_TRANSITIONS,
        NFA_EPSILONS,
        DFA_STATES,
        DFA_TRANSITIONS,
        MAX_BOUNDARIES,
        MAX_DENSE,
    >(&FULL_SPECS)
    .expect("compile full lexer")
}

fn compile_keyword() -> CompiledLexer<Tok, TOKENS, DFA_STATES, DFA_TRANSITIONS, MAX_DENSE> {
    compile::<
        Tok,
        TOKENS,
        NFA_STATES,
        NFA_TRANSITIONS,
        NFA_EPSILONS,
        DFA_STATES,
        DFA_TRANSITIONS,
        MAX_BOUNDARIES,
        MAX_DENSE,
    >(&KEYWORD_SPECS)
    .expect("compile keyword lexer")
}

fn compile_multi_keyword() -> CompiledLexer<Tok, TOKENS, DFA_STATES, DFA_TRANSITIONS, MAX_DENSE> {
    compile::<
        Tok,
        TOKENS,
        NFA_STATES,
        NFA_TRANSITIONS,
        NFA_EPSILONS,
        DFA_STATES,
        DFA_TRANSITIONS,
        MAX_BOUNDARIES,
        MAX_DENSE,
    >(&KEYWORD_MULTI_SPECS)
    .expect("compile keyword variants")
}

#[test]
fn rebuild_produces_expected_tokens() {
    let compiled = compile_full();
    let mut cache: TokenCache<Tok, CACHE> = TokenCache::new();
    let partial = cache.rebuild(&compiled, "reboot foo 42").expect("rebuild");
    assert!(partial.is_none());

    let tokens = cache.tokens();
    assert_eq!(tokens.len(), 5);

    assert_token(tokens[0], Tok::Reboot, 0, 6, false);
    assert_token(tokens[1], Tok::Whitespace, 6, 7, true);
    assert_token(tokens[2], Tok::Ident, 7, 10, false);
    assert_token(tokens[3], Tok::Whitespace, 10, 11, true);
    assert_token(tokens[4], Tok::Number, 11, 13, false);
}

#[test]
fn apply_edit_updates_affected_span() {
    let compiled = compile_full();
    let mut cache: TokenCache<Tok, CACHE> = TokenCache::new();
    cache
        .rebuild(&compiled, "foo bar baz")
        .expect("initial rebuild");
    let before: Vec<TokenRecord<Tok>> = cache.tokens().to_vec();

    let edit = TextEdit {
        range: 4..7,
        replacement_len: 3,
    };
    let partial = cache
        .apply_edit(&compiled, "foo baz baz", edit)
        .expect("apply edit");
    assert!(partial.is_none());

    let after = cache.tokens();
    assert_eq!(after.len(), before.len());
    assert_eq!(after[0], before[0]);
    assert_eq!(after[1], before[1]);
    assert_token(after[2], Tok::Ident, 4, 7, false);
}

#[test]
fn partial_token_reports_candidates() {
    let compiled = compile_keyword();
    let mut cache: TokenCache<Tok, CACHE> = TokenCache::new();
    let partial = cache
        .rebuild(&compiled, "reb")
        .expect("partial rebuild")
        .expect("partial token expected");

    assert_eq!(partial.fragment, "reb");
    assert_eq!(partial.start, 0);
    assert_eq!(partial.candidates.len(), 1);
    let candidate = partial.primary.expect("primary candidate");
    assert_eq!(candidate.token, Tok::Reboot);
    assert!(!candidate.skipped);
}

#[test]
fn cursor_view_includes_containing_token() {
    let compiled = compile_full();
    let mut cache: TokenCache<Tok, CACHE> = TokenCache::new();
    cache
        .rebuild(&compiled, "reboot foo 42")
        .expect("rebuild for cursor");

    let CursorView {
        preceding,
        containing,
        following,
    } = cache.cursor(8);

    assert_eq!(preceding.len(), 2);
    let current = containing.expect("containing token");
    assert_eq!(current.token, Tok::Ident);
    assert_eq!(current.start, 7);
    assert_eq!(following.len(), 2);
}

#[test]
fn rebuild_is_deterministic() {
    let compiled = compile_full();
    let mut cache: TokenCache<Tok, CACHE> = TokenCache::new();
    cache
        .rebuild(&compiled, "foo foo")
        .expect("initial rebuild");
    let first = cache.tokens().to_vec();

    cache.rebuild(&compiled, "foo foo").expect("repeat rebuild");
    let second = cache.tokens();

    assert_eq!(first, second);
}

#[test]
fn rebuild_errors_on_token_overflow() {
    let compiled = compile_full();
    // Capacity of 2 is not enough for the five tokens produced by the input.
    let mut small_cache: TokenCache<Tok, 2> = TokenCache::new();
    let err = small_cache
        .rebuild(&compiled, "reboot foo 42")
        .expect_err("overflow should surface as error");

    assert!(matches!(err, IncrementalError::TokenOverflow));
}

#[test]
fn apply_edit_errors_on_invalid_range() {
    let compiled = compile_full();
    let mut cache: TokenCache<Tok, CACHE> = TokenCache::new();
    cache
        .rebuild(&compiled, "foo")
        .expect("initial build succeeds");

    let err = cache
        .apply_edit(
            &compiled,
            "foo",
            TextEdit {
                range: 10..12,
                replacement_len: 0,
            },
        )
        .expect_err("invalid edit range should fail");

    assert!(matches!(err, IncrementalError::InvalidEdit));
}

#[test]
fn apply_edit_errors_on_length_mismatch() {
    let compiled = compile_full();
    let mut cache: TokenCache<Tok, CACHE> = TokenCache::new();
    cache
        .rebuild(&compiled, "foo")
        .expect("initial build succeeds");

    let err = cache
        .apply_edit(
            &compiled,
            "foobar",
            TextEdit {
                range: 0..1,
                replacement_len: 3,
            },
        )
        .expect_err("length mismatch should be detected");

    assert!(matches!(
        err,
        IncrementalError::LengthMismatch {
            expected: 5,
            actual: 6
        }
    ));
}

#[test]
fn partial_token_lists_multiple_candidates() {
    let compiled = compile_multi_keyword();
    let mut cache: TokenCache<Tok, CACHE> = TokenCache::new();
    let partial = cache
        .rebuild(&compiled, "re")
        .expect("rebuild ok")
        .expect("partial token expected");

    assert_eq!(partial.start, 0);
    assert_eq!(partial.fragment, "re");
    assert!(partial.candidates.len() >= 2);
    let mut saw_reboot = false;
    let mut saw_reset = false;
    for candidate in partial.candidates.iter() {
        if candidate.token == Tok::Reboot {
            saw_reboot = true;
        }
        if candidate.token == Tok::Reset {
            saw_reset = true;
        }
    }
    assert!(saw_reboot, "expected literal candidate");
    assert!(saw_reset, "expected second keyword candidate");
}

fn assert_token(token: TokenRecord<Tok>, kind: Tok, start: usize, end: usize, skipped: bool) {
    assert_eq!(token.token, kind);
    assert_eq!(token.start, start);
    assert_eq!(token.end, end);
    assert_eq!(token.skipped, skipped);
}
