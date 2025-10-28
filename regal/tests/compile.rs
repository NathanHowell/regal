use regal::{CharCategory, ClassAtom, CompiledLexer, Pattern, PatternNode, TokenSpec, compile};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Default)]
enum Tok {
    #[default]
    Alpha,
    Number,
    Mixed,
}

const TOKENS: usize = 4;
const NFA_STATES: usize = 64;
const NFA_TRANSITIONS: usize = 128;
const NFA_EPSILONS: usize = 128;
const DFA_STATES: usize = 64;
const DFA_TRANSITIONS: usize = 128;
const MAX_BOUNDARIES: usize = 256;
const MAX_DENSE: usize = 256;

const ALPHA_CLASS: [ClassAtom; 1] = [ClassAtom::Category(CharCategory::Alphabetic)];
const ALPHA_NODE: PatternNode<'static> = PatternNode::Class(&ALPHA_CLASS);
const ALPHA_REPEAT_NODE: PatternNode<'static> = PatternNode::Repeat {
    inner: Pattern::new(&ALPHA_NODE),
    min: 1,
    max: None,
};
const ALPHA_PATTERN: Pattern<'static> = Pattern::new(&ALPHA_REPEAT_NODE);

const DIGIT_CLASS: [ClassAtom; 1] = [ClassAtom::Category(CharCategory::Numeric)];
const DIGIT_NODE: PatternNode<'static> = PatternNode::Class(&DIGIT_CLASS);
const DIGIT_REPEAT_NODE: PatternNode<'static> = PatternNode::Repeat {
    inner: Pattern::new(&DIGIT_NODE),
    min: 1,
    max: None,
};
const DIGIT_PATTERN: Pattern<'static> = Pattern::new(&DIGIT_REPEAT_NODE);

const MIX_SEQ: [Pattern<'static>; 2] = [ALPHA_PATTERN, DIGIT_PATTERN];
const MIX_NODE: PatternNode<'static> = PatternNode::Sequence(&MIX_SEQ);
const MIX_PATTERN: Pattern<'static> = Pattern::new(&MIX_NODE);

fn compile_test_lexer() -> CompiledLexer<Tok, TOKENS, DFA_STATES, DFA_TRANSITIONS, MAX_DENSE> {
    let specs = [
        TokenSpec {
            pattern: ALPHA_PATTERN,
            token: Tok::Alpha,
            priority: 0,
            skip: false,
        },
        TokenSpec {
            pattern: DIGIT_PATTERN,
            token: Tok::Number,
            priority: 0,
            skip: false,
        },
        TokenSpec {
            pattern: MIX_PATTERN,
            token: Tok::Mixed,
            priority: 5,
            skip: false,
        },
    ];

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
    >(&specs)
    .expect("test lexer should compile")
}

#[test]
fn compile_builds_expected_tokens() {
    let compiled = compile_test_lexer();
    let mut lexer = compiled.lexer();

    assert_match(&mut lexer, "abc", Tok::Alpha, 3);
    assert_match(&mut lexer, "123", Tok::Number, 3);
    assert_match(&mut lexer, "foo123", Tok::Mixed, 6);
}

fn assert_match(
    lexer: &mut regal::Lexer<'_, Tok, TOKENS, DFA_STATES, DFA_TRANSITIONS, MAX_DENSE>,
    input: &str,
    expected_token: Tok,
    expected_len: usize,
) {
    lexer.reset();
    for ch in input.chars() {
        match lexer.advance(ch) {
            regal::Advance::Progress => {}
            other => panic!("unexpected advance state: {:?}", other),
        }
    }
    let result = lexer.finish().expect("finish should succeed");
    let m = result.expect("match expected");
    assert_eq!(m.token, expected_token);
    assert_eq!(m.length, expected_len);
}

#[test]
fn compile_errors_on_too_many_tokens() {
    const LIT_A: PatternNode<'static> = PatternNode::Literal(b"a");
    const LIT_B: PatternNode<'static> = PatternNode::Literal(b"b");
    let specs = [
        TokenSpec {
            pattern: Pattern::new(&LIT_A),
            token: Tok::Alpha,
            priority: 0,
            skip: false,
        },
        TokenSpec {
            pattern: Pattern::new(&LIT_B),
            token: Tok::Number,
            priority: 0,
            skip: false,
        },
    ];

    match compile::<Tok, 1, 8, 8, 8, 8, 8, 16, 16>(&specs) {
        Err(regal::CompileError::TooManyTokens) => {}
        Err(other) => panic!("unexpected error: {:?}", other),
        Ok(_) => panic!("expected too many tokens error"),
    }
}

#[test]
fn compile_propagates_nfa_overflow() {
    const LONG_LITERAL: PatternNode<'static> = PatternNode::Literal(b"abcdef");
    let spec = TokenSpec {
        pattern: Pattern::new(&LONG_LITERAL),
        token: Tok::Alpha,
        priority: 0,
        skip: false,
    };

    match compile::<Tok, 1, 4, 4, 4, 8, 8, 16, 16>(&[spec]) {
        Err(regal::CompileError::Nfa(regal::NfaError::StateOverflow)) => {}
        Err(other) => panic!("unexpected error: {:?}", other),
        Ok(_) => panic!("expected NFA overflow"),
    }
}

#[test]
fn compile_propagates_dfa_overflow() {
    const ALT_PARTS: [Pattern<'static>; 3] = [
        Pattern::new(&PatternNode::Literal(b"aa")),
        Pattern::new(&PatternNode::Literal(b"bb")),
        Pattern::new(&PatternNode::Literal(b"cc")),
    ];
    const ALT_NODE: PatternNode<'static> = PatternNode::Alternate(&ALT_PARTS);
    let spec = TokenSpec {
        pattern: Pattern::new(&ALT_NODE),
        token: Tok::Alpha,
        priority: 0,
        skip: false,
    };

    match compile::<Tok, 1, 16, 16, 16, 1, 2, 8, 8>(&[spec]) {
        Err(regal::CompileError::Dfa(regal::DfaError::StateOverflow)) => {}
        Err(other) => panic!("unexpected error: {:?}", other),
        Ok(_) => panic!("expected DFA overflow"),
    }
}
