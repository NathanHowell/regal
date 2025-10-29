#![cfg(feature = "alloc")]

use regal::{CharCategory, ClassAtom, Pattern, PatternNode, TokenSpec, compile};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Default)]
enum Tok {
    #[default]
    Unknown,
    Ident,
    Whitespace,
    Equals,
}

const TOKENS: usize = 4;
const NFA_STATES: usize = 32;
const NFA_TRANSITIONS: usize = 64;
const NFA_EPSILONS: usize = 64;
const DFA_STATES: usize = 32;
const DFA_TRANSITIONS: usize = 64;
const MAX_BOUNDARIES: usize = 128;
const MAX_DENSE: usize = 128;

const WS_CLASS: [ClassAtom; 1] = [ClassAtom::Category(CharCategory::Whitespace)];
const WS_NODE: PatternNode<'static> = PatternNode::Class(&WS_CLASS);
const WS_REPEAT_NODE: PatternNode<'static> = PatternNode::Repeat {
    inner: Pattern::new(&WS_NODE),
    min: 1,
    max: None,
};
const WS_PATTERN: Pattern<'static> = Pattern::new(&WS_REPEAT_NODE);

const IDENT_CLASS: [ClassAtom; 2] = [
    ClassAtom::Range {
        start: b'a' as u32,
        end: b'z' as u32,
    },
    ClassAtom::Range {
        start: b'A' as u32,
        end: b'Z' as u32,
    },
];
const IDENT_NODE: PatternNode<'static> = PatternNode::Class(&IDENT_CLASS);
const IDENT_REPEAT_NODE: PatternNode<'static> = PatternNode::Repeat {
    inner: Pattern::new(&IDENT_NODE),
    min: 1,
    max: None,
};
const IDENT_PATTERN: Pattern<'static> = Pattern::new(&IDENT_REPEAT_NODE);

const EQ_PATTERN: Pattern<'static> = Pattern::new(&PatternNode::Literal(b"="));

fn compiled() -> regal::CompiledLexer<Tok, TOKENS, DFA_STATES, DFA_TRANSITIONS, MAX_DENSE> {
    static SPECS: [TokenSpec<'static, Tok>; 3] = [
        TokenSpec {
            pattern: WS_PATTERN,
            token: Tok::Whitespace,
            priority: 10,
            skip: true,
        },
        TokenSpec {
            pattern: IDENT_PATTERN,
            token: Tok::Ident,
            priority: 0,
            skip: false,
        },
        TokenSpec {
            pattern: EQ_PATTERN,
            token: Tok::Equals,
            priority: 0,
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
    >(&SPECS)
    .expect("lexer compilation")
}

#[test]
fn advance_emits_after_progress_and_reprocesses() {
    let compiled = compiled();
    let mut lexer = compiled.lexer();
    assert!(matches!(lexer.advance('l'), regal::Advance::Progress));
    assert!(matches!(lexer.advance('e'), regal::Advance::Progress));

    match lexer.advance('t') {
        regal::Advance::Progress => {}
        other => panic!("unexpected state: {:?}", other),
    }

    match lexer.advance(' ') {
        regal::Advance::Emit {
            token,
            token_id,
            length,
            reprocess,
        } => {
            assert_eq!(token, Tok::Ident);
            assert_eq!(token_id, 1);
            assert_eq!(length, 3);
            assert!(reprocess, "identifier should request reprocess");
        }
        other => panic!("expected emit, got {:?}", other),
    }

    // Re-processing the whitespace should produce a skip entry and leave the
    // lexer ready for the next token.
    match lexer.advance(' ') {
        regal::Advance::Progress => {}
        other => panic!(
            "expected progress while consuming whitespace, got {:?}",
            other
        ),
    }

    match lexer.advance('=') {
        regal::Advance::Skip {
            length,
            token_id,
            reprocess,
        } => {
            assert_eq!(length, 1);
            assert_eq!(token_id, 0);
            assert!(reprocess);
        }
        other => panic!("expected skip, got {:?}", other),
    }

    match lexer.advance('=') {
        regal::Advance::Progress => {}
        other => panic!("expected progress while consuming '=', got {:?}", other),
    }

    let finish = lexer.finish().expect("finish ok").expect("token produced");
    assert_eq!(finish.token, Tok::Equals);
    assert_eq!(finish.length, 1);
}

#[test]
fn advance_rejects_unmatched_input() {
    let compiled = compiled();
    let mut lexer = compiled.lexer();
    match lexer.advance('?') {
        regal::Advance::Reject { span } => assert_eq!(span, 0),
        other => panic!("expected reject, got {:?}", other),
    }
    match lexer.finish() {
        Ok(None) => {}
        other => panic!("unexpected finish state: {:?}", other),
    }
}
