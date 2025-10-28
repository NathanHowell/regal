#![no_std]
#![forbid(unsafe_code)]
#![doc = "Regal: a no_std, incremental, compile-time DFA lexer library."]

mod bitset;
mod compile;
mod dfa;
mod incremental;
mod lexer;
mod nfa;
mod pattern;

pub use bitset::Bitset;
pub use compile::{CompileError, CompiledLexer, TokenSpec, compile};
pub use dfa::{DfaError, DfaState, DfaTransition, PackedDfa};
pub use incremental::{
    CursorView, IncrementalError, PartialCandidate, PartialToken, TextEdit, TokenCache, TokenRecord,
};
pub use lexer::{Advance, Checkpoint, Lexer, LexerError, Match, TokenInfo};
pub use nfa::NfaError;
pub use pattern::{CharCategory, ClassAtom, Pattern, PatternNode};

pub mod prelude {
    pub use crate::bitset::Bitset;
    pub use crate::compile::{CompiledLexer, TokenSpec, compile};
    pub use crate::dfa::{DfaError, DfaState, DfaTransition, PackedDfa};
    pub use crate::incremental::{
        CursorView, IncrementalError, PartialCandidate, PartialToken, TextEdit, TokenCache,
        TokenRecord,
    };
    pub use crate::lexer::{Advance, Checkpoint, Lexer, LexerError, Match, TokenInfo};
    pub use crate::nfa::NfaError;
    pub use crate::pattern::{CharCategory, ClassAtom, Pattern, PatternNode};
}
