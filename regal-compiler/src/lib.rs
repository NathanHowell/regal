#![no_std]
#![forbid(unsafe_code)]

extern crate alloc;

mod bitset;
mod dfa;
mod nfa;
mod pattern;

pub use crate::bitset::Bitset;
pub use crate::dfa::{HostCompiledDfa, HostDfaState, HostDfaTransition, build_dfa};
pub use crate::nfa::{DynamicNfa, NfaError, NfaSpec, NfaState, build_nfa};
pub use crate::pattern::{ClassAtom, PatternExpr};
