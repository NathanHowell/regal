use crate::dfa::DfaError;
use crate::dfa::PackedDfa;
use crate::lexer::TokenInfo;
use crate::nfa::NfaError;
use crate::pattern::Pattern;
use core::marker::PhantomData;

#[derive(Debug, Copy, Clone)]
pub struct TokenSpec<'a, T> {
    pub pattern: Pattern<'a>,
    pub token: T,
    pub priority: u16,
    pub skip: bool,
}

#[derive(Debug)]
pub enum CompileError {
    TooManyTokens,
    Nfa(NfaError),
    Dfa(DfaError),
}

pub struct CompiledLexer<
    T,
    const TOKENS: usize,
    const STATES: usize,
    const TRANSITIONS: usize,
    const DENSE: usize,
> {
    pub(crate) dfa: PackedDfa<STATES, TRANSITIONS, TOKENS, DENSE>,
    pub(crate) token_info: [TokenInfo<T>; TOKENS],
    _marker: PhantomData<T>,
}

impl<T, const TOKENS: usize, const STATES: usize, const TRANSITIONS: usize, const DENSE: usize>
    CompiledLexer<T, TOKENS, STATES, TRANSITIONS, DENSE>
where
    T: Copy + Default,
{
    pub fn token(&self, id: u16) -> Option<T> {
        self.token_info
            .get(id as usize)
            .and_then(|info| if info.skip { None } else { Some(info.token) })
    }

    pub fn token_entry(&self, id: u16) -> Option<&TokenInfo<T>> {
        self.token_info.get(id as usize)
    }

    pub fn lexer(&self) -> crate::lexer::Lexer<'_, T, TOKENS, STATES, TRANSITIONS, DENSE> {
        crate::lexer::Lexer::new(self)
    }

    pub const fn from_parts(
        dfa: PackedDfa<STATES, TRANSITIONS, TOKENS, DENSE>,
        token_info: [TokenInfo<T>; TOKENS],
    ) -> Self {
        Self {
            dfa,
            token_info,
            _marker: PhantomData,
        }
    }
}

/// Build a lexer from the supplied token specifications.
///
/// Each const generic represents a capacity limit that bounds the size of the
/// intermediate automata. These limits allow the compiler to allocate all
/// tables statically so the resulting [`CompiledLexer`] can live in `no_std`
/// environments without dynamic allocation.
///
/// * `TOKENS` – maximum number of token definitions.
/// * `NFA_STATES`/`NFA_TRANSITIONS`/`NFA_EPSILONS` – upper bounds for the
///   Thompson NFA that is constructed from the patterns.
/// * `DFA_STATES`/`DFA_TRANSITIONS` – bounds for the minimal DFA.
/// * `MAX_BOUNDARIES` – scratch capacity used during subset construction to
///   partition character ranges. A conservative value is roughly twice the
///   transition bound.
/// * `MAX_DENSE` – total capacity of dense transition slots generated for
///   states that operate on compact alphabets.
///
/// The compilation itself happens on the host at build time when invoked from a
/// procedural macro or a `build.rs` script. The resulting [`CompiledLexer`] is
/// fully static and can be embedded in firmware targets.
#[cfg(feature = "alloc")]
pub use host::compile;

#[cfg(feature = "alloc")]
fn bitset_from_possible<const MAX_TOKENS: usize>(
    possible: &[bool],
) -> Result<crate::bitset::Bitset<MAX_TOKENS>, DfaError> {
    let mut bitset = crate::bitset::Bitset::new();
    if possible.len() > MAX_TOKENS {
        return Err(DfaError::StateOverflow);
    }
    for (idx, flag) in possible.iter().enumerate() {
        if *flag {
            bitset.insert(idx);
        }
    }
    Ok(bitset)
}

#[cfg(feature = "alloc")]
mod host {
    #![allow(clippy::too_many_arguments)]

    extern crate alloc;

    use super::*;
    use alloc::boxed::Box;
    use alloc::vec;
    use alloc::vec::Vec;
    use heapless::Vec as HeaplessVec;
    use crate::dfa::{DfaState, DfaTransition, INVALID_TARGET};
    use crate::pattern::{CharCategory, ClassAtom as PatternClassAtom, PatternNode};
    use regal_compiler::{
        ClassAtom as HostClassAtom, HostCompiledDfa, NfaError as HostNfaError,
        NfaSpec as HostNfaSpec, PatternExpr as HostPatternExpr, build_dfa as host_build_dfa,
        build_nfa as host_build_nfa,
    };

    pub fn compile<
        'a,
        T,
        const TOKENS: usize,
        const NFA_STATES: usize,
        const NFA_TRANSITIONS: usize,
        const NFA_EPSILONS: usize,
        const DFA_STATES: usize,
        const DFA_TRANSITIONS: usize,
        const MAX_BOUNDARIES: usize,
        const MAX_DENSE: usize,
    >(
        specs: &[TokenSpec<'a, T>],
    ) -> Result<CompiledLexer<T, TOKENS, DFA_STATES, DFA_TRANSITIONS, MAX_DENSE>, CompileError>
    where
        T: Copy + Default,
    {
        if specs.len() > TOKENS {
            return Err(CompileError::TooManyTokens);
        }

        let mut host_specs = Vec::with_capacity(specs.len());
        let mut tokens: [TokenInfo<T>; TOKENS] = [TokenInfo::default(); TOKENS];

        for (index, spec) in specs.iter().enumerate() {
            let pattern = convert_pattern(spec.pattern);
            host_specs.push(HostNfaSpec {
                pattern,
                token_id: index as u16,
                priority: spec.priority,
            });
            tokens[index] = TokenInfo {
                token: spec.token,
                skip: spec.skip,
                priority: spec.priority,
            };
        }

        let nfa =
            host_build_nfa(&host_specs).map_err(|err| CompileError::Nfa(map_nfa_error(err)))?;

        if nfa.states.len() > NFA_STATES {
            return Err(CompileError::Nfa(NfaError::StateOverflow));
        }
        if nfa.transitions.len() > NFA_TRANSITIONS {
            return Err(CompileError::Nfa(NfaError::TransitionOverflow));
        }
        if nfa.epsilons.len() > NFA_EPSILONS {
            return Err(CompileError::Nfa(NfaError::EpsilonOverflow));
        }

        let dfa = host_build_dfa(&nfa, specs.len());

        if dfa.states.len() > DFA_STATES {
            return Err(CompileError::Dfa(DfaError::StateOverflow));
        }
        if dfa.transitions.len() > DFA_TRANSITIONS {
            return Err(CompileError::Dfa(DfaError::TransitionOverflow));
        }
        validate_boundaries::<MAX_BOUNDARIES>(&dfa).map_err(CompileError::Dfa)?;

        let packed = pack_host_dfa::<DFA_STATES, DFA_TRANSITIONS, TOKENS, MAX_DENSE>(&dfa)
            .map_err(CompileError::Dfa)?;

        Ok(CompiledLexer {
            dfa: packed,
            token_info: tokens,
            _marker: PhantomData,
        })
    }

    fn convert_pattern(pattern: Pattern<'_>) -> HostPatternExpr {
        match pattern.node() {
            PatternNode::Empty => HostPatternExpr::Empty,
            PatternNode::Literal(bytes) => HostPatternExpr::Literal(bytes.to_vec()),
            PatternNode::Char(ch) => HostPatternExpr::Class(vec![HostClassAtom::Char(*ch)]),
            PatternNode::Range { start, end } => HostPatternExpr::Class(vec![HostClassAtom::Range {
                start: *start,
                end: *end,
            }]),
            PatternNode::Class(items) => HostPatternExpr::Class(convert_class_atoms(items)),
            PatternNode::Sequence(parts) => {
                let mut seq = Vec::with_capacity(parts.len());
                for part in *parts {
                    seq.push(convert_pattern(*part));
                }
                HostPatternExpr::Sequence(seq)
            }
            PatternNode::Alternate(parts) => {
                let mut alts = Vec::with_capacity(parts.len());
                for part in *parts {
                    alts.push(convert_pattern(*part));
                }
                HostPatternExpr::Alternate(alts)
            }
            PatternNode::Repeat { inner, min, max } => HostPatternExpr::Repeat {
                inner: Box::new(convert_pattern(*inner)),
                min: *min,
                max: *max,
            },
        }
    }

    fn convert_class_atoms(atoms: &[PatternClassAtom]) -> Vec<HostClassAtom> {
        let mut result = Vec::new();
        for atom in atoms {
            match atom {
                PatternClassAtom::Char(ch) => result.push(HostClassAtom::Char(*ch)),
                PatternClassAtom::Range { start, end } => result.push(HostClassAtom::Range {
                    start: *start,
                    end: *end,
                }),
                PatternClassAtom::Category(cat) => expand_category(*cat, &mut result),
            }
        }
        result
    }

    fn expand_category(category: CharCategory, into: &mut Vec<HostClassAtom>) {
        match category {
            CharCategory::Alphabetic => {
                into.push(HostClassAtom::Range {
                    start: b'A' as u32,
                    end: b'Z' as u32,
                });
                into.push(HostClassAtom::Range {
                    start: b'a' as u32,
                    end: b'z' as u32,
                });
            }
            CharCategory::Numeric => {
                into.push(HostClassAtom::Range {
                    start: b'0' as u32,
                    end: b'9' as u32,
                });
            }
            CharCategory::Alphanumeric => {
                expand_category(CharCategory::Alphabetic, into);
                expand_category(CharCategory::Numeric, into);
            }
            CharCategory::Whitespace => {
                for ch in [0x09u32, 0x0A, 0x0B, 0x0C, 0x0D, 0x20] {
                    into.push(HostClassAtom::Char(ch));
                }
            }
            CharCategory::Lowercase => {
                into.push(HostClassAtom::Range {
                    start: b'a' as u32,
                    end: b'z' as u32,
                });
            }
            CharCategory::Uppercase => {
                into.push(HostClassAtom::Range {
                    start: b'A' as u32,
                    end: b'Z' as u32,
                });
            }
            CharCategory::HexDigit => {
                expand_category(CharCategory::Numeric, into);
                into.push(HostClassAtom::Range {
                    start: b'a' as u32,
                    end: b'f' as u32,
                });
                into.push(HostClassAtom::Range {
                    start: b'A' as u32,
                    end: b'F' as u32,
                });
            }
        }
    }

    fn map_nfa_error(err: HostNfaError) -> NfaError {
        match err {
            HostNfaError::InvalidRepeat => NfaError::InvalidRepeat,
        }
    }

    fn validate_boundaries<const MAX_BOUNDARIES: usize>(
        dfa: &HostCompiledDfa,
    ) -> Result<(), DfaError> {
        for state in &dfa.states {
            let mut boundaries: HeaplessVec<u32, MAX_BOUNDARIES> = HeaplessVec::new();
            push_unique(&mut boundaries, 0)?;

            let start = state.first_transition as usize;
            let end = start + state.transition_len as usize;
            for trans in &dfa.transitions[start..end] {
                push_unique(&mut boundaries, trans.start)?;
                if trans.end == u32::MAX {
                    push_unique(&mut boundaries, u32::MAX)?;
                } else {
                    push_unique(&mut boundaries, trans.end + 1)?;
                }
            }
        }
        Ok(())
    }

    fn push_unique<const CAP: usize>(
        set: &mut HeaplessVec<u32, CAP>,
        value: u32,
    ) -> Result<(), DfaError> {
        if set.contains(&value) {
            return Ok(());
        }
        set.push(value).map_err(|_| DfaError::WorkspaceOverflow)
    }

    fn pack_host_dfa<
        const MAX_STATES: usize,
        const MAX_TRANSITIONS: usize,
        const MAX_TOKENS: usize,
        const MAX_DENSE: usize,
    >(
        dfa: &HostCompiledDfa,
    ) -> Result<PackedDfa<MAX_STATES, MAX_TRANSITIONS, MAX_TOKENS, MAX_DENSE>, DfaError> {
        if dfa.states.len() > MAX_STATES {
            return Err(DfaError::StateOverflow);
        }
        if dfa.transitions.len() > MAX_TRANSITIONS {
            return Err(DfaError::TransitionOverflow);
        }
        let mut states = [DfaState::default(); MAX_STATES];
        for (idx, state) in dfa.states.iter().enumerate() {
            let possible = bitset_from_possible::<MAX_TOKENS>(&state.possible)?;
            states[idx] = DfaState {
                first_transition: state.first_transition,
                transition_len: state.transition_len,
                accept_token: state.accept_token,
                priority: state.priority,
                possible,
                dense_offset: 0,
                dense_len: 0,
                dense_start: 0,
            };
        }

        let mut transitions = [DfaTransition::default(); MAX_TRANSITIONS];
        for (idx, trans) in dfa.transitions.iter().enumerate() {
            transitions[idx] = DfaTransition {
                start: trans.start,
                end: trans.end,
                target: trans.target,
            };
        }

        Ok(PackedDfa::from_parts(
            dfa.start,
            states,
            dfa.states.len(),
            transitions,
            dfa.transitions.len(),
            [INVALID_TARGET; MAX_DENSE],
            0,
        ))
    }
}
