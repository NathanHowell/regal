use crate::dfa::{DfaError, PackedDfa, determinize, minimize, pack_dfa};
use crate::lexer::TokenInfo;
use crate::nfa::{Nfa, NfaError, compile_pattern};
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

    let mut nfa: Nfa<NFA_STATES, NFA_TRANSITIONS, NFA_EPSILONS> = Nfa::new();
    let start = nfa.add_state().map_err(CompileError::Nfa)?;

    let mut tokens: [TokenInfo<T>; TOKENS] = [TokenInfo::default(); TOKENS];

    for (index, spec) in specs.iter().enumerate() {
        let (pat_start, pat_end) =
            compile_pattern(&mut nfa, spec.pattern).map_err(CompileError::Nfa)?;
        nfa.add_epsilon(start, pat_start)
            .map_err(CompileError::Nfa)?;
        nfa.mark_accept(pat_end, index as u16, spec.priority);
        tokens[index] = TokenInfo {
            token: spec.token,
            skip: spec.skip,
            priority: spec.priority,
        };
    }

    let dfa = determinize::<
        NFA_STATES,
        NFA_TRANSITIONS,
        NFA_EPSILONS,
        DFA_STATES,
        DFA_TRANSITIONS,
        TOKENS,
        MAX_BOUNDARIES,
        MAX_DENSE,
    >(&nfa, start)
    .map_err(CompileError::Dfa)?;

    let minimized = minimize(&dfa).map_err(CompileError::Dfa)?;
    let packed = pack_dfa(&minimized);

    Ok(CompiledLexer {
        dfa: packed,
        token_info: tokens,
        _marker: PhantomData,
    })
}
