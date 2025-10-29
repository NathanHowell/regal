use crate::bitset::Bitset;
use core::cmp;

pub(crate) const INVALID_TARGET: u16 = u16::MAX;

#[derive(Copy, Clone, Default)]
pub struct DfaState<const MAX_TOKENS: usize> {
    pub first_transition: u32,
    pub transition_len: u32,
    pub accept_token: Option<u16>,
    pub priority: u16,
    pub possible: Bitset<MAX_TOKENS>,
    pub dense_offset: u32,
    pub dense_len: u32,
    pub dense_start: u32,
}

#[derive(Copy, Clone, Default)]
pub struct DfaTransition {
    pub start: u32,
    pub end: u32,
    pub target: u16,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DfaError {
    StateOverflow,
    TransitionOverflow,
    WorkspaceOverflow,
    DenseOverflow,
}

#[derive(Clone)]
pub struct PackedDfa<
    const MAX_STATES: usize,
    const MAX_TRANSITIONS: usize,
    const MAX_TOKENS: usize,
    const MAX_DENSE: usize,
> {
    states_len: usize,
    transitions_len: usize,
    states: [DfaState<MAX_TOKENS>; MAX_STATES],
    transitions: [DfaTransition; MAX_TRANSITIONS],
    dense_len: usize,
    dense: [u16; MAX_DENSE],
    start: u16,
}

impl<
    const MAX_STATES: usize,
    const MAX_TRANSITIONS: usize,
    const MAX_TOKENS: usize,
    const MAX_DENSE: usize,
> PackedDfa<MAX_STATES, MAX_TRANSITIONS, MAX_TOKENS, MAX_DENSE>
{
    pub(crate) const fn start_state(&self) -> u16 {
        self.start
    }

    pub(crate) fn transitions_for(&self, state: u16) -> &[DfaTransition] {
        let idx = state as usize;
        if idx >= self.states_len {
            return &[];
        }
        let state_meta = self.states[idx];
        let start = cmp::min(state_meta.first_transition as usize, self.transitions_len);
        let end = cmp::min(
            start + state_meta.transition_len as usize,
            self.transitions_len,
        );
        &self.transitions[start..end]
    }

    pub(crate) fn accept_token(&self, state: u16) -> Option<u16> {
        let idx = state as usize;
        if idx >= self.states_len {
            None
        } else {
            self.states[idx].accept_token
        }
    }

    pub(crate) fn possible_tokens(&self, state: u16) -> Bitset<MAX_TOKENS> {
        let idx = state as usize;
        if idx >= self.states_len {
            Bitset::new()
        } else {
            self.states[idx].possible
        }
    }

    pub(crate) fn state(&self, state: u16) -> Option<&DfaState<MAX_TOKENS>> {
        self.states.get(state as usize)
    }

    pub(crate) fn dense_slice(&self, offset: u32, len: u32) -> &[u16] {
        let start = cmp::min(offset as usize, self.dense_len);
        let end = cmp::min(start + len as usize, self.dense_len);
        &self.dense[start..end]
    }

    pub(crate) fn dense_target(&self, state: u16, ch: u32) -> Option<u16> {
        let meta = self.state(state)?;
        if meta.dense_len == 0 {
            return None;
        }
        let start = meta.dense_start;
        let len = meta.dense_len;
        if ch < start {
            return None;
        }
        let rel = ch - start;
        if rel >= len {
            return None;
        }
        let row = self.dense_slice(meta.dense_offset, len);
        row.get(rel as usize).copied().and_then(|target| {
            if target == INVALID_TARGET {
                None
            } else {
                Some(target)
            }
        })
    }

    pub const fn from_parts(
        start: u16,
        states: [DfaState<MAX_TOKENS>; MAX_STATES],
        states_len: usize,
        transitions: [DfaTransition; MAX_TRANSITIONS],
        transitions_len: usize,
        dense: [u16; MAX_DENSE],
        dense_len: usize,
    ) -> Self {
        Self {
            states_len,
            transitions_len,
            states,
            transitions,
            dense_len,
            dense,
            start,
        }
    }
}
