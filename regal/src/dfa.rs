use crate::bitset::Bitset;
use crate::nfa::{Nfa, NfaState};
use core::cmp;
use heapless::Vec;

pub(crate) const INVALID_TARGET: u16 = u16::MAX;
const DENSE_SPAN_LIMIT: u32 = 128;

#[derive(Clone)]
pub struct Dfa<
    const MAX_STATES: usize,
    const MAX_TRANSITIONS: usize,
    const MAX_TOKENS: usize,
    const MAX_DENSE: usize,
> {
    pub states: Vec<DfaState<MAX_TOKENS>, MAX_STATES>,
    pub transitions: Vec<DfaTransition, MAX_TRANSITIONS>,
    pub dense: Vec<u16, MAX_DENSE>,
    pub start: u16,
}

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

#[derive(Copy, Clone, Default)]
struct RangeCandidate<const N: usize> {
    start: u32,
    end: u32,
    target: Bitset<N>,
}

#[allow(dead_code)]
impl<
    const MAX_STATES: usize,
    const MAX_TRANSITIONS: usize,
    const MAX_TOKENS: usize,
    const MAX_DENSE: usize,
> Dfa<MAX_STATES, MAX_TRANSITIONS, MAX_TOKENS, MAX_DENSE>
{
    pub const fn new() -> Self {
        Self {
            states: Vec::new(),
            transitions: Vec::new(),
            dense: Vec::new(),
            start: 0,
        }
    }

    pub const fn start_state(&self) -> u16 {
        self.start
    }

    fn push_state(&mut self, state: DfaState<MAX_TOKENS>) -> Result<u16, DfaError> {
        self.states
            .push(state)
            .map_err(|_| DfaError::StateOverflow)?;
        Ok((self.states.len() - 1) as u16)
    }

    fn push_transition(&mut self, transition: DfaTransition) -> Result<(), DfaError> {
        self.transitions
            .push(transition)
            .map_err(|_| DfaError::TransitionOverflow)
    }

    pub fn transitions_for(&self, state: u16) -> &[DfaTransition] {
        let state = self.states[state as usize];
        let start = state.first_transition as usize;
        let end = start + state.transition_len as usize;
        &self.transitions[start..end]
    }

    pub fn accept_token(&self, state: u16) -> Option<u16> {
        self.states[state as usize].accept_token
    }

    pub fn possible_tokens(&self, state: u16) -> Bitset<MAX_TOKENS> {
        self.states[state as usize].possible
    }

    fn clear_dense_metadata(&mut self) {
        for state in self.states.iter_mut() {
            state.dense_offset = 0;
            state.dense_len = 0;
            state.dense_start = 0;
        }
        self.dense.clear();
    }
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
    pub const fn start_state(&self) -> u16 {
        self.start
    }

    pub const fn state_len(&self) -> usize {
        self.states_len
    }

    pub fn transitions_for(&self, state: u16) -> &[DfaTransition] {
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

    pub fn accept_token(&self, state: u16) -> Option<u16> {
        let idx = state as usize;
        if idx >= self.states_len {
            None
        } else {
            self.states[idx].accept_token
        }
    }

    pub fn possible_tokens(&self, state: u16) -> Bitset<MAX_TOKENS> {
        let idx = state as usize;
        if idx >= self.states_len {
            Bitset::new()
        } else {
            self.states[idx].possible
        }
    }

    pub fn transitions(&self) -> &[DfaTransition] {
        &self.transitions[..self.transitions_len]
    }

    pub fn states(&self) -> &[DfaState<MAX_TOKENS>] {
        &self.states[..self.states_len]
    }

    pub fn state(&self, state: u16) -> Option<&DfaState<MAX_TOKENS>> {
        self.states.get(state as usize)
    }

    pub fn dense_slice(&self, offset: u32, len: u32) -> &[u16] {
        let start = cmp::min(offset as usize, self.dense_len);
        let end = cmp::min(start + len as usize, self.dense_len);
        &self.dense[start..end]
    }

    pub fn dense_target(&self, state: u16, ch: u32) -> Option<u16> {
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

pub fn pack_dfa<
    const MAX_STATES: usize,
    const MAX_TRANSITIONS: usize,
    const MAX_TOKENS: usize,
    const MAX_DENSE: usize,
>(
    dfa: &Dfa<MAX_STATES, MAX_TRANSITIONS, MAX_TOKENS, MAX_DENSE>,
) -> PackedDfa<MAX_STATES, MAX_TRANSITIONS, MAX_TOKENS, MAX_DENSE> {
    let mut states = [DfaState::default(); MAX_STATES];
    for (index, state) in dfa.states.iter().enumerate() {
        states[index] = *state;
    }
    let mut transitions = [DfaTransition::default(); MAX_TRANSITIONS];
    for (index, trans) in dfa.transitions.iter().enumerate() {
        transitions[index] = *trans;
    }
    let mut dense = [INVALID_TARGET; MAX_DENSE];
    for (index, value) in dfa.dense.iter().copied().enumerate() {
        dense[index] = value;
    }
    PackedDfa {
        states_len: dfa.states.len(),
        transitions_len: dfa.transitions.len(),
        states,
        transitions,
        dense_len: dfa.dense.len(),
        dense,
        start: dfa.start,
    }
}

pub fn determinize<
    const NFA_STATES: usize,
    const NFA_TRANSITIONS: usize,
    const NFA_EPSILONS: usize,
    const DFA_STATES: usize,
    const DFA_TRANSITIONS: usize,
    const MAX_TOKENS: usize,
    const MAX_BOUNDARIES: usize,
    const MAX_DENSE: usize,
>(
    nfa: &Nfa<NFA_STATES, NFA_TRANSITIONS, NFA_EPSILONS>,
    start_state: u16,
) -> Result<Dfa<DFA_STATES, DFA_TRANSITIONS, MAX_TOKENS, MAX_DENSE>, DfaError> {
    let reachability =
        compute_reachable_tokens::<NFA_STATES, NFA_TRANSITIONS, NFA_EPSILONS, MAX_TOKENS>(nfa);

    let mut dfa = Dfa::new();
    let mut state_sets: Vec<Bitset<NFA_STATES>, DFA_STATES> = Vec::new();

    let mut start = Bitset::<NFA_STATES>::new();
    start.insert(start_state as usize);
    let start_closure = epsilon_closure(nfa, start);
    state_sets
        .push(start_closure)
        .map_err(|_| DfaError::StateOverflow)?;
    dfa.push_state(DfaState::default())?;
    dfa.start = 0;

    let mut index = 0;
    while index < state_sets.len() {
        let closure = state_sets[index];
        let (token, priority) = best_accept_state(closure, &nfa.states);
        let possible = collect_possible_tokens::<NFA_STATES, MAX_TOKENS>(closure, &reachability);

        let trans_offset = dfa.transitions.len() as u32;
        let mut trans_count = 0u32;

        let mut candidates: Vec<RangeCandidate<NFA_STATES>, NFA_TRANSITIONS> = Vec::new();
        collect_candidates(nfa, closure, &mut candidates)?;

        let mut boundaries: Vec<u32, MAX_BOUNDARIES> = Vec::new();
        build_boundaries(&candidates, &mut boundaries)?;

        let mut idx = 0;
        while idx + 1 < boundaries.len() {
            let start_value = boundaries[idx];
            let end_value = boundaries[idx + 1];
            if start_value >= end_value {
                idx += 1;
                continue;
            }
            let mut target = Bitset::<NFA_STATES>::new();
            accumulate_target_set(start_value, end_value, &candidates, &mut target);
            if !target.is_empty() {
                let closure_target = epsilon_closure(nfa, target);
                let state_id = find_or_insert_state(&mut state_sets, &mut dfa, closure_target)?;
                let end_inclusive = if end_value == u32::MAX {
                    u32::MAX
                } else {
                    end_value - 1
                };
                dfa.push_transition(DfaTransition {
                    start: start_value,
                    end: end_inclusive,
                    target: state_id,
                })?;
                trans_count += 1;
            }
            idx += 1;
        }

        if let Some(state) = dfa.states.get_mut(index) {
            state.first_transition = trans_offset;
            state.transition_len = trans_count;
            state.accept_token = token;
            state.priority = priority;
            state.possible = possible;
        }

        index += 1;
    }

    populate_dense_tables::<DFA_STATES, DFA_TRANSITIONS, MAX_TOKENS, MAX_DENSE>(&mut dfa)?;

    Ok(dfa)
}

pub fn minimize<
    const MAX_STATES: usize,
    const MAX_TRANSITIONS: usize,
    const MAX_TOKENS: usize,
    const MAX_DENSE: usize,
>(
    dfa: &Dfa<MAX_STATES, MAX_TRANSITIONS, MAX_TOKENS, MAX_DENSE>,
) -> Result<Dfa<MAX_STATES, MAX_TRANSITIONS, MAX_TOKENS, MAX_DENSE>, DfaError> {
    let state_count = dfa.states.len();
    if state_count == 0 {
        return Ok(dfa.clone());
    }

    let mut block_ids = [usize::MAX; MAX_STATES];
    let mut block_count = 0usize;
    let states = &dfa.states;
    let mut signatures: Vec<(Option<u16>, u16, Bitset<MAX_TOKENS>), MAX_STATES> = Vec::new();

    for idx in 0..state_count {
        let key = (
            states[idx].accept_token,
            states[idx].priority,
            states[idx].possible,
        );
        let mut assigned = None;
        for (block, stored) in signatures.iter().enumerate() {
            if *stored == key {
                assigned = Some(block);
                break;
            }
        }
        let block = match assigned {
            Some(b) => b,
            None => {
                signatures
                    .push(key)
                    .map_err(|_| DfaError::WorkspaceOverflow)?;
                let new_block = block_count;
                block_count += 1;
                new_block
            }
        };
        block_ids[idx] = block;
    }

    let mut changed = true;
    while changed {
        changed = false;
        let mut idx = 0;
        while idx < state_count {
            let mut j = idx + 1;
            while j < state_count {
                if block_ids[idx] == block_ids[j] && !states_equivalent(dfa, idx, j, &block_ids) {
                    block_ids[j] = block_count;
                    block_count += 1;
                    changed = true;
                }
                j += 1;
            }
            idx += 1;
        }
    }

    rebuild_dfa(dfa, &block_ids, block_count)
}

fn best_accept_state<const N: usize>(
    set: Bitset<N>,
    states: &Vec<NfaState, N>,
) -> (Option<u16>, u16) {
    let mut best_token = None;
    let mut best_priority = u16::MAX;
    let mut idx = 0;
    while idx < N {
        if set.contains(idx) {
            if let Some(state) = states.get(idx) {
                if let Some(candidate) = state.accept_token {
                    if state.priority < best_priority
                        || (state.priority == best_priority
                            && best_token.map_or(true, |t| candidate < t))
                    {
                        best_priority = state.priority;
                        best_token = Some(candidate);
                    }
                }
            }
        }
        idx += 1;
    }
    (best_token, best_priority)
}

fn epsilon_closure<const S: usize, const T: usize, const E: usize>(
    nfa: &Nfa<S, T, E>,
    mut set: Bitset<S>,
) -> Bitset<S> {
    let mut changed = true;
    while changed {
        changed = false;
        for eps in nfa.epsilons.iter() {
            if set.contains(eps.from as usize) && !set.contains(eps.to as usize) {
                set.insert(eps.to as usize);
                changed = true;
            }
        }
    }
    set
}

fn collect_candidates<const S: usize, const T: usize, const E: usize, const MAX: usize>(
    nfa: &Nfa<S, T, E>,
    set: Bitset<S>,
    candidates: &mut Vec<RangeCandidate<S>, MAX>,
) -> Result<(), DfaError> {
    for tr in nfa.transitions.iter() {
        if set.contains(tr.from as usize) {
            let mut target = Bitset::<S>::new();
            target.insert(tr.to as usize);
            candidates
                .push(RangeCandidate {
                    start: tr.start,
                    end: tr.end,
                    target,
                })
                .map_err(|_| DfaError::WorkspaceOverflow)?;
        }
    }
    Ok(())
}

fn build_boundaries<const N: usize, const MAX_CANDIDATES: usize, const MAX_BOUNDARIES: usize>(
    candidates: &Vec<RangeCandidate<N>, MAX_CANDIDATES>,
    boundaries: &mut Vec<u32, MAX_BOUNDARIES>,
) -> Result<(), DfaError> {
    boundaries.clear();
    boundaries
        .push(0)
        .map_err(|_| DfaError::WorkspaceOverflow)?;
    for cand in candidates.iter() {
        boundaries
            .push(cand.start)
            .map_err(|_| DfaError::WorkspaceOverflow)?;
        if cand.end < u32::MAX {
            boundaries
                .push(cand.end + 1)
                .map_err(|_| DfaError::WorkspaceOverflow)?;
        }
    }
    insertion_sort(boundaries.as_mut_slice());
    let mut write = 0usize;
    let mut read = 0usize;
    while read < boundaries.len() {
        let value = boundaries[read];
        if write == 0 || boundaries[write - 1] != value {
            if write != read {
                boundaries[write] = value;
            }
            write += 1;
        }
        read += 1;
    }
    boundaries.truncate(write);
    Ok(())
}

fn accumulate_target_set<const N: usize, const MAX_CANDIDATES: usize>(
    start: u32,
    end: u32,
    candidates: &Vec<RangeCandidate<N>, MAX_CANDIDATES>,
    result: &mut Bitset<N>,
) {
    for cand in candidates.iter() {
        let cand_end = if cand.end == u32::MAX {
            u32::MAX
        } else {
            cand.end + 1
        };
        if cand.start <= start && start < cand_end && cand.start < end {
            result.union_with(&cand.target);
        }
    }
}

fn find_or_insert_state<
    const NFA_STATES: usize,
    const DFA_STATES: usize,
    const DFA_TRANSITIONS: usize,
    const MAX_TOKENS: usize,
    const MAX_DENSE: usize,
>(
    state_sets: &mut Vec<Bitset<NFA_STATES>, DFA_STATES>,
    dfa: &mut Dfa<DFA_STATES, DFA_TRANSITIONS, MAX_TOKENS, MAX_DENSE>,
    set: Bitset<NFA_STATES>,
) -> Result<u16, DfaError> {
    for (idx, existing) in state_sets.iter().enumerate() {
        if *existing == set {
            return Ok(idx as u16);
        }
    }
    state_sets.push(set).map_err(|_| DfaError::StateOverflow)?;
    dfa.push_state(DfaState::default())
}

fn insertion_sort(values: &mut [u32]) {
    let mut i = 1;
    while i < values.len() {
        let mut j = i;
        while j > 0 && values[j - 1] > values[j] {
            values.swap(j - 1, j);
            j -= 1;
        }
        i += 1;
    }
}

fn states_equivalent<
    const MAX_STATES: usize,
    const MAX_TRANSITIONS: usize,
    const MAX_TOKENS: usize,
    const MAX_DENSE: usize,
>(
    dfa: &Dfa<MAX_STATES, MAX_TRANSITIONS, MAX_TOKENS, MAX_DENSE>,
    a: usize,
    b: usize,
    blocks: &[usize],
) -> bool {
    let state_a = dfa.states[a];
    let state_b = dfa.states[b];
    if state_a.accept_token != state_b.accept_token || state_a.priority != state_b.priority {
        return false;
    }
    if state_a.possible != state_b.possible {
        return false;
    }
    let start_a = state_a.first_transition as usize;
    let start_b = state_b.first_transition as usize;
    let len_a = state_a.transition_len as usize;
    let len_b = state_b.transition_len as usize;
    if len_a != len_b {
        return false;
    }
    let transitions = &dfa.transitions;
    for offset in 0..len_a {
        let tr_a = transitions[start_a + offset];
        let tr_b = transitions[start_b + offset];
        if tr_a.start != tr_b.start || tr_a.end != tr_b.end {
            return false;
        }
        let target_a = blocks[tr_a.target as usize];
        let target_b = blocks[tr_b.target as usize];
        if target_a != target_b {
            return false;
        }
    }
    true
}

fn rebuild_dfa<
    const MAX_STATES: usize,
    const MAX_TRANSITIONS: usize,
    const MAX_TOKENS: usize,
    const MAX_DENSE: usize,
>(
    dfa: &Dfa<MAX_STATES, MAX_TRANSITIONS, MAX_TOKENS, MAX_DENSE>,
    blocks: &[usize],
    block_count: usize,
) -> Result<Dfa<MAX_STATES, MAX_TRANSITIONS, MAX_TOKENS, MAX_DENSE>, DfaError> {
    let mut result = Dfa::new();
    let mut reps = [usize::MAX; MAX_STATES];

    for state in 0..dfa.states.len() {
        let block = blocks[state];
        if reps[block] == usize::MAX {
            reps[block] = state;
        }
    }

    for block in 0..block_count {
        let rep_state_idx = reps[block];
        let rep_state = dfa.states[rep_state_idx];
        let start = rep_state.first_transition as usize;
        let len = rep_state.transition_len as usize;
        let mut count = 0u32;
        let offset = result.transitions.len() as u32;
        for idx in 0..len {
            let tr = dfa.transitions[start + idx];
            result.push_transition(DfaTransition {
                start: tr.start,
                end: tr.end,
                target: blocks[tr.target as usize] as u16,
            })?;
            count += 1;
        }
        let mut new_state = rep_state;
        new_state.first_transition = offset;
        new_state.transition_len = count;
        new_state.dense_offset = 0;
        new_state.dense_len = 0;
        new_state.dense_start = 0;
        result.push_state(new_state)?;
    }

    result.start = blocks[dfa.start as usize] as u16;
    populate_dense_tables::<MAX_STATES, MAX_TRANSITIONS, MAX_TOKENS, MAX_DENSE>(&mut result)?;
    Ok(result)
}

fn populate_dense_tables<
    const MAX_STATES: usize,
    const MAX_TRANSITIONS: usize,
    const MAX_TOKENS: usize,
    const MAX_DENSE: usize,
>(
    dfa: &mut Dfa<MAX_STATES, MAX_TRANSITIONS, MAX_TOKENS, MAX_DENSE>,
) -> Result<(), DfaError> {
    dfa.clear_dense_metadata();

    for state_index in 0..dfa.states.len() {
        let state = &dfa.states[state_index];
        let start = state.first_transition as usize;
        let len = state.transition_len as usize;
        if len == 0 {
            continue;
        }

        let transitions = &dfa.transitions[start..start + len];
        let mut min = u32::MAX;
        let mut max = 0u32;
        let mut coverage: u64 = 0;
        let mut valid = true;

        for tr in transitions {
            min = cmp::min(min, tr.start);
            max = cmp::max(max, tr.end);
            if tr.end == u32::MAX {
                valid = false;
                break;
            }
            let span = tr.end.saturating_sub(tr.start).saturating_add(1);
            coverage = coverage.saturating_add(span as u64);
        }

        if !valid || min == u32::MAX || max < min {
            continue;
        }

        let span = max - min + 1;
        if span > DENSE_SPAN_LIMIT {
            continue;
        }

        if coverage * 2 < span as u64 {
            continue;
        }

        if dfa.dense.len() + span as usize > MAX_DENSE {
            continue;
        }

        let offset = dfa.dense.len();
        for _ in 0..span {
            dfa.dense
                .push(INVALID_TARGET)
                .map_err(|_| DfaError::DenseOverflow)?;
        }

        for tr in transitions {
            for value in tr.start..=tr.end {
                let index = offset + (value - min) as usize;
                dfa.dense[index] = tr.target;
            }
        }

        if let Some(state_meta) = dfa.states.get_mut(state_index) {
            state_meta.dense_offset = offset as u32;
            state_meta.dense_len = span;
            state_meta.dense_start = min;
        }
    }

    Ok(())
}

fn compute_reachable_tokens<
    const NFA_STATES: usize,
    const NFA_TRANSITIONS: usize,
    const NFA_EPSILONS: usize,
    const MAX_TOKENS: usize,
>(
    nfa: &Nfa<NFA_STATES, NFA_TRANSITIONS, NFA_EPSILONS>,
) -> [Bitset<MAX_TOKENS>; NFA_STATES] {
    let mut reach = [Bitset::<MAX_TOKENS>::new(); NFA_STATES];
    let state_len = nfa.states.len();

    for (idx, state) in nfa.states.iter().enumerate() {
        if let Some(token) = state.accept_token {
            reach[idx].insert(token as usize);
        }
    }

    let mut changed = true;
    while changed {
        changed = false;
        for tr in nfa.transitions.iter() {
            let from = tr.from as usize;
            let to = tr.to as usize;
            if from >= state_len || to >= state_len {
                continue;
            }
            let before = reach[from];
            let target = reach[to];
            reach[from].union_with(&target);
            if reach[from] != before {
                changed = true;
            }
        }
        for eps in nfa.epsilons.iter() {
            let from = eps.from as usize;
            let to = eps.to as usize;
            if from >= state_len || to >= state_len {
                continue;
            }
            let before = reach[from];
            let target = reach[to];
            reach[from].union_with(&target);
            if reach[from] != before {
                changed = true;
            }
        }
    }

    reach
}

fn collect_possible_tokens<const NFA_STATES: usize, const MAX_TOKENS: usize>(
    set: Bitset<NFA_STATES>,
    reachability: &[Bitset<MAX_TOKENS>; NFA_STATES],
) -> Bitset<MAX_TOKENS> {
    let mut possible = Bitset::<MAX_TOKENS>::new();
    let mut idx = 0;
    while idx < NFA_STATES {
        if set.contains(idx) {
            possible.union_with(&reachability[idx]);
        }
        idx += 1;
    }
    possible
}
