use crate::bitset::Bitset;
use crate::nfa::{DynamicNfa, NfaState};
use alloc::vec;
use alloc::vec::Vec;
use core::cmp;

const INVALID_TARGET: u16 = u16::MAX;
const DENSE_SPAN_LIMIT: u32 = 128;

#[derive(Clone)]
pub struct HostDfaState {
    pub first_transition: u32,
    pub transition_len: u32,
    pub accept_token: Option<u16>,
    pub priority: u16,
    pub possible: Vec<bool>,
    pub dense_offset: u32,
    pub dense_len: u32,
    pub dense_start: u32,
}

impl HostDfaState {
    fn new(token_count: usize) -> Self {
        Self {
            first_transition: 0,
            transition_len: 0,
            accept_token: None,
            priority: u16::MAX,
            possible: vec![false; token_count],
            dense_offset: 0,
            dense_len: 0,
            dense_start: 0,
        }
    }
}

#[derive(Clone)]
pub struct HostDfaTransition {
    pub start: u32,
    pub end: u32,
    pub target: u16,
}

#[derive(Clone)]
pub struct HostCompiledDfa {
    pub states: Vec<HostDfaState>,
    pub transitions: Vec<HostDfaTransition>,
    pub start: u16,
    pub dense: Vec<u16>,
}

pub fn build_dfa(nfa: &DynamicNfa, token_count: usize) -> HostCompiledDfa {
    let dfa = determinize(nfa, token_count);
    minimize(dfa, token_count)
}

fn determinize(nfa: &DynamicNfa, token_count: usize) -> HostCompiledDfa {
    let nfa_state_count = nfa.states.len();
    let reachability = compute_reachable_tokens(nfa, token_count);

    let mut dfa_states: Vec<HostDfaState> = Vec::new();
    let mut dfa_transitions: Vec<HostDfaTransition> = Vec::new();
    let mut state_sets: Vec<Bitset> = Vec::new();
    let mut state_index: Vec<(Vec<bool>, u16)> = Vec::new();

    let mut start_set = Bitset::new(nfa_state_count);
    start_set.insert(nfa.start as usize);
    let start_closure = epsilon_closure(nfa, &start_set);
    state_index.push((start_closure.to_vec(), 0));
    state_sets.push(start_closure.clone());
    dfa_states.push(HostDfaState::new(token_count));

    let mut index = 0;
    while index < state_sets.len() {
        let closure = &state_sets[index];
        let (token, priority) = best_accept_state(closure, &nfa.states);
        let possible = collect_possible_tokens(closure, &reachability, token_count);

        let trans_offset = dfa_transitions.len() as u32;
        let mut trans_count = 0u32;

        let candidates = collect_candidates(nfa, closure);
        let boundaries = build_boundaries(&candidates);

        for window in boundaries.windows(2) {
            let start = window[0];
            let end = window[1];
            if start >= end {
                continue;
            }
            let mut target = Bitset::new(nfa_state_count);
            accumulate_target_set(start, end, &candidates, &mut target);
            if target.is_empty() {
                continue;
            }
            let closure_target = epsilon_closure(nfa, &target);
            let key = closure_target.to_vec();
            let state_id = if let Some(existing) = find_state(&state_index, &key) {
                existing
            } else {
                let new_id = state_sets.len() as u16;
                state_index.push((key, new_id));
                state_sets.push(closure_target.clone());
                dfa_states.push(HostDfaState::new(token_count));
                new_id
            };
            let end_inclusive = if end == u32::MAX { u32::MAX } else { end - 1 };
            dfa_transitions.push(HostDfaTransition {
                start,
                end: end_inclusive,
                target: state_id,
            });
            trans_count += 1;
        }

        if let Some(state) = dfa_states.get_mut(index) {
            state.first_transition = trans_offset;
            state.transition_len = trans_count;
            state.accept_token = token;
            state.priority = priority;
            state.possible = possible;
        }

        index += 1;
    }

    let mut dfa = HostCompiledDfa {
        states: dfa_states,
        transitions: dfa_transitions,
        start: 0,
        dense: Vec::new(),
    };
    populate_dense_tables(&mut dfa);
    dfa
}

fn minimize(dfa: HostCompiledDfa, token_count: usize) -> HostCompiledDfa {
    let state_count = dfa.states.len();
    if state_count <= 1 {
        return dfa;
    }

    let mut signatures: Vec<(Option<u16>, u16, Vec<bool>)> = Vec::new();
    let mut block_ids = vec![usize::MAX; state_count];

    for (idx, state) in dfa.states.iter().enumerate() {
        let key = (state.accept_token, state.priority, state.possible.clone());
        if let Some((block, _)) = signatures.iter().enumerate().find(|(_, sig)| **sig == key) {
            block_ids[idx] = block;
        } else {
            let block = signatures.len();
            signatures.push(key);
            block_ids[idx] = block;
        }
    }

    let mut changed = true;
    while changed {
        changed = false;
        let mut new_block_ids = block_ids.clone();
        let mut new_signatures: Vec<SignatureEntry> = Vec::new();
        let mut next_block = 0usize;

        for idx in 0..state_count {
            let state = &dfa.states[idx];
            let block = block_ids[idx];
            let signature = build_transition_signature(state, &dfa.transitions, &block_ids);
            let entry_id = if let Some(entry) = new_signatures
                .iter()
                .find(|entry| entry.block == block && entry.signature == signature)
            {
                entry.new_block
            } else {
                let id = next_block;
                next_block += 1;
                new_signatures.push(SignatureEntry {
                    block,
                    signature: signature.clone(),
                    new_block: id,
                });
                id
            };

            if new_block_ids[idx] != entry_id {
                new_block_ids[idx] = entry_id;
                changed = true;
            }
        }
        block_ids = new_block_ids;
    }

    let mut representative = vec![None::<usize>; block_ids.iter().max().unwrap_or(&0) + 1];
    for (idx, block) in block_ids.iter().enumerate() {
        representative[*block] = representative[*block].or(Some(idx));
    }

    let mut new_states = Vec::new();
    let mut new_transitions = Vec::new();

    for &maybe_idx in representative.iter() {
        let idx = match maybe_idx {
            Some(i) => i,
            None => continue,
        };
        let state = &dfa.states[idx];
        let mut remapped_state = HostDfaState::new(token_count);
        remapped_state.accept_token = state.accept_token;
        remapped_state.priority = state.priority;
        remapped_state.possible = state.possible.clone();
        remapped_state.first_transition = new_transitions.len() as u32;
        let transitions = transitions_for(&dfa, idx as u16);
        for trans in transitions {
            let target_block = block_ids[trans.target as usize];
            new_transitions.push(HostDfaTransition {
                start: trans.start,
                end: trans.end,
                target: target_block as u16,
            });
        }
        remapped_state.transition_len =
            (new_transitions.len() as u32) - remapped_state.first_transition;
        remapped_state.dense_offset = state.dense_offset;
        remapped_state.dense_len = state.dense_len;
        remapped_state.dense_start = state.dense_start;
        new_states.push(remapped_state);
    }

    HostCompiledDfa {
        start: block_ids[dfa.start as usize] as u16,
        states: new_states,
        transitions: new_transitions,
        dense: dfa.dense,
    }
}

struct SignatureEntry {
    block: usize,
    signature: Vec<(u32, u16)>,
    new_block: usize,
}

fn populate_dense_tables(dfa: &mut HostCompiledDfa) {
    dfa.dense.clear();
    for state in dfa.states.iter_mut() {
        state.dense_offset = 0;
        state.dense_len = 0;
        state.dense_start = 0;

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

        let offset = dfa.dense.len();
        dfa.dense.resize(offset + span as usize, INVALID_TARGET);

        for tr in transitions {
            for value in tr.start..=tr.end {
                let index = offset + (value - min) as usize;
                dfa.dense[index] = tr.target;
            }
        }

        state.dense_offset = offset as u32;
        state.dense_len = span;
        state.dense_start = min;
    }
}

fn build_transition_signature(
    state: &HostDfaState,
    transitions: &[HostDfaTransition],
    block_ids: &[usize],
) -> Vec<(u32, u16)> {
    let start = state.first_transition as usize;
    let end = start + state.transition_len as usize;
    let mut signature = Vec::new();
    for trans in &transitions[start..end] {
        let block = block_ids[trans.target as usize];
        signature.push((trans.start, block as u16));
        signature.push((trans.end, block as u16));
    }
    signature
}

fn transitions_for(dfa: &HostCompiledDfa, state: u16) -> &[HostDfaTransition] {
    let entry = &dfa.states[state as usize];
    let start = entry.first_transition as usize;
    let end = start + entry.transition_len as usize;
    &dfa.transitions[start..end]
}

fn epsilon_closure(nfa: &DynamicNfa, start: &Bitset) -> Bitset {
    let mut closure = start.clone();
    let mut stack: Vec<usize> = start.iter().collect();
    while let Some(state) = stack.pop() {
        for eps in &nfa.epsilons {
            if eps.from as usize == state && !closure.contains(eps.to as usize) {
                closure.insert(eps.to as usize);
                stack.push(eps.to as usize);
            }
        }
    }
    closure
}

fn best_accept_state(closure: &Bitset, states: &[NfaState]) -> (Option<u16>, u16) {
    let mut best: Option<(u16, u16)> = None;
    for idx in closure.iter() {
        if let Some(entry) = states.get(idx) {
            if let Some(token) = entry.accept_token {
                match best {
                    Some((current, priority)) => {
                        if entry.priority < priority
                            || (entry.priority == priority && token < current)
                        {
                            best = Some((token, entry.priority));
                        }
                    }
                    None => {
                        best = Some((token, entry.priority));
                    }
                }
            }
        }
    }
    match best {
        Some((token, priority)) => (Some(token), priority),
        None => (None, u16::MAX),
    }
}

fn collect_candidates(nfa: &DynamicNfa, closure: &Bitset) -> Vec<RangeCandidate> {
    let mut candidates = Vec::new();
    for idx in closure.iter() {
        for trans in nfa.transitions.iter().filter(|t| t.from as usize == idx) {
            let mut target = Bitset::new(nfa.states.len());
            target.insert(trans.to as usize);
            candidates.push(RangeCandidate {
                start: trans.start,
                end: trans.end,
                target,
            });
        }
    }
    candidates
}

struct RangeCandidate {
    start: u32,
    end: u32,
    target: Bitset,
}

fn build_boundaries(candidates: &[RangeCandidate]) -> Vec<u32> {
    let mut boundaries = Vec::new();
    boundaries.push(0);
    for cand in candidates {
        boundaries.push(cand.start);
        if cand.end == u32::MAX {
            boundaries.push(u32::MAX);
        } else {
            boundaries.push(cand.end + 1);
        }
    }
    boundaries.sort_unstable();
    boundaries.dedup();
    boundaries
}

fn accumulate_target_set(start: u32, end: u32, candidates: &[RangeCandidate], target: &mut Bitset) {
    for cand in candidates {
        let cand_end = if cand.end == u32::MAX {
            u32::MAX
        } else {
            cand.end + 1
        };
        if cand.start <= start && start < cand_end && cand.start < end {
            let _ = target.union_with(&cand.target);
        }
    }
}

fn collect_possible_tokens(
    closure: &Bitset,
    reachability: &[Bitset],
    token_count: usize,
) -> Vec<bool> {
    let mut possible = Bitset::new(token_count);
    for idx in closure.iter() {
        let _ = possible.union_with(&reachability[idx]);
    }
    possible.to_vec()
}

fn compute_reachable_tokens(nfa: &DynamicNfa, token_count: usize) -> Vec<Bitset> {
    let mut reachability = vec![Bitset::new(token_count); nfa.states.len()];
    for (state_idx, state) in nfa.states.iter().enumerate() {
        if let Some(token) = state.accept_token {
            reachability[state_idx].insert(token as usize);
        }
    }

    let mut changed = true;
    while changed {
        changed = false;
        for trans in &nfa.transitions {
            let from = trans.from as usize;
            let to = trans.to as usize;
            let target = reachability[to].clone();
            if reachability[from].union_with(&target) {
                changed = true;
            }
        }
        for eps in &nfa.epsilons {
            let from = eps.from as usize;
            let to = eps.to as usize;
            let target = reachability[to].clone();
            if reachability[from].union_with(&target) {
                changed = true;
            }
        }
    }

    reachability
}

fn find_state(index: &[(Vec<bool>, u16)], key: &[bool]) -> Option<u16> {
    for (stored, value) in index {
        if stored.as_slice() == key {
            return Some(*value);
        }
    }
    None
}
