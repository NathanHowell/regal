use crate::bitset::Bitset;
use crate::nfa::{DynamicNfa, NfaState};
use alloc::vec;
use alloc::vec::Vec;
use core::cmp;

const INVALID_TARGET: u16 = u16::MAX;
const DENSE_SPAN_LIMIT: u32 = 128;
const DENSE_MIN_COVERAGE: u32 = 4;
const DENSE_RATIO_NUMERATOR: u32 = 3;
const DENSE_RATIO_DENOMINATOR: u32 = 4;

#[derive(Clone)]
pub struct ByteClassRange {
    pub start: u32,
    pub end: u32,
    pub class: u16,
}

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
    pub classes: Vec<ByteClassRange>,
    pub class_count: u16,
}

#[derive(Clone, Copy)]
struct Segment {
    start: u32,
    end: u32,
}

pub fn build_dfa(nfa: &DynamicNfa, token_count: usize) -> HostCompiledDfa {
    let dfa = determinize(nfa, token_count);
    let mut minimized = minimize(dfa, token_count);
    apply_byte_classes(&mut minimized);
    populate_dense_tables(&mut minimized);
    minimized
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

    HostCompiledDfa {
        states: dfa_states,
        transitions: dfa_transitions,
        start: 0,
        dense: Vec::new(),
        classes: Vec::new(),
        class_count: 0,
    }
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
        remapped_state.dense_offset = 0;
        remapped_state.dense_len = 0;
        remapped_state.dense_start = 0;
        new_states.push(remapped_state);
    }

    HostCompiledDfa {
        start: block_ids[dfa.start as usize] as u16,
        states: new_states,
        transitions: new_transitions,
        dense: Vec::new(),
        classes: Vec::new(),
        class_count: 0,
    }
}

struct SignatureEntry {
    block: usize,
    signature: Vec<(u32, u16)>,
    new_block: usize,
}

fn apply_byte_classes(dfa: &mut HostCompiledDfa) {
    let state_count = dfa.states.len();
    if state_count == 0 {
        dfa.classes.clear();
        dfa.class_count = 0;
        dfa.transitions.clear();
        return;
    }

    let mut boundaries: Vec<u64> = Vec::new();
    boundaries.push(0);
    boundaries.push((u32::MAX as u64).saturating_add(1));

    for state in &dfa.states {
        let start = state.first_transition as usize;
        let end = start + state.transition_len as usize;
        for trans in &dfa.transitions[start..end] {
            boundaries.push(trans.start as u64);
            let end_plus = (trans.end as u64).saturating_add(1);
            boundaries.push(end_plus);
        }
    }

    boundaries.sort_unstable();
    boundaries.dedup();

    let mut segments = Vec::new();
    for window in boundaries.windows(2) {
        let seg_start = window[0];
        let seg_end = window[1];
        if seg_start >= seg_end {
            continue;
        }
        let start = seg_start as u32;
        let end = if seg_end == (u32::MAX as u64).saturating_add(1) {
            u32::MAX
        } else {
            (seg_end as u32).saturating_sub(1)
        };
        segments.push(Segment { start, end });
    }

    if segments.is_empty() {
        segments.push(Segment {
            start: 0,
            end: u32::MAX,
        });
    }

    let mut segment_signatures: Vec<Vec<u16>> =
        vec![vec![INVALID_TARGET; state_count]; segments.len()];

    for (state_index, state) in dfa.states.iter().enumerate() {
        let start = state.first_transition as usize;
        let end = start + state.transition_len as usize;
        for trans in &dfa.transitions[start..end] {
            let begin_idx = find_segment_index(&segments, trans.start);
            let end_idx = find_segment_index(&segments, trans.end);
            for seg in begin_idx..=end_idx {
                segment_signatures[seg][state_index] = trans.target;
            }
        }
    }

    let mut class_signatures: Vec<Vec<u16>> = Vec::new();
    let mut segment_classes: Vec<u16> = Vec::with_capacity(segments.len());

    for signature in &segment_signatures {
        if let Some((idx, _)) = class_signatures
            .iter()
            .enumerate()
            .find(|(_, existing)| *existing == signature)
        {
            segment_classes.push(idx as u16);
        } else {
            let new_id = class_signatures.len() as u16;
            class_signatures.push(signature.clone());
            segment_classes.push(new_id);
        }
    }

    let mut classes = Vec::new();
    if !segments.is_empty() {
        let mut current_class = segment_classes[0];
        let mut current_start = segments[0].start;
        let mut current_end = segments[0].end;

        for (idx, seg) in segments.iter().enumerate().skip(1) {
            let class_id = segment_classes[idx];
            if class_id == current_class && seg.start == current_end.saturating_add(1) {
                current_end = seg.end;
            } else {
                classes.push(ByteClassRange {
                    start: current_start,
                    end: current_end,
                    class: current_class,
                });
                current_class = class_id;
                current_start = seg.start;
                current_end = seg.end;
            }
        }

        classes.push(ByteClassRange {
            start: current_start,
            end: current_end,
            class: current_class,
        });
    }

    let mut new_transitions = Vec::new();

    for (state_index, state) in dfa.states.iter_mut().enumerate() {
        state.first_transition = new_transitions.len() as u32;
        let mut range_start: Option<u32> = None;
        let mut current_target = INVALID_TARGET;

        for class_index in 0..class_signatures.len() {
            let class_target = class_signatures[class_index][state_index];
            match (range_start, class_target == INVALID_TARGET) {
                (None, true) => {}
                (None, false) => {
                    range_start = Some(class_index as u32);
                    current_target = class_target;
                }
                (Some(start), true) => {
                    new_transitions.push(HostDfaTransition {
                        start,
                        end: (class_index as u32).saturating_sub(1),
                        target: current_target,
                    });
                    range_start = None;
                    current_target = INVALID_TARGET;
                }
                (Some(start), false) if class_target != current_target => {
                    new_transitions.push(HostDfaTransition {
                        start,
                        end: (class_index as u32).saturating_sub(1),
                        target: current_target,
                    });
                    range_start = Some(class_index as u32);
                    current_target = class_target;
                }
                _ => {}
            }
        }

        if let Some(start) = range_start {
            new_transitions.push(HostDfaTransition {
                start,
                end: (class_signatures.len() as u32).saturating_sub(1),
                target: current_target,
            });
        }

        state.transition_len =
            (new_transitions.len() as u32).saturating_sub(state.first_transition);
    }

    dfa.transitions = new_transitions;
    dfa.classes = classes;
    dfa.class_count = class_signatures.len() as u16;
}

fn find_segment_index(segments: &[Segment], value: u32) -> usize {
    let mut lo = 0usize;
    let mut hi = segments.len();
    while lo < hi {
        let mid = (lo + hi) / 2;
        let segment = &segments[mid];
        if value < segment.start {
            hi = mid;
        } else if value > segment.end {
            lo = mid + 1;
        } else {
            return mid;
        }
    }
    segments.len().saturating_sub(1)
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
        let mut coverage_classes: u32 = 0;

        for tr in transitions {
            min = cmp::min(min, tr.start);
            max = cmp::max(max, tr.end);
            if tr.end < tr.start {
                continue;
            }
            let span = tr.end.saturating_sub(tr.start).saturating_add(1);
            coverage = coverage.saturating_add(span as u64);
            coverage_classes = coverage_classes.saturating_add(span);
        }

        if min == u32::MAX || max < min {
            continue;
        }

        let span = max - min + 1;
        let total_classes = cmp::max(dfa.class_count as u32, 1);
        let fanout = transitions.len() as u32;
        let dense_candidate = span <= DENSE_SPAN_LIMIT
            || (coverage_classes >= DENSE_MIN_COVERAGE
                && coverage_classes * DENSE_RATIO_DENOMINATOR >= span * DENSE_RATIO_NUMERATOR)
            || (fanout > 8 && coverage_classes * 3 >= total_classes);

        if !dense_candidate {
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
