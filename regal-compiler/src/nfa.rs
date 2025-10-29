use crate::pattern::{ClassAtom, PatternExpr};
use alloc::vec::Vec;

#[derive(Clone)]
pub struct NfaSpec {
    pub pattern: PatternExpr,
    pub token_id: u16,
    pub priority: u16,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NfaError {
    InvalidRepeat,
}

#[derive(Clone)]
pub struct NfaState {
    pub accept_token: Option<u16>,
    pub priority: u16,
}

impl NfaState {
    fn new() -> Self {
        Self {
            accept_token: None,
            priority: u16::MAX,
        }
    }
}

#[derive(Clone)]
pub struct NfaTransition {
    pub from: u16,
    pub to: u16,
    pub start: u32,
    pub end: u32,
}

#[derive(Clone)]
pub struct NfaEpsilon {
    pub from: u16,
    pub to: u16,
}

#[derive(Clone)]
pub struct DynamicNfa {
    pub states: Vec<NfaState>,
    pub transitions: Vec<NfaTransition>,
    pub epsilons: Vec<NfaEpsilon>,
    pub start: u16,
}

pub fn build_nfa(specs: &[NfaSpec]) -> Result<DynamicNfa, NfaError> {
    let mut nfa = DynamicNfa {
        states: Vec::new(),
        transitions: Vec::new(),
        epsilons: Vec::new(),
        start: 0,
    };

    let start = nfa.add_state();
    nfa.start = start;

    for spec in specs {
        let (pat_start, pat_end) = compile_pattern(&mut nfa, &spec.pattern)?;
        nfa.add_epsilon(start, pat_start);
        nfa.mark_accept(pat_end, spec.token_id, spec.priority);
    }

    Ok(nfa)
}

impl DynamicNfa {
    pub fn add_state(&mut self) -> u16 {
        self.states.push(NfaState::new());
        (self.states.len() - 1) as u16
    }

    pub fn mark_accept(&mut self, state: u16, token: u16, priority: u16) {
        if let Some(entry) = self.states.get_mut(state as usize) {
            entry.accept_token = Some(token);
            entry.priority = priority;
        }
    }

    pub fn add_transition(&mut self, from: u16, to: u16, start: u32, end: u32) {
        self.transitions.push(NfaTransition {
            from,
            to,
            start,
            end,
        });
    }

    pub fn add_epsilon(&mut self, from: u16, to: u16) {
        self.epsilons.push(NfaEpsilon { from, to });
    }
}

fn compile_pattern(nfa: &mut DynamicNfa, pattern: &PatternExpr) -> Result<(u16, u16), NfaError> {
    match pattern {
        PatternExpr::Empty => {
            let start = nfa.add_state();
            let end = nfa.add_state();
            nfa.add_epsilon(start, end);
            Ok((start, end))
        }
        PatternExpr::Literal(bytes) => {
            let mut prev = nfa.add_state();
            let start = prev;
            for &byte in bytes {
                let next = nfa.add_state();
                nfa.add_transition(prev, next, byte as u32, byte as u32);
                prev = next;
            }
            Ok((start, prev))
        }
        PatternExpr::Class(atoms) => {
            let start = nfa.add_state();
            let end = nfa.add_state();
            emit_class(nfa, start, end, atoms);
            Ok((start, end))
        }
        PatternExpr::Sequence(parts) => compile_sequence(nfa, parts),
        PatternExpr::Alternate(parts) => compile_alternate(nfa, parts),
        PatternExpr::Repeat { inner, min, max } => {
            compile_repeat(nfa, inner, *min as usize, max.map(|value| value as usize))
        }
    }
}

fn compile_sequence(nfa: &mut DynamicNfa, parts: &[PatternExpr]) -> Result<(u16, u16), NfaError> {
    let mut iter = parts.iter();
    if let Some(first) = iter.next() {
        let (start, mut end) = compile_pattern(nfa, first)?;
        for part in iter {
            let (next_start, next_end) = compile_pattern(nfa, part)?;
            nfa.add_epsilon(end, next_start);
            end = next_end;
        }
        Ok((start, end))
    } else {
        let start = nfa.add_state();
        let end = nfa.add_state();
        nfa.add_epsilon(start, end);
        Ok((start, end))
    }
}

fn compile_alternate(nfa: &mut DynamicNfa, parts: &[PatternExpr]) -> Result<(u16, u16), NfaError> {
    let start = nfa.add_state();
    let end = nfa.add_state();
    if parts.is_empty() {
        nfa.add_epsilon(start, end);
        return Ok((start, end));
    }
    for part in parts {
        let (branch_start, branch_end) = compile_pattern(nfa, part)?;
        nfa.add_epsilon(start, branch_start);
        nfa.add_epsilon(branch_end, end);
    }
    Ok((start, end))
}

fn compile_repeat(
    nfa: &mut DynamicNfa,
    pattern: &PatternExpr,
    min: usize,
    max: Option<usize>,
) -> Result<(u16, u16), NfaError> {
    if let Some(max) = max {
        if max < min {
            return Err(NfaError::InvalidRepeat);
        }
    }
    let start = nfa.add_state();
    let end = nfa.add_state();
    let mut current = start;

    for _ in 0..min {
        let (seg_start, seg_end) = compile_pattern(nfa, pattern)?;
        nfa.add_epsilon(current, seg_start);
        current = seg_end;
    }

    nfa.add_epsilon(current, end);

    match max {
        Some(limit) => {
            let mut count = min;
            let mut prev_end = current;
            while count < limit {
                let (seg_start, seg_end) = compile_pattern(nfa, pattern)?;
                nfa.add_epsilon(prev_end, seg_start);
                prev_end = seg_end;
                nfa.add_epsilon(prev_end, end);
                count += 1;
            }
        }
        None => {
            let (seg_start, seg_end) = compile_pattern(nfa, pattern)?;
            nfa.add_epsilon(current, seg_start);
            nfa.add_epsilon(seg_end, current);
            nfa.add_epsilon(seg_end, end);
        }
    }

    Ok((start, end))
}

fn emit_class(nfa: &mut DynamicNfa, from: u16, to: u16, atoms: &[ClassAtom]) {
    for atom in atoms {
        match atom {
            ClassAtom::Char(ch) => nfa.add_transition(from, to, *ch, *ch),
            ClassAtom::Range { start, end } => nfa.add_transition(from, to, *start, *end),
        }
    }
}
