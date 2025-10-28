use crate::pattern::{ClassAtom, PatternExpr};
use std::vec::Vec;

#[derive(Clone)]
pub(crate) struct NfaSpec {
    pub(crate) pattern: PatternExpr,
    pub(crate) token_id: u16,
    pub(crate) priority: u16,
}

#[derive(Clone)]
pub(crate) struct NfaState {
    pub(crate) accept_token: Option<u16>,
    pub(crate) priority: u16,
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
pub(crate) struct NfaTransition {
    pub(crate) from: u16,
    pub(crate) to: u16,
    pub(crate) start: u32,
    pub(crate) end: u32,
}

#[derive(Clone)]
pub(crate) struct NfaEpsilon {
    pub(crate) from: u16,
    pub(crate) to: u16,
}

pub(crate) struct DynamicNfa {
    pub(crate) states: Vec<NfaState>,
    pub(crate) transitions: Vec<NfaTransition>,
    pub(crate) epsilons: Vec<NfaEpsilon>,
    pub(crate) start: u16,
}

pub(crate) fn build_nfa(specs: &[NfaSpec]) -> Result<DynamicNfa, String> {
    let mut nfa = DynamicNfa {
        states: Vec::new(),
        transitions: Vec::new(),
        epsilons: Vec::new(),
        start: 0,
    };

    let start = nfa.add_state()?;
    nfa.start = start;

    for spec in specs {
        let (pat_start, pat_end) = compile_pattern(&mut nfa, &spec.pattern)?;
        nfa.add_epsilon(start, pat_start)?;
        nfa.mark_accept(pat_end, spec.token_id, spec.priority);
    }

    Ok(nfa)
}

impl DynamicNfa {
    fn add_state(&mut self) -> Result<u16, String> {
        if self.states.len() >= u16::MAX as usize {
            return Err("NFA state limit exceeded".into());
        }
        self.states.push(NfaState::new());
        Ok((self.states.len() - 1) as u16)
    }

    fn add_transition(&mut self, from: u16, to: u16, start: u32, end: u32) -> Result<(), String> {
        self.transitions.push(NfaTransition {
            from,
            to,
            start,
            end,
        });
        Ok(())
    }

    fn add_epsilon(&mut self, from: u16, to: u16) -> Result<(), String> {
        self.epsilons.push(NfaEpsilon { from, to });
        Ok(())
    }

    fn mark_accept(&mut self, state: u16, token: u16, priority: u16) {
        if let Some(entry) = self.states.get_mut(state as usize) {
            match entry.accept_token {
                Some(existing) => {
                    if priority < entry.priority {
                        entry.accept_token = Some(token);
                        entry.priority = priority;
                    } else if priority == entry.priority && token < existing {
                        entry.accept_token = Some(token);
                    }
                }
                None => {
                    entry.accept_token = Some(token);
                    entry.priority = priority;
                }
            }
        }
    }
}

fn compile_pattern(nfa: &mut DynamicNfa, pattern: &PatternExpr) -> Result<(u16, u16), String> {
    match pattern {
        PatternExpr::Empty => {
            let start = nfa.add_state()?;
            let end = nfa.add_state()?;
            nfa.add_epsilon(start, end)?;
            Ok((start, end))
        }
        PatternExpr::Literal(bytes) => {
            let start = nfa.add_state()?;
            let mut prev = start;
            for &byte in bytes {
                let next = nfa.add_state()?;
                nfa.add_transition(prev, next, byte as u32, byte as u32)?;
                prev = next;
            }
            Ok((start, prev))
        }
        PatternExpr::Class(atoms) => {
            let start = nfa.add_state()?;
            let end = nfa.add_state()?;
            emit_class(nfa, start, end, atoms)?;
            Ok((start, end))
        }
        PatternExpr::Sequence(parts) => compile_sequence(nfa, parts),
        PatternExpr::Alternate(parts) => compile_alternate(nfa, parts),
        PatternExpr::Repeat { inner, min, max } => {
            compile_repeat(nfa, inner.as_ref(), *min as usize, max.map(|m| m as usize))
        }
    }
}

fn compile_sequence(nfa: &mut DynamicNfa, parts: &[PatternExpr]) -> Result<(u16, u16), String> {
    let mut iter = parts.iter();
    if let Some(first) = iter.next() {
        let (start, mut end) = compile_pattern(nfa, first)?;
        for part in iter {
            let (next_start, next_end) = compile_pattern(nfa, part)?;
            nfa.add_epsilon(end, next_start)?;
            end = next_end;
        }
        Ok((start, end))
    } else {
        let start = nfa.add_state()?;
        let end = nfa.add_state()?;
        nfa.add_epsilon(start, end)?;
        Ok((start, end))
    }
}

fn compile_alternate(nfa: &mut DynamicNfa, parts: &[PatternExpr]) -> Result<(u16, u16), String> {
    let start = nfa.add_state()?;
    let end = nfa.add_state()?;
    if parts.is_empty() {
        nfa.add_epsilon(start, end)?;
        return Ok((start, end));
    }
    for part in parts {
        let (branch_start, branch_end) = compile_pattern(nfa, part)?;
        nfa.add_epsilon(start, branch_start)?;
        nfa.add_epsilon(branch_end, end)?;
    }
    Ok((start, end))
}

fn compile_repeat(
    nfa: &mut DynamicNfa,
    pattern: &PatternExpr,
    min: usize,
    max: Option<usize>,
) -> Result<(u16, u16), String> {
    if let Some(max) = max {
        if max < min {
            return Err("invalid repetition range".into());
        }
    }
    let start = nfa.add_state()?;
    let end = nfa.add_state()?;
    let mut current = start;

    for _ in 0..min {
        let (seg_start, seg_end) = compile_pattern(nfa, pattern)?;
        nfa.add_epsilon(current, seg_start)?;
        current = seg_end;
    }

    nfa.add_epsilon(current, end)?;

    match max {
        Some(limit) => {
            let mut count = min;
            let mut prev_end = current;
            while count < limit {
                let (seg_start, seg_end) = compile_pattern(nfa, pattern)?;
                nfa.add_epsilon(prev_end, seg_start)?;
                prev_end = seg_end;
                nfa.add_epsilon(prev_end, end)?;
                count += 1;
            }
        }
        None => {
            let (seg_start, seg_end) = compile_pattern(nfa, pattern)?;
            nfa.add_epsilon(current, seg_start)?;
            nfa.add_epsilon(seg_end, current)?;
            nfa.add_epsilon(seg_end, end)?;
        }
    }

    Ok((start, end))
}

fn emit_class(nfa: &mut DynamicNfa, from: u16, to: u16, atoms: &[ClassAtom]) -> Result<(), String> {
    for atom in atoms {
        match atom {
            ClassAtom::Char(ch) => nfa.add_transition(from, to, *ch, *ch)?,
            ClassAtom::Range { start, end } => nfa.add_transition(from, to, *start, *end)?,
        }
    }
    Ok(())
}
