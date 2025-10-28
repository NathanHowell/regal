use crate::pattern::{CharCategory, ClassAtom, Pattern, PatternNode};
use heapless::Vec;

#[derive(Clone)]
pub struct Nfa<const MAX_STATES: usize, const MAX_TRANSITIONS: usize, const MAX_EPSILONS: usize> {
    pub states: Vec<NfaState, MAX_STATES>,
    pub transitions: Vec<NfaTransition, MAX_TRANSITIONS>,
    pub epsilons: Vec<NfaEpsilon, MAX_EPSILONS>,
}

#[derive(Copy, Clone)]
pub struct NfaState {
    pub accept_token: Option<u16>,
    pub priority: u16,
}

impl Default for NfaState {
    fn default() -> Self {
        Self {
            accept_token: None,
            priority: u16::MAX,
        }
    }
}

#[derive(Copy, Clone, Default)]
pub struct NfaTransition {
    pub from: u16,
    pub to: u16,
    pub start: u32,
    pub end: u32,
}

#[derive(Copy, Clone, Default)]
pub struct NfaEpsilon {
    pub from: u16,
    pub to: u16,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NfaError {
    StateOverflow,
    TransitionOverflow,
    EpsilonOverflow,
    InvalidRepeat,
}

impl<const MAX_STATES: usize, const MAX_TRANSITIONS: usize, const MAX_EPSILONS: usize>
    Nfa<MAX_STATES, MAX_TRANSITIONS, MAX_EPSILONS>
{
    pub const fn new() -> Self {
        Self {
            states: Vec::new(),
            transitions: Vec::new(),
            epsilons: Vec::new(),
        }
    }

    pub fn add_state(&mut self) -> Result<u16, NfaError> {
        self.states
            .push(NfaState::default())
            .map_err(|_| NfaError::StateOverflow)?;
        Ok((self.states.len() - 1) as u16)
    }

    pub fn mark_accept(&mut self, state: u16, token: u16, priority: u16) {
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

    pub fn add_transition(
        &mut self,
        from: u16,
        to: u16,
        start: u32,
        end: u32,
    ) -> Result<(), NfaError> {
        self.transitions
            .push(NfaTransition {
                from,
                to,
                start,
                end,
            })
            .map_err(|_| NfaError::TransitionOverflow)
    }

    pub fn add_epsilon(&mut self, from: u16, to: u16) -> Result<(), NfaError> {
        self.epsilons
            .push(NfaEpsilon { from, to })
            .map_err(|_| NfaError::EpsilonOverflow)
    }
}

pub fn compile_pattern<
    const MAX_STATES: usize,
    const MAX_TRANSITIONS: usize,
    const MAX_EPSILONS: usize,
>(
    nfa: &mut Nfa<MAX_STATES, MAX_TRANSITIONS, MAX_EPSILONS>,
    pattern: Pattern<'_>,
) -> Result<(u16, u16), NfaError> {
    match pattern.node() {
        PatternNode::Empty => {
            let start = nfa.add_state()?;
            let end = nfa.add_state()?;
            nfa.add_epsilon(start, end)?;
            Ok((start, end))
        }
        PatternNode::Literal(bytes) => {
            let start = nfa.add_state()?;
            let mut prev = start;
            for &byte in (*bytes).iter() {
                let next = nfa.add_state()?;
                nfa.add_transition(prev, next, byte as u32, byte as u32)?;
                prev = next;
            }
            Ok((start, prev))
        }
        PatternNode::Char(code) => {
            let start = nfa.add_state()?;
            let end = nfa.add_state()?;
            nfa.add_transition(start, end, *code, *code)?;
            Ok((start, end))
        }
        PatternNode::Range { start, end } => {
            let s = nfa.add_state()?;
            let e = nfa.add_state()?;
            nfa.add_transition(s, e, *start, *end)?;
            Ok((s, e))
        }
        PatternNode::Class(atoms) => {
            let start = nfa.add_state()?;
            let end = nfa.add_state()?;
            emit_class(nfa, start, end, atoms)?;
            Ok((start, end))
        }
        PatternNode::Sequence(parts) => compile_sequence(nfa, parts),
        PatternNode::Alternate(parts) => compile_alternate(nfa, parts),
        PatternNode::Repeat { inner, min, max } => {
            compile_repeat(nfa, *inner, *min as usize, max.map(|v| v as usize))
        }
    }
}

fn compile_sequence<
    const MAX_STATES: usize,
    const MAX_TRANSITIONS: usize,
    const MAX_EPSILONS: usize,
>(
    nfa: &mut Nfa<MAX_STATES, MAX_TRANSITIONS, MAX_EPSILONS>,
    parts: &[Pattern<'_>],
) -> Result<(u16, u16), NfaError> {
    let mut iter = parts.iter();
    if let Some(first) = iter.next() {
        let (start, mut end) = compile_pattern(nfa, *first)?;
        for part in iter {
            let (next_start, next_end) = compile_pattern(nfa, *part)?;
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

fn compile_alternate<
    const MAX_STATES: usize,
    const MAX_TRANSITIONS: usize,
    const MAX_EPSILONS: usize,
>(
    nfa: &mut Nfa<MAX_STATES, MAX_TRANSITIONS, MAX_EPSILONS>,
    parts: &[Pattern<'_>],
) -> Result<(u16, u16), NfaError> {
    let start = nfa.add_state()?;
    let end = nfa.add_state()?;
    if parts.is_empty() {
        nfa.add_epsilon(start, end)?;
        return Ok((start, end));
    }
    for part in parts {
        let (branch_start, branch_end) = compile_pattern(nfa, *part)?;
        nfa.add_epsilon(start, branch_start)?;
        nfa.add_epsilon(branch_end, end)?;
    }
    Ok((start, end))
}

fn compile_repeat<
    const MAX_STATES: usize,
    const MAX_TRANSITIONS: usize,
    const MAX_EPSILONS: usize,
>(
    nfa: &mut Nfa<MAX_STATES, MAX_TRANSITIONS, MAX_EPSILONS>,
    pattern: Pattern<'_>,
    min: usize,
    max: Option<usize>,
) -> Result<(u16, u16), NfaError> {
    if let Some(max) = max {
        if max < min {
            return Err(NfaError::InvalidRepeat);
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

fn emit_class<const MAX_STATES: usize, const MAX_TRANSITIONS: usize, const MAX_EPSILONS: usize>(
    nfa: &mut Nfa<MAX_STATES, MAX_TRANSITIONS, MAX_EPSILONS>,
    from: u16,
    to: u16,
    atoms: &[ClassAtom],
) -> Result<(), NfaError> {
    for atom in atoms {
        match atom {
            ClassAtom::Char(ch) => nfa.add_transition(from, to, *ch, *ch)?,
            ClassAtom::Range { start, end } => nfa.add_transition(from, to, *start, *end)?,
            ClassAtom::Category(cat) => emit_category(nfa, from, to, *cat)?,
        }
    }
    Ok(())
}

fn emit_category<
    const MAX_STATES: usize,
    const MAX_TRANSITIONS: usize,
    const MAX_EPSILONS: usize,
>(
    nfa: &mut Nfa<MAX_STATES, MAX_TRANSITIONS, MAX_EPSILONS>,
    from: u16,
    to: u16,
    cat: CharCategory,
) -> Result<(), NfaError> {
    match cat {
        CharCategory::Alphabetic => {
            nfa.add_transition(from, to, b'a' as u32, b'z' as u32)?;
            nfa.add_transition(from, to, b'A' as u32, b'Z' as u32)?;
        }
        CharCategory::Numeric => {
            nfa.add_transition(from, to, b'0' as u32, b'9' as u32)?;
        }
        CharCategory::Alphanumeric => {
            emit_category(nfa, from, to, CharCategory::Alphabetic)?;
            emit_category(nfa, from, to, CharCategory::Numeric)?;
        }
        CharCategory::Whitespace => {
            for ch in [0x09u32, 0x0A, 0x0B, 0x0C, 0x0D, 0x20] {
                nfa.add_transition(from, to, ch, ch)?;
            }
        }
        CharCategory::Lowercase => {
            nfa.add_transition(from, to, b'a' as u32, b'z' as u32)?;
        }
        CharCategory::Uppercase => {
            nfa.add_transition(from, to, b'A' as u32, b'Z' as u32)?;
        }
        CharCategory::HexDigit => {
            emit_category(nfa, from, to, CharCategory::Numeric)?;
            nfa.add_transition(from, to, b'a' as u32, b'f' as u32)?;
            nfa.add_transition(from, to, b'A' as u32, b'F' as u32)?;
        }
    }
    Ok(())
}
