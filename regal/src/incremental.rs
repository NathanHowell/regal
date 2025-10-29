use crate::bitset::Bitset;
use crate::compile::CompiledLexer;
use crate::lexer::{Advance, Checkpoint, Lexer, LexerError, Match};
use core::ops::Range;
use heapless::Vec;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct TokenRecord<T> {
    pub id: u16,
    pub token: T,
    pub start: usize,
    pub end: usize,
    pub skipped: bool,
    pub entry: Checkpoint,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct PartialCandidate<T> {
    pub id: u16,
    pub token: T,
    pub skipped: bool,
    pub priority: u16,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PartialToken<'a, T, const TOKENS: usize> {
    pub start: usize,
    pub fragment: &'a str,
    pub entry: Checkpoint,
    pub primary: Option<PartialCandidate<T>>,
    pub candidates: Vec<PartialCandidate<T>, TOKENS>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum IncrementalError {
    TokenOverflow,
    Reject { offset: usize, span: usize },
    UnknownToken(u16),
    InvalidEdit,
    LengthMismatch { expected: usize, actual: usize },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextEdit {
    pub range: Range<usize>,
    pub replacement_len: usize,
}

#[derive(Debug)]
pub struct TokenCache<T, const MAX_TOKENS: usize> {
    tokens: Vec<TokenRecord<T>, MAX_TOKENS>,
    tail: Checkpoint,
    source_len: usize,
}

impl<T, const MAX_TOKENS: usize> TokenCache<T, MAX_TOKENS>
where
    T: Copy + Default,
{
    pub const fn new() -> Self {
        Self {
            tokens: Vec::new(),
            tail: Checkpoint::start(0),
            source_len: 0,
        }
    }

    pub fn clear(&mut self) {
        self.tokens.clear();
        self.tail = Checkpoint::start(0);
        self.source_len = 0;
    }

    pub fn tokens(&self) -> &[TokenRecord<T>] {
        self.tokens.as_slice()
    }

    pub fn tail(&self) -> Checkpoint {
        self.tail
    }

    pub fn rebuild<
        'a,
        const TOKENS: usize,
        const STATES: usize,
        const TRANSITIONS: usize,
        const DENSE: usize,
    >(
        &mut self,
        compiled: &'a CompiledLexer<T, TOKENS, STATES, TRANSITIONS, DENSE>,
        input: &'a str,
    ) -> Result<Option<PartialToken<'a, T, TOKENS>>, IncrementalError> {
        self.tokens.clear();
        let mut lexer = compiled.lexer();
        let (tail, partial) = run_lexer(compiled, &mut lexer, input, 0, &mut self.tokens)?;
        self.tail = tail;
        self.source_len = input.len();
        Ok(partial)
    }

    pub fn apply_edit<
        'a,
        const TOKENS: usize,
        const STATES: usize,
        const TRANSITIONS: usize,
        const DENSE: usize,
    >(
        &mut self,
        compiled: &'a CompiledLexer<T, TOKENS, STATES, TRANSITIONS, DENSE>,
        input: &'a str,
        edit: TextEdit,
    ) -> Result<Option<PartialToken<'a, T, TOKENS>>, IncrementalError> {
        if edit.range.end > self.source_len {
            return Err(IncrementalError::InvalidEdit);
        }
        let replaced = edit.range.end.saturating_sub(edit.range.start);
        let expected = self.source_len - replaced + edit.replacement_len;
        if expected != input.len() {
            return Err(IncrementalError::LengthMismatch {
                expected,
                actual: input.len(),
            });
        }

        let restart_index = find_restart_index(self.tokens.as_slice(), edit.range.start);
        let (restart_offset, checkpoint) = match restart_index {
            idx if idx < self.tokens.len() => {
                let token = self.tokens[idx];
                (token.start, token.entry)
            }
            _ => {
                let fallback = if self.tokens.is_empty() {
                    Checkpoint::start(compiled.dfa.start_state())
                } else {
                    self.tail
                };
                (core::cmp::min(edit.range.start, input.len()), fallback)
            }
        };

        self.tokens.truncate(restart_index);
        let mut lexer = compiled.lexer();
        lexer.restore(checkpoint);
        let (tail, partial) = run_lexer(
            compiled,
            &mut lexer,
            input,
            restart_offset,
            &mut self.tokens,
        )?;
        self.tail = tail;
        self.source_len = input.len();
        Ok(partial)
    }

    pub fn cursor<'cache>(&'cache self, cursor: usize) -> CursorView<'cache, T> {
        let mut index = 0;
        while index < self.tokens.len() {
            let token = &self.tokens[index];
            if cursor < token.start {
                break;
            }
            if cursor < token.end {
                return CursorView {
                    preceding: &self.tokens[..index],
                    containing: Some(token),
                    following: &self.tokens[index + 1..],
                };
            }
            index += 1;
        }

        CursorView {
            preceding: &self.tokens[..index],
            containing: None,
            following: &self.tokens[index..],
        }
    }
}

impl<T, const MAX_TOKENS: usize> Default for TokenCache<T, MAX_TOKENS>
where
    T: Copy + Default,
{
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy)]
pub struct CursorView<'cache, T> {
    pub preceding: &'cache [TokenRecord<T>],
    pub containing: Option<&'cache TokenRecord<T>>,
    pub following: &'cache [TokenRecord<T>],
}

fn find_restart_index<T>(tokens: &[TokenRecord<T>], offset: usize) -> usize {
    let mut idx = 0;
    while idx < tokens.len() {
        if tokens[idx].end > offset {
            break;
        }
        idx += 1;
    }
    idx
}

fn run_lexer<
    'a,
    T,
    const TOKENS: usize,
    const STATES: usize,
    const TRANSITIONS: usize,
    const MAX_TOKENS: usize,
    const DENSE: usize,
>(
    compiled: &'a CompiledLexer<T, TOKENS, STATES, TRANSITIONS, DENSE>,
    lexer: &mut Lexer<'a, T, TOKENS, STATES, TRANSITIONS, DENSE>,
    input: &'a str,
    mut offset: usize,
    records: &mut Vec<TokenRecord<T>, MAX_TOKENS>,
) -> Result<(Checkpoint, Option<PartialToken<'a, T, TOKENS>>), IncrementalError>
where
    T: Copy + Default,
{
    if offset > input.len() {
        offset = input.len();
    }
    let mut token_start = offset;
    let mut entry = lexer.checkpoint();

    while offset < input.len() {
        let slice = &input[offset..];
        let mut chars = slice.chars();
        let ch = match chars.next() {
            Some(ch) => ch,
            None => break,
        };
        let ch_len = ch.len_utf8();
        match lexer.advance(ch) {
            Advance::Progress => {
                offset += ch_len;
            }
            Advance::Emit {
                token,
                token_id,
                length: _,
                reprocess: _,
            } => {
                let info = compiled
                    .token_entry(token_id)
                    .ok_or(IncrementalError::UnknownToken(token_id))?;
                let record = TokenRecord {
                    id: token_id,
                    token,
                    start: token_start,
                    end: offset,
                    skipped: info.skip,
                    entry,
                };
                records
                    .push(record)
                    .map_err(|_| IncrementalError::TokenOverflow)?;
                entry = lexer.checkpoint();
                token_start = offset;
            }
            Advance::Skip {
                length: _,
                token_id,
                reprocess: _,
            } => {
                let info = compiled
                    .token_entry(token_id)
                    .ok_or(IncrementalError::UnknownToken(token_id))?;
                let record = TokenRecord {
                    id: token_id,
                    token: info.token,
                    start: token_start,
                    end: offset,
                    skipped: true,
                    entry,
                };
                records
                    .push(record)
                    .map_err(|_| IncrementalError::TokenOverflow)?;
                entry = lexer.checkpoint();
                token_start = offset;
            }
            Advance::Reject { span } => {
                return Err(IncrementalError::Reject { offset, span });
            }
        }
    }

    match lexer.finish() {
        Ok(Some(Match {
            token,
            token_id,
            length: _,
        })) => {
            let info = compiled
                .token_entry(token_id)
                .ok_or(IncrementalError::UnknownToken(token_id))?;
            let record = TokenRecord {
                id: token_id,
                token,
                start: token_start,
                end: offset,
                skipped: info.skip,
                entry,
            };
            records
                .push(record)
                .map_err(|_| IncrementalError::TokenOverflow)?;
            Ok((lexer.checkpoint(), None))
        }
        Ok(None) => Ok((lexer.checkpoint(), None)),
        Err(LexerError::UnexpectedEnd { span: _ }) => {
            let fragment = &input[token_start..offset];
            let state_checkpoint = lexer.checkpoint();
            let possible = compiled.dfa.possible_tokens(state_checkpoint.state());
            let partial = build_partial(compiled, fragment, token_start, entry, possible);
            Ok((state_checkpoint, Some(partial)))
        }
        Err(LexerError::NoMatch) => Err(IncrementalError::Reject { offset, span: 0 }),
    }
}

fn build_partial<
    'a,
    T,
    const TOKENS: usize,
    const STATES: usize,
    const TRANSITIONS: usize,
    const DENSE: usize,
>(
    compiled: &'a CompiledLexer<T, TOKENS, STATES, TRANSITIONS, DENSE>,
    fragment: &'a str,
    start: usize,
    entry: Checkpoint,
    possible: Bitset<TOKENS>,
) -> PartialToken<'a, T, TOKENS>
where
    T: Copy + Default,
{
    let mut candidates: Vec<PartialCandidate<T>, TOKENS> = Vec::new();
    let mut primary: Option<PartialCandidate<T>> = None;
    for id in possible.iter() {
        if let Some(info) = compiled.token_entry(id as u16) {
            let candidate = PartialCandidate {
                id: id as u16,
                token: info.token,
                skipped: info.skip,
                priority: info.priority,
            };
            if let Some(best) = &mut primary {
                if candidate.priority < best.priority
                    || (candidate.priority == best.priority && candidate.id < best.id)
                {
                    *best = candidate;
                }
            } else {
                primary = Some(candidate);
            }
            let _ = candidates.push(candidate);
        }
    }

    PartialToken {
        start,
        fragment,
        entry,
        primary,
        candidates,
    }
}
