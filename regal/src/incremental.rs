//! Incremental lexing support.
//!
//! A [`TokenCache`] keeps a minimally invalidated stream of [`TokenRecord`]s
//! synced with a text buffer as edits arrive. Each record carries the
//! [`Checkpoint`] captured just before its token began, letting
//! [`TokenCache::apply_edit`] replay the lexer from the nearest unaffected
//! token boundary rather than from the start of the buffer.
//!
//! # Offset model
//!
//! All offsets exposed by this module — [`TokenRecord`]'s `start`/`end`,
//! [`TextEdit::range`], [`PartialToken::start`], and the `cursor` argument
//! to [`TokenCache::cursor`] — are **byte** offsets into the current `&str`
//! source. Offsets produced by the lexer always fall on UTF-8 char
//! boundaries. Offsets supplied by callers (in [`TextEdit`] and
//! [`TokenCache::cursor`]) must also fall on char boundaries; passing a
//! mid-codepoint offset can cause a panic inside the lexer's UTF-8
//! slicing.

use crate::bitset::Bitset;
use crate::compile::CompiledLexer;
use crate::lexer::{Advance, Checkpoint, Lexer, LexerError, Match};
use core::ops::Range;
use heapless::Vec;

/// A single lexed token in a [`TokenCache`].
///
/// `start`/`end` are byte offsets into the source `&str` that produced the
/// record; the substring `source[start..end]` is the token's text. `entry`
/// is the lexer [`Checkpoint`] captured immediately before this token
/// began being scanned — it is the point [`TokenCache::apply_edit`] will
/// replay from if an edit invalidates the token.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct TokenRecord<T> {
    /// Numeric token id assigned by the compiled lexer.
    pub id: u16,
    /// User-facing token payload (typically an enum variant).
    pub token: T,
    /// Byte offset in the source where this token starts.
    pub start: usize,
    /// Byte offset in the source one past this token's last byte.
    pub end: usize,
    /// `true` if the token was produced by a `skip` pattern and should be
    /// treated as trivia.
    pub skipped: bool,
    /// Lexer state captured immediately before the first character of
    /// this token was consumed. Used by [`TokenCache::apply_edit`] as the
    /// replay point when the token is invalidated.
    pub entry: Checkpoint,
}

/// One possible completion of a [`PartialToken`].
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct PartialCandidate<T> {
    /// Numeric token id assigned by the compiled lexer.
    pub id: u16,
    /// Token payload that would be emitted if this candidate resolves.
    pub token: T,
    /// Whether this candidate is trivia (from a `skip` pattern).
    pub skipped: bool,
    /// Priority from the compiled lexer (lower wins on ties).
    pub priority: u16,
}

/// An in-progress match at the end of the source buffer.
///
/// Returned by [`TokenCache::rebuild`] and [`TokenCache::apply_edit`] when
/// the lexer has consumed characters at the tail of the source but has
/// not reached an accepting state. Typical case: an editor cursor mid-word
/// — the characters are in the DFA but no complete token has been emitted.
///
/// [`Self::primary`] is the best candidate under the usual tie-break rules
/// (lowest `priority`, then lowest `id`); [`Self::candidates`] enumerates
/// every token that could still match if more characters are appended.
/// Use these for completion proposals.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PartialToken<'a, T, const TOKENS: usize> {
    /// Byte offset in the source where this partial match began.
    pub start: usize,
    /// The substring of the source already consumed into the DFA.
    pub fragment: &'a str,
    /// Lexer state captured immediately before `fragment`'s first
    /// character was consumed.
    pub entry: Checkpoint,
    /// Preferred candidate under priority/id tie-breaking.
    pub primary: Option<PartialCandidate<T>>,
    /// Every token that could still match given additional characters.
    pub candidates: Vec<PartialCandidate<T>, TOKENS>,
}

/// Errors produced by [`TokenCache::rebuild`] and [`TokenCache::apply_edit`].
#[derive(Debug, PartialEq, Eq)]
pub enum IncrementalError {
    /// The cache's `MAX_TOKENS` capacity was exceeded while lexing.
    TokenOverflow,
    /// The lexer could not extend the current match and could not
    /// backtrack to an accept state. `offset` is the byte offset where
    /// rejection happened; `span` is the number of bytes already consumed
    /// in the failed attempt.
    Reject {
        /// Byte offset in the source where rejection occurred.
        offset: usize,
        /// Bytes consumed by the failed attempt.
        span: usize,
    },
    /// The lexer emitted a token id unknown to the compiled lexer.
    /// Indicates API misuse — typically mixing a [`TokenCache`] with a
    /// different [`CompiledLexer`] than the one that produced its prior
    /// state.
    UnknownToken(u16),
    /// [`TextEdit::range`]'s end exceeded the pre-edit source length.
    InvalidEdit,
    /// The new source's byte length does not match what the [`TextEdit`]
    /// predicts: `expected == old_source.len() - (range.end - range.start)
    /// + replacement_len`.
    LengthMismatch {
        /// Byte length implied by the edit description.
        expected: usize,
        /// Byte length actually provided.
        actual: usize,
    },
}

/// A single contiguous replacement to apply to a source buffer.
///
/// Bytes `range.start..range.end` in the previous source are replaced by
/// `replacement_len` bytes of new content. The caller is responsible for
/// producing the post-edit `&str` and passing it to
/// [`TokenCache::apply_edit`] alongside the edit description;
/// [`TokenCache::apply_edit`] cross-checks that
/// `new_source.len() == old_source.len() - range.len() + replacement_len`
/// and returns [`IncrementalError::LengthMismatch`] on disagreement.
///
/// # Invariants
///
/// The caller must uphold:
///
/// * `range.start <= range.end`
/// * `range.end <= old_source.len()` (checked; surfaces as
///   [`IncrementalError::InvalidEdit`])
/// * `range.start` and `range.end` fall on UTF-8 char boundaries of the
///   pre-edit source
/// * in the post-edit source, the inserted bytes occupy
///   `range.start..range.start + replacement_len` and that range is
///   char-aligned
///
/// Length invariants are checked and surfaced as errors; char-boundary
/// violations can panic inside the lexer's UTF-8 slicing when the replay
/// falls back to the edit start (i.e. the edit is past the last cached
/// token).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextEdit {
    /// Byte range in the pre-edit source that this edit replaces.
    pub range: Range<usize>,
    /// Byte length of the replacement content.
    pub replacement_len: usize,
}

/// A lexed token stream plus the state needed to update it incrementally
/// as the source changes.
///
/// Construct with [`Self::new`], seed with [`Self::rebuild`] against the
/// initial source, and then apply edits via [`Self::apply_edit`]. All
/// storage is stack-allocated with a `MAX_TOKENS` capacity; overflowing
/// that capacity returns [`IncrementalError::TokenOverflow`] rather than
/// allocating.
///
/// The `T` parameter is the compiled lexer's token payload and must
/// implement `Copy + Default`.
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
    /// Creates an empty cache. Equivalent to [`Default::default`].
    pub const fn new() -> Self {
        Self {
            tokens: Vec::new(),
            tail: Checkpoint::start(0),
            source_len: 0,
        }
    }

    /// Discards all cached tokens and returns the cache to its empty state.
    pub fn clear(&mut self) {
        self.tokens.clear();
        self.tail = Checkpoint::start(0);
        self.source_len = 0;
    }

    /// Returns the cached tokens in source order, including trivia
    /// tokens (those with [`TokenRecord::skipped`] set).
    pub fn tokens(&self) -> &[TokenRecord<T>] {
        self.tokens.as_slice()
    }

    /// Returns the lexer [`Checkpoint`] at the end of the currently
    /// cached source. Safe to use as the resume point for appending
    /// further input.
    pub fn tail(&self) -> Checkpoint {
        self.tail
    }

    /// Discards any cached tokens and lexes `input` from scratch.
    ///
    /// Returns `Ok(Some(partial))` if lexing stopped mid-token at the end
    /// of `input` (see [`PartialToken`]), `Ok(None)` if `input` was
    /// consumed into complete tokens. On success, [`Self::tail`] reflects
    /// the checkpoint at the end of the consumed input and
    /// [`Self::tokens`] exposes the produced stream.
    ///
    /// # Errors
    ///
    /// * [`IncrementalError::TokenOverflow`] — the produced stream would
    ///   exceed `MAX_TOKENS`.
    /// * [`IncrementalError::Reject`] — unrecoverable lexer rejection.
    /// * [`IncrementalError::UnknownToken`] — `compiled` reported a token
    ///   id not in its own table (indicates API misuse).
    pub fn rebuild<
        'a,
        const TOKENS: usize,
        const STATES: usize,
        const TRANSITIONS: usize,
        const DENSE: usize,
        const CLASSES: usize,
    >(
        &mut self,
        compiled: &'a CompiledLexer<T, TOKENS, STATES, TRANSITIONS, DENSE, CLASSES>,
        input: &'a str,
    ) -> Result<Option<PartialToken<'a, T, TOKENS>>, IncrementalError> {
        self.tokens.clear();
        let mut lexer = compiled.lexer();
        let (tail, partial) = run_lexer(compiled, &mut lexer, input, 0, &mut self.tokens)?;
        self.tail = tail;
        self.source_len = input.len();
        Ok(partial)
    }

    /// Updates the cached token stream to reflect `edit` being applied to
    /// the previous source, producing the new source `input`.
    ///
    /// The caller must have already produced `input` by applying `edit`
    /// to the source used for the previous [`Self::rebuild`] or
    /// [`Self::apply_edit`] call. This method truncates the cached
    /// stream back to the nearest token boundary at or before
    /// `edit.range.start`, replays the lexer from that token's
    /// [`TokenRecord::entry`] checkpoint, and re-lexes forward. Tokens
    /// before the restart boundary are preserved in place. Return value
    /// matches [`Self::rebuild`].
    ///
    /// `compiled` must be the same [`CompiledLexer`] used for the
    /// previous call; mixing compiled lexers produces
    /// [`IncrementalError::UnknownToken`] or garbled output.
    ///
    /// # Errors
    ///
    /// * [`IncrementalError::InvalidEdit`] — `edit.range.end` exceeds the
    ///   pre-edit source length recorded by the cache.
    /// * [`IncrementalError::LengthMismatch`] — `input.len()` does not
    ///   match what `edit` predicts for the new source length.
    /// * [`IncrementalError::TokenOverflow`], `Reject`, `UnknownToken` —
    ///   as for [`Self::rebuild`].
    ///
    /// # Panics
    ///
    /// Passing a `TextEdit` whose range is not aligned to UTF-8 char
    /// boundaries of `input` can panic inside the lexer's UTF-8 slicing
    /// when the restart falls back to the edit start offset (i.e. the
    /// edit lies past the last cached token).
    pub fn apply_edit<
        'a,
        const TOKENS: usize,
        const STATES: usize,
        const TRANSITIONS: usize,
        const DENSE: usize,
        const CLASSES: usize,
    >(
        &mut self,
        compiled: &'a CompiledLexer<T, TOKENS, STATES, TRANSITIONS, DENSE, CLASSES>,
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

    /// Returns a [`CursorView`] partitioning the cached tokens relative
    /// to byte position `cursor`.
    ///
    /// A token `t` is the `containing` token iff `t.start <= cursor <
    /// t.end`. An exact-boundary cursor (`cursor == t.end`) places `t`
    /// in `preceding`, not `containing`. The `preceding` and `following`
    /// slices include skipped (trivia) tokens alongside emitted ones.
    ///
    /// `cursor` should be a valid byte offset into the current source;
    /// values beyond the last token's `end` simply yield an empty
    /// `following` slice.
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

/// A partitioning of a [`TokenCache`]'s tokens relative to a cursor.
///
/// Returned by [`TokenCache::cursor`]. `preceding` contains tokens fully
/// before the cursor, `following` contains tokens at or after the cursor,
/// and `containing` is `Some(t)` when the cursor lies strictly inside `t`
/// (i.e. `t.start <= cursor < t.end`).
#[derive(Clone, Copy)]
pub struct CursorView<'cache, T> {
    /// Tokens whose end is at or before the cursor.
    pub preceding: &'cache [TokenRecord<T>],
    /// The token the cursor lies strictly inside, if any.
    pub containing: Option<&'cache TokenRecord<T>>,
    /// Tokens whose start is at or after the cursor (excluding
    /// `containing`).
    pub following: &'cache [TokenRecord<T>],
}

fn find_restart_index<T>(tokens: &[TokenRecord<T>], offset: usize) -> usize {
    let mut idx = 0;
    while idx < tokens.len() {
        if tokens[idx].end >= offset {
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
    const CLASSES: usize,
>(
    compiled: &'a CompiledLexer<T, TOKENS, STATES, TRANSITIONS, DENSE, CLASSES>,
    lexer: &mut Lexer<'a, T, TOKENS, STATES, TRANSITIONS, DENSE, CLASSES>,
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
    const CLASSES: usize,
>(
    compiled: &'a CompiledLexer<T, TOKENS, STATES, TRANSITIONS, DENSE, CLASSES>,
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
