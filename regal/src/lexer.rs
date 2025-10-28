use crate::compile::CompiledLexer;
use core::marker::PhantomData;

#[derive(Copy, Clone)]
pub struct TokenInfo<T> {
    pub token: T,
    pub skip: bool,
    pub priority: u16,
}

impl<T> Default for TokenInfo<T>
where
    T: Copy + Default,
{
    fn default() -> Self {
        Self {
            token: T::default(),
            skip: false,
            priority: u16::MAX,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Match<T> {
    pub token_id: u16,
    pub token: T,
    pub length: usize,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LexerError {
    NoMatch,
    UnexpectedEnd { span: usize },
}

/// Result of feeding a character into the streaming lexer.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Advance<T> {
    Progress,
    Emit {
        token: T,
        token_id: u16,
        length: usize,
        reprocess: bool,
    },
    Skip {
        length: usize,
        token_id: u16,
        reprocess: bool,
    },
    Reject {
        span: usize,
    },
}

/// Incremental DFA interpreter.
pub struct Lexer<
    'a,
    T,
    const TOKENS: usize,
    const STATES: usize,
    const TRANSITIONS: usize,
    const DENSE: usize,
> where
    T: Copy + Default,
{
    compiled: &'a CompiledLexer<T, TOKENS, STATES, TRANSITIONS, DENSE>,
    state: u16,
    progress_len: usize,
    last_accept: Option<Accept>,
    _marker: PhantomData<T>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) struct Accept {
    token_id: u16,
    length: usize,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Checkpoint {
    pub(crate) state: u16,
    pub(crate) progress_len: usize,
    pub(crate) last_accept: Option<Accept>,
}

impl Checkpoint {
    pub const fn start(state: u16) -> Self {
        Self {
            state,
            progress_len: 0,
            last_accept: None,
        }
    }

    pub(crate) const fn from_parts(
        state: u16,
        progress_len: usize,
        last_accept: Option<Accept>,
    ) -> Self {
        Self {
            state,
            progress_len,
            last_accept,
        }
    }

    pub const fn state(&self) -> u16 {
        self.state
    }
}

impl<'a, T, const TOKENS: usize, const STATES: usize, const TRANSITIONS: usize, const DENSE: usize>
    Lexer<'a, T, TOKENS, STATES, TRANSITIONS, DENSE>
where
    T: Copy + Default,
{
    pub fn new(compiled: &'a CompiledLexer<T, TOKENS, STATES, TRANSITIONS, DENSE>) -> Self {
        let start = Checkpoint::start(compiled.dfa.start_state());
        Self {
            compiled,
            state: start.state,
            progress_len: start.progress_len,
            last_accept: start.last_accept,
            _marker: PhantomData,
        }
    }

    pub fn reset(&mut self) {
        let start = Checkpoint::start(self.compiled.dfa.start_state());
        self.restore(start);
    }

    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint::from_parts(self.state, self.progress_len, self.last_accept)
    }

    pub fn restore(&mut self, checkpoint: Checkpoint) {
        self.state = checkpoint.state;
        self.progress_len = checkpoint.progress_len;
        self.last_accept = checkpoint.last_accept;
    }

    /// Feed a single character into the automaton.
    ///
    /// When `reprocess` is flagged on the returned [`Advance::Emit`] or
    /// [`Advance::Skip`] variant, the caller should continue driving the lexer
    /// with the same character so it can be interpreted as the first symbol of
    /// the next token.
    pub fn advance(&mut self, ch: char) -> Advance<T> {
        let code = ch as u32;
        match self.next_state(self.state, code) {
            Some(next_state) => {
                self.state = next_state;
                self.progress_len += 1;
                if let Some(token_id) = self.compiled.dfa.accept_token(next_state) {
                    self.last_accept = Some(Accept {
                        token_id,
                        length: self.progress_len,
                    });
                }
                Advance::Progress
            }
            None => {
                if let Some(accept) = self.last_accept.take() {
                    let length = accept.length;
                    self.reset_to_start();
                    if let Some(info) = self.compiled.token_entry(accept.token_id) {
                        if info.skip {
                            Advance::Skip {
                                length,
                                token_id: accept.token_id,
                                reprocess: true,
                            }
                        } else {
                            Advance::Emit {
                                token: info.token,
                                token_id: accept.token_id,
                                length,
                                reprocess: true,
                            }
                        }
                    } else {
                        Advance::Reject { span: length }
                    }
                } else {
                    let span = self.progress_len;
                    self.reset_to_start();
                    Advance::Reject { span }
                }
            }
        }
    }

    /// Finalise lexing when the input stream has been exhausted.
    pub fn finish(&mut self) -> Result<Option<Match<T>>, LexerError> {
        if let Some(accept) = self.last_accept.take() {
            self.reset_to_start();
            match self.compiled.token_entry(accept.token_id) {
                Some(info) if info.skip => Ok(None),
                Some(info) => Ok(Some(Match {
                    token_id: accept.token_id,
                    token: info.token,
                    length: accept.length,
                })),
                None => Err(LexerError::NoMatch),
            }
        } else if self.state == self.compiled.dfa.start_state() {
            Ok(None)
        } else {
            Err(LexerError::UnexpectedEnd {
                span: self.progress_len,
            })
        }
    }

    fn reset_to_start(&mut self) {
        self.reset();
    }

    fn next_state(&self, state: u16, ch: u32) -> Option<u16> {
        if let Some(target) = self.compiled.dfa.dense_target(state, ch) {
            return Some(target);
        }

        let transitions = self.compiled.dfa.transitions_for(state);
        for transition in transitions {
            if transition.start <= ch && ch <= transition.end {
                return Some(transition.target);
            }
        }
        None
    }
}
