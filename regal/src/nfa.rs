#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NfaError {
    StateOverflow,
    TransitionOverflow,
    EpsilonOverflow,
    InvalidRepeat,
}
