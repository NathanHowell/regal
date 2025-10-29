use alloc::boxed::Box;
use alloc::vec::Vec;

#[derive(Clone, Debug)]
pub enum PatternExpr {
    Empty,
    Literal(Vec<u8>),
    Class(Vec<ClassAtom>),
    Sequence(Vec<PatternExpr>),
    Alternate(Vec<PatternExpr>),
    Repeat {
        inner: Box<PatternExpr>,
        min: u32,
        max: Option<u32>,
    },
}

#[derive(Clone, Debug)]
pub enum ClassAtom {
    Char(u32),
    Range { start: u32, end: u32 },
}
