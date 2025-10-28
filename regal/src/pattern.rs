//! Static pattern descriptors used to construct lexical recognisers.
//!
//! The pattern API mirrors a small regular-expression algebra that can be
//! assembled from constants. A pattern is represented as a tree of
//! [`PatternNode`] values and wrapped in a [`Pattern`] handle. Because the
//! structures are `Copy` and `no_std`, patterns can be defined entirely with
//! `const` items and fed into [`crate::compile::compile`].

use core::fmt;

#[derive(Copy, Clone)]
pub struct Pattern<'a> {
    node: &'a PatternNode<'a>,
}

impl<'a> Pattern<'a> {
    pub const fn new(node: &'a PatternNode<'a>) -> Self {
        Self { node }
    }

    pub const fn node(&self) -> &'a PatternNode<'a> {
        self.node
    }
}

impl fmt::Debug for Pattern<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.node.fmt(f)
    }
}

#[derive(Copy, Clone)]
pub enum PatternNode<'a> {
    Empty,
    Literal(&'a [u8]),
    Char(u32),
    Range {
        start: u32,
        end: u32,
    },
    Class(&'a [ClassAtom]),
    Sequence(&'a [Pattern<'a>]),
    Alternate(&'a [Pattern<'a>]),
    Repeat {
        inner: Pattern<'a>,
        min: u32,
        max: Option<u32>,
    },
}

#[derive(Copy, Clone)]
pub enum ClassAtom {
    Char(u32),
    Range { start: u32, end: u32 },
    Category(CharCategory),
}

#[derive(Copy, Clone, Debug)]
pub enum CharCategory {
    Alphabetic,
    Numeric,
    Alphanumeric,
    Whitespace,
    Lowercase,
    Uppercase,
    HexDigit,
}

impl ClassAtom {
    pub const fn matches(&self, ch: u32) -> bool {
        match self {
            Self::Char(c) => *c == ch,
            Self::Range { start, end } => *start <= ch && ch <= *end,
            Self::Category(cat) => match cat {
                CharCategory::Alphabetic => is_alpha(ch),
                CharCategory::Numeric => is_numeric(ch),
                CharCategory::Alphanumeric => is_alphanumeric(ch),
                CharCategory::Whitespace => is_whitespace(ch),
                CharCategory::Lowercase => is_lowercase(ch),
                CharCategory::Uppercase => is_uppercase(ch),
                CharCategory::HexDigit => is_hex_digit(ch),
            },
        }
    }
}

const fn is_alpha(ch: u32) -> bool {
    matches!(ch, 0x41..=0x5A | 0x61..=0x7A)
}

const fn is_numeric(ch: u32) -> bool {
    matches!(ch, 0x30..=0x39)
}

const fn is_alphanumeric(ch: u32) -> bool {
    is_alpha(ch) || is_numeric(ch)
}

const fn is_whitespace(ch: u32) -> bool {
    matches!(ch, 0x09 | 0x0A | 0x0B | 0x0C | 0x0D | 0x20)
}

const fn is_lowercase(ch: u32) -> bool {
    matches!(ch, 0x61..=0x7A)
}

const fn is_uppercase(ch: u32) -> bool {
    matches!(ch, 0x41..=0x5A)
}

const fn is_hex_digit(ch: u32) -> bool {
    matches!(ch, 0x30..=0x39 | 0x41..=0x46 | 0x61..=0x66)
}

impl fmt::Debug for PatternNode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PatternNode::Empty => f.write_str("ε"),
            PatternNode::Literal(bytes) => write!(f, "literal({:?})", bytes),
            PatternNode::Char(ch) => write!(f, "char({:#X})", ch),
            PatternNode::Range { start, end } => write!(f, "range({:#X}-{:#X})", start, end),
            PatternNode::Class(items) => {
                f.write_str("class[")?;
                for (i, item) in items.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{:?}", item)?;
                }
                f.write_str("]")
            }
            PatternNode::Sequence(parts) => {
                f.write_str("seq(")?;
                for (i, part) in parts.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{:?}", part)?;
                }
                f.write_str(")")
            }
            PatternNode::Alternate(parts) => {
                f.write_str("alt(")?;
                for (i, part) in parts.iter().enumerate() {
                    if i != 0 {
                        f.write_str(" | ")?;
                    }
                    write!(f, "{:?}", part)?;
                }
                f.write_str(")")
            }
            PatternNode::Repeat { inner, min, max } => {
                f.write_str("repeat(")?;
                write!(f, "{:?}", inner)?;
                match max {
                    Some(max) => write!(f, ", {}..={})", min, max),
                    None => write!(f, ", {}..=∞)", min),
                }
            }
        }
    }
}

impl fmt::Debug for ClassAtom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClassAtom::Char(ch) => match core::char::from_u32(*ch) {
                Some(display) => write!(f, "'{}'", display),
                None => write!(f, "'\\u{{{:X}}}'", ch),
            },
            ClassAtom::Range { start, end } => {
                match (core::char::from_u32(*start), core::char::from_u32(*end)) {
                    (Some(lo), Some(hi)) => write!(f, "['{}'-'{}']", lo, hi),
                    _ => write!(f, "[\\u{{{:X}}}-\\u{{{:X}}}]", start, end),
                }
            }
            ClassAtom::Category(cat) => write!(f, "<{:?}>", cat),
        }
    }
}
