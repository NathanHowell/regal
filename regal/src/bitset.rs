use core::fmt;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Bitset<const N: usize> {
    data: [bool; N],
}

impl<const N: usize> Bitset<N> {
    pub const fn new() -> Self {
        Self { data: [false; N] }
    }

    pub const fn from_array(data: [bool; N]) -> Self {
        Self { data }
    }

    pub const fn is_empty(&self) -> bool {
        let mut idx = 0;
        while idx < N {
            if self.data[idx] {
                return false;
            }
            idx += 1;
        }
        true
    }

    pub const fn contains(&self, bit: usize) -> bool {
        if bit < N { self.data[bit] } else { false }
    }

    #[cfg_attr(not(feature = "alloc"), allow(dead_code))]
    pub(crate) const fn insert(&mut self, bit: usize) {
        if bit < N {
            self.data[bit] = true;
        }
    }

    #[allow(dead_code)]
    pub(crate) const fn union_with(&mut self, other: &Self) {
        let mut idx = 0;
        while idx < N {
            self.data[idx] = self.data[idx] || other.data[idx];
            idx += 1;
        }
    }

    pub(crate) const fn iter(&self) -> BitsetIter<N> {
        BitsetIter {
            set: *self,
            next: 0,
        }
    }
}

impl<const N: usize> Default for Bitset<N> {
    fn default() -> Self {
        Self::new()
    }
}

impl<const N: usize> fmt::Debug for Bitset<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;
        write!(f, "{{")?;
        for bit in self.iter() {
            if !first {
                write!(f, ", ")?;
            }
            write!(f, "{}", bit)?;
            first = false;
        }
        write!(f, "}}")
    }
}

pub(crate) struct BitsetIter<const N: usize> {
    set: Bitset<N>,
    next: usize,
}

impl<const N: usize> Iterator for BitsetIter<N> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        while self.next < N {
            let bit = self.next;
            self.next += 1;
            if self.set.contains(bit) {
                return Some(bit);
            }
        }
        None
    }
}
