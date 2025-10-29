use alloc::vec;
use alloc::vec::Vec;

#[derive(Clone, PartialEq, Eq)]
pub struct Bitset {
    data: Vec<bool>,
}

impl Bitset {
    pub fn new(size: usize) -> Self {
        Self {
            data: vec![false; size],
        }
    }

    pub fn insert(&mut self, bit: usize) {
        if bit < self.data.len() {
            self.data[bit] = true;
        }
    }

    pub fn contains(&self, bit: usize) -> bool {
        bit < self.data.len() && self.data[bit]
    }

    pub fn union_with(&mut self, other: &Self) -> bool {
        let len = self.data.len().min(other.data.len());
        let mut changed = false;
        for idx in 0..len {
            let new_value = self.data[idx] || other.data[idx];
            if new_value != self.data[idx] {
                self.data[idx] = new_value;
                changed = true;
            }
        }
        changed
    }

    pub fn is_empty(&self) -> bool {
        self.data.iter().all(|flag| !*flag)
    }

    pub fn iter(&self) -> impl Iterator<Item = usize> + '_ {
        self.data
            .iter()
            .enumerate()
            .filter(|(_, flag)| **flag)
            .map(|(idx, _)| idx)
    }

    pub fn to_vec(&self) -> Vec<bool> {
        self.data.clone()
    }
}
