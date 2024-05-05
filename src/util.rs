use std::{collections::HashSet, hash::BuildHasher, hash::Hash};

/// 与えられた数のLSBからのビットが全て1の数を返す
pub fn bit_mask(bits: u64) -> u64 {
    (0..bits).fold(0, |acc, x| acc | (1 << x))
}

/// HashSetの集合演算を短く書くための拡張トレイト
pub trait HashSetExt<T, S> {
    fn or(&self, other: &HashSet<T, S>) -> HashSet<T, S>;
    fn and(&self, other: &HashSet<T, S>) -> HashSet<T, S>;
    fn sub(&self, other: &HashSet<T, S>) -> HashSet<T, S>;
    fn and_assign(&mut self, other: &HashSet<T, S>);
    fn or_assign(&mut self, other: &HashSet<T, S>);
    fn sub_assign(&mut self, other: &HashSet<T, S>);
}

impl<T, S> HashSetExt<T, S> for HashSet<T, S>
where
    T: Eq + Hash + Copy,
    S: BuildHasher + Default,
{
    fn and(&self, other: &HashSet<T, S>) -> HashSet<T, S> {
        self.intersection(other).copied().collect()
    }

    fn or(&self, other: &HashSet<T, S>) -> HashSet<T, S> {
        self.union(other).copied().collect()
    }

    fn sub(&self, other: &HashSet<T, S>) -> HashSet<T, S> {
        self.difference(other).copied().collect()
    }

    fn and_assign(&mut self, other: &HashSet<T, S>) {
        *self = self.and(other);
    }

    fn or_assign(&mut self, other: &HashSet<T, S>) {
        *self = self.or(other);
    }

    fn sub_assign(&mut self, other: &HashSet<T, S>) {
        *self = self.sub(other);
    }
}
