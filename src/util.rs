/// 与えられた数のLSBからのビットが全て1の数を返す
pub fn bit_mask(bits: u64) -> u64 {
    (0..bits).fold(0, |acc, x| acc | (1 << x))
}
