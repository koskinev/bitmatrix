use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Shr};

/// Represents an unsigned integer with `N * 64` bits.
pub(crate) struct Unsigned<const N: usize>([u64; N]);

impl<const N: usize> Unsigned<N> {
    /// Creates a new `Unsigned` from a slice of bytes. The length of the slice must be `N * 8`.
    pub fn from_bytes(bytes: &[u8]) -> Self {
        assert!(bytes.len() == N * 8, "bytes must have a length of N * 8.");
        let mut arr = [0; N];
        for (index, byte) in bytes.iter().enumerate() {
            arr[index / 8] |= (*byte as u64) << (index % 8 * 8);
        }
        Unsigned(arr)
    }
}

impl<const N: usize> BitAnd for Unsigned<N> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        let mut result = rhs.0;
        for (index, val) in self.0.iter().enumerate() {
            result[index] &= val;
        }
        Unsigned(result)
    }
}

impl<const N: usize> BitOr for Unsigned<N> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        let mut result = rhs.0;
        for (index, val) in self.0.iter().enumerate() {
            result[index] |= val;
        }
        Unsigned(result)
    }
}

impl<const N: usize> BitXor for Unsigned<N> {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        let mut result = rhs.0;
        for (index, val) in self.0.iter().enumerate() {
            result[index] ^= val;
        }
        Unsigned(result)
    }
}

impl<const N: usize> BitAndAssign for Unsigned<N> {
    fn bitand_assign(&mut self, rhs: Self) {
        for (index, val) in rhs.0.iter().enumerate() {
            self.0[index] &= val;
        }
    }
}

impl<const N: usize> BitOrAssign for Unsigned<N> {
    fn bitor_assign(&mut self, rhs: Self) {
        for (index, val) in rhs.0.iter().enumerate() {
            self.0[index] |= val;
        }
    }
}

impl<const N: usize> BitXorAssign for Unsigned<N> {
    fn bitxor_assign(&mut self, rhs: Self) {
        for (index, val) in rhs.0.iter().enumerate() {
            self.0[index] ^= val;
        }
    }
}

impl<const N: usize> Shr<usize> for Unsigned<N> {
    type Output = Self;

    fn shr(self, rhs: usize) -> Self::Output {
        let mut arr = [0; N];
        for (index, val) in self.0.iter().enumerate() {
            arr[index] |= val >> rhs;
            if index < N - 1 {
                arr[index + 1] = val << (64 - rhs);
            }
        }
        Unsigned(arr)
    }
}
