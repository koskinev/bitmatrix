use core::fmt::{Debug, Display};
use core::ops::Mul;

#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub struct BitMatrix8x8([u8; 8]);

impl BitMatrix8x8 {
    /// The 8x8 identity matrix. This is the matrix with all bits set to 0, except for the diagonal
    /// from the top-left to the bottom-right, which is set to 1.
    pub const IDENTITY: Self = Self([
        0b_0_0_0_0_0_0_0_1,
        0b_0_0_0_0_0_0_1_0,
        0b_0_0_0_0_0_1_0_0,
        0b_0_0_0_0_1_0_0_0,
        0b_0_0_0_1_0_0_0_0,
        0b_0_0_1_0_0_0_0_0,
        0b_0_1_0_0_0_0_0_0,
        0b_1_0_0_0_0_0_0_0,
    ]);
    /// The 8x8 zero matrix. This is the matrix with all bits set to 0.
    pub const ZERO: Self = Self([0; 8]);

    /// Creates a new bit matrix with the given rows.
    ///
    /// ```
    /// # use bitmatrix::BitMatrix8x8;
    /// let matrix = BitMatrix8x8::new([
    ///     0b1000_0000,
    ///     0b0100_0000,
    ///     0b0010_0000,
    ///     0b0001_0000,
    ///     0b0000_1000,
    ///     0b0000_0100,
    ///     0b0000_0010,
    ///     0b0000_0001,
    /// ]);
    ///
    /// assert_eq!(matrix, BitMatrix8x8::IDENTITY);
    /// ```
    pub fn new(rows: [u8; 8]) -> Self {
        Self(rows)
    }

    /// Returns the rows of the matrix as a slice of bytes.
    pub fn rows(&self) -> &[u8] {
        &self.0
    }

    /// Returns the rows of the matrix as a mutable slice of bytes.
    pub fn rows_mut(&mut self) -> &mut [u8] {
        &mut self.0
    }

    /// Transposes the matrix in-place.
    ///
    /// ```
    /// # use bitmatrix::BitMatrix8x8;
    /// let mut row = 0_u8;
    /// let mut rows = core::array::from_fn(move |_| {
    ///     row = (row << 1) + 1;
    ///     row.reverse_bits()
    /// });
    /// let mut matrix = BitMatrix8x8::new(rows);
    /// matrix.transpose();
    /// assert_eq!(
    ///     matrix.rows(),
    ///     &[
    ///         0b1111_1111,
    ///         0b0111_1111,
    ///         0b0011_1111,
    ///         0b0001_1111,
    ///         0b0000_1111,
    ///         0b0000_0111,
    ///         0b0000_0011,
    ///         0b0000_0001,
    ///     ]
    /// );
    /// ```
    pub fn transpose(&mut self) {
        unsafe fn swap(a: *mut u8, b: *mut u8, j: u32, mask: u8) {
            let t = (*a ^ (*b >> j)) & mask;
            *a ^= t;
            *b ^= t << j;
        }

        let ptr = self as *mut _ as *mut u8;
        let mut mask = u8::MAX;

        unsafe {
            let (a0, a1, a2, a3, a4, a5, a6, a7) = (
                ptr.add(0),
                ptr.add(1),
                ptr.add(2),
                ptr.add(3),
                ptr.add(4),
                ptr.add(5),
                ptr.add(6),
                ptr.add(7),
            );

            mask ^= mask << 4;
            swap(a0, a4, 4, mask);
            swap(a1, a5, 4, mask);
            swap(a2, a6, 4, mask);
            swap(a3, a7, 4, mask);

            mask ^= mask << 2;
            swap(a0, a2, 2, mask);
            swap(a1, a3, 2, mask);
            swap(a4, a6, 2, mask);
            swap(a5, a7, 2, mask);

            mask ^= mask << 1;
            swap(a0, a1, 1, mask);
            swap(a2, a3, 1, mask);
            swap(a4, a5, 1, mask);
            swap(a6, a7, 1, mask);
        }
    }
}

impl Debug for BitMatrix8x8 {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "BitMatrix8x8([ ")?;
        for row in self.rows() {
            write!(f, "0b{row:08b}, ")?;
        }
        write!(f, "])")?;
        Ok(())
    }
}

impl Display for BitMatrix8x8 {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut first = true;
        write!(f, "[ ")?;
        for row in self.rows() {
            if !first {
                write!(f, "\n  ")?;
            } else {
                first = false;
            }
            for pos in 0..8 {
                write!(f, "{} ", (row >> pos) & 1)?;
            }
        }
        write!(f, "]")?;
        Ok(())
    }
}

impl Mul for BitMatrix8x8 {
    type Output = Self;

    /// Multiplies the matrix by `rhs`.
    ///
    /// ```
    /// # use bitmatrix::BitMatrix8x8;
    /// let matrix = BitMatrix8x8::new([
    ///     0b_1_1_1_1_1_1_1_1,
    ///     0b_0_1_1_1_1_1_1_1,
    ///     0b_0_0_1_1_1_1_1_1,
    ///     0b_0_0_0_1_1_1_1_1,
    ///     0b_0_0_0_0_1_1_1_1,
    ///     0b_0_0_0_0_0_1_1_1,
    ///     0b_0_0_0_0_0_0_1_1,
    ///     0b_0_0_0_0_0_0_0_1,
    /// ]);
    /// assert_eq!(matrix * BitMatrix8x8::IDENTITY, matrix);
    /// ```
    fn mul(self, rhs: Self) -> Self {
        const ROW: u64 = 0x00000000000000FF;
        const COL: u64 = 0x0101010101010101;

        let this = u64::from_ne_bytes(self.0);
        let rhs = u64::from_ne_bytes(rhs.0);
        let mut result = 0;

        result |= (COL & this) * (ROW & rhs);
        result |= (COL & (this >> 1)) * (ROW & (rhs >> 8));
        result |= (COL & (this >> 2)) * (ROW & (rhs >> 16));
        result |= (COL & (this >> 3)) * (ROW & (rhs >> 24));
        result |= (COL & (this >> 4)) * (ROW & (rhs >> 32));
        result |= (COL & (this >> 5)) * (ROW & (rhs >> 40));
        result |= (COL & (this >> 6)) * (ROW & (rhs >> 48));
        result |= (COL & (this >> 7)) * (ROW & (rhs >> 56));

        Self(result.to_ne_bytes())
    }
}

impl Mul<&BitMatrix8x8> for BitMatrix8x8 {
    type Output = <BitMatrix8x8 as Mul>::Output;

    /// Multiplies the matrix by `rhs`. See [BitMatrix8x8::mul] for an example.
    fn mul(self, rhs: &Self) -> BitMatrix8x8 {
        self * *rhs
    }
}

impl Mul<&BitMatrix8x8> for &BitMatrix8x8 {
    type Output = <BitMatrix8x8 as Mul>::Output;

    /// Multiplies the matrix by `rhs`. See [BitMatrix8x8::mul] for an example.
    fn mul(self, rhs: &BitMatrix8x8) -> BitMatrix8x8 {
        *self * *rhs
    }
}

impl Mul<BitMatrix8x8> for &BitMatrix8x8 {
    type Output = <BitMatrix8x8 as Mul>::Output;

    /// Multiplies the matrix by `rhs`. See [BitMatrix8x8::mul] for an example.
    fn mul(self, rhs: BitMatrix8x8) -> BitMatrix8x8 {
        *self * rhs
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mul_8x8() {
        let matrix = BitMatrix8x8::new([
            0b_1_1_1_1_1_1_1_1,
            0b_0_1_1_1_1_1_1_1,
            0b_0_0_1_1_1_1_1_1,
            0b_0_0_0_1_1_1_1_1,
            0b_0_0_0_0_1_1_1_1,
            0b_0_0_0_0_0_1_1_1,
            0b_0_0_0_0_0_0_1_1,
            0b_0_0_0_0_0_0_0_1,
        ]);
        assert_eq!(matrix * BitMatrix8x8::IDENTITY, matrix);
    }

    #[test]
    fn transpose_8x8() {
        let mut matrix = BitMatrix8x8::new([
            0b_1_0_0_0_0_0_0_0,
            0b_1_1_0_0_0_0_0_0,
            0b_1_1_1_0_0_0_0_0,
            0b_1_1_1_1_0_0_0_0,
            0b_1_1_1_1_1_0_0_0,
            0b_1_1_1_1_1_1_0_0,
            0b_1_1_1_1_1_1_1_0,
            0b_1_1_1_1_1_1_1_1,
        ]);
        matrix.transpose();
        assert_eq!(
            matrix,
            BitMatrix8x8([
                0b_1_1_1_1_1_1_1_1,
                0b_0_1_1_1_1_1_1_1,
                0b_0_0_1_1_1_1_1_1,
                0b_0_0_0_1_1_1_1_1,
                0b_0_0_0_0_1_1_1_1,
                0b_0_0_0_0_0_1_1_1,
                0b_0_0_0_0_0_0_1_1,
                0b_0_0_0_0_0_0_0_1,
            ])
        );
    }
}
