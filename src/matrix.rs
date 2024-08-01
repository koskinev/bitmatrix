use crate::shuffle::BitShuffle;

/// A trait for bit matrices.
pub trait BitMatrix {
    /// The number of rows and columns in the matrix.
    const SIZE: (usize, usize);

    /// The identity matrix. This is the matrix with all bits set to 0, except for the diagonal
    /// from the top-left to the bottom-right, which is set to 1.
    const IDENTITY: Self;

    /// The zero matrix. This is the matrix with all bits set to 0.
    const ZERO: Self;

    /// The type used to represent a row in the matrix.
    type Row;

    /// Returns the number of ´1´-bits in the matrix.
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// type BitMatrix8x8 = [u8; 8];
    ///
    /// let identity = BitMatrix8x8::IDENTITY;
    /// assert_eq!(identity.count_ones(), 8);
    /// ```
    fn count_ones(&self) -> u32;

    /// Returns the number of ´0´-bits in the matrix.
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// type BitMatrix8x8 = [u8; 8];
    ///
    /// let identity = BitMatrix8x8::IDENTITY;
    /// assert_eq!(identity.count_zeros(), 8 * 8 - 8);
    /// ```
    fn count_zeros(&self) -> u32;

    /// Returns `1` if the bit at the specified row and column is set. Otherwise returns `0`.
    /// ```
    /// use bitmatrix::BitMatrix;
    /// type BitMatrix8x8 = [u8; 8];
    ///
    /// let identity = BitMatrix8x8::IDENTITY;
    ///
    /// for i in 0..8 {
    ///     for j in 0..8 {
    ///         assert_eq!(identity.get(i, j), (i == j) as u8);
    ///     }
    /// }
    /// ```
    fn get(&self, row: usize, col: usize) -> u8;

    /// Multiplies the matrix by `rhs`.
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// type BitMatrix8x8 = [u8; 8];
    /// let matrix = [
    ///     0b_1_1_1_1_1_1_1_1,
    ///     0b_0_1_1_1_1_1_1_1,
    ///     0b_0_0_1_1_1_1_1_1,
    ///     0b_0_0_0_1_1_1_1_1,
    ///     0b_0_0_0_0_1_1_1_1,
    ///     0b_0_0_0_0_0_1_1_1,
    ///     0b_0_0_0_0_0_0_1_1,
    ///     0b_0_0_0_0_0_0_0_1,
    /// ];
    /// assert_eq!(matrix.matmul(&BitMatrix::IDENTITY), matrix);
    /// ```
    fn matmul(self, rhs: &Self) -> Self;

    /// Reverses the order of the rows.
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let mut matrix = [
    ///     0b_1_1_1_1_1_1_1_1,
    ///     0b_0_1_1_1_1_1_1_1,
    ///     0b_0_0_1_1_1_1_1_1,
    ///     0b_0_0_0_1_1_1_1_1,
    ///     0b_0_0_0_0_1_1_1_1,
    ///     0b_0_0_0_0_0_1_1_1,
    ///     0b_0_0_0_0_0_0_1_1,
    ///     0b_0_0_0_0_0_0_0_1,
    /// ];
    /// matrix.reverse_rows();
    /// assert_eq!(
    ///     matrix,
    ///     [
    ///         0b_0_0_0_0_0_0_0_1,
    ///         0b_0_0_0_0_0_0_1_1,
    ///         0b_0_0_0_0_0_1_1_1,
    ///         0b_0_0_0_0_1_1_1_1,
    ///         0b_0_0_0_1_1_1_1_1,
    ///         0b_0_0_1_1_1_1_1_1,
    ///         0b_0_1_1_1_1_1_1_1,
    ///         0b_1_1_1_1_1_1_1_1,
    ///     ]
    /// );
    /// ```
    fn reverse_rows(&mut self);

    /// Reverses the order of the columns.
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let mut matrix = [
    ///     0b_1_0_0_0_0_0_0_0,
    ///     0b_1_1_0_0_0_0_0_0,
    ///     0b_1_1_1_0_0_0_0_0,
    ///     0b_1_1_1_1_0_0_0_0,
    ///     0b_1_1_1_1_1_0_0_0,
    ///     0b_1_1_1_1_1_1_0_0,
    ///     0b_1_1_1_1_1_1_1_0,
    ///     0b_1_1_1_1_1_1_1_1,
    /// ];
    /// matrix.reverse_columns();
    /// assert_eq!(
    ///     matrix,
    ///     [
    ///         0b_0_0_0_0_0_0_0_1,
    ///         0b_0_0_0_0_0_0_1_1,
    ///         0b_0_0_0_0_0_1_1_1,
    ///         0b_0_0_0_0_1_1_1_1,
    ///         0b_0_0_0_1_1_1_1_1,
    ///         0b_0_0_1_1_1_1_1_1,
    ///         0b_0_1_1_1_1_1_1_1,
    ///         0b_1_1_1_1_1_1_1_1,
    ///     ]
    /// );
    /// ```
    fn reverse_columns(&mut self);

    /// Sorts the bits of each row so that all ´1´-bits in a row are moved to the most significant
    /// positions.
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let mut matrix = [
    ///     0b_0_0_0_1_0_0_0_0,
    ///     0b_0_1_0_0_0_1_0_0,
    ///     0b_0_0_1_0_1_0_1_0,
    ///     0b_1_0_1_1_0_1_0_0,
    ///     0b_0_1_1_0_1_1_1_0,
    ///     0b_0_1_1_1_1_1_0_1,
    ///     0b_0_1_1_1_1_1_1_1,
    ///     0b_1_1_1_1_1_1_1_1,
    /// ];
    /// matrix.sort_rows();
    /// assert_eq!(
    ///     matrix,
    ///     [
    ///         0b_1_0_0_0_0_0_0_0,
    ///         0b_1_1_0_0_0_0_0_0,
    ///         0b_1_1_1_0_0_0_0_0,
    ///         0b_1_1_1_1_0_0_0_0,
    ///         0b_1_1_1_1_1_0_0_0,
    ///         0b_1_1_1_1_1_1_0_0,
    ///         0b_1_1_1_1_1_1_1_0,
    ///         0b_1_1_1_1_1_1_1_1,
    ///     ]
    /// );
    /// ```
    fn sort_rows(&mut self);

    /// Sorts the bits of each column so that all ´1´-bits in a column are moved to the last rows of
    /// the matrix.
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let mut matrix = [
    ///     0b_0_0_0_1_0_0_0_0,
    ///     0b_0_1_0_0_0_1_0_0,
    ///     0b_0_0_1_0_1_0_1_0,
    ///     0b_1_0_1_1_0_1_0_0,
    ///     0b_0_1_1_0_1_0_1_0,
    ///     0b_0_1_1_1_1_1_0_1,
    ///     0b_0_1_1_1_1_1_1_1,
    ///     0b_1_1_1_1_1_1_1_1,
    /// ];
    /// matrix.sort_columns();
    /// assert_eq!(
    ///     matrix,
    ///     [
    ///         0b_0_0_0_0_0_0_0_0,
    ///         0b_0_0_0_0_0_0_0_0,
    ///         0b_0_0_1_0_0_0_0_0,
    ///         0b_0_1_1_1_1_1_0_0,
    ///         0b_0_1_1_1_1_1_1_0,
    ///         0b_0_1_1_1_1_1_1_1,
    ///         0b_1_1_1_1_1_1_1_1,
    ///         0b_1_1_1_1_1_1_1_1,
    ///     ]
    /// );
    /// ```
    fn sort_columns(&mut self);

    /// Transposes the matrix in-place.
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let mut matrix = [
    ///     0b_1_1_1_1_1_1_1_1,
    ///     0b_1_1_1_1_1_1_1_1,
    ///     0b_0_0_0_1_1_0_0_0,
    ///     0b_0_0_0_1_1_0_0_0,
    ///     0b_0_0_0_1_1_0_0_0,
    ///     0b_0_0_0_1_1_0_0_0,
    ///     0b_0_0_0_1_1_0_0_0,
    ///     0b_0_0_0_1_1_0_0_0,
    /// ];
    ///
    /// matrix.transpose();
    ///
    /// assert_eq!(
    ///     matrix,
    ///     [
    ///         0b_0_0_0_0_0_0_1_1,
    ///         0b_0_0_0_0_0_0_1_1,
    ///         0b_0_0_0_0_0_0_1_1,
    ///         0b_1_1_1_1_1_1_1_1,
    ///         0b_1_1_1_1_1_1_1_1,
    ///         0b_0_0_0_0_0_0_1_1,
    ///         0b_0_0_0_0_0_0_1_1,
    ///         0b_0_0_0_0_0_0_1_1,
    ///     ]
    /// );
    /// ```
    fn transpose(&mut self);

    /// Returns a transposed copy of the matrix.
    fn transposed(&self) -> Self
    where
        Self: Copy,
    {
        let mut mat = *self;
        mat.transpose();
        mat
    }
}

impl BitMatrix for [u8; 8] {
    type Row = u8;

    const IDENTITY: Self = [
        0b_0_0_0_0_0_0_0_1,
        0b_0_0_0_0_0_0_1_0,
        0b_0_0_0_0_0_1_0_0,
        0b_0_0_0_0_1_0_0_0,
        0b_0_0_0_1_0_0_0_0,
        0b_0_0_1_0_0_0_0_0,
        0b_0_1_0_0_0_0_0_0,
        0b_1_0_0_0_0_0_0_0,
    ];
    const SIZE: (usize, usize) = (8, 8);
    const ZERO: Self = [0; 8];

    fn count_ones(&self) -> u32 {
        self.iter().fold(0, |acc, row| acc + row.count_ones())
    }

    fn count_zeros(&self) -> u32 {
        self.iter().fold(0, |acc, row| acc + row.count_zeros())
    }

    fn get(&self, row: usize, col: usize) -> u8 {
        (self[row] >> col) & 1
    }

    fn matmul(self, rhs: &Self) -> Self {
        const ROW: u64 = 0x00000000000000FF;
        const COL: u64 = 0x0101010101010101;

        let this = u64::from_ne_bytes(self);
        let rhs = u64::from_ne_bytes(*rhs);
        let mut result = 0;

        result |= (COL & this) * (ROW & rhs);
        result |= (COL & (this >> 1)) * (ROW & (rhs >> 8));
        result |= (COL & (this >> 2)) * (ROW & (rhs >> 16));
        result |= (COL & (this >> 3)) * (ROW & (rhs >> 24));
        result |= (COL & (this >> 4)) * (ROW & (rhs >> 32));
        result |= (COL & (this >> 5)) * (ROW & (rhs >> 40));
        result |= (COL & (this >> 6)) * (ROW & (rhs >> 48));
        result |= (COL & (this >> 7)) * (ROW & (rhs >> 56));

        result.to_ne_bytes()
    }

    fn reverse_rows(&mut self) {
        self.reverse();
    }

    fn reverse_columns(&mut self) {
        for row in self.iter_mut() {
            *row = row.reverse_bits();
        }
    }

    fn sort_rows(&mut self) {
        for row in self.iter_mut() {
            let shift = row.count_zeros();
            let (x, overflow) = u8::MAX.overflowing_shl(shift);
            *row = x * (!overflow as u8);
        }
    }

    fn sort_columns(&mut self) {
        let mut mask = 0;
        let mut unsorted = 0;
        let mut temp = *self;
        for row in &*self {
            mask |= *row;
            unsorted |= *row ^ mask;
        }
        while unsorted > 0 {
            mask = 1 << unsorted.trailing_zeros();
            if let Some(l) = temp.iter().position(|row| row & mask != 0) {
                if let Some(x) = temp[l..].iter().rposition(|row| row & mask == 0) {
                    let r = x + l + 1;
                    temp[l..r].sort_unstable_by_key(|row| row & mask);
                }
            }
            for (row, bits) in self.iter_mut().zip(temp.iter()) {
                *row &= !(mask);
                *row |= bits & mask;
            }
            unsorted &= !mask;
        }
    }

    fn transpose(&mut self) {
        let x = u64::from_ne_bytes(*self);

        // x: 01234567 89abcdef ghijklmn opqrstuv wxyzABCD EFGHIJKL MNOPQRST UVWXYZ$.
        // t: 08g0wEMU 19hpxFNV 2aiqyGOW 3bjrzHPX 4cksAIQY 5dltBJRZ 6emuCKS$ 7fnvDLT.
        //
        // m0: 00000001 00000001 00000001 00000001 00000001 00000001 00000001 00000001
        // t0: compress(x, m0)
        //     00000000 00000000 00000000 00000000 00000000 00000000 00000000 7fnvDLT.
        //
        // m1: 00000010 00000010 00000010 00000010 00000010 00000010 00000010 00000010
        // t1: compress(x, m1) << 8
        //     00000000 00000000 00000000 00000000 00000000 00000000 6emuCKS$ 00000000.
        //
        // etc.

        let t = x.compress(0x101010101010101)
            | (x.compress(0x202020202020202) << 8)
            | (x.compress(0x404040404040404) << 16)
            | (x.compress(0x808080808080808) << 24)
            | (x.compress(0x1010101010101010) << 32)
            | (x.compress(0x2020202020202020) << 40)
            | (x.compress(0x4040404040404040) << 48)
            | (x.compress(0x8080808080808080) << 56);

        *self = t.to_ne_bytes();
    }
}

#[cfg(test)]
mod tests {
    use core::array;

    use super::*;
    use crate::wyrand::WyRng;

    #[test]
    fn mul_8x8() {
        let iters = 1000;
        let mut rng: WyRng = Default::default();
        let identity = <[u8; 8]>::IDENTITY;
        for _ in 0..iters {
            let matrix: [u8; 8] = array::from_fn(|_| rng.u8());
            assert_eq!(matrix.matmul(&identity), matrix);
        }
    }

    #[test]
    fn transpose_8x8() {
        let mut matrix = [
            0b_1_1_1_1_1_1_1_1,
            0b_1_1_1_1_1_1_1_1,
            0b_0_0_0_1_1_0_0_0,
            0b_0_0_0_1_1_0_0_0,
            0b_0_0_0_1_1_0_0_0,
            0b_0_0_0_1_1_0_0_0,
            0b_0_0_0_1_1_0_0_0,
            0b_0_0_0_1_1_0_0_0,
        ];

        matrix.transpose();

        assert_eq!(
            matrix,
            [
                0b_0_0_0_0_0_0_1_1,
                0b_0_0_0_0_0_0_1_1,
                0b_0_0_0_0_0_0_1_1,
                0b_1_1_1_1_1_1_1_1,
                0b_1_1_1_1_1_1_1_1,
                0b_0_0_0_0_0_0_1_1,
                0b_0_0_0_0_0_0_1_1,
                0b_0_0_0_0_0_0_1_1,
            ]
        );

        let iters = 1000;
        let mut rng: WyRng = Default::default();
        for _ in 0..iters {
            let matrix: [u8; 8] = array::from_fn(|_| rng.u8());
            let transpose = matrix.transposed();
            let i = rng.bounded_usize(0, 8);
            let j = rng.bounded_usize(0, 8);
            assert_eq!(matrix.get(i, j), transpose.get(j, i));
        }
    }

    #[test]
    fn sort_row_bits_u8() {
        let mut matrix = [
            0b_0_0_0_0_0_0_0_1,
            0b_0_1_0_0_0_1_0_0,
            0b_0_0_1_0_1_0_1_0,
            0b_1_0_1_1_0_1_0_0,
            0b_0_1_1_0_1_1_1_0,
            0b_0_1_1_1_1_1_0_1,
            0b_0_1_1_1_1_1_1_1,
            0b_1_1_1_1_1_1_1_1,
        ];
        matrix.sort_rows();
        assert_eq!(
            matrix,
            [
                0b_1_0_0_0_0_0_0_0,
                0b_1_1_0_0_0_0_0_0,
                0b_1_1_1_0_0_0_0_0,
                0b_1_1_1_1_0_0_0_0,
                0b_1_1_1_1_1_0_0_0,
                0b_1_1_1_1_1_1_0_0,
                0b_1_1_1_1_1_1_1_0,
                0b_1_1_1_1_1_1_1_1,
            ]
        );
    }

    #[test]
    fn sort_column_bits_u8() {
        let mut matrix = [
            0b_0_0_0_1_0_0_0_0,
            0b_0_1_0_0_0_1_0_0,
            0b_0_0_1_0_1_0_1_0,
            0b_1_0_1_1_0_1_0_0,
            0b_0_1_1_0_1_0_1_0,
            0b_0_1_1_1_1_1_0_1,
            0b_0_1_1_1_1_1_1_1,
            0b_1_1_1_1_1_1_1_1,
        ];
        matrix.sort_columns();
        assert_eq!(
            matrix,
            [
                0b_0_0_0_0_0_0_0_0,
                0b_0_0_0_0_0_0_0_0,
                0b_0_0_1_0_0_0_0_0,
                0b_0_1_1_1_1_1_0_0,
                0b_0_1_1_1_1_1_1_0,
                0b_0_1_1_1_1_1_1_1,
                0b_1_1_1_1_1_1_1_1,
                0b_1_1_1_1_1_1_1_1,
            ]
        );
    }
}
