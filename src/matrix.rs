
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
    fn count_ones(&self) -> u32;

    /// Returns the number of ´0´-bits in the matrix.
    fn count_zeros(&self) -> u32;

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
    /// assert_eq!(matrix * BitMatrix8x8::IDENTITY, matrix);
    /// ```
    fn matmul(self, rhs: &Self) -> Self;

    /// Reverses the order of the rows.
    fn reverse_rows(&mut self);

    /// Reverses the order of the columns.
    fn reverse_columns(&mut self);

    /// Sorts the bits of each row so that all ´1´-bits in a row are moved to the most significant
    /// positions.
    fn sort_row_bits(&mut self);

    /// Sorts the bits of each column so that all ´1´-bits in a column are moved to the last rows of
    /// the matrix.
    fn sort_column_bits(&mut self);

    /// Transposes the matrix in-place.
    ///
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
    ///
    /// matrix.transpose();
    /// assert_eq!(
    ///     matrix,
    ///     [
    ///         0b_1_1_1_1_1_1_1_1,
    ///         0b_0_1_1_1_1_1_1_1,
    ///         0b_0_0_1_1_1_1_1_1,
    ///         0b_0_0_0_1_1_1_1_1,
    ///         0b_0_0_0_0_1_1_1_1,
    ///         0b_0_0_0_0_0_1_1_1,
    ///         0b_0_0_0_0_0_0_1_1,
    ///         0b_0_0_0_0_0_0_0_1
    ///     ])
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

    fn sort_row_bits(&mut self) {
        for row in self.iter_mut() {
            let ones = row.count_ones();
            let (x, overflow) = row.overflowing_shl(ones);
            *row = (x - 1) * (!overflow as u8);
        }
    }

    fn sort_column_bits(&mut self) {
        let mut mask = 0;
        let mut unsorted = 0;
        for row in &*self {
            mask |= *row;
            unsorted |= *row ^ mask;
        }
        while unsorted > 0 {
            mask = 1 << unsorted.trailing_zeros();
            if let Some(l) = self.iter().position(|row| row & mask != 0) {
                if let Some(x) = self[l..].iter().rposition(|row| row & mask == 0) {
                    let r = x + l + 1;
                    self[l..r].sort_unstable_by_key(|row| row & mask);
                }
            }
            unsorted &= !mask;
        }
    }

    fn transpose(&mut self) {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mul_8x8() {
        type BitMatrix8x8 = [u8; 8];
        let matrix = [
            0b_1_1_1_1_1_1_1_1,
            0b_0_1_1_1_1_1_1_1,
            0b_0_0_1_1_1_1_1_1,
            0b_0_0_0_1_1_1_1_1,
            0b_0_0_0_0_1_1_1_1,
            0b_0_0_0_0_0_1_1_1,
            0b_0_0_0_0_0_0_1_1,
            0b_0_0_0_0_0_0_0_1,
        ];
        let identity = BitMatrix8x8::IDENTITY;
        assert_eq!(matrix.matmul(&identity), matrix);
    }

    #[test]
    fn transpose_8x8() {
        let mut matrix = [
            0b_1_0_0_0_0_0_0_0,
            0b_1_1_0_0_0_0_0_0,
            0b_1_1_1_0_0_0_0_0,
            0b_1_1_1_1_0_0_0_0,
            0b_1_1_1_1_1_0_0_0,
            0b_1_1_1_1_1_1_0_0,
            0b_1_1_1_1_1_1_1_0,
            0b_1_1_1_1_1_1_1_1,
        ];
        matrix.transpose();
        assert_eq!(
            matrix,
            [
                0b_1_1_1_1_1_1_1_1,
                0b_0_1_1_1_1_1_1_1,
                0b_0_0_1_1_1_1_1_1,
                0b_0_0_0_1_1_1_1_1,
                0b_0_0_0_0_1_1_1_1,
                0b_0_0_0_0_0_1_1_1,
                0b_0_0_0_0_0_0_1_1,
                0b_0_0_0_0_0_0_0_1,
            ]
        );
    }
}
