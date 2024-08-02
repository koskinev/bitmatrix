use crate::shuffle::BitOps;

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
    type RowRepr;

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
    ///     0b_1_1_1_1_1_1_1_1,
    ///     0b_1_1_1_1_1_1_1_1,
    ///     0b_0_0_0_1_1_0_0_0,
    ///     0b_0_0_0_1_1_0_0_0,
    ///     0b_0_0_0_1_1_0_0_0,
    ///     0b_0_0_0_1_1_0_0_0,
    ///     0b_0_0_0_1_1_0_0_0,
    ///     0b_0_0_0_1_1_0_0_0,
    /// ];
    /// matrix.sort_rows();
    /// assert_eq!(
    ///     matrix,
    ///     [
    ///         0b__1_1_1_1_1_1_1_1,
    ///         0b__1_1_1_1_1_1_1_1,
    ///         0b__1_1_0_0_0_0_0_0,
    ///         0b__1_1_0_0_0_0_0_0,
    ///         0b__1_1_0_0_0_0_0_0,
    ///         0b__1_1_0_0_0_0_0_0,
    ///         0b__1_1_0_0_0_0_0_0,
    ///         0b__1_1_0_0_0_0_0_0,
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
    ///     0b_1_1_1_1_1_1_1_1,
    ///     0b_1_1_1_1_1_1_1_1,
    ///     0b_0_0_0_1_1_0_0_0,
    ///     0b_0_0_0_1_1_0_0_0,
    ///     0b_0_0_0_1_1_0_0_0,
    ///     0b_0_0_0_1_1_0_0_0,
    ///     0b_0_0_0_1_1_0_0_0,
    ///     0b_0_0_0_1_1_0_0_0,
    /// ];
    /// matrix.sort_columns();
    /// assert_eq!(
    ///     matrix,
    ///     [
    ///         0b_0_0_0_1_1_0_0_0,
    ///         0b_0_0_0_1_1_0_0_0,
    ///         0b_0_0_0_1_1_0_0_0,
    ///         0b_0_0_0_1_1_0_0_0,
    ///         0b_0_0_0_1_1_0_0_0,
    ///         0b_0_0_0_1_1_0_0_0,
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
    ///     0b_0101_0000,
    ///     0b_0101_0000,
    ///     0b_0101_0000,
    ///     0b_0101_0000,
    ///     0b_1111_1010,
    ///     0b_1111_1010,
    ///     0b_1111_1010,
    ///     0b_1111_1010,
    /// ];
    ///
    /// matrix.transpose();
    ///
    /// assert_eq!(
    ///     matrix,
    ///     [
    ///         0b_0000_0000,
    ///         0b_1111_0000,
    ///         0b_0000_0000,
    ///         0b_1111_0000,
    ///         0b_1111_1111,
    ///         0b_1111_0000,
    ///         0b_1111_1111,
    ///         0b_1111_0000,
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
    type RowRepr = u8;

    const IDENTITY: Self = [0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80];
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
            let (x, overflow) = Self::RowRepr::MAX.overflowing_shl(shift);
            *row = x * (!overflow as Self::RowRepr);
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

        // A         M0       T0       M1       T1             T
        //
        // .$ZYXWVU  00000001 00000000 00000010 00000000       .TLDvnf7
        // TSRQPONM  00000001 00000000 00000010 00000000       $SKCume6
        // LKJIHGFE  00000001 00000000 00000010 00000000       ZRJBtld5
        // DCBAzyxw  00000001 00000000 00000010 00000000  ...  YQIAskc4
        // vutsrqpo  00000001 00000000 00000010 00000000       XPHzrjb3
        // nmlkjihg  00000001 00000000 00000010 00000000       WOGyqia2
        // fedcba98  00000001 00000000 00000010 VNFxph91       VNFxph91
        // 76543210  00000001 UMEwog80 00000010 UMEwog80       UMEwog80
        //
        // T0 = A.compress(M0), T1 = T0 | (A.compress(M1) << 8) ...

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

impl BitMatrix for [u16; 16] {
    type RowRepr = u16;

    const IDENTITY: Self = [
        0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80, 0x100, 0x200, 0x400, 0x800, 0x1000, 0x2000,
        0x4000, 0x8000,
    ];
    const SIZE: (usize, usize) = (16, 16);
    const ZERO: Self = [0; 16];

    fn count_ones(&self) -> u32 {
        self.iter().fold(0, |acc, row| acc + row.count_ones())
    }

    fn count_zeros(&self) -> u32 {
        self.iter().fold(0, |acc, row| acc + row.count_zeros())
    }

    fn get(&self, row: usize, col: usize) -> u8 {
        ((self[row] >> col) & 1) as u8
    }

    fn matmul(mut self, rhs: &Self) -> Self {
        use crate::shuffle::ByteShuffle;

        let this: [u8; 32] = unsafe { core::mem::transmute(self) };
        let rhs: [u8; 32] = unsafe { core::mem::transmute(*rhs) };
        let m_self: [u8; 32] = [
            0, 8, 1, 9, 2, 10, 3, 11, 4, 12, 5, 13, 6, 14, 7, 15, 16, 24, 17, 25, 18, 26, 19, 27,
            20, 28, 21, 29, 22, 30, 23, 31,
        ];
        let m_rhs: [u8; 32] = [
            0, 8, 1, 9, 2, 10, 3, 11, 16, 24, 17, 25, 18, 26, 19, 27, 4, 12, 5, 13, 6, 14, 7, 15,
            20, 28, 21, 29, 22, 30, 23, 31,
        ];

        // let mut blocks: [[u8; 8]; 4] = unsafe { core::mem::transmute(this.shuffle_bytes(m_self))
        // };

        todo!()
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
            let (x, overflow) = Self::RowRepr::MAX.overflowing_shl(shift);
            *row = x * (!overflow as Self::RowRepr);
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
        // use crate::shuffle::ByteShuffle;

        // let this: &mut [u8; 32] = unsafe { core::mem::transmute(self) };
        // let mask: [u8; 32] = [
        //     0, 8, 1, 9, 2, 10, 3, 11, 4, 12, 5, 13, 6, 14, 7, 15, 16, 24, 17, 25, 18, 26, 19, 27,
        //     20, 28, 21, 29, 22, 30, 23, 31,
        // ];

        // this.shuffle_bytes(mask);
        // let blocks: &mut [[u8; 8]; 4] = unsafe { core::mem::transmute(this) };

        // for block in blocks.iter_mut() {
        //     block.transpose();
        // }

        let [a00, a01, a02, a03, a04, a05, a06, a07, a08, a09, a10, a11, a12, a13, a14, a15] =
        self;

        let mut mask = u16::MAX;

        macro_rules! rot_exchange {
            ($a:expr, $b:expr, $rot:expr) => {
                *$b = $b.rotate_left($rot);
                (*$a, *$b) = $a.exchange(*$b, mask);
            };
        }

        mask ^= mask >> 8;
        rot_exchange!(a00, a08, 8);
        rot_exchange!(a01, a09, 8);
        rot_exchange!(a02, a10, 8);
        rot_exchange!(a03, a11, 8);
        rot_exchange!(a04, a12, 8);
        rot_exchange!(a05, a13, 8);
        rot_exchange!(a06, a14, 8);
        rot_exchange!(a07, a15, 8);

        mask ^= mask >> 4;
        rot_exchange!(a00, a04, 4);
        rot_exchange!(a01, a05, 4);
        rot_exchange!(a02, a06, 4);
        rot_exchange!(a03, a07, 4);
        rot_exchange!(a08, a12, 4);
        rot_exchange!(a09, a13, 4);
        rot_exchange!(a10, a14, 4);
        rot_exchange!(a11, a15, 4);

        mask ^= mask >> 2;
        rot_exchange!(a00, a02, 2);
        rot_exchange!(a01, a03, 2);
        rot_exchange!(a04, a06, 2);
        rot_exchange!(a05, a07, 2);
        rot_exchange!(a08, a10, 2);
        rot_exchange!(a09, a11, 2);
        rot_exchange!(a12, a14, 2);
        rot_exchange!(a13, a15, 2);

        mask ^= mask >> 1;
        rot_exchange!(a00, a01, 1);
        rot_exchange!(a02, a03, 1);
        rot_exchange!(a04, a05, 1);
        rot_exchange!(a06, a07, 1);
        rot_exchange!(a08, a09, 1);
        rot_exchange!(a10, a11, 1);
        rot_exchange!(a12, a13, 1);
        rot_exchange!(a14, a15, 1);

        *a01 = a01.rotate_right(1);
        *a02 = a02.rotate_right(2);
        *a03 = a03.rotate_right(3);
        *a04 = a04.rotate_right(4);
        *a05 = a05.rotate_right(5);
        *a06 = a06.rotate_right(6);
        *a07 = a07.rotate_right(7);
        *a08 = a08.rotate_right(8);
        *a09 = a09.rotate_right(9);
        *a10 = a10.rotate_right(10);
        *a11 = a11.rotate_right(11);
        *a12 = a12.rotate_right(12);
        *a13 = a13.rotate_right(13);
        *a14 = a14.rotate_right(14);
        *a15 = a15.rotate_right(15);
    }
}

#[rustfmt::skip]
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
             0b_0101_0000,
             0b_0101_0000,
             0b_0101_0000,
             0b_0101_0000,
             0b_1111_1010,
             0b_1111_1010,
             0b_1111_1010,
             0b_1111_1010,
        ];

        matrix.transpose();

        assert_eq!(
            matrix,
            [
                0b_0000_0000,
                0b_1111_0000,
                0b_0000_0000,
                0b_1111_0000,
                0b_1111_1111,
                0b_1111_0000,
                0b_1111_1111,
                0b_1111_0000,
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
    fn transpose_16x16() {

        let mut matrix = [
            0b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
            0b_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0,
            0b_0_1_1_0_0_0_0_0_0_0_0_1_1_0_0_0,
            0b_0_1_1_0_0_0_0_0_0_0_0_0_1_1_0_0,
            0b_0_1_1_0_0_0_0_0_0_0_0_0_1_1_0_0,
            0b_0_1_1_0_0_0_0_0_0_0_0_0_1_1_0_0,
            0b_0_1_1_0_0_0_0_0_0_0_0_0_1_1_0_0,
            0b_0_1_1_0_0_0_0_0_0_0_0_1_1_0_0_0,
            0b_0_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0,
            0b_0_1_1_0_0_0_0_0_1_1_0_0_0_0_0_0,
            0b_0_1_1_0_0_0_0_0_0_1_1_0_0_0_0_0,
            0b_0_1_1_0_0_0_0_0_0_0_1_1_0_0_0_0,
            0b_0_1_1_0_0_0_0_0_0_0_0_1_1_0_0_0,
            0b_0_1_1_0_0_0_0_0_0_0_0_0_1_1_0_0,
            0b_0_1_1_0_0_0_0_0_0_0_0_0_0_1_1_0,
            0b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
        ];

        matrix.transpose();

        assert_eq!(
            matrix,
            [
                0b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
                0b_0_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0,
                0b_0_1_1_0_0_0_0_0_0_1_1_1_1_0_0_0,
                0b_0_0_1_1_0_0_0_0_1_1_1_1_1_1_0_0,
                0b_0_0_0_1_1_0_0_1_1_0_0_0_0_1_1_0,
                0b_0_0_0_0_1_1_0_1_0_0_0_0_0_0_1_0,
                0b_0_0_0_0_0_1_1_1_0_0_0_0_0_0_1_0,
                0b_0_0_0_0_0_0_1_1_0_0_0_0_0_0_1_0,
                0b_0_0_0_0_0_0_0_1_0_0_0_0_0_0_1_0,
                0b_0_0_0_0_0_0_0_1_0_0_0_0_0_0_1_0,
                0b_0_0_0_0_0_0_0_1_0_0_0_0_0_0_1_0,
                0b_0_0_0_0_0_0_0_1_0_0_0_0_0_0_1_0,
                0b_0_0_0_0_0_0_0_1_0_0_0_0_0_0_1_0,
                0b_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0,
                0b_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1_0,
                0b_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0
            ]
        );

        let iters = 1000;
        let mut rng: WyRng = Default::default();
        for _ in 0..iters {
            let matrix: [u16; 16] = array::from_fn(|_| rng.u16());
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
