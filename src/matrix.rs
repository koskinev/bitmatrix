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
    fn matmul(&self, rhs: &Self) -> Self;

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
    ///     0b_1_1_1_1_1_1_0_0,
    ///     0b_1_0_0_0_0_0_1_0,
    ///     0b_1_0_0_0_0_0_1_0,
    ///     0b_1_0_0_0_0_0_1_0,
    ///     0b_1_1_1_1_1_1_0_0,
    ///     0b_1_0_0_0_0_1_0_0,
    ///     0b_1_0_0_0_0_0_1_0,
    ///     0b_1_0_0_0_0_0_0_1,
    /// ];
    ///
    /// matrix.transpose();
    ///
    /// assert_eq!(
    ///     matrix,
    ///     [
    ///         0b_1_0_0_0_0_0_0_0,
    ///         0b_0_1_0_0_1_1_1_0,
    ///         0b_0_0_1_1_0_0_0_1,
    ///         0b_0_0_0_1_0_0_0_1,
    ///         0b_0_0_0_1_0_0_0_1,
    ///         0b_0_0_0_1_0_0_0_1,
    ///         0b_0_0_0_1_0_0_0_1,
    ///         0b_1_1_1_1_1_1_1_1,
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

    fn matmul(&self, rhs: &Self) -> Self {
        const ROW: u64 = 0x00000000000000FF;
        const COL: u64 = 0x0101010101010101;

        let this = u64::from_ne_bytes(*self);
        let rhs = u64::from_ne_bytes(*rhs);
        let mut result = 0;

        result ^= (COL & this) * (ROW & rhs);
        result ^= (COL & (this >> 1)) * (ROW & (rhs >> 8));
        result ^= (COL & (this >> 2)) * (ROW & (rhs >> 16));
        result ^= (COL & (this >> 3)) * (ROW & (rhs >> 24));
        result ^= (COL & (this >> 4)) * (ROW & (rhs >> 32));
        result ^= (COL & (this >> 5)) * (ROW & (rhs >> 40));
        result ^= (COL & (this >> 6)) * (ROW & (rhs >> 48));
        result ^= (COL & (this >> 7)) * (ROW & (rhs >> 56));

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
        // The following code was generated using the bit permutation calculator source code
        // available at https://programming.sirrida.de/sources.zip
        fn permute_step(x: u64, m: u64, shift: u32) -> u64 {
            let t = ((x >> shift) ^ x) & m;
            (x ^ t) ^ (t << shift)
        }

        let mut x = u64::from_ne_bytes(*self);
        x = permute_step(x, 0x00aa00aa00aa00aa, 7);
        x = permute_step(x, 0x0000cccc0000cccc, 14);
        x = permute_step(x, 0x00000000f0f0f0f0, 28);
        *self = x.to_ne_bytes();

        // An alternative implementation from Hacker's Delight 2nd ed. by Henry S. Warren, Jr.
        // (2013), section 7-3 "Transposing a Bit Matrix".

        // // A         M0       T0       M1       T1             T
        // //
        // // .$ZYXWVU  00000001 00000000 00000010 00000000       .TLDvnf7
        // // TSRQPONM  00000001 00000000 00000010 00000000       $SKCume6
        // // LKJIHGFE  00000001 00000000 00000010 00000000       ZRJBtld5
        // // DCBAzyxw  00000001 00000000 00000010 00000000  ...  YQIAskc4
        // // vutsrqpo  00000001 00000000 00000010 00000000       XPHzrjb3
        // // nmlkjihg  00000001 00000000 00000010 00000000       WOGyqia2
        // // fedcba98  00000001 00000000 00000010 VNFxph91       VNFxph91
        // // 76543210  00000001 UMEwog80 00000010 UMEwog80       UMEwog80
        // //
        // // T0 = A.compress(M0), T1 = T0 | (A.compress(M1) << 8) ...

        // let x = u64::from_ne_bytes(*self);
        // let t = x.compress(0x101010101010101)
        //     | (x.compress(0x202020202020202) << 8)
        //     | (x.compress(0x404040404040404) << 16)
        //     | (x.compress(0x808080808080808) << 24)
        //     | (x.compress(0x1010101010101010) << 32)
        //     | (x.compress(0x2020202020202020) << 40)
        //     | (x.compress(0x4040404040404040) << 48)
        //     | (x.compress(0x8080808080808080) << 56);

        // *self = t.to_ne_bytes();
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

    fn matmul(&self, rhs: &Self) -> Self {
        const BLOCK: [u8; 32] = [
            0, 2, 4, 6, 8, 10, 12, 14, 1, 3, 5, 7, 9, 11, 13, 15, // 1st half
            0, 2, 4, 6, 8, 10, 12, 14, 1, 3, 5, 7, 9, 11, 13, 15, // 2nd half
        ];

        const UNBLOCK: [u8; 32] = [
            0, 8, 1, 9, 2, 10, 3, 11, 4, 12, 5, 13, 6, 14, 7, 15, // ...
            0, 8, 1, 9, 2, 10, 3, 11, 4, 12, 5, 13, 6, 14, 7, 15,
        ];

        let mut lhs = *self;
        let mut rhs = *rhs;
        let mut result = Self::ZERO;

        shuffle_bytes(unsafe { &mut *lhs.as_mut_ptr().cast() }, &BLOCK);
        shuffle_bytes(unsafe { &mut *rhs.as_mut_ptr().cast() }, &BLOCK);

        let lhs_blocks: &mut [[[u8; 8]; 2]; 2] = unsafe { &mut *lhs.as_mut_ptr().cast() };
        let rhs_blocks: &mut [[[u8; 8]; 2]; 2] = unsafe { &mut *rhs.as_mut_ptr().cast() };

        let b00x00 = u64::from_ne_bytes(lhs_blocks[0][0].matmul(&rhs_blocks[0][0]));
        let b00x01 = u64::from_ne_bytes(lhs_blocks[0][0].matmul(&rhs_blocks[0][1]));
        let b01x10 = u64::from_ne_bytes(lhs_blocks[0][1].matmul(&rhs_blocks[1][0]));
        let b01x11 = u64::from_ne_bytes(lhs_blocks[0][1].matmul(&rhs_blocks[1][1]));
        let b10x00 = u64::from_ne_bytes(lhs_blocks[1][0].matmul(&rhs_blocks[0][0]));
        let b10x01 = u64::from_ne_bytes(lhs_blocks[1][0].matmul(&rhs_blocks[0][1]));
        let b11x10 = u64::from_ne_bytes(lhs_blocks[1][1].matmul(&rhs_blocks[1][0]));
        let b11x11 = u64::from_ne_bytes(lhs_blocks[1][1].matmul(&rhs_blocks[1][1]));

        let res_blocks: &mut [[[u8; 8]; 2]; 2] = unsafe { &mut *result.as_mut_ptr().cast() };
        res_blocks[0][0] = (b00x00 ^ b01x10).to_ne_bytes();
        res_blocks[0][1] = (b00x01 ^ b01x11).to_ne_bytes();
        res_blocks[1][0] = (b10x00 ^ b11x10).to_ne_bytes();
        res_blocks[1][1] = (b10x01 ^ b11x11).to_ne_bytes();

        shuffle_bytes(unsafe { &mut *result.as_mut_ptr().cast() }, &UNBLOCK);
        result
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
        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
        {
            if is_x86_feature_detected!("sse2") && is_x86_feature_detected!("avx2") {
                // We treat the 16x16 matrix as four 8x8 blocks and transpose each block in-place.
                //
                //  A B => A'B' => A'B'
                //  C D    C'D'    C'D'
                //
                // The steps are:
                // - convert the matrix to a row blocked format
                // - transpose each block in-place with a 8x8 transpose
                // - swap the 2nd and 3rd blocks
                // - convert the matrix back to a column blocked format
                //
                // The conversion to and from the row blocked format can be visualized by viewing
                // the matrix as an 16x2 array of bytes, where each byte is a row in an 8x8 matrix.
                // We shuffle the rows as follows:
                // ```text
                // 
                //  Index       Initial        Row blocked:    Mapping:  Reverse:
                //
                //  [ 0,  1,    [ a0, b0,  =>  [ a0, a1,       [  0, 2,   [  0,  8,
                //    2,  3,      a1, b1,        a2, a3,          4, 6,      1,  9,
                //    4,  5,      a2, b2,        a4, a5,          8, 10,     2, 10,
                //    6,  7,      a3, b3,        a6, a7,         12, 14,     3, 11,
                //    8,  9,      a4, b4,        b0, b1,          1,  3,     4, 12,
                //   10, 11,      a5, b5,        b2, b3,          5,  7,     5, 13,
                //   12, 13,      a6  b6,        b4, b5,          9, 11,     6, 14,
                //   14, 15,      a7, b7,        b6, b7,         13, 15,     7, 15,
                //   16, 17,      c0, d0,        c0, c1,         16, 18,    16, 24,
                //   18, 19,      c1, d1,        c2, c3,         20, 22,    17, 25,
                //   20, 21,      c2, d2,        c4, c5,         24, 26,    18, 26,
                //   22, 23,      c3, d3,        c6, c7,         28, 30,    19, 27,
                //   24, 25,      c4, d4,        d0, d1,         17, 19,    20, 28,
                //   26, 27,      c5, d5,        d2, d3,         21, 23,    21, 29,
                //   28, 29,      c6, d6,        d4, d5,         25, 27,    22, 30,
                //   30, 31, ]    c7, d7, ]      d6, d7, ]       29, 31,    23, 31, ]
                // ```
                //
                // The reverse operation can be reversed by repeating the shuffle with the same
                // mapping.

                // shuffle_bytes requires a 32 byte mapping vector, where each byte is an
                // index into two 128-bit lanes. The mapping is applied to each 128-bit lane
                // separately.
                const BLOCK: [u8; 32] = [
                    0, 2, 4, 6, 8, 10, 12, 14, 1, 3, 5, 7, 9, 11, 13, 15, // 1st half
                    0, 2, 4, 6, 8, 10, 12, 14, 1, 3, 5, 7, 9, 11, 13, 15, // 2nd half
                ];

                const UNBLOCK: [u8; 32] = [
                    0, 8, 1, 9, 2, 10, 3, 11, 4, 12, 5, 13, 6, 14, 7, 15, // ...
                    0, 8, 1, 9, 2, 10, 3, 11, 4, 12, 5, 13, 6, 14, 7, 15,
                ];

                let this: &mut [u8; 32] = unsafe { &mut *self.as_mut_ptr().cast() };
                shuffle_bytes(this, &BLOCK);

                // Each 8x8 block is transposed in-place.
                let blocks: &mut [[u8; 8]; 4] = unsafe { &mut *self.as_mut_ptr().cast() };
                for block in blocks.iter_mut() {
                    block.transpose();
                }

                // The 2nd and 3rd blocks are swapped.
                blocks.swap(1, 2);

                // Finally, the matrix is converted back to it's original format.
                shuffle_bytes(this, &UNBLOCK);

                return;
            }
        }

        // The fallback implementation is adapted from the algorithm described in Hacker's Delight
        // 2nd ed. by Henry S. Warren, Jr. (2013), section 7-3 "Transposing a Bit Matrix".
        let [a00, a01, a02, a03, a04, a05, a06, a07, a08, a09, a10, a11, a12, a13, a14, a15] = self;

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

/// A helper method for shuffling bytes in an ´[u8; 32]´ array. The mapping is applied separately to
/// the first and second half of the array.
fn shuffle_bytes(data: &mut [u8; 32], mapping: &[u8; 32]) {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    {
        if is_x86_feature_detected!("sse2") && is_x86_feature_detected!("avx2") {
            #[cfg(target_arch = "x86")]
            use core::arch::x86::{_mm256_loadu_si256, _mm256_shuffle_epi8, _mm256_storeu_si256};

            #[cfg(target_arch = "x86_64")]
            use core::arch::x86_64::{
                _mm256_loadu_si256, _mm256_shuffle_epi8, _mm256_storeu_si256,
            };

            unsafe {
                let v = _mm256_loadu_si256(data.as_ptr().cast());
                let m = _mm256_loadu_si256(mapping.as_ptr().cast());
                _mm256_storeu_si256(data.as_mut_ptr().cast(), _mm256_shuffle_epi8(v, m));
            }
            return;
        }
    }

    let mut result = [0; 32];
    for i in 0..16 {
        // if the most significant bit of b is set,
        // then the destination byte is set to 0.
        if mapping[i] & 0x80 == 0u8 {
            result[i] = data[(mapping[i] % 16) as usize];
        }
        if mapping[i + 16] & 0x80 == 0u8 {
            result[i + 16] = data[(mapping[i + 16] % 16 + 16) as usize];
        }
    }
    *data = result;
}

#[rustfmt::skip]
#[cfg(test)]
mod tests {
    use core::array;

    use super::*;
    use crate::wyrand::WyRng;

    #[test]
    fn matmul_8x8() {
        let iters = 1000;
        let mut rng: WyRng = Default::default();
        let identity = <[u8; 8]>::IDENTITY;
        for _ in 0..iters {
            let a: [u8; 8] = array::from_fn(|_| rng.u8());
            let b: [u8; 8] = array::from_fn(|_| rng.u8());
            assert_eq!(a.matmul(&identity), a);

            let a_x_i = a.matmul(&identity);
            let i_x_a = identity.matmul(&a);
            
            assert_eq!(a_x_i, a);
            assert_eq!(i_x_a, a);

            let a_x_b = a.matmul(&b);
            let b_t = b.transposed();
            let mut expected = [0; 8];
            for (i, row) in a.iter().enumerate() {
                for (j, col) in b_t.iter().enumerate() {
                    expected[i] |= (0..8).fold(0, |acc, k| acc ^ ((row >> k) & (col >> k) & 1)) << j;
                }
            }
            assert_eq!(a_x_b, expected);
        }
    }

    #[test]
    fn matmul_16x16() {
        let iters = 1000;
        let mut rng: WyRng = Default::default();
        
        let identity = <[u16; 16]>::IDENTITY;
        for _ in 0..iters {
            let a: [u16; 16] = array::from_fn(|_| rng.u16());
            let b: [u16; 16] = array::from_fn(|_| rng.u16());
            
            let a_x_i = a.matmul(&identity);
            let i_x_a = identity.matmul(&a);
            
            assert_eq!(a_x_i, a);
            assert_eq!(i_x_a, a);

            let a_x_b = a.matmul(&b);
            let b_t = b.transposed();
            let mut expected = [0; 16];
            for (i, row) in a.iter().enumerate() {
                for (j, col) in b_t.iter().enumerate() {
                    expected[i] |= (0..16).fold(0, |acc, k| acc ^ ((row >> k) & (col >> k) & 1)) << j;
                }
            }
            assert_eq!(a_x_b, expected);
        }
    }

    #[test]
    fn transpose_8x8() {
        let mut matrix = [
             0b_1_1_1_1_1_1_0_0,
             0b_1_0_0_0_0_0_1_0,
             0b_1_0_0_0_0_0_1_0,
             0b_1_0_0_0_0_0_1_0,
             0b_1_1_1_1_1_1_0_0,
             0b_1_0_0_0_0_1_0_0,
             0b_1_0_0_0_0_0_1_0,
             0b_1_0_0_0_0_0_0_1,
        ];

        matrix.transpose();

        assert_eq!(
            matrix,
            [
                0b_1_0_0_0_0_0_0_0,
                0b_0_1_0_0_1_1_1_0,
                0b_0_0_1_1_0_0_0_1,
                0b_0_0_0_1_0_0_0_1,
                0b_0_0_0_1_0_0_0_1,
                0b_0_0_0_1_0_0_0_1,
                0b_0_0_0_1_0_0_0_1,
                0b_1_1_1_1_1_1_1_1,
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
