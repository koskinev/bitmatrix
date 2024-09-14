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
        // Multiplication of two 8x8 bit matrices stored in 64-bit integers A and B.
        //
        // A                                 B
        //
        // a00 a01 a02 a03 a04 a05 a06 a07   b00 b01 b02 b03 b04 b05 b06 b07
        // a10 a11 a12 a13 a14 a15 a16 a17   b10 b11 b12 b13 b14 b15 b16 b17
        // a20 a21 a22 a23 a24 a25 a26 a27   b20 b21 b22 b23 b24 b25 b26 b27
        // a30 a31 a32 a33 a34 a35 a36 a37   b30 b31 b32 b33 b34 b35 b36 b37
        // a40 a41 a42 a43 a44 a45 a46 a47   b40 b41 b42 b43 b44 b45 b46 b47
        // a50 a51 a52 a53 a54 a55 a56 a57   b50 b51 b52 b53 b54 b55 b56 b57
        // a60 a61 a62 a63 a64 a65 a66 a67   b60 b61 b62 b63 b64 b65 b66 b67
        // a70 a71 a72 a73 a74 a75 a76 a77   b70 b71 b72 b73 b74 b75 b76 b77
        //
        // The element on the `i`th row and `j`th column cij of the resulting matrix
        //
        // cij = (ai0 & b0j) ^ (ai1 & b1j) ^ ... ^ (ai7 & b7j).
        //
        // The function uses two masks. The first masks a row in A and the second 
        // a column in B.
        //
        // COL                ROW
        //
        // 0 0 0 0 0 0 0 1    0 0 0 0 0 0 0 0
        // 0 0 0 0 0 0 0 1    0 0 0 0 0 0 0 0
        // 0 0 0 0 0 0 0 1    0 0 0 0 0 0 0 0
        // 0 0 0 0 0 0 0 1    0 0 0 0 0 0 0 0
        // 0 0 0 0 0 0 0 1    0 0 0 0 0 0 0 0
        // 0 0 0 0 0 0 0 1    0 0 0 0 0 0 0 0
        // 0 0 0 0 0 0 0 1    0 0 0 0 0 0 0 0
        // 0 0 0 0 0 0 0 1    1 1 1 1 1 1 1 1
        //
        // D = (COL & A) * (ROW & B) gives the result:
        //
        // (a70 & b07) (a70 & b17) . . . (a70 & b77)
        // (a71 & b07)    . . .          (a71 & b77)
        //      .          .                  .
        //      .            .                .
        //      .              .              .
        // (a76 & b07)      . . .        (a76 & b77)
        // (a77 & b07) (a77 & b17) . . . (a77 & b77)
        //
        // or dij = a7i & bj7, which is the last element in the XOR sum above. The
        // second to last element of the sum a6i & bj6 can be calculated by shifting 
        // A to the right by on (moving the columns to the left by one) and B to 
        // the left by8 (moving the rows down by one). The rest of the sum can be
        // calculated in a similar way.
        
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
        // Input:    Mask 0:   Step 0:   Mask 1:   Step 1:  Mask 2:   Result:
        // .$ZYXWVU  00000000  .TZRXPVN  00000000  .TLDXPHz 00000000  .TLDvnf7
        // TSRQPONM  10101010  $SYQWOUM  00000000  $SKCWOGy 00000000  $SKCume6
        // LKJIHGFE  00000000  LDJBHzFx  11001100  ZRJBVNFx 00000000  ZRJBtld5
        // DCBAzyxw  10101010  KCIAGyEw  11001100  YQIAUMEw 00000000  YQIAskc4
        // vutsrqpo  00000000  vntlrjph  00000000  vnf7rjb3 11110000  XPHzrjb3
        // nmlkjihg  10101010  umskqiog  00000000  ume6qia2 11110000  WOGyqia2
        // fedcba98  00000000  f7d5b391  11001100  tld5ph91 11110000  VNFxph91
        // 76543210  10101010  e6c4a280  11001100  skc4og80 11110000  UMEwog80

        let mut x = u64::from_ne_bytes(*self);
        x = delta_swap(x, 0x00aa00aa00aa00aa, 7);
        x = delta_swap(x, 0x0000cccc0000cccc, 14);
        x = delta_swap(x, 0x00000000f0f0f0f0, 28);
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
        const MASK0: [u64; 4] = [0xAAAA0000AAAA; 4];
        const MASK1: [u64; 4] = [0xCCCCCCCC; 4];
        const MASK2: [u64; 4] = [0xF0F0F0F0F0F0F0F0, 0, 0xF0F0F0F0F0F0F0F0, 0];
        const MASK3: [u64; 4] = [0xFF00FF00FF00FF00, 0xFF00FF00FF00FF00, 0, 0];

        let ptr: *mut [u64; 4] = self.as_mut_ptr().cast();
        let mut x = unsafe { ptr.read_unaligned() };

        x = long_delta_swap(x, MASK0, 15);
        x = long_delta_swap(x, MASK1, 30);
        x = long_delta_swap(x, MASK2, 60);
        x = long_delta_swap(x, MASK3, 120);

        unsafe { ptr.write_unaligned(x) };

    }
}

/// Moves the masked bits in `x` to the left by `shift` positions. For this function to work
/// properly, the mask and the shifted mask should not overlap, ie. `mask & (mask << shift) == 0`
/// and no bits should be shifted out, ie. `((mask << shift) >> shift) ==  mask`.  
///
/// ```text
///   x <- abcd_efgh
///   m <- 0001_0001
///   s <- x.delta_swap(m, 3)
///   s == dbca_hfge
/// ```
fn delta_swap(x: u64, m: u64, shift: u32) -> u64 {
    let t = ((x >> shift) ^ x) & m;
    (x ^ t) ^ (t << shift)
}

/// Moves the masked bits in `x` to the left by `shift` positions. This is a generalization of
/// `delta_swap` to `u64` arrays. See `delta_swap` for more information.
fn long_delta_swap<const N: usize>(x: [u64; N], m: [u64; N], shift: u32) -> [u64; N] {
    let t = long_and(&long_xor(&long_shr(x, shift), &x), &m);
    long_xor(&long_xor(&x, &t), &long_shl(t, shift))
}

/// Performs the `and` operation on two unsigned integers stored in `u64` arrays.
fn long_and<const N: usize>(a: &[u64; N], b: &[u64; N]) -> [u64; N] {
    let mut result = [0; N];
    for i in 0..N {
        result[i] = a[i] & b[i];
    }
    result
}

/// Performs the `xor` operation on two unsigned integers stored in `u64` arrays
fn long_xor<const N: usize>(a: &[u64; N], b: &[u64; N]) -> [u64; N] {
    let mut result = [0; N];
    for i in 0..N {
        result[i] = a[i] ^ b[i];
    }
    result
}

/// Multiplies two unsigned integers using the grade-school method. The arguments and
/// the result are arrays of `u64`s, where the least significant word is at index 0.
fn long_mul<const N: usize>(a: &[u64; N], b: &[u64; N]) -> [u64; N] {
    let mut result = [0; N];
    for i in 0..N {
        let mut c: u64 = 0;
        for j in 0..(N - i) {
            let t = a[i] as u128 * b[j] as u128 + result[i + j] as u128 + c as u128;
            result[i + j] = t as u64;
            c = (t >> 64) as u64;
        }
    }
    result
}

/// Performs the right shift operation. The least significant word is assumed to be at index 0.
fn long_shr<const N: usize>(mut v: [u64; N], shift: u32) -> [u64; N] {
    let delta = shift as usize / 64;
    let shift = shift % 64;
    let mask = !(!0 >> shift);
    v[0] = v[delta] >> shift;
    for i in 1..(N - delta) {
        let mut t = v[i + delta].rotate_right(shift);
        (t, v[i - 1]) = t.exchange(v[i - 1], mask);
        v[i] = t;
    }
    for a in &mut v[(N - delta)..] {
        *a = 0;
    }
    v
}

/// Performs the left shift operation. The least significant word is assumed to be at index 0.
fn long_shl<const N: usize>(mut v: [u64; N], shift: u32) -> [u64; N] {
    let delta = shift as usize / 64;
    let shift = shift % 64;
    let mask = !(!0 << shift);
    v[N - 1] = v[N - 1 - delta] << shift;
    for i in (delta..(N - 1)).rev() {
        let mut t = v[i - delta].rotate_left(shift);
        (t, v[i + 1]) = t.exchange(v[i + 1], mask);
        v[i] = t;
    }
    for a in &mut v[..delta] {
        *a = 0;
    }
    v
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

#[cfg(test)]
mod tests {
    use core::array;

    use super::*;
    use crate::wyrand::WyRng;

    #[test]
    fn long_ops_256() {
        let mut a: [u64; 4] = [
            3411780966643309290,
            18136081398534378773,
            9219065852671733502,
            8725777230731513545,
        ];
        let mut b: [u64; 4] = [
            13226243723955440707,
            2151928491308778980,
            12605647679825650081,
            324627457777546534,
        ];

        assert_eq!(
            long_mul(&a, &b),
            [
                7663115068934357822,
                11641022341552665496,
                15601942919079094697,
                5725101895204243413
            ]
        );

        a = long_shl(a, 170);
        b = long_shr(b, 19);

        assert_eq!(a, [0, 0, 15738858040729796608, 15588177382644832736]);
        assert_eq!(
            &b,
            &[
                1206004764774689763,
                9526278400645396304,
                7396271637646597879,
                619177737765
            ]
        );
        let mut a: [u64; 4] = [
            3767482649846035678,
            8196783407023646100,
            584292305772007898,
            12702511834212947002,
        ];
        let mut b: [u64; 4] = [
            8715585780955075423,
            13132038111537412268,
            9346838749455166110,
            423783674454801745,
        ];

        assert_eq!(
            long_mul(&a, &b),
            [
                15563909172995528802,
                1628886569001990366,
                6108754696538904360,
                14693161311349477231
            ]
        );

        a = long_shl(a, 52);
        b = long_shr(b, 246);

        assert_eq!(
            a,
            [
                5611485135703638016,
                6432060063453878355,
                2136707391197595486,
                4873037446303590541
            ]
        );
        assert_eq!(b, [23, 0, 0, 0]);

        let mut a: [u64; 4] = [
            598365326611093333,
            10338934721397851550,
            15716960247935151312,
            1479676015519540286,
        ];
        let mut b: [u64; 4] = [
            12968446461158918448,
            12780835236968280994,
            9895886854841068309,
            3372007258361930805,
        ];

        assert_eq!(
            long_mul(&a, &b),
            [
                16413989336046165232,
                6195171311622268739,
                4132736093975731865,
                8426831723610196255
            ]
        );

        a = long_shl(a, 94);
        b = long_shr(b, 94);

        assert_eq!(
            a,
            [
                0,
                622664246315974656,
                8662605058578543750,
                12954335171722531222
            ]
        );
        assert_eq!(
            b,
            [13909838412790798266, 8849169567253332647, 3140426481, 0]
        );
    }

    #[test]
    fn long_mul_1024() {
        let mut a: [u64; 16] = [
            6644251180660479427,
            15375144009903779421,
            11331245272523287800,
            13303551853398386828,
            17434003064827756660,
            11296844175318992981,
            5560957280063003477,
            1930804660948188527,
            9011616370765215235,
            3955907964002714030,
            10563655696236183650,
            14782017524823165738,
            356083664922244647,
            14267588606385061924,
            8257302408516677364,
            613792976545283567,
        ];
        let mut b: [u64; 16] = [
            6435477686572270493,
            15322659430818327295,
            1945775863637021984,
            393356772086011205,
            9301616734277197146,
            1763018018437677469,
            15661237430003529900,
            18388969952484669357,
            7139247846141788917,
            14969267384589550089,
            4746278236595994671,
            14272572912459441903,
            1754695267683551763,
            2206528858639209206,
            9859223317057709586,
            16161135551658693928,
        ];

        assert_eq!(
            long_mul(&a, &b),
            [
                6728071202426298775,
                800901682447864017,
                2446809230861829908,
                18386674905398012292,
                15738311996737897385,
                14142983105005950648,
                510995996386088842,
                18269057886309408751,
                3627382636336550314,
                9623552145502661218,
                3193369802868267560,
                9299859282271574595,
                17222133033300977738,
                12002415550249873749,
                6792208012154254790,
                3890904784391795613
            ]
        );

        a = long_shl(a, 856);
        b = long_shr(b, 887);

        assert_eq!(
            a,
            [
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                4538991625744941056,
                295208076076397855,
                9958465022408875900
            ]
        );
        assert_eq!(
            b,
            [
                11961206210839716925,
                10360057427372167441,
                448,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0
            ]
        );

        let mut a: [u64; 16] = [
            4129621589208185374,
            10282645349545992521,
            8138323506775935308,
            12560927580787899729,
            4232648962313989009,
            3568654794918009891,
            4741658618461894646,
            16756237985131987877,
            17405225263068866572,
            17803728918210971903,
            14422562067456280936,
            17686801495406915711,
            16991513979673859564,
            14294337257237172282,
            1760000570133965158,
            10390822289827181767,
        ];
        let mut b: [u64; 16] = [
            5620743761134239871,
            13378778125014143077,
            10657548045285664511,
            7172912901277174850,
            1888702459779789059,
            5880811507705288644,
            14112087999037257161,
            7694725826617954742,
            10355822629999972552,
            11740637184166906850,
            8092155905662476753,
            14656352361262531110,
            8143177533160265646,
            6543593581502180789,
            3853584917579324944,
            14100675213984029125,
        ];

        assert_eq!(
            long_mul(&a, &b),
            [
                13501817612426728674,
                8899611418571476318,
                8364890198696805470,
                4795978311326914456,
                12862561002109506846,
                16615116899367206053,
                1526892800474043703,
                4798914603494840412,
                15996273591059184135,
                13463544011154614465,
                7177511043023512041,
                2265618949848185506,
                4595966079890399157,
                14704864079719082982,
                10076509859502337707,
                16485883865638316824
            ]
        );

        a = long_shl(a, 164);
        b = long_shr(b, 458);

        assert_eq!(
            a,
            [
                0,
                0,
                10996560898599944192,
                3471734986841116490,
                9904705071155033971,
                7228230221966746053,
                1903537317681172824,
                1247992098940402916,
                17974850601353644558,
                13488345016910485057,
                1008384991121496756,
                8433148628045693583,
                15858465730702397943,
                12194349197137525908,
                12156727013771723781,
                6056501122911158024
            ]
        );
        assert_eq!(
            b,
            [
                3610394082586453396,
                17916425226462201444,
                8388160772909285554,
                9915821676216714712,
                16983876240034824368,
                7880244470453353829,
                9517992641113423275,
                8164285791316412311,
                13770190638656278,
                0,
                0,
                0,
                0,
                0,
                0,
                0
            ]
        );
        let mut a: [u64; 16] = [
            5409040664802577928,
            4337689103478077781,
            16572597422271931629,
            15369599225302760991,
            11559187944828407506,
            15033416988803371293,
            13765855692493603883,
            9662387262921880977,
            14351159509606292376,
            15998888353016910707,
            2860074819173280356,
            2284715345801021135,
            7753044493140388060,
            3385700548584035278,
            17960670407846100523,
            9199163298471302427,
        ];
        let mut b: [u64; 16] = [
            16417971319772498526,
            2422929543108770505,
            9743954572451819187,
            13805321096598699655,
            10947986539340510467,
            15630727282541713814,
            8347422254267615416,
            9049332421657438551,
            15600012390401415886,
            10104349279631261589,
            182793590123344121,
            8186085328265164590,
            17727725571624441461,
            4033849156266580122,
            17471063214030402589,
            6727386048098136304,
        ];

        assert_eq!(
            long_mul(&a, &b),
            [
                2810647460032870128,
                1294832851195895176,
                17988804482227675645,
                10238243024103904741,
                11344647340393550453,
                16322344134089412857,
                15091863005503959096,
                17641855065955955360,
                2359357080642552055,
                2764074877833358885,
                1959615557669726182,
                17529415291297684265,
                15234545123731262587,
                10844187904060412016,
                2967559009752448786,
                2511775429903223195
            ]
        );

        a = long_shl(a, 727);
        b = long_shr(b, 163);

        assert_eq!(
            a,
            [
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                16844757448878194688,
                11598865107641534562,
                14297813166776523080,
                9811695553521843928,
                1776596142674060772
            ]
        );
        assert_eq!(
            b,
            [
                10997535001224359777,
                1541575364977019724,
                13771157956298924536,
                718879493176782836,
                10081911582142169098,
                13524929091776722428,
                5718630287815986195,
                12092347652394924932,
                9189884098470030649,
                7748745312762812967,
                17192750045913461007,
                12544115421483066253,
                17724442029432157677,
                195792702,
                0,
                0
            ]
        );
    }
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
                    expected[i] |=
                        (0..8).fold(0, |acc, k| acc ^ ((row >> k) & (col >> k) & 1)) << j;
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
                    expected[i] |=
                        (0..16).fold(0, |acc, k| acc ^ ((row >> k) & (col >> k) & 1)) << j;
                }
            }
            assert_eq!(a_x_b, expected);
        }
    }

    #[rustfmt::skip]
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

    #[rustfmt::skip]
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

    #[rustfmt::skip]
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
