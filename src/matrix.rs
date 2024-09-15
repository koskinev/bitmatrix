#[cfg(test)]
mod tests;

mod utils;

use utils::{delta_swap, transpose_mask, w_and, w_delta_swap, w_mul_n1, w_shr, w_xor};

/// A trait for bit matrices.
pub trait BitMatrix {
    /// The number of rows and columns in the matrix.
    const SIZE: usize;

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
    /// type M = [u8; 8];
    ///
    /// let identity = M::IDENTITY;
    /// assert_eq!(identity.count_ones(), 8);
    /// ```
    fn count_ones(&self) -> u32;

    /// Returns the number of ´0´-bits in the matrix.
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// type M = [u8; 8];
    ///
    /// let identity = M::IDENTITY;
    /// assert_eq!(identity.count_zeros(), 8 * 8 - 8);
    /// ```
    fn count_zeros(&self) -> u32;

    /// Returns `1` if the bit at the specified row and column is set. Otherwise returns `0`.
    /// ```
    /// use bitmatrix::BitMatrix;
    /// type M = [u8; 8];
    ///
    /// let identity = M::IDENTITY;
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
    /// type M = [u8; 8];
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
    /// assert_eq!(matrix.matmul(M::IDENTITY), matrix);
    /// ```
    fn matmul(self, rhs: Self) -> Self;

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
    const SIZE: usize = 8;
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

    fn matmul(self, rhs: Self) -> Self {
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

        const COL: u64 = 0x0101010101010101;
        const ROW: u64 = 0x00000000000000FF;

        let this = u64::from_ne_bytes(self);
        let other = u64::from_ne_bytes(rhs);

        let mut result = (COL & this) * (ROW & other);
        result ^= (COL & (this >> 1)) * (ROW & (other >> 8));
        result ^= (COL & (this >> 2)) * (ROW & (other >> 16));
        result ^= (COL & (this >> 3)) * (ROW & (other >> 24));
        result ^= (COL & (this >> 4)) * (ROW & (other >> 32));
        result ^= (COL & (this >> 5)) * (ROW & (other >> 40));
        result ^= (COL & (this >> 6)) * (ROW & (other >> 48));
        result ^= (COL & (this >> 7)) * (ROW & (other >> 56));

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
        x = delta_swap(x, 0x00AA00AA00AA00AA, 7);
        x = delta_swap(x, 0x0000CCCC0000CCCC, 14);
        x = delta_swap(x, 0x00000000F0F0F0F0, 28);
        *self = x.to_ne_bytes();

        // There is an alternative implementation from Hacker's Delight 2nd ed.
        // by Henry S. Warren, Jr. (2013), section 7-3 "Transposing a Bit Matrix".
    }
}

impl BitMatrix for [u16; 16] {
    type RowRepr = u16;

    const IDENTITY: Self = [
        0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80, 0x100, 0x200, 0x400, 0x800, 0x1000, 0x2000,
        0x4000, 0x8000,
    ];
    const SIZE: usize = 16;
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

    fn matmul(mut self, rhs: Self) -> Self {
        type BV = [u64; 4];
        const COL: [u64; 4] = [0x1000100010001; 4];
        const ROW: [u64; 4] = [0xFFFF, 0x0, 0x0, 0x0];

        let ptr: *mut BV = self.as_mut_ptr().cast();
        let mut this = unsafe { ptr.cast::<BV>().read_unaligned() };
        let mut other = unsafe { rhs.as_ptr().cast::<BV>().read_unaligned() };
        let mut result: BV = w_mul_n1(w_and(COL, this), w_and(ROW, other));

        for _ in 0..Self::SIZE - 1 {
            // this >>= 1; other >>= 16;
            (this, other) = (w_shr(this, 1), w_shr(other, Self::SIZE as u32));

            // result ^= (COL & this) * (ROW & other);
            result = w_xor(result, w_mul_n1(w_and(COL, this), w_and(ROW, other)));
        }

        unsafe { ptr.write_unaligned(result) }
        self
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
        type BV = [u64; 4];
        const MASK0: BV = transpose_mask(1);
        const MASK1: BV = transpose_mask(2);
        const MASK2: BV = transpose_mask(4);
        const MASK3: BV = transpose_mask(8);

        let ptr: *mut BV = self.as_mut_ptr().cast();
        let mut x = unsafe { ptr.read_unaligned() };

        x = w_delta_swap(x, MASK0, 15);
        x = w_delta_swap(x, MASK1, 30);
        x = w_delta_swap(x, MASK2, 60);
        x = w_delta_swap(x, MASK3, 120);

        unsafe { ptr.write_unaligned(x) };
    }
}

impl BitMatrix for [u32; 32] {
    type RowRepr = u32;

    const IDENTITY: Self = [
        0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80, 0x100, 0x200, 0x400, 0x800, 0x1000, 0x2000,
        0x4000, 0x8000, 0x10000, 0x20000, 0x40000, 0x80000, 0x100000, 0x200000, 0x400000, 0x800000,
        0x1000000, 0x2000000, 0x4000000, 0x8000000, 0x10000000, 0x20000000, 0x40000000, 0x80000000,
    ];
    const SIZE: usize = 32;
    const ZERO: Self = [0; 32];

    fn count_ones(&self) -> u32 {
        self.iter().fold(0, |acc, row| acc + row.count_ones())
    }

    fn count_zeros(&self) -> u32 {
        self.iter().fold(0, |acc, row| acc + row.count_zeros())
    }

    fn get(&self, row: usize, col: usize) -> u8 {
        ((self[row] >> col) & 1) as u8
    }

    fn matmul(mut self, rhs: Self) -> Self {
        type BV = [u64; 16];
        const COL: BV = [0x100000001; 16];
        const ROW: BV = [0xFFFFFFFF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

        let ptr: *mut BV = self.as_mut_ptr().cast();
        let mut this = unsafe { ptr.cast::<BV>().read_unaligned() };
        let mut other = unsafe { rhs.as_ptr().cast::<BV>().read_unaligned() };
        let mut result: BV = w_mul_n1(w_and(COL, this), w_and(ROW, other));

        for _ in 0..Self::SIZE - 1 {
            // this >>= 1; other >>= 16;
            (this, other) = (w_shr(this, 1), w_shr(other, Self::SIZE as u32));

            // result ^= (COL & this) * (ROW & other);
            result = w_xor(result, w_mul_n1(w_and(COL, this), w_and(ROW, other)));
        }

        unsafe { ptr.write_unaligned(result) }
        self
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
        type BV = [u64; 16];

        const MASK0: BV = transpose_mask(1);
        const MASK1: BV = transpose_mask(2);
        const MASK2: BV = transpose_mask(4);
        const MASK3: BV = transpose_mask(8);
        const MASK4: BV = transpose_mask(16);

        let ptr: *mut BV = self.as_mut_ptr().cast();
        let mut x = unsafe { ptr.read_unaligned() };

        x = w_delta_swap(x, MASK0, 31);
        x = w_delta_swap(x, MASK1, 62);
        x = w_delta_swap(x, MASK2, 124);
        x = w_delta_swap(x, MASK3, 248);
        x = w_delta_swap(x, MASK4, 496);

        unsafe { ptr.write_unaligned(x) };
    }
}

impl BitMatrix for [u64; 64] {
    type RowRepr = u64;

    const IDENTITY: Self = [
        0x1,
        0x2,
        0x4,
        0x8,
        0x10,
        0x20,
        0x40,
        0x80,
        0x100,
        0x200,
        0x400,
        0x800,
        0x1000,
        0x2000,
        0x4000,
        0x8000,
        0x10000,
        0x20000,
        0x40000,
        0x80000,
        0x100000,
        0x200000,
        0x400000,
        0x800000,
        0x1000000,
        0x2000000,
        0x4000000,
        0x8000000,
        0x10000000,
        0x20000000,
        0x40000000,
        0x80000000,
        0x100000000,
        0x200000000,
        0x400000000,
        0x800000000,
        0x1000000000,
        0x2000000000,
        0x4000000000,
        0x8000000000,
        0x10000000000,
        0x20000000000,
        0x40000000000,
        0x80000000000,
        0x100000000000,
        0x200000000000,
        0x400000000000,
        0x800000000000,
        0x1000000000000,
        0x2000000000000,
        0x4000000000000,
        0x8000000000000,
        0x10000000000000,
        0x20000000000000,
        0x40000000000000,
        0x80000000000000,
        0x100000000000000,
        0x200000000000000,
        0x400000000000000,
        0x800000000000000,
        0x1000000000000000,
        0x2000000000000000,
        0x4000000000000000,
        0x8000000000000000,
    ];
    const SIZE: usize = 64;
    const ZERO: Self = [0; 64];

    fn count_ones(&self) -> u32 {
        self.iter().fold(0, |acc, row| acc + row.count_ones())
    }

    fn count_zeros(&self) -> u32 {
        self.iter().fold(0, |acc, row| acc + row.count_zeros())
    }

    fn get(&self, row: usize, col: usize) -> u8 {
        ((self[row] >> col) & 1) as u8
    }

    fn matmul(mut self, mut rhs: Self) -> Self {
        type BV = [u64; 64];
        const COL: [u64; 64] = [0x1; 64];
        const ROW: [u64; 64] = {
            let mut row = [0; 64];
            row[0] = u64::MAX;
            row
        };

        let mut result: BV = w_mul_n1(w_and(COL, self), w_and(ROW, rhs));
        for _ in 0..Self::SIZE - 1 {
            // self >>= 1; rhs >>= 64;
            (self, rhs) = (w_shr(self, 1), w_shr(rhs, Self::SIZE as u32));

            // result ^= (COL & self) * (ROW & rhs);
            result = w_xor(result, w_mul_n1(w_and(COL, self), w_and(ROW, rhs)));
        }
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
        type BV = [u64; 64];

        const MASK0: BV = transpose_mask(1);
        const MASK1: BV = transpose_mask(2);
        const MASK2: BV = transpose_mask(4);
        const MASK3: BV = transpose_mask(8);
        const MASK4: BV = transpose_mask(16);
        const MASK5: BV = transpose_mask(32);

        *self = w_delta_swap(*self, MASK0, 63);
        *self = w_delta_swap(*self, MASK1, 126);
        *self = w_delta_swap(*self, MASK2, 252);
        *self = w_delta_swap(*self, MASK3, 504);
        *self = w_delta_swap(*self, MASK4, 1008);
        *self = w_delta_swap(*self, MASK5, 2016);
    }
}
