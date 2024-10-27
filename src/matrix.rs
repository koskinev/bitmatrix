#[cfg(test)]
mod tests;

mod utils;

use utils::{delta_exchange, delta_swap, row_sum_index, stripe_index};

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

    const IDENTITY: Self = {
        let mut identity = [1; Self::SIZE];
        let mut shift = 0;
        while shift < Self::SIZE {
            identity[shift] = 1 << shift;
            shift += 1;
        }
        identity
    };
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
            temp.sort_unstable_by_key(|row| row & mask);
            for (row, bits) in self.iter_mut().zip(temp.iter()) {
                *row &= !mask;
                *row |= bits & mask;
            }
            unsorted ^= mask;
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

        let mut res = u64::from_ne_bytes(*self);
        let t = ((res >> 7) ^ res) & 0x00AA00AA00AA00AA;
        res = (res ^ t) ^ (t << 7);
        let t = ((res >> 14) ^ res) & 0x0000CCCC0000CCCC;
        res = (res ^ t) ^ (t << 14);
        let t = ((res >> 28) ^ res) & 0x00000000F0F0F0F0;
        res = (res ^ t) ^ (t << 28);
        *self = res.to_ne_bytes();

        // Below is an alternative implementation adapted from Hacker's Delight 2nd ed.
        // by Henry S. Warren, Jr. (2013), section 7-3 "Transposing a Bit Matrix".

        // let mut res = u64::from_ne_bytes(*self);
        // res = res & 0xAA55AA55AA55AA55
        //     | (res & 0x00AA00AA00AA00AA) << 7
        //     | (res >> 7) & 0x00AA00AA00AA00AA;
        // res = res & 0xCCCC3333CCCC3333
        //     | (res & 0x0000CCCC0000CCCC) << 14
        //     | (res >> 14) & 0x0000CCCC0000CCCC;
        // res = res & 0xF0F0F0F00F0F0F0F
        //     | (res & 0x00000000F0F0F0F0) << 28
        //     | (res >> 28) & 0x00000000F0F0F0F0;
        // *self = res.to_ne_bytes();
    }
}

impl BitMatrix for [u16; 16] {
    type RowRepr = u16;

    const IDENTITY: Self = {
        let mut identity = [1; Self::SIZE];
        let mut shift = 0;
        while shift < Self::SIZE {
            identity[shift] = 1 << shift;
            shift += 1;
        }
        identity
    };
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

    fn matmul(self, rhs: Self) -> Self {
        const STRIPE_BITS: usize = 4;
        const SUM_TABLE_LEN: usize = 1 << STRIPE_BITS;
        const STRIPE_INDEX: [usize; SUM_TABLE_LEN] = stripe_index();
        const ROW_SUM_INDEX: [usize; SUM_TABLE_LEN] = row_sum_index();

        let mut sums = [0; SUM_TABLE_LEN];
        let mut result = [0; Self::SIZE];
        let mut mask = (1 << STRIPE_BITS) - 1;
        let mut shift = 0;
        while mask != 0 {
            let range = shift..(shift + STRIPE_BITS).min(Self::SIZE);
            let stripe = &rhs[range];
            sums[0] = 0;
            for i in 1..SUM_TABLE_LEN {
                let j = STRIPE_INDEX[i];
                sums[i] = sums[i - 1] ^ stripe[j];
            }

            for (i, row) in self.iter().enumerate() {
                let prefix = ((row & mask) >> shift) as usize;
                let j = ROW_SUM_INDEX[prefix];
                result[i] ^= sums[j];
            }
            mask <<= STRIPE_BITS;
            shift += STRIPE_BITS;
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
        // The matrix is read into four 64-bit integers, each containing four rows of the matrix.
        let ptr: *mut [u64; 4] = self.as_mut_ptr().cast();
        let [mut a, mut b, mut c, mut d] = unsafe { ptr.read_unaligned() };

        // The strategy is to first swap bits between `a`, `b`, `c`, and `d` so that each contains
        // the bits of corresponding block of the transposed matrix, but not in the correct
        // order. After this, the bits are permuted to get the correct order. The following performs
        // the first step:

        const MASK0: u64 = 0x00FF00FF00FF00FF;
        const MASK1: u64 = 0x0F0F0F0F0F0F0F0F;
        const MASK2: u64 = 0x0000AAAA0000AAAA;
        const MASK3: u64 = 0x00000000CCCCCCCC;

        (c, a) = delta_exchange(c, a, MASK0, 8);
        (d, b) = delta_exchange(d, b, MASK0, 8);
        (b, a) = delta_exchange(b, a, MASK1, 4);
        (d, c) = delta_exchange(d, c, MASK1, 4);

        // The following code performs the second step.
        a = delta_swap(a, MASK2, 15);
        b = delta_swap(b, MASK2, 15);
        c = delta_swap(c, MASK2, 15);
        d = delta_swap(d, MASK2, 15);
        a = delta_swap(a, MASK3, 30);
        b = delta_swap(b, MASK3, 30);
        c = delta_swap(c, MASK3, 30);
        d = delta_swap(d, MASK3, 30);

        // Read the result back into the matrix.
        unsafe { ptr.write_unaligned([a, b, c, d]) };
    }
}

impl BitMatrix for [u32; 32] {
    type RowRepr = u32;

    const IDENTITY: Self = {
        let mut identity = [1; Self::SIZE];
        let mut shift = 0;
        while shift < Self::SIZE {
            identity[shift] = 1 << shift;
            shift += 1;
        }
        identity
    };
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

    fn matmul(self, rhs: Self) -> Self {
        const STRIPE_BITS: usize = 4;
        const SUM_TABLE_LEN: usize = 1 << STRIPE_BITS;
        const STRIPE_INDEX: [usize; SUM_TABLE_LEN] = stripe_index();
        const ROW_SUM_INDEX: [usize; SUM_TABLE_LEN] = row_sum_index();

        let mut sums = [0; SUM_TABLE_LEN];
        let mut result = [0; Self::SIZE];
        let mut mask = (1 << STRIPE_BITS) - 1;
        let mut shift = 0;
        while mask != 0 {
            let range = shift..(shift + STRIPE_BITS).min(Self::SIZE);
            let stripe = &rhs[range];
            sums[0] = 0;
            for i in 1..SUM_TABLE_LEN {
                let j = STRIPE_INDEX[i];
                sums[i] = sums[i - 1] ^ stripe[j];
            }

            for (i, row) in self.iter().enumerate() {
                let prefix = ((row & mask) >> shift) as usize;
                let j = ROW_SUM_INDEX[prefix];
                result[i] ^= sums[j];
            }
            mask <<= STRIPE_BITS;
            shift += STRIPE_BITS;
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
        let mask = 0xFFFF;
        (self[16], self[0]) = delta_exchange(self[16], self[0], mask, 16);
        (self[17], self[1]) = delta_exchange(self[17], self[1], mask, 16);
        (self[18], self[2]) = delta_exchange(self[18], self[2], mask, 16);
        (self[19], self[3]) = delta_exchange(self[19], self[3], mask, 16);
        (self[20], self[4]) = delta_exchange(self[20], self[4], mask, 16);
        (self[21], self[5]) = delta_exchange(self[21], self[5], mask, 16);
        (self[22], self[6]) = delta_exchange(self[22], self[6], mask, 16);
        (self[23], self[7]) = delta_exchange(self[23], self[7], mask, 16);
        (self[24], self[8]) = delta_exchange(self[24], self[8], mask, 16);
        (self[25], self[9]) = delta_exchange(self[25], self[9], mask, 16);
        (self[26], self[10]) = delta_exchange(self[26], self[10], mask, 16);
        (self[27], self[11]) = delta_exchange(self[27], self[11], mask, 16);
        (self[28], self[12]) = delta_exchange(self[28], self[12], mask, 16);
        (self[29], self[13]) = delta_exchange(self[29], self[13], mask, 16);
        (self[30], self[14]) = delta_exchange(self[30], self[14], mask, 16);
        (self[31], self[15]) = delta_exchange(self[31], self[15], mask, 16);

        let mask = 0xFF00FF;
        (self[8], self[0]) = delta_exchange(self[8], self[0], mask, 8);
        (self[9], self[1]) = delta_exchange(self[9], self[1], mask, 8);
        (self[10], self[2]) = delta_exchange(self[10], self[2], mask, 8);
        (self[11], self[3]) = delta_exchange(self[11], self[3], mask, 8);
        (self[12], self[4]) = delta_exchange(self[12], self[4], mask, 8);
        (self[13], self[5]) = delta_exchange(self[13], self[5], mask, 8);
        (self[14], self[6]) = delta_exchange(self[14], self[6], mask, 8);
        (self[15], self[7]) = delta_exchange(self[15], self[7], mask, 8);
        (self[24], self[16]) = delta_exchange(self[24], self[16], mask, 8);
        (self[25], self[17]) = delta_exchange(self[25], self[17], mask, 8);
        (self[26], self[18]) = delta_exchange(self[26], self[18], mask, 8);
        (self[27], self[19]) = delta_exchange(self[27], self[19], mask, 8);
        (self[28], self[20]) = delta_exchange(self[28], self[20], mask, 8);
        (self[29], self[21]) = delta_exchange(self[29], self[21], mask, 8);
        (self[30], self[22]) = delta_exchange(self[30], self[22], mask, 8);
        (self[31], self[23]) = delta_exchange(self[31], self[23], mask, 8);

        let mask = 0xF0F0F0F;
        (self[4], self[0]) = delta_exchange(self[4], self[0], mask, 4);
        (self[5], self[1]) = delta_exchange(self[5], self[1], mask, 4);
        (self[6], self[2]) = delta_exchange(self[6], self[2], mask, 4);
        (self[7], self[3]) = delta_exchange(self[7], self[3], mask, 4);
        (self[12], self[8]) = delta_exchange(self[12], self[8], mask, 4);
        (self[13], self[9]) = delta_exchange(self[13], self[9], mask, 4);
        (self[14], self[10]) = delta_exchange(self[14], self[10], mask, 4);
        (self[15], self[11]) = delta_exchange(self[15], self[11], mask, 4);
        (self[20], self[16]) = delta_exchange(self[20], self[16], mask, 4);
        (self[21], self[17]) = delta_exchange(self[21], self[17], mask, 4);
        (self[22], self[18]) = delta_exchange(self[22], self[18], mask, 4);
        (self[23], self[19]) = delta_exchange(self[23], self[19], mask, 4);
        (self[28], self[24]) = delta_exchange(self[28], self[24], mask, 4);
        (self[29], self[25]) = delta_exchange(self[29], self[25], mask, 4);
        (self[30], self[26]) = delta_exchange(self[30], self[26], mask, 4);
        (self[31], self[27]) = delta_exchange(self[31], self[27], mask, 4);

        let mask = 0x33333333;
        (self[2], self[0]) = delta_exchange(self[2], self[0], mask, 2);
        (self[3], self[1]) = delta_exchange(self[3], self[1], mask, 2);
        (self[6], self[4]) = delta_exchange(self[6], self[4], mask, 2);
        (self[7], self[5]) = delta_exchange(self[7], self[5], mask, 2);
        (self[10], self[8]) = delta_exchange(self[10], self[8], mask, 2);
        (self[11], self[9]) = delta_exchange(self[11], self[9], mask, 2);
        (self[14], self[12]) = delta_exchange(self[14], self[12], mask, 2);
        (self[15], self[13]) = delta_exchange(self[15], self[13], mask, 2);
        (self[18], self[16]) = delta_exchange(self[18], self[16], mask, 2);
        (self[19], self[17]) = delta_exchange(self[19], self[17], mask, 2);
        (self[22], self[20]) = delta_exchange(self[22], self[20], mask, 2);
        (self[23], self[21]) = delta_exchange(self[23], self[21], mask, 2);
        (self[26], self[24]) = delta_exchange(self[26], self[24], mask, 2);
        (self[27], self[25]) = delta_exchange(self[27], self[25], mask, 2);
        (self[30], self[28]) = delta_exchange(self[30], self[28], mask, 2);
        (self[31], self[29]) = delta_exchange(self[31], self[29], mask, 2);

        let mask = 0x55555555;
        (self[1], self[0]) = delta_exchange(self[1], self[0], mask, 1);
        (self[3], self[2]) = delta_exchange(self[3], self[2], mask, 1);
        (self[5], self[4]) = delta_exchange(self[5], self[4], mask, 1);
        (self[7], self[6]) = delta_exchange(self[7], self[6], mask, 1);
        (self[9], self[8]) = delta_exchange(self[9], self[8], mask, 1);
        (self[11], self[10]) = delta_exchange(self[11], self[10], mask, 1);
        (self[13], self[12]) = delta_exchange(self[13], self[12], mask, 1);
        (self[15], self[14]) = delta_exchange(self[15], self[14], mask, 1);
        (self[17], self[16]) = delta_exchange(self[17], self[16], mask, 1);
        (self[19], self[18]) = delta_exchange(self[19], self[18], mask, 1);
        (self[21], self[20]) = delta_exchange(self[21], self[20], mask, 1);
        (self[23], self[22]) = delta_exchange(self[23], self[22], mask, 1);
        (self[25], self[24]) = delta_exchange(self[25], self[24], mask, 1);
        (self[27], self[26]) = delta_exchange(self[27], self[26], mask, 1);
        (self[29], self[28]) = delta_exchange(self[29], self[28], mask, 1);
        (self[31], self[30]) = delta_exchange(self[31], self[30], mask, 1);
    }
}

impl BitMatrix for [u64; 64] {
    type RowRepr = u64;

    const IDENTITY: Self = {
        let mut identity = [1; Self::SIZE];
        let mut shift = 0;
        while shift < Self::SIZE {
            identity[shift] = 1 << shift;
            shift += 1;
        }
        identity
    };
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

    fn matmul(self, rhs: Self) -> Self {
        const STRIPE_BITS: usize = 4;
        const SUM_TABLE_LEN: usize = 1 << STRIPE_BITS;
        const STRIPE_INDEX: [usize; SUM_TABLE_LEN] = stripe_index();
        const ROW_SUM_INDEX: [usize; SUM_TABLE_LEN] = row_sum_index();

        let mut sums = [0; SUM_TABLE_LEN];
        let mut result = [0; Self::SIZE];
        let mut mask = (1 << STRIPE_BITS) - 1;
        let mut shift = 0;
        while mask != 0 {
            let range = shift..(shift + STRIPE_BITS).min(Self::SIZE);
            let stripe = &rhs[range];
            sums[0] = 0;
            for i in 1..SUM_TABLE_LEN {
                let j = STRIPE_INDEX[i];
                sums[i] = sums[i - 1] ^ stripe[j];
            }

            for (i, row) in self.iter().enumerate() {
                let prefix = ((row & mask) >> shift) as usize;
                let j = ROW_SUM_INDEX[prefix];
                result[i] ^= sums[j];
            }
            mask <<= STRIPE_BITS;
            shift += STRIPE_BITS;
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
        let mask = 0xFFFFFFFF;
        (self[32], self[0]) = delta_exchange(self[32], self[0], mask, 32);
        (self[33], self[1]) = delta_exchange(self[33], self[1], mask, 32);
        (self[34], self[2]) = delta_exchange(self[34], self[2], mask, 32);
        (self[35], self[3]) = delta_exchange(self[35], self[3], mask, 32);
        (self[36], self[4]) = delta_exchange(self[36], self[4], mask, 32);
        (self[37], self[5]) = delta_exchange(self[37], self[5], mask, 32);
        (self[38], self[6]) = delta_exchange(self[38], self[6], mask, 32);
        (self[39], self[7]) = delta_exchange(self[39], self[7], mask, 32);
        (self[40], self[8]) = delta_exchange(self[40], self[8], mask, 32);
        (self[41], self[9]) = delta_exchange(self[41], self[9], mask, 32);
        (self[42], self[10]) = delta_exchange(self[42], self[10], mask, 32);
        (self[43], self[11]) = delta_exchange(self[43], self[11], mask, 32);
        (self[44], self[12]) = delta_exchange(self[44], self[12], mask, 32);
        (self[45], self[13]) = delta_exchange(self[45], self[13], mask, 32);
        (self[46], self[14]) = delta_exchange(self[46], self[14], mask, 32);
        (self[47], self[15]) = delta_exchange(self[47], self[15], mask, 32);
        (self[48], self[16]) = delta_exchange(self[48], self[16], mask, 32);
        (self[49], self[17]) = delta_exchange(self[49], self[17], mask, 32);
        (self[50], self[18]) = delta_exchange(self[50], self[18], mask, 32);
        (self[51], self[19]) = delta_exchange(self[51], self[19], mask, 32);
        (self[52], self[20]) = delta_exchange(self[52], self[20], mask, 32);
        (self[53], self[21]) = delta_exchange(self[53], self[21], mask, 32);
        (self[54], self[22]) = delta_exchange(self[54], self[22], mask, 32);
        (self[55], self[23]) = delta_exchange(self[55], self[23], mask, 32);
        (self[56], self[24]) = delta_exchange(self[56], self[24], mask, 32);
        (self[57], self[25]) = delta_exchange(self[57], self[25], mask, 32);
        (self[58], self[26]) = delta_exchange(self[58], self[26], mask, 32);
        (self[59], self[27]) = delta_exchange(self[59], self[27], mask, 32);
        (self[60], self[28]) = delta_exchange(self[60], self[28], mask, 32);
        (self[61], self[29]) = delta_exchange(self[61], self[29], mask, 32);
        (self[62], self[30]) = delta_exchange(self[62], self[30], mask, 32);
        (self[63], self[31]) = delta_exchange(self[63], self[31], mask, 32);

        let mask = 0xFFFF0000FFFF;
        (self[16], self[0]) = delta_exchange(self[16], self[0], mask, 16);
        (self[17], self[1]) = delta_exchange(self[17], self[1], mask, 16);
        (self[18], self[2]) = delta_exchange(self[18], self[2], mask, 16);
        (self[19], self[3]) = delta_exchange(self[19], self[3], mask, 16);
        (self[20], self[4]) = delta_exchange(self[20], self[4], mask, 16);
        (self[21], self[5]) = delta_exchange(self[21], self[5], mask, 16);
        (self[22], self[6]) = delta_exchange(self[22], self[6], mask, 16);
        (self[23], self[7]) = delta_exchange(self[23], self[7], mask, 16);
        (self[24], self[8]) = delta_exchange(self[24], self[8], mask, 16);
        (self[25], self[9]) = delta_exchange(self[25], self[9], mask, 16);
        (self[26], self[10]) = delta_exchange(self[26], self[10], mask, 16);
        (self[27], self[11]) = delta_exchange(self[27], self[11], mask, 16);
        (self[28], self[12]) = delta_exchange(self[28], self[12], mask, 16);
        (self[29], self[13]) = delta_exchange(self[29], self[13], mask, 16);
        (self[30], self[14]) = delta_exchange(self[30], self[14], mask, 16);
        (self[31], self[15]) = delta_exchange(self[31], self[15], mask, 16);
        (self[48], self[32]) = delta_exchange(self[48], self[32], mask, 16);
        (self[49], self[33]) = delta_exchange(self[49], self[33], mask, 16);
        (self[50], self[34]) = delta_exchange(self[50], self[34], mask, 16);
        (self[51], self[35]) = delta_exchange(self[51], self[35], mask, 16);
        (self[52], self[36]) = delta_exchange(self[52], self[36], mask, 16);
        (self[53], self[37]) = delta_exchange(self[53], self[37], mask, 16);
        (self[54], self[38]) = delta_exchange(self[54], self[38], mask, 16);
        (self[55], self[39]) = delta_exchange(self[55], self[39], mask, 16);
        (self[56], self[40]) = delta_exchange(self[56], self[40], mask, 16);
        (self[57], self[41]) = delta_exchange(self[57], self[41], mask, 16);
        (self[58], self[42]) = delta_exchange(self[58], self[42], mask, 16);
        (self[59], self[43]) = delta_exchange(self[59], self[43], mask, 16);
        (self[60], self[44]) = delta_exchange(self[60], self[44], mask, 16);
        (self[61], self[45]) = delta_exchange(self[61], self[45], mask, 16);
        (self[62], self[46]) = delta_exchange(self[62], self[46], mask, 16);
        (self[63], self[47]) = delta_exchange(self[63], self[47], mask, 16);

        let mask = 0xFF00FF00FF00FF;
        (self[8], self[0]) = delta_exchange(self[8], self[0], mask, 8);
        (self[9], self[1]) = delta_exchange(self[9], self[1], mask, 8);
        (self[10], self[2]) = delta_exchange(self[10], self[2], mask, 8);
        (self[11], self[3]) = delta_exchange(self[11], self[3], mask, 8);
        (self[12], self[4]) = delta_exchange(self[12], self[4], mask, 8);
        (self[13], self[5]) = delta_exchange(self[13], self[5], mask, 8);
        (self[14], self[6]) = delta_exchange(self[14], self[6], mask, 8);
        (self[15], self[7]) = delta_exchange(self[15], self[7], mask, 8);
        (self[24], self[16]) = delta_exchange(self[24], self[16], mask, 8);
        (self[25], self[17]) = delta_exchange(self[25], self[17], mask, 8);
        (self[26], self[18]) = delta_exchange(self[26], self[18], mask, 8);
        (self[27], self[19]) = delta_exchange(self[27], self[19], mask, 8);
        (self[28], self[20]) = delta_exchange(self[28], self[20], mask, 8);
        (self[29], self[21]) = delta_exchange(self[29], self[21], mask, 8);
        (self[30], self[22]) = delta_exchange(self[30], self[22], mask, 8);
        (self[31], self[23]) = delta_exchange(self[31], self[23], mask, 8);
        (self[40], self[32]) = delta_exchange(self[40], self[32], mask, 8);
        (self[41], self[33]) = delta_exchange(self[41], self[33], mask, 8);
        (self[42], self[34]) = delta_exchange(self[42], self[34], mask, 8);
        (self[43], self[35]) = delta_exchange(self[43], self[35], mask, 8);
        (self[44], self[36]) = delta_exchange(self[44], self[36], mask, 8);
        (self[45], self[37]) = delta_exchange(self[45], self[37], mask, 8);
        (self[46], self[38]) = delta_exchange(self[46], self[38], mask, 8);
        (self[47], self[39]) = delta_exchange(self[47], self[39], mask, 8);
        (self[56], self[48]) = delta_exchange(self[56], self[48], mask, 8);
        (self[57], self[49]) = delta_exchange(self[57], self[49], mask, 8);
        (self[58], self[50]) = delta_exchange(self[58], self[50], mask, 8);
        (self[59], self[51]) = delta_exchange(self[59], self[51], mask, 8);
        (self[60], self[52]) = delta_exchange(self[60], self[52], mask, 8);
        (self[61], self[53]) = delta_exchange(self[61], self[53], mask, 8);
        (self[62], self[54]) = delta_exchange(self[62], self[54], mask, 8);
        (self[63], self[55]) = delta_exchange(self[63], self[55], mask, 8);

        let mask = 0xF0F0F0F0F0F0F0F;
        (self[4], self[0]) = delta_exchange(self[4], self[0], mask, 4);
        (self[5], self[1]) = delta_exchange(self[5], self[1], mask, 4);
        (self[6], self[2]) = delta_exchange(self[6], self[2], mask, 4);
        (self[7], self[3]) = delta_exchange(self[7], self[3], mask, 4);
        (self[12], self[8]) = delta_exchange(self[12], self[8], mask, 4);
        (self[13], self[9]) = delta_exchange(self[13], self[9], mask, 4);
        (self[14], self[10]) = delta_exchange(self[14], self[10], mask, 4);
        (self[15], self[11]) = delta_exchange(self[15], self[11], mask, 4);
        (self[20], self[16]) = delta_exchange(self[20], self[16], mask, 4);
        (self[21], self[17]) = delta_exchange(self[21], self[17], mask, 4);
        (self[22], self[18]) = delta_exchange(self[22], self[18], mask, 4);
        (self[23], self[19]) = delta_exchange(self[23], self[19], mask, 4);
        (self[28], self[24]) = delta_exchange(self[28], self[24], mask, 4);
        (self[29], self[25]) = delta_exchange(self[29], self[25], mask, 4);
        (self[30], self[26]) = delta_exchange(self[30], self[26], mask, 4);
        (self[31], self[27]) = delta_exchange(self[31], self[27], mask, 4);
        (self[36], self[32]) = delta_exchange(self[36], self[32], mask, 4);
        (self[37], self[33]) = delta_exchange(self[37], self[33], mask, 4);
        (self[38], self[34]) = delta_exchange(self[38], self[34], mask, 4);
        (self[39], self[35]) = delta_exchange(self[39], self[35], mask, 4);
        (self[44], self[40]) = delta_exchange(self[44], self[40], mask, 4);
        (self[45], self[41]) = delta_exchange(self[45], self[41], mask, 4);
        (self[46], self[42]) = delta_exchange(self[46], self[42], mask, 4);
        (self[47], self[43]) = delta_exchange(self[47], self[43], mask, 4);
        (self[52], self[48]) = delta_exchange(self[52], self[48], mask, 4);
        (self[53], self[49]) = delta_exchange(self[53], self[49], mask, 4);
        (self[54], self[50]) = delta_exchange(self[54], self[50], mask, 4);
        (self[55], self[51]) = delta_exchange(self[55], self[51], mask, 4);
        (self[60], self[56]) = delta_exchange(self[60], self[56], mask, 4);
        (self[61], self[57]) = delta_exchange(self[61], self[57], mask, 4);
        (self[62], self[58]) = delta_exchange(self[62], self[58], mask, 4);
        (self[63], self[59]) = delta_exchange(self[63], self[59], mask, 4);

        let mask = 0x3333333333333333;
        (self[2], self[0]) = delta_exchange(self[2], self[0], mask, 2);
        (self[3], self[1]) = delta_exchange(self[3], self[1], mask, 2);
        (self[6], self[4]) = delta_exchange(self[6], self[4], mask, 2);
        (self[7], self[5]) = delta_exchange(self[7], self[5], mask, 2);
        (self[10], self[8]) = delta_exchange(self[10], self[8], mask, 2);
        (self[11], self[9]) = delta_exchange(self[11], self[9], mask, 2);
        (self[14], self[12]) = delta_exchange(self[14], self[12], mask, 2);
        (self[15], self[13]) = delta_exchange(self[15], self[13], mask, 2);
        (self[18], self[16]) = delta_exchange(self[18], self[16], mask, 2);
        (self[19], self[17]) = delta_exchange(self[19], self[17], mask, 2);
        (self[22], self[20]) = delta_exchange(self[22], self[20], mask, 2);
        (self[23], self[21]) = delta_exchange(self[23], self[21], mask, 2);
        (self[26], self[24]) = delta_exchange(self[26], self[24], mask, 2);
        (self[27], self[25]) = delta_exchange(self[27], self[25], mask, 2);
        (self[30], self[28]) = delta_exchange(self[30], self[28], mask, 2);
        (self[31], self[29]) = delta_exchange(self[31], self[29], mask, 2);
        (self[34], self[32]) = delta_exchange(self[34], self[32], mask, 2);
        (self[35], self[33]) = delta_exchange(self[35], self[33], mask, 2);
        (self[38], self[36]) = delta_exchange(self[38], self[36], mask, 2);
        (self[39], self[37]) = delta_exchange(self[39], self[37], mask, 2);
        (self[42], self[40]) = delta_exchange(self[42], self[40], mask, 2);
        (self[43], self[41]) = delta_exchange(self[43], self[41], mask, 2);
        (self[46], self[44]) = delta_exchange(self[46], self[44], mask, 2);
        (self[47], self[45]) = delta_exchange(self[47], self[45], mask, 2);
        (self[50], self[48]) = delta_exchange(self[50], self[48], mask, 2);
        (self[51], self[49]) = delta_exchange(self[51], self[49], mask, 2);
        (self[54], self[52]) = delta_exchange(self[54], self[52], mask, 2);
        (self[55], self[53]) = delta_exchange(self[55], self[53], mask, 2);
        (self[58], self[56]) = delta_exchange(self[58], self[56], mask, 2);
        (self[59], self[57]) = delta_exchange(self[59], self[57], mask, 2);
        (self[62], self[60]) = delta_exchange(self[62], self[60], mask, 2);
        (self[63], self[61]) = delta_exchange(self[63], self[61], mask, 2);

        let mask = 0x5555555555555555;
        (self[1], self[0]) = delta_exchange(self[1], self[0], mask, 1);
        (self[3], self[2]) = delta_exchange(self[3], self[2], mask, 1);
        (self[5], self[4]) = delta_exchange(self[5], self[4], mask, 1);
        (self[7], self[6]) = delta_exchange(self[7], self[6], mask, 1);
        (self[9], self[8]) = delta_exchange(self[9], self[8], mask, 1);
        (self[11], self[10]) = delta_exchange(self[11], self[10], mask, 1);
        (self[13], self[12]) = delta_exchange(self[13], self[12], mask, 1);
        (self[15], self[14]) = delta_exchange(self[15], self[14], mask, 1);
        (self[17], self[16]) = delta_exchange(self[17], self[16], mask, 1);
        (self[19], self[18]) = delta_exchange(self[19], self[18], mask, 1);
        (self[21], self[20]) = delta_exchange(self[21], self[20], mask, 1);
        (self[23], self[22]) = delta_exchange(self[23], self[22], mask, 1);
        (self[25], self[24]) = delta_exchange(self[25], self[24], mask, 1);
        (self[27], self[26]) = delta_exchange(self[27], self[26], mask, 1);
        (self[29], self[28]) = delta_exchange(self[29], self[28], mask, 1);
        (self[31], self[30]) = delta_exchange(self[31], self[30], mask, 1);
        (self[33], self[32]) = delta_exchange(self[33], self[32], mask, 1);
        (self[35], self[34]) = delta_exchange(self[35], self[34], mask, 1);
        (self[37], self[36]) = delta_exchange(self[37], self[36], mask, 1);
        (self[39], self[38]) = delta_exchange(self[39], self[38], mask, 1);
        (self[41], self[40]) = delta_exchange(self[41], self[40], mask, 1);
        (self[43], self[42]) = delta_exchange(self[43], self[42], mask, 1);
        (self[45], self[44]) = delta_exchange(self[45], self[44], mask, 1);
        (self[47], self[46]) = delta_exchange(self[47], self[46], mask, 1);
        (self[49], self[48]) = delta_exchange(self[49], self[48], mask, 1);
        (self[51], self[50]) = delta_exchange(self[51], self[50], mask, 1);
        (self[53], self[52]) = delta_exchange(self[53], self[52], mask, 1);
        (self[55], self[54]) = delta_exchange(self[55], self[54], mask, 1);
        (self[57], self[56]) = delta_exchange(self[57], self[56], mask, 1);
        (self[59], self[58]) = delta_exchange(self[59], self[58], mask, 1);
        (self[61], self[60]) = delta_exchange(self[61], self[60], mask, 1);
        (self[63], self[62]) = delta_exchange(self[63], self[62], mask, 1);
    }
}
