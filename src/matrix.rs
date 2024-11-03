#[cfg(test)]
mod tests;

use std::{
    array,
    ops::{
        BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not, Shl, ShlAssign, Shr,
        ShrAssign,
    },
};

pub trait BitOps
where
    Self: Copy + PartialEq + Eq + PartialOrd + Ord,
    Self: BitAnd<Output = Self> + BitOr<Output = Self> + BitXor<Output = Self> + Not<Output = Self>,
    Self: BitAndAssign + BitOrAssign + BitXorAssign,
    Self: Shl<u32, Output = Self> + Shr<u32, Output = Self>,
    Self: ShlAssign<u32> + ShrAssign<u32>,
{
}

impl BitOps for u8 {}
impl BitOps for u16 {}
impl BitOps for u32 {}
impl BitOps for u64 {}
impl BitOps for u128 {}

/// A trait for bit matrices.
pub trait BitMatrix
where
    Self: Copy,
{
    /// The number of rows and columns in the matrix.
    const SIZE: usize;

    /// The identity matrix. This is the matrix with all bits set to 0, except for the diagonal
    /// from the top-left to the bottom-right, which is set to 1.
    const IDENTITY: Self;

    /// The zero matrix. This is the matrix with all bits set to 0.
    const ZERO: Self;

    /// The type used to represent a row in the matrix.
    type RowRepr: BitOps;

    /// Performs a bitwise AND operation with another matrix.
    ///
    /// Returns a new matrix where each bit is the logical AND of the corresponding bits in `self`
    /// and `rhs`.
    fn and(self, rhs: Self) -> Self {
        self.zip(&rhs, |a, b| a & b)
    }

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

    /// Applies a function to each row in the matrix and returns the result.
    fn map<F>(&self, f: F) -> Self
    where
        F: FnMut(Self::RowRepr) -> Self::RowRepr;

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

    /// Returns a matrix where each bit is the logical negation of the corresponding bit in `self`.
    fn not(self) -> Self {
        self.map(|row| !row)
    }

    /// Performs a bitwise OR operation with another matrix.
    ///
    /// Returns a new matrix where each bit is the logical OR of the corresponding bits in `self`
    /// and `rhs`.
    fn or(self, rhs: Self) -> Self
    where
        Self: Copy,
        Self::RowRepr: BitOr<Output = Self::RowRepr>,
    {
        self.zip(&rhs, |a, b| a | b)
    }

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
    fn transposed(&self) -> Self {
        let mut mat = *self;
        mat.transpose();
        mat
    }

    /// Performs a bitwise XOR operation with another matrix.
    ///
    /// Returns a new matrix where each bit is the logical XOR of the corresponding bits in `self`
    /// and `rhs`.
    fn xor(self, rhs: Self) -> Self {
        self.zip(&rhs, |a, b| a ^ b)
    }

    /// Applies a function to corresponding elements of two matrices.
    ///
    /// Returns a new matrix where each row is the result of applying the function `f` to the
    /// corresponding rows of `self` and `rhs`.
    fn zip<F>(&self, rhs: &Self, f: F) -> Self
    where
        F: FnMut(Self::RowRepr, Self::RowRepr) -> Self::RowRepr;
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
        // (a07 & b70) (a07 & b71) . . . (a07 & b77)
        // (a17 & b70)    . . .          (a17 & b77)
        //      .          .                  .
        //      .            .                .
        //      .              .              .
        // (a67 & b70)      . . .        (a67 & b77)
        // (a77 & b70) (a77 & b71) . . . (a77 & b77)
        //
        // or dij = ai7 & b7j, which is the last element in the XOR sum above. The
        // second to last element of the sum a6i & bj6 can be calculated by shifting
        // A to the right by on (moving the columns to the left by one) and B to
        // the left by8 (moving the rows down by one). The rest of the sum can be
        // calculated in a similar way.

        const COL: u64 = 0x0101010101010101;

        let this = u64::from_ne_bytes(self);
        let mut result = (COL & this) * rhs[0] as u64;
        result ^= (COL & (this >> 1)) * rhs[1] as u64;
        result ^= (COL & (this >> 2)) * rhs[2] as u64;
        result ^= (COL & (this >> 3)) * rhs[3] as u64;
        result ^= (COL & (this >> 4)) * rhs[4] as u64;
        result ^= (COL & (this >> 5)) * rhs[5] as u64;
        result ^= (COL & (this >> 6)) * rhs[6] as u64;
        result ^= (COL & (this >> 7)) * rhs[7] as u64;

        result.to_ne_bytes()
    }

    fn map<F>(&self, mut f: F) -> Self
    where
        F: FnMut(Self::RowRepr) -> Self::RowRepr,
    {
        array::from_fn(|i| f(self[i]))
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

        macro_rules! delta_swap {
            ($mask:expr, $shift:literal) => {
                let t = ((res >> $shift) ^ res) & $mask;
                res = (res ^ t) ^ (t << $shift);
            };
        }

        delta_swap!(0x00AA00AA00AA00AA, 7);
        delta_swap!(0x0000CCCC0000CCCC, 14);
        delta_swap!(0x00000000F0F0F0F0, 28);

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

    fn zip<F>(&self, rhs: &Self, mut f: F) -> Self
    where
        F: FnMut(Self::RowRepr, Self::RowRepr) -> Self::RowRepr,
        Self: Copy,
    {
        array::from_fn(|i| f(self[i], rhs[i]))
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

    fn map<F>(&self, mut f: F) -> Self
    where
        F: FnMut(Self::RowRepr) -> Self::RowRepr,
    {
        array::from_fn(|i| f(self[i]))
    }

    fn matmul(mut self, rhs: Self) -> Self {
        // Read the matrix into four 64-bit integers, each containing four rows of the matrix.
        let this = unsafe { self.as_mut_ptr().cast::<[u64; 4]>().read_unaligned() };
        let mut res: [u64; 4] = [0; 4];

        const COL: u64 = 0x0001000100010001;

        (0..16).for_each(|k| {
            let row = rhs[k] as u64;
            res[0] ^= (COL & (this[0]) >> k) * row;
            res[1] ^= (COL & (this[1]) >> k) * row;
            res[2] ^= (COL & (this[2]) >> k) * row;
            res[3] ^= (COL & (this[3]) >> k) * row;
        });

        let ptr: *mut Self = res.as_mut_ptr().cast();
        unsafe { ptr.read() }

        // The following is an alternative implementation using the four russians algorithm.

        // const STRIPE_BITS: usize = 4;
        // const SUM_TABLE_LEN: usize = 1 << STRIPE_BITS;
        // const STRIPE_INDEX: [usize; SUM_TABLE_LEN] = stripe_index();
        // const ROW_SUM_INDEX: [usize; SUM_TABLE_LEN] = row_sum_index();

        // let mut sums = [0; SUM_TABLE_LEN];
        // let mut result = [0; Self::SIZE];
        // let mut mask = (1 << STRIPE_BITS) - 1;
        // let mut shift = 0;
        // while mask != 0 {
        //     let range = shift..(shift + STRIPE_BITS).min(Self::SIZE);
        //     let stripe = &rhs[range];
        //     sums[0] = 0;
        //     for i in 1..SUM_TABLE_LEN {
        //         let j = STRIPE_INDEX[i];
        //         sums[i] = sums[i - 1] ^ stripe[j];
        //     }

        //     for (i, row) in self.iter().enumerate() {
        //         let prefix = ((row & mask) >> shift) as usize;
        //         let j = ROW_SUM_INDEX[prefix];
        //         result[i] ^= sums[j];
        //     }
        //     mask <<= STRIPE_BITS;
        //     shift += STRIPE_BITS;
        // }
        // result
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
        // Read the matrix into four 64-bit integers, each containing four rows of the matrix.
        let ptr: *mut [u64; 4] = self.as_mut_ptr().cast();
        let mut res = unsafe { ptr.read_unaligned() };

        macro_rules! delta_exchange {
            ($a:literal, $b:literal, $mask:expr, $shift:literal) => {
                let t = ((res[$b] >> $shift) ^ res[$a]) & $mask;
                res[$a] ^= t;
                res[$b] ^= t << $shift;
            };
        }

        macro_rules! delta_swap {
            ($i:expr, $mask:expr, $shift:literal) => {
                let t = ((res[$i] >> $shift) ^ res[$i]) & $mask;
                res[$i] = (res[$i] ^ t) ^ (t << $shift);
            };
        }

        // The strategy is to first swap bits between `a`, `b`, `c`, and `d` so that each contains
        // the bits of corresponding block of the transposed matrix, but not in the correct
        // order. After this, the bits are permuted to get the correct order. The following performs
        // the first step:

        const MASK0: u64 = 0x00FF00FF00FF00FF;
        const MASK1: u64 = 0x0F0F0F0F0F0F0F0F;
        const MASK2: u64 = 0x0000AAAA0000AAAA;
        const MASK3: u64 = 0x00000000CCCCCCCC;

        delta_exchange!(2, 0, MASK0, 8);
        delta_exchange!(3, 1, MASK0, 8);
        delta_exchange!(1, 0, MASK1, 4);
        delta_exchange!(3, 2, MASK1, 4);

        // The following code performs the second step.
        delta_swap!(0, MASK2, 15);
        delta_swap!(1, MASK2, 15);
        delta_swap!(2, MASK2, 15);
        delta_swap!(3, MASK2, 15);
        delta_swap!(0, MASK3, 30);
        delta_swap!(1, MASK3, 30);
        delta_swap!(2, MASK3, 30);
        delta_swap!(3, MASK3, 30);

        // Read the result back into the matrix.
        unsafe { ptr.write_unaligned(res) };
    }

    fn zip<F>(&self, rhs: &Self, mut f: F) -> Self
    where
        F: FnMut(Self::RowRepr, Self::RowRepr) -> Self::RowRepr,
        Self: Copy,
    {
        array::from_fn(|i| f(self[i], rhs[i]))
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

    fn map<F>(&self, mut f: F) -> Self
    where
        F: FnMut(Self::RowRepr) -> Self::RowRepr,
    {
        array::from_fn(|i| f(self[i]))
    }

    fn matmul(mut self, rhs: Self) -> Self {
        // Read the matrix into four 64-bit integers, each containing four rows of the matrix.
        let this = unsafe { self.as_mut_ptr().cast::<[u64; 16]>().read_unaligned() };
        let mut res: [u64; 16] = [0; 16];

        const COL: u64 = 0x0000000100000001;

        (0..32).for_each(|k| {
            let row = rhs[k] as u64;
            res[0] ^= (COL & (this[0]) >> k) * row;
            res[1] ^= (COL & (this[1]) >> k) * row;
            res[2] ^= (COL & (this[2]) >> k) * row;
            res[3] ^= (COL & (this[3]) >> k) * row;
            res[4] ^= (COL & (this[4]) >> k) * row;
            res[5] ^= (COL & (this[5]) >> k) * row;
            res[6] ^= (COL & (this[6]) >> k) * row;
            res[7] ^= (COL & (this[7]) >> k) * row;
            res[8] ^= (COL & (this[8]) >> k) * row;
            res[9] ^= (COL & (this[9]) >> k) * row;
            res[10] ^= (COL & (this[10]) >> k) * row;
            res[11] ^= (COL & (this[11]) >> k) * row;
            res[12] ^= (COL & (this[12]) >> k) * row;
            res[13] ^= (COL & (this[13]) >> k) * row;
            res[14] ^= (COL & (this[14]) >> k) * row;
            res[15] ^= (COL & (this[15]) >> k) * row;
        });

        let ptr: *mut Self = res.as_mut_ptr().cast();
        unsafe { ptr.read() }
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
        // Read the matrix into 8 128-bit integers, each containing four rows of the matrix.
        let ptr: *mut [u64; 16] = self.as_mut_ptr().cast();
        let mut res = unsafe { ptr.read_unaligned() };

        macro_rules! delta_exchange {
            ($a:expr, $b:expr, $mask:expr, $shift:literal) => {
                let t = ((res[$b] >> $shift) ^ res[$a]) & $mask;
                res[$a] ^= t;
                res[$b] ^= t << $shift;
            };
        }

        macro_rules! delta_swap {
            ($i:expr, $mask:expr, $shift:literal) => {
                let t = ((res[$i] >> $shift) ^ res[$i]) & $mask;
                res[$i] = (res[$i] ^ t) ^ (t << $shift);
            };
        }

        const MASK0: u64 = 0x0000FFFF0000FFFF;
        const MASK1: u64 = 0x00FF00FF00FF00FF;
        const MASK2: u64 = 0x0F0F0F0F0F0F0F0F;
        const MASK3: u64 = 0x3333333333333333;
        const MASK4: u64 = 0x00000000AAAAAAAA;

        delta_exchange!(8, 0, MASK0, 16);
        delta_exchange!(9, 1, MASK0, 16);
        delta_exchange!(10, 2, MASK0, 16);
        delta_exchange!(11, 3, MASK0, 16);
        delta_exchange!(12, 4, MASK0, 16);
        delta_exchange!(13, 5, MASK0, 16);
        delta_exchange!(14, 6, MASK0, 16);
        delta_exchange!(15, 7, MASK0, 16);

        delta_exchange!(4, 0, MASK1, 8);
        delta_exchange!(5, 1, MASK1, 8);
        delta_exchange!(6, 2, MASK1, 8);
        delta_exchange!(7, 3, MASK1, 8);
        delta_exchange!(12, 8, MASK1, 8);
        delta_exchange!(13, 9, MASK1, 8);
        delta_exchange!(14, 10, MASK1, 8);
        delta_exchange!(15, 11, MASK1, 8);

        delta_exchange!(2, 0, MASK2, 4);
        delta_exchange!(3, 1, MASK2, 4);
        delta_exchange!(6, 4, MASK2, 4);
        delta_exchange!(7, 5, MASK2, 4);
        delta_exchange!(10, 8, MASK2, 4);
        delta_exchange!(11, 9, MASK2, 4);
        delta_exchange!(14, 12, MASK2, 4);
        delta_exchange!(15, 13, MASK2, 4);

        delta_exchange!(1, 0, MASK3, 2);
        delta_exchange!(3, 2, MASK3, 2);
        delta_exchange!(5, 4, MASK3, 2);
        delta_exchange!(7, 6, MASK3, 2);
        delta_exchange!(9, 8, MASK3, 2);
        delta_exchange!(11, 10, MASK3, 2);
        delta_exchange!(13, 12, MASK3, 2);
        delta_exchange!(15, 14, MASK3, 2);

        (0..16).for_each(|k| {
            delta_swap!(k, MASK4, 31);
        });

        // Read the result back into the matrix.
        unsafe { ptr.write_unaligned(res) };
    }

    fn zip<F>(&self, rhs: &Self, mut f: F) -> Self
    where
        F: FnMut(Self::RowRepr, Self::RowRepr) -> Self::RowRepr,
        Self: Copy,
    {
        array::from_fn(|i| f(self[i], rhs[i]))
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

    fn map<F>(&self, mut f: F) -> Self
    where
        F: FnMut(Self::RowRepr) -> Self::RowRepr,
    {
        array::from_fn(|i| f(self[i]))
    }

    fn matmul(self, rhs: Self) -> Self {
        // The commented code below is an implementation of the Strassen algorithm for 64x64 bit
        // matrices. In it's current form it is much slower than the four russians
        // algorithm, but it is included for reference.

        // let a00: [u32; 32] = array::from_fn(|i| self[i]  as u32);
        // let a01: [u32; 32] = array::from_fn(|i| (self[i] >> 32) as u32);
        // let a10: [u32; 32] = array::from_fn(|i| (self[i + 32] ) as u32);
        // let a11: [u32; 32] = array::from_fn(|i| (self[i + 32] >> 32) as u32);

        // let b00: [u32; 32] = array::from_fn(|i| rhs[i]  as u32);
        // let b01: [u32; 32] = array::from_fn(|i| (rhs[i] >> 32) as u32);
        // let b10: [u32; 32] = array::from_fn(|i| rhs[i + 32]  as u32);
        // let b11: [u32; 32] = array::from_fn(|i| (rhs[i + 32] >> 32) as u32);

        // let m0 = a00.xor(a11).matmul(b00.xor(b11));
        // let m1 = a10.xor(a11).matmul(b00);
        // let m2 = a00.matmul(b01.xor(b11));
        // let m3 = a11.matmul(b10.xor(b00));
        // let m4 = a00.xor(a01).matmul(b11);
        // let m5 = a10.xor(a00).matmul(b00.xor(b01));
        // let m6 = a01.xor(a11).matmul(b10.xor(b11));

        // let c00 = m0.xor(m3).xor(m4).xor(m6);
        // let c01 = m2.xor(m4);
        // let c10 = m1.xor(m3);
        // let c11 = m0.xor(m1).xor(m2).xor(m5);

        // let mut result = Self::ZERO;
        // for i in 0..32 {
        //     result[i] = ((c01[i] as u64) << 32) | c00[i] as u64;
        //     result[i + 32] = ((c11[i] as u64) << 32) | c10[i] as u64;
        // }
        // result

        const STRIPE_BITS: usize = 5;
        const SUM_TABLE_LEN: usize = 1 << STRIPE_BITS;
        const STRIPE_INDEX: [usize; SUM_TABLE_LEN] = gray_code_diffs();
        const ROW_SUM_INDEX: [usize; SUM_TABLE_LEN] = gray_code_index();

        let mut sums = [0; SUM_TABLE_LEN];
        let mut result = [0; Self::SIZE];
        let mut mask = (1 << STRIPE_BITS) - 1;
        let mut shift = 0;
        for _ in 0..(Self::SIZE / STRIPE_BITS) {
            let range = shift..(shift + STRIPE_BITS).min(Self::SIZE);
            let stripe = &rhs[range];
            for i in 1..SUM_TABLE_LEN {
                let j = STRIPE_INDEX[i];
                sums[i] = sums[i-1] ^ stripe.get(j).unwrap_or(&0);
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
        macro_rules! delta_exchange {
            ($a:literal, $b:literal, $mask:expr, $shift:literal) => {
                let t = ((self[$b] >> $shift) ^ self[$a]) & $mask;
                self[$a] ^= t;
                self[$b] ^= t << $shift;
            };
        }

        let mask = 0xFFFFFFFF;
        delta_exchange!(32, 0, mask, 32);
        delta_exchange!(33, 1, mask, 32);
        delta_exchange!(34, 2, mask, 32);
        delta_exchange!(35, 3, mask, 32);
        delta_exchange!(36, 4, mask, 32);
        delta_exchange!(37, 5, mask, 32);
        delta_exchange!(38, 6, mask, 32);
        delta_exchange!(39, 7, mask, 32);
        delta_exchange!(40, 8, mask, 32);
        delta_exchange!(41, 9, mask, 32);
        delta_exchange!(42, 10, mask, 32);
        delta_exchange!(43, 11, mask, 32);
        delta_exchange!(44, 12, mask, 32);
        delta_exchange!(45, 13, mask, 32);
        delta_exchange!(46, 14, mask, 32);
        delta_exchange!(47, 15, mask, 32);
        delta_exchange!(48, 16, mask, 32);
        delta_exchange!(49, 17, mask, 32);
        delta_exchange!(50, 18, mask, 32);
        delta_exchange!(51, 19, mask, 32);
        delta_exchange!(52, 20, mask, 32);
        delta_exchange!(53, 21, mask, 32);
        delta_exchange!(54, 22, mask, 32);
        delta_exchange!(55, 23, mask, 32);
        delta_exchange!(56, 24, mask, 32);
        delta_exchange!(57, 25, mask, 32);
        delta_exchange!(58, 26, mask, 32);
        delta_exchange!(59, 27, mask, 32);
        delta_exchange!(60, 28, mask, 32);
        delta_exchange!(61, 29, mask, 32);
        delta_exchange!(62, 30, mask, 32);
        delta_exchange!(63, 31, mask, 32);

        let mask = 0xFFFF0000FFFF;
        delta_exchange!(16, 0, mask, 16);
        delta_exchange!(17, 1, mask, 16);
        delta_exchange!(18, 2, mask, 16);
        delta_exchange!(19, 3, mask, 16);
        delta_exchange!(20, 4, mask, 16);
        delta_exchange!(21, 5, mask, 16);
        delta_exchange!(22, 6, mask, 16);
        delta_exchange!(23, 7, mask, 16);
        delta_exchange!(24, 8, mask, 16);
        delta_exchange!(25, 9, mask, 16);
        delta_exchange!(26, 10, mask, 16);
        delta_exchange!(27, 11, mask, 16);
        delta_exchange!(28, 12, mask, 16);
        delta_exchange!(29, 13, mask, 16);
        delta_exchange!(30, 14, mask, 16);
        delta_exchange!(31, 15, mask, 16);
        delta_exchange!(48, 32, mask, 16);
        delta_exchange!(49, 33, mask, 16);
        delta_exchange!(50, 34, mask, 16);
        delta_exchange!(51, 35, mask, 16);
        delta_exchange!(52, 36, mask, 16);
        delta_exchange!(53, 37, mask, 16);
        delta_exchange!(54, 38, mask, 16);
        delta_exchange!(55, 39, mask, 16);
        delta_exchange!(56, 40, mask, 16);
        delta_exchange!(57, 41, mask, 16);
        delta_exchange!(58, 42, mask, 16);
        delta_exchange!(59, 43, mask, 16);
        delta_exchange!(60, 44, mask, 16);
        delta_exchange!(61, 45, mask, 16);
        delta_exchange!(62, 46, mask, 16);
        delta_exchange!(63, 47, mask, 16);

        let mask = 0xFF00FF00FF00FF;
        delta_exchange!(8, 0, mask, 8);
        delta_exchange!(9, 1, mask, 8);
        delta_exchange!(10, 2, mask, 8);
        delta_exchange!(11, 3, mask, 8);
        delta_exchange!(12, 4, mask, 8);
        delta_exchange!(13, 5, mask, 8);
        delta_exchange!(14, 6, mask, 8);
        delta_exchange!(15, 7, mask, 8);
        delta_exchange!(24, 16, mask, 8);
        delta_exchange!(25, 17, mask, 8);
        delta_exchange!(26, 18, mask, 8);
        delta_exchange!(27, 19, mask, 8);
        delta_exchange!(28, 20, mask, 8);
        delta_exchange!(29, 21, mask, 8);
        delta_exchange!(30, 22, mask, 8);
        delta_exchange!(31, 23, mask, 8);
        delta_exchange!(40, 32, mask, 8);
        delta_exchange!(41, 33, mask, 8);
        delta_exchange!(42, 34, mask, 8);
        delta_exchange!(43, 35, mask, 8);
        delta_exchange!(44, 36, mask, 8);
        delta_exchange!(45, 37, mask, 8);
        delta_exchange!(46, 38, mask, 8);
        delta_exchange!(47, 39, mask, 8);
        delta_exchange!(56, 48, mask, 8);
        delta_exchange!(57, 49, mask, 8);
        delta_exchange!(58, 50, mask, 8);
        delta_exchange!(59, 51, mask, 8);
        delta_exchange!(60, 52, mask, 8);
        delta_exchange!(61, 53, mask, 8);
        delta_exchange!(62, 54, mask, 8);
        delta_exchange!(63, 55, mask, 8);

        let mask = 0xF0F0F0F0F0F0F0F;
        delta_exchange!(4, 0, mask, 4);
        delta_exchange!(5, 1, mask, 4);
        delta_exchange!(6, 2, mask, 4);
        delta_exchange!(7, 3, mask, 4);
        delta_exchange!(12, 8, mask, 4);
        delta_exchange!(13, 9, mask, 4);
        delta_exchange!(14, 10, mask, 4);
        delta_exchange!(15, 11, mask, 4);
        delta_exchange!(20, 16, mask, 4);
        delta_exchange!(21, 17, mask, 4);
        delta_exchange!(22, 18, mask, 4);
        delta_exchange!(23, 19, mask, 4);
        delta_exchange!(28, 24, mask, 4);
        delta_exchange!(29, 25, mask, 4);
        delta_exchange!(30, 26, mask, 4);
        delta_exchange!(31, 27, mask, 4);
        delta_exchange!(36, 32, mask, 4);
        delta_exchange!(37, 33, mask, 4);
        delta_exchange!(38, 34, mask, 4);
        delta_exchange!(39, 35, mask, 4);
        delta_exchange!(44, 40, mask, 4);
        delta_exchange!(45, 41, mask, 4);
        delta_exchange!(46, 42, mask, 4);
        delta_exchange!(47, 43, mask, 4);
        delta_exchange!(52, 48, mask, 4);
        delta_exchange!(53, 49, mask, 4);
        delta_exchange!(54, 50, mask, 4);
        delta_exchange!(55, 51, mask, 4);
        delta_exchange!(60, 56, mask, 4);
        delta_exchange!(61, 57, mask, 4);
        delta_exchange!(62, 58, mask, 4);
        delta_exchange!(63, 59, mask, 4);

        let mask = 0x3333333333333333;
        delta_exchange!(2, 0, mask, 2);
        delta_exchange!(3, 1, mask, 2);
        delta_exchange!(6, 4, mask, 2);
        delta_exchange!(7, 5, mask, 2);
        delta_exchange!(10, 8, mask, 2);
        delta_exchange!(11, 9, mask, 2);
        delta_exchange!(14, 12, mask, 2);
        delta_exchange!(15, 13, mask, 2);
        delta_exchange!(18, 16, mask, 2);
        delta_exchange!(19, 17, mask, 2);
        delta_exchange!(22, 20, mask, 2);
        delta_exchange!(23, 21, mask, 2);
        delta_exchange!(26, 24, mask, 2);
        delta_exchange!(27, 25, mask, 2);
        delta_exchange!(30, 28, mask, 2);
        delta_exchange!(31, 29, mask, 2);
        delta_exchange!(34, 32, mask, 2);
        delta_exchange!(35, 33, mask, 2);
        delta_exchange!(38, 36, mask, 2);
        delta_exchange!(39, 37, mask, 2);
        delta_exchange!(42, 40, mask, 2);
        delta_exchange!(43, 41, mask, 2);
        delta_exchange!(46, 44, mask, 2);
        delta_exchange!(47, 45, mask, 2);
        delta_exchange!(50, 48, mask, 2);
        delta_exchange!(51, 49, mask, 2);
        delta_exchange!(54, 52, mask, 2);
        delta_exchange!(55, 53, mask, 2);
        delta_exchange!(58, 56, mask, 2);
        delta_exchange!(59, 57, mask, 2);
        delta_exchange!(62, 60, mask, 2);
        delta_exchange!(63, 61, mask, 2);

        let mask = 0x5555555555555555;
        delta_exchange!(1, 0, mask, 1);
        delta_exchange!(3, 2, mask, 1);
        delta_exchange!(5, 4, mask, 1);
        delta_exchange!(7, 6, mask, 1);
        delta_exchange!(9, 8, mask, 1);
        delta_exchange!(11, 10, mask, 1);
        delta_exchange!(13, 12, mask, 1);
        delta_exchange!(15, 14, mask, 1);
        delta_exchange!(17, 16, mask, 1);
        delta_exchange!(19, 18, mask, 1);
        delta_exchange!(21, 20, mask, 1);
        delta_exchange!(23, 22, mask, 1);
        delta_exchange!(25, 24, mask, 1);
        delta_exchange!(27, 26, mask, 1);
        delta_exchange!(29, 28, mask, 1);
        delta_exchange!(31, 30, mask, 1);
        delta_exchange!(33, 32, mask, 1);
        delta_exchange!(35, 34, mask, 1);
        delta_exchange!(37, 36, mask, 1);
        delta_exchange!(39, 38, mask, 1);
        delta_exchange!(41, 40, mask, 1);
        delta_exchange!(43, 42, mask, 1);
        delta_exchange!(45, 44, mask, 1);
        delta_exchange!(47, 46, mask, 1);
        delta_exchange!(49, 48, mask, 1);
        delta_exchange!(51, 50, mask, 1);
        delta_exchange!(53, 52, mask, 1);
        delta_exchange!(55, 54, mask, 1);
        delta_exchange!(57, 56, mask, 1);
        delta_exchange!(59, 58, mask, 1);
        delta_exchange!(61, 60, mask, 1);
        delta_exchange!(63, 62, mask, 1);
    }

    fn zip<F>(&self, rhs: &Self, mut f: F) -> Self
    where
        F: FnMut(Self::RowRepr, Self::RowRepr) -> Self::RowRepr,
    {
        array::from_fn(|i| f(self[i], rhs[i]))
    }
}

/// Computes the gray code of a number `i`.
const fn gray_code(i: usize) -> usize {
    i ^ (i >> 1)
}

/// Computes an index into the stripe for constructing the row sum table in the four russians
/// algorithm. The element at `index` is the index of the row that should be `XOR`ed with the
/// previous row sum.
const fn gray_code_diffs<const N: usize>() -> [usize; N] {
    let mut diffs = [0; N];
    let mut prev = 0;
    let mut i = 1;
    while i < N {
        let code = gray_code(i);
        diffs[i] = (usize::BITS - (code ^ prev).leading_zeros() - 1) as usize;
        // diffs[i] = (code ^ prev).ilog2() as usize;
        prev = code;
        i += 1
    }
    diffs
}

/// Computes an index into the row sum table in the four russians algorithm. The element at `prefix`
/// is the index of the row sum for that prefix.
const fn gray_code_index<const N: usize>() -> [usize; N] {
    let mut codes = [0; N];
    let mut i = 1;
    while i < N {
        codes[gray_code(i)] = i;
        i += 1;
    }
    codes
}
