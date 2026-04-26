//! Dynamic rectangular bit matrices stored as 64 by 64 bit blocks.
//!
//! [`BitMatrix`] stores logical `rows` by `cols` bits in block-row-major order. Each block is a
//! `[u64; 64]`: the array index selects a row inside the block and the bits of the `u64` select
//! columns inside that block. Matrix dimensions that are not multiples of 64 are padded internally;
//! padding bits are kept zero and are not visible through the public API.

use std::fmt::{self, Display, Formatter};

use crate::rng::C64Rng as Rng;

#[cfg(test)]
mod tests;

const BLOCK_BITS: usize = 64;
const BLOCK_MASK: usize = BLOCK_BITS - 1;
const M4RI_INVERSE_THRESHOLD: usize = 32;
const M4RI_MAX_STRIPE_BITS: usize = 8;

/// A dynamic bit matrix with packed logical-bit operations.
///
/// Bits are indexed by logical `(row, col)` coordinates, with each stored `u64` row word using
/// least-significant-bit-first column numbering inside a 64-bit block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BitMatrix {
    /// The bits of the matrix, stored in block-row-major order. Each block is a `[u64; 64]`
    /// representing 64 rows and 64 columns of bits.
    blocks: Box<[[u64; BLOCK_BITS]]>,
    /// The number of logical rows in the matrix.
    rows: usize,
    /// The number of logical columns in the matrix.
    cols: usize,
    /// The number of block rows in the matrix, equal to `blocks.len() / block_cols`.
    block_rows: usize,
    /// The number of block columns in the matrix, equal to `blocks[0].len()`.
    block_cols: usize,
}

impl BitMatrix {
    /// Returns the element-wise bitwise AND of `self` and `rhs`.
    ///
    /// Both matrices must have the same logical dimensions.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let lhs = BitMatrix::from_rows([[0b01], [0b10]], 2);
    /// let rhs = BitMatrix::from_rows([[0b11], [0b00]], 2);
    ///
    /// let and = lhs.and(&rhs);
    /// assert_eq!(and.get(0, 0), 1);
    /// assert_eq!(and.get(0, 1), 0);
    /// assert_eq!(and.get(1, 1), 0);
    /// ```
    pub fn and(&self, rhs: &Self) -> Self {
        self.zip(rhs, |lhs, rhs| lhs & rhs)
    }

    /// Builds `[self | I]` for square-matrix inversion.
    fn augmented_with_identity(&self) -> Self {
        assert_eq!(self.rows(), self.cols());

        let n = self.rows();
        let augmented_cols = n
            .checked_mul(2)
            .expect("augmented matrix column count overflow");
        let mut augmented = Self::new(n, augmented_cols);
        for row in 0..n {
            for word_col in 0..self.block_cols() {
                augmented.set_row_word(row, word_col, self.row_word(row, word_col));
            }
            augmented.set(row, n + row, 1);
        }

        augmented
    }

    /// Returns the packed 64 by 64 block at the given block coordinates.
    ///
    /// The returned block exposes the internal storage format, where each element of the array is
    /// a packed row word for that block.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let mut matrix = BitMatrix::new(70, 70);
    /// matrix.set(0, 0, 1);
    /// matrix.set(65, 65, 1);
    ///
    /// let top_left = matrix.block(0, 0).unwrap();
    /// let bottom_right = matrix.block(1, 1).unwrap();
    ///
    /// assert_eq!(top_left[0] & 1, 1);
    /// assert_eq!(bottom_right[1] & (1 << 1), 1 << 1);
    /// ```
    pub fn block(&self, block_row: usize, block_col: usize) -> Option<&[u64; BLOCK_BITS]> {
        if block_row < self.block_rows && block_col < self.block_cols {
            Some(&self.blocks[self.block_index(block_row, block_col)])
        } else {
            None
        }
    }

    /// Returns the number of stored block columns.
    fn block_cols(&self) -> usize {
        self.block_cols
    }

    /// Returns the flat storage index for a block at `(block_row, block_col)`.
    fn block_index(&self, block_row: usize, block_col: usize) -> usize {
        debug_assert!(block_row < self.block_rows);
        debug_assert!(block_col < self.block_cols);
        block_row * self.block_cols + block_col
    }

    /// Returns a mutable reference to the packed block at the given block coordinates.
    fn block_mut(&mut self, block_row: usize, block_col: usize) -> Option<&mut [u64; BLOCK_BITS]> {
        if block_row < self.block_rows && block_col < self.block_cols {
            let index = self.block_index(block_row, block_col);
            Some(&mut self.blocks[index])
        } else {
            None
        }
    }

    /// Returns the number of stored block rows.
    fn block_rows(&self) -> usize {
        self.block_rows
    }

    /// Returns all packed blocks in block-row-major order.
    fn blocks(&self) -> &[[u64; BLOCK_BITS]] {
        &self.blocks
    }

    /// Returns all packed blocks in block-row-major order for in-place mutation.
    fn blocks_mut(&mut self) -> &mut [[u64; BLOCK_BITS]] {
        &mut self.blocks
    }

    /// Builds a table of all XOR combinations of the pivot rows for one stripe.
    fn build_m4ri_row_table(&self, stripe_start: usize, stripe_len: usize) -> Vec<Vec<u64>> {
        assert!(stripe_len <= M4RI_MAX_STRIPE_BITS);

        let table_len = 1_usize << stripe_len;
        let start_word = stripe_start / BLOCK_BITS;
        let start_bit = stripe_start & BLOCK_MASK;
        let suffix_words = self.block_cols - start_word;
        let mut table = vec![vec![0; suffix_words]; table_len];
        let mut current = vec![0; suffix_words];
        let mut previous_code = 0;

        for index in 1..table_len {
            let code = gray_code(index);
            let changed = (code ^ previous_code).trailing_zeros() as usize;
            let pivot_row = stripe_start + changed;

            for (offset, current_word) in current.iter_mut().enumerate() {
                let mut word = self.row_word(pivot_row, start_word + offset);
                if offset == 0 && start_bit != 0 {
                    word &= !low_bits_mask(start_bit);
                }
                *current_word ^= word;
            }

            table[code].clone_from(&current);
            previous_code = code;
        }

        table
    }

    /// Clears any bits that live outside the logical matrix dimensions.
    fn clear_padding_bits(&mut self) {
        if self.is_empty() {
            return;
        }

        // Only the logical rows and columns are part of the public matrix. Any padding introduced
        // by the 64 by 64 block layout must be cleared so operations like equality, row export,
        // and chained transforms never observe stale bits outside the logical dimensions.
        for block_row in 0..self.block_rows {
            let valid_rows = self.valid_rows_in_block(block_row);
            for block_col in 0..self.block_cols {
                let valid_cols = self.valid_cols_in_block(block_col);
                let col_mask = low_bits_mask(valid_cols);
                let index = self.block_index(block_row, block_col);
                let block = &mut self.blocks[index];

                for row in &mut block[..valid_rows] {
                    *row &= col_mask;
                }
                for row in &mut block[valid_rows..] {
                    *row = 0;
                }
            }
        }
    }

    /// Returns the number of logical columns in the matrix.
    pub fn cols(&self) -> usize {
        self.cols
    }

    /// Counts the number of logical one bits in the matrix.
    ///
    /// Padding bits in the packed representation are ignored.
    pub fn count_ones(&self) -> u64 {
        let mut count = 0;
        for block_row in 0..self.block_rows() {
            let valid_rows = if block_row + 1 == self.block_rows() {
                self.rows() - block_row * 64
            } else {
                64
            };
            for block_col in 0..self.block_cols() {
                let valid_cols = if block_col + 1 == self.block_cols() {
                    self.cols() - block_col * 64
                } else {
                    64
                };
                let mask = low_bits_mask(valid_cols);
                let block = self
                    .block(block_row, block_col)
                    .expect("valid block coordinates");
                count += block[..valid_rows]
                    .iter()
                    .map(|row| (row & mask).count_ones() as u64)
                    .sum::<u64>();
            }
        }
        count
    }

    /// Counts the number of logical zero bits in the matrix.
    pub fn count_zeros(&self) -> u64 {
        (self.rows() as u64 * self.cols() as u64) - self.count_ones()
    }

    /// Returns the logical shape of the matrix as `(rows, cols)`.
    pub fn dimensions(&self) -> (usize, usize) {
        (self.rows, self.cols)
    }

    /// Establishes identity rows for one M4RI stripe.
    fn establish_m4ri_pivots(&mut self, stripe_start: usize, stripe_end: usize) -> Option<()> {
        for pivot in stripe_start..stripe_end {
            for row in pivot..self.rows {
                for prev in stripe_start..pivot {
                    if self.get(row, prev) == 1 {
                        self.xor_rows_from(row, prev, prev);
                    }
                }
            }

            let pivot_row = (pivot..self.rows).find(|&row| self.get(row, pivot) == 1)?;
            self.swap_rows(pivot, pivot_row);

            for row in stripe_start..pivot {
                if self.get(row, pivot) == 1 {
                    self.xor_rows_from(row, pivot, pivot);
                }
            }
        }

        Some(())
    }

    /// Creates a matrix with the given dimensions where each logical bit is determined by `f`.
    ///
    /// The closure `f` is called once for each logical bit coordinate `(row, col)`, and the
    /// returned value is treated as a bit where `true` is `1` and `false` is `0`.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let matrix = BitMatrix::from_fn(3, 3, |row, col| row == col);
    /// let identity = BitMatrix::identity(3);
    ///
    /// assert_eq!(matrix, identity);
    /// ```
    pub fn from_fn<F>(rows: usize, cols: usize, mut f: F) -> Self
    where
        F: FnMut(usize, usize) -> bool,
    {
        let mut matrix = Self::new(rows, cols);
        for row in 0..rows {
            for col in 0..cols {
                matrix.set(row, col, f(row, col) as u8);
            }
        }
        matrix
    }

    /// Builds a matrix from packed row words.
    ///
    /// Each slice in `rows` must have length at least `cols.div_ceil(64)`. Only the low `cols`
    /// bits of each row word are used; the rest are ignored. The number of rows is determined by
    /// the number of slices in `rows`.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let matrix = BitMatrix::from_rows([[0b1011], [0b0101]], 4);
    ///
    /// assert_eq!(matrix.dimensions(), (2, 4));
    /// assert_eq!(matrix.get(0, 0), 1);
    /// assert_eq!(matrix.get(0, 2), 0);
    /// assert_eq!(matrix.get(1, 2), 1);
    /// ```
    pub fn from_rows<I, R>(rows: I, cols: usize) -> Self
    where
        I: IntoIterator<Item = R>,
        R: AsRef<[u64]>,
    {
        let words_per_row = blocks_for(cols);
        let rows = Vec::from_iter(rows);
        for row_words in &rows {
            assert!(row_words.as_ref().len() >= words_per_row);
        }

        let mut matrix = Self::new(rows.len(), cols);
        for (row, row_words) in rows.into_iter().enumerate() {
            for (word_col, &word) in row_words.as_ref().iter().take(words_per_row).enumerate() {
                let block_col = word_col;
                let block_row = row / BLOCK_BITS;
                let row_in_block = row & BLOCK_MASK;
                let index = matrix.block_index(block_row, block_col);
                matrix.blocks[index][row_in_block] = word;
            }
        }
        matrix.clear_padding_bits();
        matrix
    }

    /// Returns `1` if the bit at `row`, `col` is set and `0` otherwise.
    ///
    /// Panics if `row` or `col` is outside the logical dimensions.
    pub fn get(&self, row: usize, col: usize) -> u8 {
        assert!(row < self.rows);
        assert!(col < self.cols);

        let (block_row, block_col, row_in_block, col_in_block) = bit_coords(row, col);
        let index = self.block_index(block_row, block_col);
        ((self.blocks[index][row_in_block] >> col_in_block) & 1) as u8
    }

    /// Creates an `n` by `n` identity matrix.
    ///
    /// The diagonal bits are set to `1` and every other logical bit is `0`.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let identity = BitMatrix::identity(3);
    ///
    /// assert_eq!(identity.get(0, 0), 1);
    /// assert_eq!(identity.get(1, 1), 1);
    /// assert_eq!(identity.get(2, 2), 1);
    /// assert_eq!(identity.get(0, 2), 0);
    /// ```
    pub fn identity(n: usize) -> Self {
        let mut matrix = Self::new(n, n);
        for i in 0..n {
            matrix.set(i, i, 1);
        }
        matrix
    }

    /// Returns `true` if either logical dimension is zero.
    pub fn is_empty(&self) -> bool {
        self.rows == 0 || self.cols == 0
    }

    /// Applies `f` to every stored row word and returns the transformed matrix.
    ///
    /// The closure operates on packed `u64` row words, not on individual logical bits.
    /// Padding bits are cleared before the new matrix is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let matrix = BitMatrix::from_rows([[0b0001], [0b0100]], 4);
    ///
    /// let shifted = matrix.map(|row| row << 1);
    /// assert_eq!(shifted.get(0, 1), 1);
    /// assert_eq!(shifted.get(1, 3), 1);
    /// assert_eq!(shifted.get(0, 0), 0);
    /// ```
    pub fn map<F>(&self, f: F) -> Self
    where
        F: FnMut(u64) -> u64,
    {
        let mut result = self.clone();
        result.map_in_place(f);
        result
    }

    /// Applies `f` in-place to every stored row word.
    ///
    /// The closure operates on packed `u64` row words, and padding bits are cleared after the
    /// transformation so the logical shape remains valid.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let mut matrix = BitMatrix::from_rows([[0b0001]], 4);
    /// matrix.map_in_place(|row| row << 2);
    ///
    /// assert_eq!(matrix.get(0, 2), 1);
    /// assert_eq!(matrix.get(0, 0), 0);
    /// ```
    pub fn map_in_place<F>(&mut self, mut f: F)
    where
        F: FnMut(u64) -> u64,
    {
        for block in self.blocks_mut() {
            for row in block {
                *row = f(*row);
            }
        }
        self.clear_padding_bits();
    }

    /// Multiplies `self` by `rhs` over GF(2).
    ///
    /// The number of columns in `self` must equal the number of rows in `rhs`. Addition in the
    /// dot product is XOR and multiplication is AND.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let lhs = BitMatrix::from_rows([[0b101], [0b010]], 3);
    /// let rhs = BitMatrix::from_rows([[0b10], [0b00], [0b11]], 2);
    ///
    /// let product = lhs.matmul(&rhs);
    /// assert_eq!(product.dimensions(), (2, 2));
    /// assert_eq!(product.get(0, 0), 1);
    /// assert_eq!(product.get(0, 1), 0);
    /// assert_eq!(product.get(1, 0), 0);
    /// assert_eq!(product.get(1, 1), 0);
    /// ```
    pub fn matmul(&self, rhs: &Self) -> Self {
        assert_eq!(self.cols(), rhs.rows());

        let mut result = Self::new(self.rows(), rhs.cols());
        for block_row in 0..self.block_rows() {
            for block_col in 0..rhs.block_cols() {
                let mut accumulator = [0; 64];
                for shared_block in 0..self.block_cols() {
                    let lhs = self
                        .block(block_row, shared_block)
                        .expect("valid lhs block coordinates");
                    let rhs = rhs
                        .block(shared_block, block_col)
                        .expect("valid rhs block coordinates");
                    let product = block_matmul(lhs, rhs);
                    for (acc, prod) in accumulator.iter_mut().zip(&product) {
                        *acc ^= prod;
                    }
                }
                *result
                    .block_mut(block_row, block_col)
                    .expect("valid result block coordinates") = accumulator;
            }
        }

        result.clear_padding_bits();
        result
    }

    /// Multiplies `self` by `rhs` over the Boolean OR/AND semiring.
    ///
    /// The number of columns in `self` must equal the number of rows in `rhs`. Addition in the
    /// dot product is OR, so `1 + 1 = 1`, and multiplication is AND.
    ///
    /// This differs from [`BitMatrix::matmul`], which uses GF(2) addition and therefore treats
    /// `1 + 1 = 0`.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let lhs = BitMatrix::from_rows([[0b101]], 3);
    /// let rhs = BitMatrix::from_rows([[0b1], [0b0], [0b1]], 1);
    ///
    /// let product = lhs.matmul_or(&rhs);
    /// assert_eq!(product.dimensions(), (1, 1));
    /// assert_eq!(product.get(0, 0), 1);
    /// assert_eq!(lhs.matmul(&rhs).get(0, 0), 0);
    /// ```
    pub fn matmul_or(&self, rhs: &Self) -> Self {
        assert_eq!(self.cols(), rhs.rows());

        let mut result = Self::new(self.rows(), rhs.cols());
        for block_row in 0..self.block_rows() {
            for block_col in 0..rhs.block_cols() {
                let mut accumulator = [0; 64];
                for shared_block in 0..self.block_cols() {
                    let lhs = self
                        .block(block_row, shared_block)
                        .expect("valid lhs block coordinates");
                    let rhs = rhs
                        .block(shared_block, block_col)
                        .expect("valid rhs block coordinates");
                    let product = block_matmul_or(lhs, rhs);
                    for (acc, prod) in accumulator.iter_mut().zip(&product) {
                        *acc |= prod;
                    }
                }
                *result
                    .block_mut(block_row, block_col)
                    .expect("valid result block coordinates") = accumulator;
            }
        }

        result.clear_padding_bits();
        result
    }

    /// Replaces each logical bit with its bitwise NOT.
    pub fn negate(&mut self) {
        self.map_in_place(|row| !row);
    }

    /// Creates a zero-filled matrix with `rows` rows and `cols` columns.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let matrix = BitMatrix::new(2, 3);
    /// assert_eq!(matrix.dimensions(), (2, 3));
    /// assert_eq!(matrix.count_ones(), 0);
    /// ```
    pub fn new(rows: usize, cols: usize) -> Self {
        let block_rows = blocks_for(rows);
        let block_cols = blocks_for(cols);
        let blocks = vec![[0; BLOCK_BITS]; block_rows * block_cols].into_boxed_slice();

        Self {
            blocks,
            rows,
            cols,
            block_rows,
            block_cols,
        }
    }

    /// Returns a matrix where each logical bit is inverted.
    ///
    /// This is the non-mutating counterpart to [`BitMatrix::negate`].
    pub fn not(&self) -> Self {
        self.map(|row| !row)
    }

    /// Returns the element-wise bitwise OR of `self` and `rhs`.
    ///
    /// Both matrices must have the same logical dimensions.
    pub fn or(&self, rhs: &Self) -> Self {
        self.zip(rhs, |lhs, rhs| lhs | rhs)
    }

    /// Creates a matrix with pseudorandom logical bits.
    ///
    /// The exact bit pattern is not part of the public contract, but the returned matrix always
    /// has the requested dimensions and cleared padding bits.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let matrix = BitMatrix::random(8, 9);
    /// assert_eq!(matrix.dimensions(), (8, 9));
    /// assert_eq!(matrix.count_ones() + matrix.count_zeros(), 72);
    /// ```
    pub fn random(rows: usize, cols: usize) -> Self {
        let mut matrix = Self::new(rows, cols);
        let rng = Rng::with_seed(0);
        for block in matrix.blocks_mut() {
            for row in block {
                *row = rng.random();
            }
        }
        matrix.clear_padding_bits();
        matrix
    }

    /// Resizes the matrix in place.
    ///
    /// The top-left overlap with the old matrix is preserved. Any newly created rows or columns
    /// are filled with zero bits, and bits outside the new bounds are discarded.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let mut matrix = BitMatrix::from_rows([[0b01], [0b10]], 2);
    ///
    /// matrix.resize(3, 4);
    /// assert_eq!(matrix.get(0, 0), 1);
    /// assert_eq!(matrix.get(1, 1), 1);
    /// assert_eq!(matrix.get(2, 3), 0);
    /// ```
    pub fn resize(&mut self, rows: usize, cols: usize) {
        if self.rows == rows && self.cols == cols {
            return;
        }

        let copy_block_rows = self.block_rows.min(blocks_for(rows));
        let copy_block_cols = self.block_cols.min(blocks_for(cols));
        let mut resized = Self::new(rows, cols);

        for block_row in 0..copy_block_rows {
            for block_col in 0..copy_block_cols {
                let src_index = self.block_index(block_row, block_col);
                let dst_index = resized.block_index(block_row, block_col);
                resized.blocks[dst_index] = self.blocks[src_index];
            }
        }

        resized.clear_padding_bits();
        *self = resized;
    }

    /// Extracts the right half of an `n` by `2n` augmented matrix.
    fn right_half(&self) -> Self {
        assert_eq!(self.cols(), self.rows() * 2);

        let n = self.rows();
        let mut result = Self::new(n, n);
        for row in 0..n {
            for col in 0..n {
                result.set(row, col, self.get(row, n + col));
            }
        }

        result
    }

    /// Reads up to `usize::BITS` consecutive bits from a logical row.
    fn row_bits(&self, row: usize, start_col: usize, len: usize) -> usize {
        assert!(row < self.rows);
        assert!(start_col + len <= self.cols);
        assert!(len < usize::BITS as usize);

        let mut bits = 0;
        for offset in 0..len {
            bits |= (self.get(row, start_col + offset) as usize) << offset;
        }
        bits
    }

    /// Returns the packed word for a logical row and 64-column word index.
    fn row_word(&self, row: usize, word_col: usize) -> u64 {
        assert!(row < self.rows);
        assert!(word_col < self.block_cols);

        let block_row = row / BLOCK_BITS;
        let row_in_block = row & BLOCK_MASK;
        let index = self.block_index(block_row, word_col);
        self.blocks[index][row_in_block]
    }

    /// Returns the number of logical rows in the matrix.
    pub fn rows(&self) -> usize {
        self.rows
    }

    /// Reduces the left `rank_cols` columns to RREF using M4RI-style row-combination tables.
    fn rref_m4ri(&mut self, rank_cols: usize) -> Option<()> {
        assert!(rank_cols <= self.rows);
        assert!(rank_cols <= self.cols);

        let stripe_bits = m4ri_stripe_bits(rank_cols);
        let mut stripe_start = 0;
        while stripe_start < rank_cols {
            let stripe_len = stripe_bits.min(rank_cols - stripe_start);
            let stripe_end = stripe_start + stripe_len;

            self.establish_m4ri_pivots(stripe_start, stripe_end)?;
            let table = self.build_m4ri_row_table(stripe_start, stripe_len);

            for row in 0..self.rows {
                if (stripe_start..stripe_end).contains(&row) {
                    continue;
                }
                let prefix = self.row_bits(row, stripe_start, stripe_len);
                if prefix != 0 {
                    self.xor_row_words_from(row, stripe_start, &table[prefix]);
                }
            }

            stripe_start = stripe_end;
        }

        Some(())
    }

    /// Sets the bit at `row`, `col` to `bit & 1`.
    ///
    /// Any nonzero input is treated as `1`. Panics if `row` or `col` is outside the logical
    /// dimensions.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let mut matrix = BitMatrix::new(1, 3);
    /// matrix.set(0, 1, 5);
    /// assert_eq!(matrix.get(0, 1), 1);
    ///
    /// matrix.set(0, 1, 0);
    /// assert_eq!(matrix.get(0, 1), 0);
    /// ```
    pub fn set(&mut self, row: usize, col: usize, bit: u8) {
        assert!(row < self.rows);
        assert!(col < self.cols);

        let (block_row, block_col, row_in_block, col_in_block) = bit_coords(row, col);
        let index = self.block_index(block_row, block_col);
        let mask = 1_u64 << col_in_block;
        if bit & 1 == 1 {
            self.blocks[index][row_in_block] |= mask;
        } else {
            self.blocks[index][row_in_block] &= !mask;
        }
    }

    /// Replaces the packed word for a logical row and 64-column word index.
    fn set_row_word(&mut self, row: usize, word_col: usize, word: u64) {
        assert!(row < self.rows);
        assert!(word_col < self.block_cols);

        let block_row = row / BLOCK_BITS;
        let row_in_block = row & BLOCK_MASK;
        let index = self.block_index(block_row, word_col);
        self.blocks[index][row_in_block] = word;
    }

    /// Swaps two logical rows in place.
    fn swap_rows(&mut self, lhs: usize, rhs: usize) {
        assert!(lhs < self.rows);
        assert!(rhs < self.rows);

        if lhs == rhs {
            return;
        }

        for word_col in 0..self.block_cols {
            let lhs_word = self.row_word(lhs, word_col);
            let rhs_word = self.row_word(rhs, word_col);
            self.set_row_word(lhs, word_col, rhs_word);
            self.set_row_word(rhs, word_col, lhs_word);
        }
    }

    /// Returns the matrix as packed row words grouped by logical row.
    ///
    /// Each logical row is represented by `cols.div_ceil(64)` words. Bits outside the logical
    /// width are guaranteed to be zero.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let matrix = BitMatrix::from_rows([[1, 1 << 5], [0, 0]], 70);
    ///
    /// let rows = matrix.to_rows();
    /// assert_eq!(rows.len(), 2);
    /// assert_eq!(rows[0].len(), 2);
    /// assert_eq!(rows[0][0] & 1, 1);
    /// assert_eq!(rows[0][1] & (1 << 5), 1 << 5);
    /// ```
    pub fn to_rows(&self) -> Vec<Vec<u64>> {
        let words_per_row = blocks_for(self.cols);
        let mut rows = vec![vec![0; words_per_row]; self.rows];

        for (row, row_words) in rows.iter_mut().enumerate() {
            let block_row = row / BLOCK_BITS;
            let row_in_block = row & BLOCK_MASK;
            for (block_col, word) in row_words.iter_mut().enumerate() {
                let index = self.block_index(block_row, block_col);
                *word = self.blocks[index][row_in_block];
            }
        }
        rows
    }

    /// Replaces this matrix with its transpose.
    ///
    /// This is the in-place counterpart to [`BitMatrix::transposed`].
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let mut matrix = BitMatrix::from_rows([[0b010], [0b100]], 3);
    /// matrix.transpose();
    ///
    /// assert_eq!(matrix.dimensions(), (3, 2));
    /// assert_eq!(matrix.get(1, 0), 1);
    /// assert_eq!(matrix.get(2, 1), 1);
    /// ```
    pub fn transpose(&mut self) {
        *self = self.transposed();
    }

    /// Returns a transposed copy of the matrix.
    ///
    /// Rows become columns and columns become rows.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let matrix = BitMatrix::from_rows([[0b010], [0b100]], 3);
    ///
    /// let transposed = matrix.transposed();
    /// assert_eq!(transposed.dimensions(), (3, 2));
    /// assert_eq!(transposed.get(1, 0), 1);
    /// assert_eq!(transposed.get(2, 1), 1);
    /// ```
    pub fn transposed(&self) -> Self {
        let mut result = Self::new(self.cols(), self.rows());

        for block_row in 0..self.block_rows() {
            for block_col in 0..self.block_cols() {
                let mut block = *self
                    .block(block_row, block_col)
                    .expect("valid source block coordinates");
                block_transpose(&mut block);
                *result
                    .block_mut(block_col, block_row)
                    .expect("valid destination block coordinates") = block;
            }
        }

        result.clear_padding_bits();
        result
    }

    /// Returns the inverse of this square matrix over GF(2), if it exists.
    ///
    /// The matrix must be square. Singular matrices return `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let matrix = BitMatrix::from_rows([[0b01], [0b11]], 2);
    /// let inverse = matrix.try_inverse().unwrap();
    ///
    /// assert_eq!(matrix.matmul(&inverse), BitMatrix::identity(2));
    /// assert_eq!(inverse.matmul(&matrix), BitMatrix::identity(2));
    /// ```
    pub fn try_inverse(&self) -> Option<Self> {
        assert_eq!(self.rows(), self.cols());

        if self.rows() >= M4RI_INVERSE_THRESHOLD {
            self.try_inverse_m4ri()
        } else {
            self.try_inverse_gauss_jordan()
        }
    }

    /// Returns the inverse using packed Gauss-Jordan elimination.
    fn try_inverse_gauss_jordan(&self) -> Option<Self> {
        let n = self.rows();
        let mut augmented = self.augmented_with_identity();

        for pivot in 0..n {
            let pivot_row = (pivot..n).find(|&row| augmented.get(row, pivot) == 1)?;
            augmented.swap_rows(pivot, pivot_row);

            for row in 0..n {
                if row != pivot && augmented.get(row, pivot) == 1 {
                    augmented.xor_rows_from(row, pivot, pivot);
                }
            }
        }

        Some(augmented.right_half())
    }

    /// Returns the inverse using a deterministic M4RI-style full elimination.
    fn try_inverse_m4ri(&self) -> Option<Self> {
        let n = self.rows();
        let mut augmented = self.augmented_with_identity();
        augmented.rref_m4ri(n)?;

        Some(augmented.right_half())
    }

    /// Returns how many logical columns are present in the given block column.
    fn valid_cols_in_block(&self, block_col: usize) -> usize {
        valid_len_in_block(self.cols, block_col)
    }

    /// Returns how many logical rows are present in the given block row.
    fn valid_rows_in_block(&self, block_row: usize) -> usize {
        valid_len_in_block(self.rows, block_row)
    }

    /// Returns the element-wise bitwise XOR of `self` and `rhs`.
    ///
    /// Both matrices must have the same logical dimensions.
    pub fn xor(&self, rhs: &Self) -> Self {
        self.zip(rhs, |lhs, rhs| lhs ^ rhs)
    }

    /// XORs packed suffix words into `dst` starting at `start_col`.
    fn xor_row_words_from(&mut self, dst: usize, start_col: usize, words: &[u64]) {
        assert!(dst < self.rows);
        assert!(start_col <= self.cols);

        if start_col == self.cols {
            assert!(words.is_empty());
            return;
        }

        let start_word = start_col / BLOCK_BITS;
        let start_bit = start_col & BLOCK_MASK;
        assert_eq!(words.len(), self.block_cols - start_word);

        for (offset, &word) in words.iter().enumerate() {
            let mut word = word;
            if offset == 0 && start_bit != 0 {
                word &= !low_bits_mask(start_bit);
            }

            let word_col = start_word + offset;
            let dst_word = self.row_word(dst, word_col);
            self.set_row_word(dst, word_col, dst_word ^ word);
        }
    }

    /// XORs `src` into `dst` from `start_col` through the end of the row.
    fn xor_rows_from(&mut self, dst: usize, src: usize, start_col: usize) {
        assert!(dst < self.rows);
        assert!(src < self.rows);
        assert!(start_col <= self.cols);

        if start_col == self.cols {
            return;
        }

        let start_word = start_col / BLOCK_BITS;
        let start_bit = start_col & BLOCK_MASK;
        for word_col in start_word..self.block_cols {
            let mut src_word = self.row_word(src, word_col);
            if word_col == start_word && start_bit != 0 {
                src_word &= !low_bits_mask(start_bit);
            }

            let dst_word = self.row_word(dst, word_col);
            self.set_row_word(dst, word_col, dst_word ^ src_word);
        }
    }

    /// Creates a zero-filled matrix with `rows` rows and `cols` columns.
    ///
    /// This is an alias for [`BitMatrix::new`].
    pub fn zeros(rows: usize, cols: usize) -> Self {
        Self::new(rows, cols)
    }

    /// Applies `f` to corresponding stored row words of `self` and `rhs`.
    ///
    /// The closure sees packed `u64` row words from matrices with matching dimensions. Padding
    /// bits are cleared before the returned matrix is exposed.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let lhs = BitMatrix::from_rows([[0b0001]], 4);
    /// let rhs = BitMatrix::from_rows([[0b0010]], 4);
    ///
    /// let combined = lhs.zip(&rhs, |left, right| left | right);
    /// assert_eq!(combined.get(0, 0), 1);
    /// assert_eq!(combined.get(0, 1), 1);
    /// ```
    pub fn zip<F>(&self, rhs: &Self, mut f: F) -> Self
    where
        F: FnMut(u64, u64) -> u64,
    {
        assert_eq!(self.dimensions(), rhs.dimensions());

        let mut result = Self::new(self.rows(), self.cols());
        for ((out_block, lhs_block), rhs_block) in result
            .blocks_mut()
            .iter_mut()
            .zip(self.blocks())
            .zip(rhs.blocks())
        {
            for ((out_row, lhs_row), rhs_row) in out_block.iter_mut().zip(lhs_block).zip(rhs_block)
            {
                *out_row = f(*lhs_row, *rhs_row);
            }
        }
        result.clear_padding_bits();
        result
    }

    /// Replaces each row word in `self` with the result of applying `f` to corresponding row words
    /// of `self` and `rhs`.
    ///
    /// The matrices must have the same logical dimensions. Padding bits are cleared after the
    /// update.
    ///
    /// # Examples
    ///
    /// ```
    /// use bitmatrix::BitMatrix;
    ///
    /// let mut lhs = BitMatrix::from_rows([[0b0001]], 4);
    /// let rhs = BitMatrix::from_rows([[0b0010]], 4);
    ///
    /// lhs.zip_with(&rhs, |left, right| left ^ right);
    /// assert_eq!(lhs.get(0, 0), 1);
    /// assert_eq!(lhs.get(0, 1), 1);
    /// ```
    pub fn zip_with<F>(&mut self, rhs: &Self, mut f: F)
    where
        F: FnMut(u64, u64) -> u64,
    {
        assert_eq!(self.dimensions(), rhs.dimensions());
        for (lhs_block, rhs_block) in self.blocks_mut().iter_mut().zip(rhs.blocks()) {
            for (lhs_row, rhs_row) in lhs_block.iter_mut().zip(rhs_block) {
                *lhs_row = f(*lhs_row, *rhs_row);
            }
        }
        self.clear_padding_bits();
    }
}

impl Display for BitMatrix {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "BitMatrix {{rows: {}, cols: {}}}",
            self.rows(),
            self.cols()
        )?;

        if self.rows() > 80 || self.cols() > 80 {
            return Ok(());
        }

        if self.rows() == 0 || self.cols() == 0 {
            return write!(f, "\n[ ]");
        }

        writeln!(f)?;
        for row in 0..self.rows() {
            write!(f, "{}", if row == 0 { '[' } else { ' ' })?;
            for col in 0..self.cols() {
                write!(f, " {}", self.get(row, col))?;
            }

            if row + 1 == self.rows() {
                write!(f, " ]")?;
            } else {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}

/// Converts logical bit coordinates into block and in-block offsets.
#[inline]
fn bit_coords(row: usize, col: usize) -> (usize, usize, usize, usize) {
    (
        row / BLOCK_BITS,
        col / BLOCK_BITS,
        row & BLOCK_MASK,
        col & BLOCK_MASK,
    )
}

/// Multiplies two packed 64 by 64 blocks over GF(2).
fn block_matmul(lhs: &[u64; BLOCK_BITS], rhs: &[u64; BLOCK_BITS]) -> [u64; BLOCK_BITS] {
    const STRIPE_BITS: usize = 5;
    const SUM_TABLE_LEN: usize = 1 << STRIPE_BITS;
    const STRIPE_INDEX: [usize; SUM_TABLE_LEN] = gray_code_diffs();
    const ROW_SUM_INDEX: [usize; SUM_TABLE_LEN] = gray_code_index();
    const STRIPES: usize = BLOCK_BITS.div_ceil(STRIPE_BITS);

    let mut sums = [0; SUM_TABLE_LEN];
    let mut result = [0; BLOCK_BITS];
    let mut mask = (1 << STRIPE_BITS) - 1;
    let mut shift = 0;

    for _ in 0..STRIPES {
        let range = shift..(shift + STRIPE_BITS).min(BLOCK_BITS);
        let stripe = &rhs[range];

        // Precompute every XOR combination of the rows in this stripe. Enumerating combinations in
        // Gray-code order means each new table entry differs by exactly one source row, so the
        // table can be filled with one XOR instead of recomputing each combination from scratch.
        for i in 1..SUM_TABLE_LEN {
            let row = STRIPE_INDEX[i];
            sums[i] = sums[i - 1] ^ stripe.get(row).unwrap_or(&0);
        }

        // The current stripe of each lhs row selects which precomputed XOR sum contributes to the
        // output row. ROW_SUM_INDEX translates the stripe bits back to the Gray-code table index.
        for (row_index, row) in lhs.iter().enumerate() {
            let prefix = ((row & mask) >> shift) as usize;
            let sum_index = ROW_SUM_INDEX[prefix];
            result[row_index] ^= sums[sum_index];
        }

        mask <<= STRIPE_BITS;
        shift += STRIPE_BITS;
    }

    result
}

/// Multiplies two packed 64 by 64 blocks over the Boolean OR/AND semiring.
fn block_matmul_or(lhs: &[u64; BLOCK_BITS], rhs: &[u64; BLOCK_BITS]) -> [u64; BLOCK_BITS] {
    const STRIPE_BITS: usize = 5;
    const SUM_TABLE_LEN: usize = 1 << STRIPE_BITS;
    const STRIPES: usize = BLOCK_BITS.div_ceil(STRIPE_BITS);

    let mut sums = [0; SUM_TABLE_LEN];
    let mut result = [0; BLOCK_BITS];
    let mut shift = 0;

    for _ in 0..STRIPES {
        let stripe_len = (BLOCK_BITS - shift).min(STRIPE_BITS);
        let table_len = 1 << stripe_len;
        let stripe_mask = (1_u64 << stripe_len) - 1;
        let stripe = &rhs[shift..shift + stripe_len];

        // OR cannot undo a row contribution, so build subset combinations directly instead of
        // using Gray-code incremental XOR updates like the GF(2) kernel does.
        sums[0] = 0;
        for subset in 1_usize..table_len {
            let bit = subset.trailing_zeros() as usize;
            let remainder = subset & (subset - 1);
            sums[subset] = sums[remainder] | stripe[bit];
        }

        for (row_index, row) in lhs.iter().enumerate() {
            let subset = ((row >> shift) & stripe_mask) as usize;
            result[row_index] |= sums[subset];
        }

        shift += STRIPE_BITS;
    }

    result
}

/// Transposes a packed 64 by 64 block in place.
fn block_transpose(block: &mut [u64; BLOCK_BITS]) {
    let mut mask: u64 = 0xFFFFFFFF;
    let mut shift = 32;
    while shift > 0 {
        let mut b = 0;
        while b < BLOCK_BITS {
            let a = b + shift;

            // Swap progressively smaller bit fields across the diagonal. Each pass exchanges
            // `shift`-wide groups, which turns the packed 64 by 64 block into its transpose in
            // place without allocating another block.
            let t = (block[a] ^ (block[b] >> shift)) & mask;
            block[a] ^= t;
            block[b] ^= t << shift;
            b = (a + 1) & !shift;
        }
        shift >>= 1;
        mask ^= mask << shift;
    }
}

/// Returns the number of 64-bit blocks needed to store `bits` logical positions.
#[inline]
fn blocks_for(bits: usize) -> usize {
    bits.div_ceil(BLOCK_BITS)
}

/// Returns the Gray-code encoding of `i`.
const fn gray_code(i: usize) -> usize {
    i ^ (i >> 1)
}

/// Builds a lookup table describing which bit changes between successive Gray codes.
const fn gray_code_diffs<const N: usize>() -> [usize; N] {
    let mut diffs = [0; N];
    let mut prev = 0;
    let mut i = 1;
    while i < N {
        let code = gray_code(i);

        // Successive Gray codes differ by one bit, so this records which source row toggles when
        // advancing from the previous table entry to the next one.
        diffs[i] = (usize::BITS - (code ^ prev).leading_zeros() - 1) as usize;
        prev = code;
        i += 1;
    }
    diffs
}

/// Builds a reverse lookup from a Gray-coded value to its sequence index.
const fn gray_code_index<const N: usize>() -> [usize; N] {
    let mut codes = [0; N];
    let mut i = 1;
    while i < N {
        // Reverse lookup from a stripe bit pattern to its position in Gray-code order.
        codes[gray_code(i)] = i;
        i += 1;
    }
    codes
}

/// Returns a mask with the lowest `bits` bits set.
#[inline]
fn low_bits_mask(bits: usize) -> u64 {
    match bits {
        0 => 0,
        BLOCK_BITS => u64::MAX,
        _ => (1_u64 << bits) - 1,
    }
}

/// Returns the stripe width used for M4RI row-combination tables.
fn m4ri_stripe_bits(rank_cols: usize) -> usize {
    if rank_cols <= 1 {
        1
    } else {
        ((usize::BITS - rank_cols.leading_zeros() - 1) as usize).clamp(1, M4RI_MAX_STRIPE_BITS)
    }
}

/// Returns how many logical entries fall inside the given block index.
#[inline]
fn valid_len_in_block(len: usize, block: usize) -> usize {
    let start = block * BLOCK_BITS;
    len.saturating_sub(start).min(BLOCK_BITS)
}
