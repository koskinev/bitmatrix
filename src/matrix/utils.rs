use std::ops::{BitAnd, BitXor, Shl, Shr};

#[inline]
/// Exchanges the masked bits in `x` with the bits in `y`, masked by `mask << shift`. Returns the
/// tuple (x, y) with the exchanged bits. For this function to work properly, the mask  and the
/// shifted mask should not overlap, ie. `mask & (mask << shift) == 0` and no bits should be shifted
/// out, ie. `((mask << shift) >> shift) == mask`.
///
/// ```text
///  x <- abcd_efgh
///  y <- ijkl_mnop
///  m <- 0011_0011
///  (x, y) <- delta_exchange(x, y, m, 2)
///  x == abij_efmn
///  y == cdkl_ghop
/// ```
pub(crate) fn delta_exchange<U>(x: U, y: U, mask: U, shift: u32) -> (U, U)
where
    U: BitXor<Output = U> + BitAnd<Output = U> + Shl<u32, Output = U> + Shr<u32, Output = U> + Copy,
{
    let t = ((y >> shift) ^ x) & mask;
    (x ^ t, y ^ (t << shift))
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
pub(crate) fn delta_swap(x: u64, mask: u64, shift: u32) -> u64 {
    let t = ((x >> shift) ^ x) & mask;
    (x ^ t) ^ (t << shift)
}

/// Exchanges the masked bits of `x` with the corresponding bits in `y`, and returns the
/// result.
///
/// ```text
///  x <- abcd_efgh
///  y <- ijkl_mnop
///  m <- 1100_1100
///  exchange(x, y, m)
///  x == ijcd_mngh
///  y == abkl_efop
/// ```
pub(crate) fn exchange(mut x: u64, mut y: u64, mask: u64) -> (u64, u64) {
    x ^= y;
    y ^= x & mask;
    x ^= y;
    (x, y)
}

/// Computes the gray code of a number `i`.
const fn gray_code(i: usize) -> usize {
    i ^ (i >> 1)
}

/// Computes the integer square root of `n` using Newton's method.
const fn isqrt(n: usize) -> usize {
    let mut x = n;
    let mut y = (x + 1) / 2;
    while y < x {
        x = y;
        y = (x + n / x) / 2;
    }
    x
}

/// Computes an index into the row sum table in the four russians algorithm. The element at `prefix`
/// is the index of the row sum for that prefix.
pub(crate) const fn row_sum_index<const N: usize>() -> [usize; N] {
    let mut codes = [0; N];
    let mut i = 1;
    while i < N {
        codes[gray_code(i)] = i;
        i += 1;
    }
    codes
}

/// Computes an index into the stripe for constructing the row sum table in the four russians
/// algorithm. The element at `index` is the index of the row that should be `XOR`ed with the
/// previous row sum.
pub(crate) const fn stripe_index<const N: usize>() -> [usize; N] {
    let mut diffs = [0; N];
    let mut prev = 0;
    let mut i = 1;
    while i < N {
        let code = gray_code(i);
        diffs[i] = (code ^ prev).ilog2() as usize;
        // diffs[i] = (usize::BITS - (code ^ prev).leading_zeros() - 1) as usize;
        prev = code;
        i += 1
    }
    diffs
}

/// Constructs a mask for the `transpose` operation. The mask can be thought of as a bit matrix
/// where the set bits mask a block that is exchanged with the block mirrored along the diagonal.
/// See the comments of `BitMatrix::transpose` implementation for `[u8; 8]` for an example.
pub(crate) const fn transpose_mask<const N: usize>(block: usize) -> [u64; N] {
    let size = isqrt(64 * N);
    let mut mask = [0; N];
    let mut i = 0;
    while i < 64 * N {
        let col = i % size;
        let row = i / size;
        let bit = (col % (2 * block) >= block) & (row % (2 * block) < block);
        mask[i / 64] |= (bit as u64) << (i % 64);
        i += 1;
    }
    mask
}

/// Moves the masked bits in `x` to the left by `shift` positions. This is a generalization of
/// `delta_swap` to `u64` arrays. See `delta_swap` for more information.
pub(crate) fn wide_delta_swap<const N: usize>(x: [u64; N], m: [u64; N], shift: u32) -> [u64; N] {
    let t = wide_and(wide_xor(wide_shr(x, shift), x), m);
    wide_xor(wide_xor(x, t), wide_shl(t, shift))
}

/// Performs the `and` operation on two unsigned integers stored in `u64` arrays.
pub(crate) fn wide_and<const N: usize>(a: [u64; N], b: [u64; N]) -> [u64; N] {
    let mut result = [0; N];
    for i in 0..N {
        result[i] = a[i] & b[i];
    }
    result
}

/// Performs the `xor` operation on two unsigned integers stored in `u64` arrays
pub(crate) fn wide_xor<const N: usize>(a: [u64; N], b: [u64; N]) -> [u64; N] {
    let mut result = [0; N];
    for i in 0..N {
        result[i] = a[i] ^ b[i];
    }
    result
}

/// Multiplies two unsigned integers using the grade-school method. The arguments and
/// the result are arrays of `u64`s, where the least significant word is at index 0.
/// The result is truncated if the multiplication overflows.
pub(crate) fn wide_mul<const N: usize>(a: [u64; N], b: [u64; N]) -> [u64; N] {
    let mut result = [0; N];
    for i in 0..N {
        let mut carry: u64 = 0;
        for j in 0..(N - i) {
            let t = a[i] as u128 * b[j] as u128 + result[i + j] as u128 + carry as u128;
            result[i + j] = t as u64;
            carry = (t >> 64) as u64;
        }
    }
    result
}

/// Performs the right shift operation. The least significant word is assumed to be at index 0.
pub(crate) fn wide_shr<const N: usize>(mut v: [u64; N], shift: u32) -> [u64; N] {
    let delta = shift as usize / 64;
    let shift = shift % 64;
    let mask = !(!0 >> shift);
    v[0] = v[delta] >> shift;
    for i in 1..(N - delta) {
        let mut t = v[i + delta].rotate_right(shift);
        (t, v[i - 1]) = exchange(t, v[i - 1], mask);
        v[i] = t;
    }
    for a in &mut v[(N - delta)..] {
        *a = 0;
    }
    v
}

/// Performs the left shift operation. The least significant word is assumed to be at index 0.
pub(crate) fn wide_shl<const N: usize>(mut v: [u64; N], shift: u32) -> [u64; N] {
    let delta = shift as usize / 64;
    let shift = shift % 64;
    let mask = !(!0 << shift);
    v[N - 1] = v[N - 1 - delta] << shift;
    for i in (delta..(N - 1)).rev() {
        let mut t = v[i - delta].rotate_left(shift);
        (t, v[i + 1]) = exchange(t, v[i + 1], mask);
        v[i] = t;
    }
    for a in &mut v[..delta] {
        *a = 0;
    }
    v
}
