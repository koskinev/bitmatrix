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
pub(crate) fn delta_swap(x: u64, m: u64, shift: u32) -> u64 {
    let t = ((x >> shift) ^ x) & m;
    (x ^ t) ^ (t << shift)
}

/// Moves the masked bits in `x` to the left by `shift` positions. This is a generalization of
/// `delta_swap` to `u64` arrays. See `delta_swap` for more information.
pub(crate) fn w_delta_swap<const N: usize>(x: [u64; N], m: [u64; N], shift: u32) -> [u64; N] {
    let t = w_and(w_xor(w_shr(x, shift), x), m);
    w_xor(w_xor(x, t), w_shl(t, shift))
}

/// Performs the `and` operation on two unsigned integers stored in `u64` arrays.
pub(crate) fn w_and<const N: usize>(a: [u64; N], b: [u64; N]) -> [u64; N] {
    let mut result = [0; N];
    for i in 0..N {
        result[i] = a[i] & b[i];
    }
    result
}

/// Performs the `xor` operation on two unsigned integers stored in `u64` arrays
pub(crate) fn w_xor<const N: usize>(a: [u64; N], b: [u64; N]) -> [u64; N] {
    let mut result = [0; N];
    for i in 0..N {
        result[i] = a[i] ^ b[i];
    }
    result
}

/// Multiplies two unsigned integers using the grade-school method. The arguments and
/// the result are arrays of `u64`s, where the least significant word is at index 0.
/// The result is truncated if the multiplication overflows.
pub(crate) fn w_mul<const N: usize>(a: [u64; N], b: [u64; N]) -> [u64; N] {
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

/// An optimized version of `w_mul` for the case where the `b` array has only one non-zero element
/// at index 0.
pub(crate) fn w_mul_n1<const N: usize>(a: [u64; N], b: [u64; N]) -> [u64; N] {
    let mut result = [0; N];
    let mut carry: u64 = 0;
    for i in 0..N {
        let t = a[i] as u128 * b[0] as u128 + result[i] as u128 + carry as u128;
        result[i] = t as u64;
        carry = (t >> 64) as u64;
    }
    result
}

/// Performs the right shift operation. The least significant word is assumed to be at index 0.
pub(crate) fn w_shr<const N: usize>(mut v: [u64; N], shift: u32) -> [u64; N] {
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
pub(crate) fn w_shl<const N: usize>(mut v: [u64; N], shift: u32) -> [u64; N] {
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
