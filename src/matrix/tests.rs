use std::time::Duration;

use utils::{wide_and, wide_mul, wide_shl, wide_shr, wide_xor};

use super::*;
use crate::wyrand::{Random, WyRng};

/// Converts an array of `u64`s to a `Vec<u8>`, where each element represents one bit in the
/// input array. The bits are stored in little-endian order.
fn u64s_to_bv<const N: usize>(v: [u64; N]) -> Vec<u8> {
    let mut result = Vec::with_capacity(N * 64);
    for x in &v {
        for i in 0..64 {
            result.push((x >> i) as u8 & 1);
        }
    }
    result.reverse();
    result
}

/// Converts a little-endian bit vector to an array of `u64`s. The first `u64` in the array
/// represents the least significant bits in the bit vector.
fn bv_to_u64s<const N: usize>(v: Vec<u8>) -> [u64; N] {
    assert_eq!(v.len(), N * 64);
    let mut result = [0; N];
    for (pos, bit) in v.iter().enumerate() {
        let i = N - pos / 64 - 1;
        let j = 63 - pos % 64;
        result[i] |= (*bit as u64) << j;
    }
    result
}

/// Performs the and operation on the bit vector.
fn bv_and(a: &[u8], b: &[u8]) -> Vec<u8> {
    assert_eq!(a.len(), b.len());
    a.iter().zip(b.iter()).map(|(a, b)| a & b).collect()
}

/// Performs the or operation on the bit vector.
fn bv_or(a: &[u8], b: &[u8]) -> Vec<u8> {
    assert_eq!(a.len(), b.len());
    a.iter().zip(b.iter()).map(|(a, b)| a | b).collect()
}

/// Performs the xor operation on the bit vector.
fn bv_xor(a: &[u8], b: &[u8]) -> Vec<u8> {
    assert_eq!(a.len(), b.len());
    a.iter().zip(b.iter()).map(|(a, b)| a ^ b).collect()
}

/// Performs the right shift operation the bit vector.
fn bv_shr(v: &[u8], shift: u32) -> Vec<u8> {
    let mut result = Vec::from(v);
    let clear = shift as usize;
    result.rotate_right(shift as usize);
    for x in &mut result[..clear] {
        *x = 0;
    }
    result
}

/// Performs the left shift operation the bit vector.
fn bv_shl(v: &[u8], shift: u32) -> Vec<u8> {
    let mut result = Vec::from(v);
    let clear = v.len() - shift as usize;
    result.rotate_left(shift as usize);
    for x in &mut result[clear..] {
        *x = 0;
    }
    result
}

/// Adds two bit vectors.
fn bv_add(a: &[u8], b: &[u8]) -> Vec<u8> {
    assert_eq!(a.len(), b.len());
    let zero = vec![0; a.len()];
    let mut result = bv_xor(a, b);
    let mut carry = bv_and(a, b);
    while carry != zero {
        let shifted = bv_shl(&carry, 1);
        carry = bv_and(&result, &shifted);
        result = bv_xor(&result, &shifted);
    }
    result
}

/// Multiplies two bit vectors.
fn bv_mul(a: &[u8], b: &[u8]) -> Vec<u8> {
    let mut a = Vec::from(a);
    let mut b = Vec::from(b);
    let zero = vec![0; a.len()];
    let mut result = vec![0; a.len()];
    let last = a.len() - 1;
    while a != zero {
        if a[last] == 1 {
            result = bv_add(&result, &b);
        }
        a = bv_shr(&a, 1);
        b = bv_shl(&b, 1);
    }
    result
}

#[test]
fn wide_ops() {
    // First, test the bit vector operations on randomized u128s.
    const ITERS: usize = 100;
    let mut rng = WyRng::default();
    let split = |x: u128| [x as u64, (x >> 64) as u64];

    for _ in 0..ITERS {
        let a: u128 = rng.random();
        let b: u128 = rng.random();
        let shl: u32 = rng.bounded(0, 128);
        let shr: u32 = rng.bounded(0, 128);

        let tru_and = split(a & b);
        let tru_or = split(a | b);
        let tru_xor = split(a ^ b);
        let tru_shr = split(a >> shr);
        let tru_shl = split(b << shl);
        let tru_mul = split(a.wrapping_mul(b));
        let tru_add = split(a.wrapping_add(b));

        let bv_a = u64s_to_bv([a as u64, (a >> 64) as u64]);
        let bv_b = u64s_to_bv([b as u64, (b >> 64) as u64]);

        let bv_and = bv_to_u64s(bv_and(&bv_a, &bv_b));
        let bv_or = bv_to_u64s(bv_or(&bv_a, &bv_b));
        let bv_xor = bv_to_u64s(bv_xor(&bv_a, &bv_b));
        let bv_shr = bv_to_u64s(bv_shr(&bv_a, shr));
        let bv_shl = bv_to_u64s(bv_shl(&bv_b, shl));
        let bv_add = bv_to_u64s(bv_add(&bv_a, &bv_b));
        let bv_mul = bv_to_u64s(bv_mul(&bv_a, &bv_b));

        assert_eq!(tru_and, bv_and);
        assert_eq!(tru_or, bv_or);
        assert_eq!(tru_xor, bv_xor);
        assert_eq!(tru_shr, bv_shr);
        assert_eq!(tru_shl, bv_shl);
        assert_eq!(tru_add, bv_add);
        assert_eq!(tru_mul, bv_mul);
    }

    // Next, test the long operations on randomized u64s.
    const N: usize = 4;
    for _ in 0..ITERS {
        let a: [u64; N] = rng.random();
        let b: [u64; N] = rng.random();

        let shl = rng.bounded(0, 64 * N as u32);
        let shr = rng.bounded(0, 64 * N as u32);

        let bv_a = u64s_to_bv(a);
        let bv_b = u64s_to_bv(b);

        let v_and = bv_and(&bv_a, &bv_b);
        let v_xor = bv_xor(&bv_a, &bv_b);
        let v_shr = bv_shr(&bv_a, shr);
        let v_shl = bv_shl(&bv_b, shl);
        let v_mul = bv_mul(&bv_a, &bv_b);

        let w_and = wide_and(a, b);
        let w_xor = wide_xor(a, b);
        let w_shr = wide_shr(a, shr);
        let w_shl = wide_shl(b, shl);
        let w_mul = wide_mul(a, b);

        assert_eq!(w_and, bv_to_u64s(v_and));
        assert_eq!(w_xor, bv_to_u64s(v_xor));
        assert_eq!(w_shr, bv_to_u64s(v_shr));
        assert_eq!(w_shl, bv_to_u64s(v_shl));
        assert_eq!(w_mul, bv_to_u64s(v_mul));
    }
}

#[test]
fn matmul_8x8() {
    const ITERS: usize = 1000;
    let mut rng: WyRng = Default::default();
    type Mat = [u8; 8];

    let identity = Mat::IDENTITY;
    for _i in 0..ITERS {
        let a: Mat = rng.random();
        let b: Mat = rng.random();

        let a_x_i = a.matmul(identity);
        let i_x_a = identity.matmul(a);

        assert_eq!(a_x_i, a);
        assert_eq!(i_x_a, a);

        let a_x_b = a.matmul(b);
        let b_t = b.transposed();
        let mut expected = Mat::ZERO;
        for (i, row) in a.iter().enumerate() {
            for (j, col) in b_t.iter().enumerate() {
                expected[i] |=
                    (0..Mat::SIZE).fold(0, |acc, k| acc ^ ((row >> k) & (col >> k) & 1)) << j;
            }
        }
        assert_eq!(a_x_b, expected);
    }
}

#[test]
fn matmul_16x16() {
    const ITERS: usize = 1000;
    let mut rng: WyRng = Default::default();
    type Mat = [u16; 16];

    let identity = Mat::IDENTITY;
    for _i in 0..ITERS {
        let a: Mat = rng.random();
        let b: Mat = rng.random();

        let a_x_i = a.matmul(identity);
        let i_x_a = identity.matmul(a);

        assert_eq!(a_x_i, a);
        assert_eq!(i_x_a, a);

        let a_x_b = a.matmul(b);
        let b_t = b.transposed();
        let mut expected = Mat::ZERO;
        for (i, row) in a.iter().enumerate() {
            for (j, col) in b_t.iter().enumerate() {
                expected[i] |=
                    (0..Mat::SIZE).fold(0, |acc, k| acc ^ ((row >> k) & (col >> k) & 1)) << j;
            }
        }
        assert_eq!(a_x_b, expected);
    }
}

#[test]
fn matmul_32x32() {
    const ITERS: usize = 1000;
    let mut rng: WyRng = Default::default();
    type Mat = [u32; 32];

    let identity = Mat::IDENTITY;
    for _i in 0..ITERS {
        let a: Mat = rng.random();
        let b: Mat = rng.random();

        let a_x_i = a.matmul(identity);
        let i_x_a = identity.matmul(a);

        assert_eq!(a_x_i, a);
        assert_eq!(i_x_a, a);

        let a_x_b = a.matmul(b);
        let b_t = b.transposed();
        let mut expected = Mat::ZERO;
        for (i, row) in a.iter().enumerate() {
            for (j, col) in b_t.iter().enumerate() {
                expected[i] |=
                    (0..Mat::SIZE).fold(0, |acc, k| acc ^ ((row >> k) & (col >> k) & 1)) << j;
            }
        }
        assert_eq!(a_x_b, expected);
    }
}

#[test]
fn matmul_64x64() {
    const ITERS: usize = 1000;
    let mut rng: WyRng = Default::default();
    type Mat = [u64; 64];

    let identity = Mat::IDENTITY;
    for _i in 0..ITERS {
        let a: Mat = rng.random();
        let b: Mat = rng.random();

        let a_x_i = a.matmul(identity);
        let i_x_a = identity.matmul(a);

        assert_eq!(a_x_i, a);
        assert_eq!(i_x_a, a);

        let a_x_b = a.matmul(b);
        let b_t = b.transposed();
        let mut expected = Mat::ZERO;
        for (i, row) in a.iter().enumerate() {
            for (j, col) in b_t.iter().enumerate() {
                expected[i] |=
                    (0..Mat::SIZE).fold(0, |acc, k| acc ^ ((row >> k) & (col >> k) & 1)) << j;
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

    const ITERS: usize = 1000;
    let mut rng: WyRng = Default::default();
    type Mat = [u8; 8];

    for _i in 0..ITERS {
        let matrix: Mat = rng.random();
        let transpose = matrix.transposed();
        for i in 0..Mat::SIZE {
            for j in 0..Mat::SIZE {
                assert_eq!(matrix.get(i, j), transpose.get(j, i));
            }
        }
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

    const ITERS: usize = 1000;
    let mut rng: WyRng = Default::default();
    type Mat = [u16; 16];

    for _i in 0..ITERS {
        let matrix: Mat = rng.random();
        let transpose = matrix.transposed();
        for i in 0..Mat::SIZE {
            for j in 0..Mat::SIZE {
                assert_eq!(matrix.get(i, j), transpose.get(j, i));
            }
        }
    }
}

#[test]
fn transpose_32x32() {
    const ITERS: usize = 1000;
    let mut rng: WyRng = Default::default();
    type Mat = [u32; 32];

    for _i in 0..ITERS {
        let matrix: Mat = rng.random();
        let transpose = matrix.transposed();
        for i in 0..Mat::SIZE {
            for j in 0..Mat::SIZE {
                assert_eq!(matrix.get(i, j), transpose.get(j, i));
            }
        }
    }
}

#[ignore]
#[test]
fn transpose_perf() {
    // cargo test --release -- --nocapture --ignored transpose_perf

    use std::time::Instant;

    fn run<U, const N: usize>(count: usize)
    where
        [U; N]: BitMatrix,
        U: Random<WyRng> + Copy,
    {
        // Generate test matrices.
        let mut rng: WyRng = Default::default();
        let original: Vec<[U; N]> = (0..count).map(|_| rng.random()).collect();

        // Create a mutable copy of the original matrices.
        let mut matrices = original.clone();

        // Transpose each matrix and measure the time.
        let start = Instant::now();
        for matrix in &mut matrices {
            matrix.transpose();
        }
        let duration = start.elapsed();

        // Sample one element from each matrix and check if it was transposed correctly.
        for (matrix, transpose) in original.iter().zip(matrices.iter()) {
            let i = rng.bounded(0, N);
            let j = rng.bounded(0, N);
            assert_eq!(matrix.get(i, j), transpose.get(j, i));
        }

        println!(
            "Transpose {N}x{N}: {duration:?} ({count} ops, {tput:.6} ns/op)",
            tput = duration.as_nanos() as f64 / count as f64
        );
    }

    run::<u8, 8>(100_000_000);
    run::<u16, 16>(50_000_000);
    run::<u32, 32>(25_000_000);
    run::<u64, 64>(10_000_000);
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
