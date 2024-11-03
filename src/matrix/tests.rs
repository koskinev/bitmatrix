use super::*;
use crate::wyrand::{Random, WyRng};

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

#[ignore]
#[test]
fn matmul_perf() {
    // cargo test --release -- --nocapture --ignored matmul_perf

    use std::time::Instant;

    fn run<U, const N: usize>(count: usize)
    where
        [U; N]: BitMatrix,
        U: Random<WyRng> + Copy,
    {
        // Generate test matrices.
        let mut rng: WyRng = Default::default();
        let mut data: Vec<([U; N], [U; N], [U; N])> = (0..count)
            .map(|_| (rng.random(), rng.random(), <[U; N]>::IDENTITY))
            .collect();

        // Multiply the matrices and measure the time.
        let start = Instant::now();
        for (a, b, result) in &mut data {
            *result = a.matmul(*b);
        }
        let duration = start.elapsed();

        println!(
            "Multiply {N}x{N}: {duration:?} ({count} ops, {tput:.6} ns/op)",
            tput = duration.as_nanos() as f64 / count as f64
        );
    }

    run::<u8, 8>(200_000_000);
    run::<u16, 16>(20_000_000);
    run::<u32, 32>(5_000_000);
    run::<u64, 64>(1_000_000);
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
