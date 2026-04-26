use super::BitMatrix;

fn assert_same_bits(lhs: &BitMatrix, rhs: &BitMatrix) {
    assert_eq!(lhs.dimensions(), rhs.dimensions());
    for row in 0..lhs.rows() {
        for col in 0..lhs.cols() {
            assert_eq!(lhs.get(row, col), rhs.get(row, col), "at ({row}, {col})");
        }
    }
}

fn naive_matmul(lhs: &BitMatrix, rhs: &BitMatrix) -> BitMatrix {
    assert_eq!(lhs.cols(), rhs.rows());

    let mut result = BitMatrix::new(lhs.rows(), rhs.cols());
    for row in 0..lhs.rows() {
        for col in 0..rhs.cols() {
            let mut bit = 0;
            for shared in 0..lhs.cols() {
                bit ^= lhs.get(row, shared) & rhs.get(shared, col);
            }
            result.set(row, col, bit);
        }
    }
    result
}

fn patterned(rows: usize, cols: usize) -> BitMatrix {
    let mut matrix = BitMatrix::new(rows, cols);
    for row in 0..rows {
        for col in 0..cols {
            let bit = ((row * 31 + col * 17 + row * col + 3) % 11 < 5) as u8;
            matrix.set(row, col, bit);
        }
    }
    matrix
}

fn assert_zero_outside_overlap(matrix: &BitMatrix, preserved_rows: usize, preserved_cols: usize) {
    for row in 0..matrix.rows() {
        for col in 0..matrix.cols() {
            if row >= preserved_rows || col >= preserved_cols {
                assert_eq!(matrix.get(row, col), 0, "expected zero at ({row}, {col})");
            }
        }
    }
}

#[test]
fn test_bitwise_operations_match_pointwise_logic() {
    let lhs = patterned(65, 70);
    let rhs = patterned(65, 70).not();

    let and = lhs.and(&rhs);
    let or = lhs.or(&rhs);
    let xor = lhs.xor(&rhs);

    for row in 0..lhs.rows() {
        for col in 0..lhs.cols() {
            assert_eq!(and.get(row, col), lhs.get(row, col) & rhs.get(row, col));
            assert_eq!(or.get(row, col), lhs.get(row, col) | rhs.get(row, col));
            assert_eq!(xor.get(row, col), lhs.get(row, col) ^ rhs.get(row, col));
        }
    }
    assert_eq!(or.count_ones(), 65 * 70);
    assert_eq!(and.count_ones(), 0);
}

#[test]
fn test_counts_and_padding_are_correct() {
    let mut matrix = BitMatrix::new(100, 137);
    assert_eq!(matrix.dimensions(), (100, 137));
    assert_eq!(matrix.count_ones(), 0);
    assert_eq!(matrix.count_zeros(), 13_700);

    for row in 0..matrix.rows() {
        for col in 0..matrix.cols() {
            matrix.set(row, col, 1);
        }
    }

    assert_eq!(matrix.count_ones(), 13_700);
    assert_eq!(matrix.count_zeros(), 0);
    let rows = matrix.to_rows();
    assert_eq!(rows.len(), 100);
    assert_eq!(rows[0].len(), 3);
    assert_eq!(rows[0][2] >> 9, 0);
}

#[test]
fn test_empty_matrices_are_supported() {
    let matrix = BitMatrix::new(0, 12);
    assert!(matrix.is_empty());
    assert_eq!(matrix.count_ones(), 0);
    assert_eq!(matrix.count_zeros(), 0);
    assert_eq!(matrix.transposed().dimensions(), (12, 0));

    let lhs = BitMatrix::new(7, 0);
    let rhs = BitMatrix::new(0, 9);
    assert_eq!(lhs.matmul(&rhs), BitMatrix::new(7, 9));
}

#[test]
fn test_display_pretty_prints_small_matrices() {
    let mut matrix = BitMatrix::new(4, 4);
    for (row, col) in [
        (0, 1),
        (0, 2),
        (1, 0),
        (1, 2),
        (2, 0),
        (2, 2),
        (2, 3),
        (3, 0),
        (3, 1),
        (3, 2),
        (3, 3),
    ] {
        matrix.set(row, col, 1);
    }

    let expected = concat!(
        "BitMatrix {rows: 4, cols: 4}\n",
        "[ 0 1 1 0\n",
        "  1 0 1 0\n",
        "  1 0 1 1\n",
        "  1 1 1 1 ]",
    );

    assert_eq!(matrix.to_string(), expected);
}

#[test]
fn test_display_uses_boundary_for_pretty_printing() {
    let small = BitMatrix::new(80, 80);
    let large = BitMatrix::new(80, 81);

    assert!(
        small
            .to_string()
            .starts_with("BitMatrix {rows: 80, cols: 80}\n[")
    );
    assert_eq!(large.to_string(), "BitMatrix {rows: 80, cols: 81}");
}

#[test]
fn test_from_rows_puts_least_significant_bits_to_left() {
    let matrix = BitMatrix::from_rows([[0b001], [0b010], [0b111]], 3);
    let expected = concat!(
        "BitMatrix {rows: 3, cols: 3}\n",
        "[ 1 0 0\n",
        "  0 1 0\n",
        "  1 1 1 ]",
    );
    assert_eq!(matrix.to_string(), expected);
}

#[test]
fn test_from_rows_masks_padding() {
    let rows = [
        0xFFFF_FFFF_FFFF_FFFF,
        0xFFFF_FFFF_FFFF_FFFF,
        0x0123_4567_89AB_CDEF,
        0xFFFF_FFFF_FFFF_FFFF,
        0,
        0xFFFF_FFFF_FFFF_FFFF,
    ];
    let matrix = BitMatrix::from_rows([&rows[0..2], &rows[2..4], &rows[4..6]], 70);
    let roundtrip = matrix.to_rows();

    assert_eq!(roundtrip[0], vec![u64::MAX, 0x3F]);
    assert_eq!(roundtrip[1], vec![0x0123_4567_89AB_CDEF, 0x3F]);
    assert_eq!(roundtrip[2], vec![0, 0x3F]);
}

#[test]
fn test_resize_grows_and_zero_fills() {
    let original = patterned(63, 70);
    let mut resized = original.clone();

    resized.resize(70, 130);

    assert_eq!(resized.dimensions(), (70, 130));
    for row in 0..original.rows() {
        for col in 0..original.cols() {
            assert_eq!(resized.get(row, col), original.get(row, col));
        }
    }
    assert_zero_outside_overlap(&resized, original.rows(), original.cols());
}

#[test]
fn test_resize_shrinks_and_truncates() {
    let original = patterned(100, 137);
    let mut resized = original.clone();

    resized.resize(63, 70);

    assert_eq!(resized.dimensions(), (63, 70));
    for row in 0..resized.rows() {
        for col in 0..resized.cols() {
            assert_eq!(resized.get(row, col), original.get(row, col));
        }
    }
}

#[test]
fn test_resize_clears_stale_bits_after_shrink_then_grow() {
    let mut matrix = BitMatrix::new(70, 70);
    for row in 0..matrix.rows() {
        for col in 0..matrix.cols() {
            matrix.set(row, col, 1);
        }
    }

    matrix.resize(10, 10);
    matrix.resize(70, 70);

    for row in 0..10 {
        for col in 0..10 {
            assert_eq!(matrix.get(row, col), 1);
        }
    }
    assert_zero_outside_overlap(&matrix, 10, 10);
}

#[test]
fn test_resize_handles_zero_dimensions() {
    let original = patterned(7, 9);
    let mut matrix = original.clone();

    matrix.resize(0, 9);
    assert_eq!(matrix.dimensions(), (0, 9));
    assert!(matrix.is_empty());

    matrix.resize(7, 0);
    assert_eq!(matrix.dimensions(), (7, 0));
    assert!(matrix.is_empty());

    matrix.resize(0, 0);
    assert_eq!(matrix.dimensions(), (0, 0));
    assert!(matrix.is_empty());

    matrix.resize(7, 9);
    assert_eq!(matrix.dimensions(), (7, 9));
    assert_zero_outside_overlap(&matrix, 0, 0);

    let mut restored = original;
    restored.resize(0, 0);
    restored.resize(7, 9);
    assert_eq!(restored, matrix);
}

#[test]
fn test_matmul_identity_handles_rectangular_matrices() {
    let matrix = patterned(70, 130);
    let right_identity = BitMatrix::identity(matrix.cols());
    let left_identity = BitMatrix::identity(matrix.rows());

    assert_eq!(matrix.matmul(&right_identity), matrix);
    assert_eq!(left_identity.matmul(&matrix), matrix);
}

#[test]
fn test_matmul_matches_naive_reference_on_odd_dimensions() {
    let lhs = patterned(67, 70);
    let rhs = patterned(70, 65);

    let actual = lhs.matmul(&rhs);
    let expected = naive_matmul(&lhs, &rhs);

    assert_same_bits(&actual, &expected);
}

#[test]
fn test_try_inverse_handles_identity_matrices() {
    for size in [0, 1, 5, 64, 65] {
        let identity = BitMatrix::identity(size);

        assert_eq!(identity.try_inverse(), Some(identity));
    }
}

#[test]
fn test_try_inverse_inverts_known_small_matrix() {
    let matrix = BitMatrix::from_rows([[0b001], [0b011], [0b110]], 3);
    let expected = BitMatrix::from_rows([[0b001], [0b011], [0b111]], 3);

    let inverse = matrix.try_inverse().unwrap();

    assert_eq!(inverse, expected);
    assert_eq!(matrix.matmul(&inverse), BitMatrix::identity(3));
    assert_eq!(inverse.matmul(&matrix), BitMatrix::identity(3));
}

#[test]
fn test_try_inverse_roundtrips_block_boundary_matrices() {
    for size in [2, 63, 64, 65, 127, 128, 129] {
        let matrix = BitMatrix::from_fn(size, size, |row, col| row == col || col == row + 1);

        let inverse = matrix.try_inverse().unwrap();

        assert_eq!(matrix.matmul(&inverse), BitMatrix::identity(size));
        assert_eq!(inverse.matmul(&matrix), BitMatrix::identity(size));
    }
}

#[test]
fn test_try_inverse_returns_none_for_singular_matrices() {
    assert_eq!(BitMatrix::new(4, 4).try_inverse(), None);

    let dependent = BitMatrix::from_rows([[0b101], [0b101], [0b010]], 3);
    assert_eq!(dependent.try_inverse(), None);
}

#[test]
#[should_panic]
fn test_try_inverse_panics_for_non_square_matrices() {
    BitMatrix::new(2, 3).try_inverse();
}

#[test]
fn test_transpose_handles_rectangular_and_odd_dimensions() {
    for (rows, cols) in [(1, 1), (1, 65), (65, 1), (63, 70), (70, 63), (100, 137)] {
        let matrix = patterned(rows, cols);
        let transpose = matrix.transposed();

        assert_eq!(transpose.dimensions(), (cols, rows));
        for row in 0..rows {
            for col in 0..cols {
                assert_eq!(matrix.get(row, col), transpose.get(col, row));
            }
        }
        assert_eq!(transpose.transposed(), matrix);
    }
}
