//! Bit matrices with packed storage and logical-bit matrix operations.
//!
//! [`BitMatrix`] stores a dynamic rectangular matrix of bits and supports element access,
//! bitwise combination, transpose, resizing, inversion, and both GF(2) and Boolean-semiring
//! matrix multiplication. Internally the matrix is stored as 64 by 64 blocks of packed `u64`
//! row words so bulk operations can work on whole machine words at a time.
//!
//! The public API exposes logical row and column indices. Padding introduced by the packed
//! representation is kept internal and is always cleared before results are returned.
//!
//! # Capabilities
//!
//! - Build matrices from dimensions, packed rows, or as an identity matrix.
//! - Read and write individual bits.
//! - Combine matrices with bitwise AND, OR, and XOR.
//! - Transpose, resize, and convert back to packed rows.
//! - Invert square matrices over GF(2), when an inverse exists.
//! - Multiply matrices over GF(2) or over the Boolean OR/AND semiring.
//!
//! # Example
//!
//! ```
//! use bitmatrix::BitMatrix;
//!
//! let lhs = BitMatrix::from_rows([[0b101], [0b010]], 3);
//! let rhs = BitMatrix::from_rows([[0b10], [0b00], [0b11]], 2);
//!
//! let product = lhs.matmul(&rhs);
//! assert_eq!(product.dimensions(), (2, 2));
//! assert_eq!(product.get(0, 0), 1);
//! assert_eq!(product.get(0, 1), 0);
//! assert_eq!(product.get(1, 0), 0);
//! assert_eq!(product.get(1, 1), 0);
//!
//! let lhs = BitMatrix::from_rows([[0b101]], 3);
//! let rhs = BitMatrix::from_rows([[0b1], [0b0], [0b1]], 1);
//!
//! assert_eq!(lhs.matmul(&rhs).get(0, 0), 0);
//! assert_eq!(lhs.matmul_or(&rhs).get(0, 0), 1);
//! ```

pub mod bitmatrix;

#[allow(dead_code)]
mod rng;

pub use bitmatrix::BitMatrix;
