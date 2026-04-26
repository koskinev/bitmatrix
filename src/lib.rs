//! Bit matrices with packed storage and GF(2) operations.
//!
//! [`BitMatrix`] stores a dynamic rectangular matrix of bits and supports element access,
//! bitwise combination, transpose, resizing, inversion, and matrix multiplication over GF(2).
//! Internally the matrix is stored as 64 by 64 blocks of packed `u64` row words so bulk
//! operations can work on whole machine words at a time.
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
//! - Multiply matrices over GF(2).
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
//! ```

pub mod bitmatrix;

#[allow(dead_code)]
mod rng;

pub use bitmatrix::BitMatrix;
