// Run with `cargo bench --bench bitmatrix_ops` and options like `--large` and `--no-verify` (see
// `--help`).
use std::env;
use std::hint::black_box;
use std::time::{Duration, Instant};

use bitmatrix::BitMatrix;

const DEFAULT_MIN_SAMPLE_TIME: Duration = Duration::from_millis(250);
const MAX_ITERS: usize = 1024;
const WARMUP_ITERS: usize = 1;

fn main() {
    let config = Config::from_env();

    println!("bitmatrix benchmark harness");
    println!(
        "mode={} large={} huge_inverse={} verify={}",
        config.mode.label(),
        config.include_large,
        config.include_huge_inverse,
        config.verify
    );
    println!();

    bench_transpose(&config);
    bench_matmul(&config);
    bench_matmul_or(&config);
    bench_inverse(&config);
}

#[derive(Clone, Copy, Debug)]
enum Mode {
    Default,
    Extended,
}

impl Mode {
    fn label(self) -> &'static str {
        match self {
            Self::Default => "default",
            Self::Extended => "extended",
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Config {
    include_large: bool,
    include_huge_inverse: bool,
    verify: bool,
    mode: Mode,
}

impl Config {
    fn from_env() -> Self {
        let mut include_large = false;
        let mut include_huge_inverse = false;
        let mut verify = true;
        let mut mode = Mode::Default;

        for arg in env::args().skip(1) {
            match arg.as_str() {
                "--bench" => {}
                "--large" => include_large = true,
                "--huge-inverse" => include_huge_inverse = true,
                "--no-verify" => verify = false,
                "--extended" => mode = Mode::Extended,
                "--default" => mode = Mode::Default,
                "--help" | "-h" => {
                    print_help();
                    std::process::exit(0);
                }
                other => {
                    eprintln!("unrecognized argument: {other}");
                    print_help();
                    std::process::exit(2);
                }
            }
        }

        Self {
            include_large,
            include_huge_inverse,
            verify,
            mode,
        }
    }
}

fn print_help() {
    println!("usage: cargo bench --bench bitmatrix -- [--large] [--extended] [--no-verify]");
    println!("  --large      enable extra-large benchmark cases, especially 10k-scale shapes");
    println!("  --huge-inverse enable very large inverse cases, including 10k-scale squares");
    println!("  --extended   run longer samples for more stable numbers");
    println!("  --no-verify  skip correctness checks outside timed regions");
}

#[derive(Clone, Copy)]
struct BenchCase {
    name: &'static str,
    dims: Dims,
}

#[derive(Clone, Copy)]
struct Dims {
    rows: usize,
    cols: usize,
    shared: usize,
}

impl Dims {
    fn logical_bits(self) -> u128 {
        self.rows as u128 * self.cols as u128
    }

    fn multiply_work(self) -> u128 {
        self.rows as u128 * self.shared as u128 * self.cols as u128
    }
}

struct Summary {
    iterations: usize,
    total: Duration,
    best: Duration,
}

impl Summary {
    fn average_seconds(&self) -> f64 {
        self.total.as_secs_f64() / self.iterations as f64
    }

    fn best_seconds(&self) -> f64 {
        self.best.as_secs_f64()
    }

    fn ops_per_second(&self) -> f64 {
        1.0 / self.average_seconds()
    }
}

fn bench_transpose(config: &Config) {
    println!("== transpose ==");
    println!("case                 iter      avg ms     best ms    ops/s    MiB/s    density");

    for case in transpose_cases(config.include_large) {
        let matrix = patterned_matrix(case.dims.rows, case.dims.cols, 0xA5A5_0000_0000_0001);
        if config.verify {
            verify_transpose(&matrix);
        }

        let summary = measure(config.mode, || black_box(matrix.transposed()));
        let mib_per_second = mib_per_second(case.dims.logical_bits(), summary.average_seconds());
        println!(
            "{:<20} {:>5} {:>11.3} {:>11.3} {:>8.2} {:>8.2} {:>9.4}",
            case.name,
            summary.iterations,
            millis(summary.average_seconds()),
            millis(summary.best_seconds()),
            summary.ops_per_second(),
            mib_per_second,
            density(&matrix),
        );
    }
    println!();
}

fn bench_matmul(config: &Config) {
    println!("== matmul gf(2) ==");
    println!("case                 iter      avg ms     best ms    ops/s    Gbit-op/s  out dens");

    for case in matmul_cases(config.include_large) {
        let lhs = patterned_matrix(case.dims.rows, case.dims.shared, 0xB100_0000_0000_0001);
        let rhs = patterned_matrix(case.dims.shared, case.dims.cols, 0xB100_0000_0000_1001);

        if config.verify {
            verify_matmul(&lhs, &rhs);
        }

        let summary = measure(config.mode, || black_box(lhs.matmul(&rhs)));
        let sample = lhs.matmul(&rhs);
        println!(
            "{:<20} {:>5} {:>11.3} {:>11.3} {:>8.2} {:>10.3} {:>9.4}",
            case.name,
            summary.iterations,
            millis(summary.average_seconds()),
            millis(summary.best_seconds()),
            summary.ops_per_second(),
            giga_units_per_second(case.dims.multiply_work(), summary.average_seconds()),
            density(&sample),
        );
    }
    println!();
}

fn bench_matmul_or(config: &Config) {
    println!("== matmul or/and ==");
    println!("case                 iter      avg ms     best ms    ops/s    Gbit-op/s  out dens");

    for case in matmul_cases(config.include_large) {
        let lhs = patterned_matrix(case.dims.rows, case.dims.shared, 0xC200_0000_0000_0001);
        let rhs = patterned_matrix(case.dims.shared, case.dims.cols, 0xC200_0000_0000_1001);

        if config.verify {
            verify_matmul_or(&lhs, &rhs);
        }

        let summary = measure(config.mode, || black_box(lhs.matmul_or(&rhs)));
        let sample = lhs.matmul_or(&rhs);
        println!(
            "{:<20} {:>5} {:>11.3} {:>11.3} {:>8.2} {:>10.3} {:>9.4}",
            case.name,
            summary.iterations,
            millis(summary.average_seconds()),
            millis(summary.best_seconds()),
            summary.ops_per_second(),
            giga_units_per_second(case.dims.multiply_work(), summary.average_seconds()),
            density(&sample),
        );
    }
    println!();
}

fn bench_inverse(config: &Config) {
    println!("== inverse gf(2) ==");
    println!("case                 algo      iter      avg ms     best ms    mats/s   inv dens");

    for case in inverse_cases(config.include_large, config.include_huge_inverse) {
        let matrix = invertible_matrix(
            case.dims.rows,
            0xD300_0000_0000_0001 ^ case.dims.rows as u64,
        );

        if config.verify {
            verify_inverse(&matrix);
        }

        let summary = measure(config.mode, || black_box(matrix.try_inverse()));
        let inverse = matrix
            .try_inverse()
            .expect("generated matrix should invert");
        let algorithm = if case.dims.rows >= 512 {
            "m4ri"
        } else {
            "gauss"
        };
        println!(
            "{:<20} {:<8} {:>5} {:>11.3} {:>11.3} {:>8.2} {:>9.4}",
            case.name,
            algorithm,
            summary.iterations,
            millis(summary.average_seconds()),
            millis(summary.best_seconds()),
            summary.ops_per_second(),
            density(&inverse),
        );
    }
    println!();
}

fn measure<F, T>(mode: Mode, mut f: F) -> Summary
where
    F: FnMut() -> T,
{
    for _ in 0..WARMUP_ITERS {
        black_box(f());
    }

    let target = match mode {
        Mode::Default => DEFAULT_MIN_SAMPLE_TIME,
        Mode::Extended => DEFAULT_MIN_SAMPLE_TIME.saturating_mul(3),
    };

    let mut iterations = 0;
    let mut total = Duration::ZERO;
    let mut best = Duration::MAX;

    while iterations < MAX_ITERS && (iterations == 0 || total < target) {
        let start = Instant::now();
        black_box(f());
        let elapsed = start.elapsed();
        iterations += 1;
        total += elapsed;
        best = best.min(elapsed);
    }

    Summary {
        iterations,
        total,
        best,
    }
}

fn transpose_cases(include_large: bool) -> Vec<BenchCase> {
    let mut cases = vec![
        BenchCase {
            name: "transpose-256x384",
            dims: Dims {
                rows: 256,
                cols: 384,
                shared: 0,
            },
        },
        BenchCase {
            name: "transpose-1024x1536",
            dims: Dims {
                rows: 1_024,
                cols: 1_536,
                shared: 0,
            },
        },
        BenchCase {
            name: "transpose-4096x4096",
            dims: Dims {
                rows: 4_096,
                cols: 4_096,
                shared: 0,
            },
        },
    ];

    if include_large {
        cases.push(BenchCase {
            name: "transpose-8192x12288",
            dims: Dims {
                rows: 8_192,
                cols: 12_288,
                shared: 0,
            },
        });
        cases.push(BenchCase {
            name: "transpose-12288x12288",
            dims: Dims {
                rows: 12_288,
                cols: 12_288,
                shared: 0,
            },
        });
    }

    cases
}

fn matmul_cases(include_large: bool) -> Vec<BenchCase> {
    let mut cases = vec![
        BenchCase {
            name: "mul-256x256x256",
            dims: Dims {
                rows: 256,
                shared: 256,
                cols: 256,
            },
        },
        BenchCase {
            name: "mul-1024x1024x1024",
            dims: Dims {
                rows: 1_024,
                shared: 1_024,
                cols: 1_024,
            },
        },
        BenchCase {
            name: "mul-2048x2048x2048",
            dims: Dims {
                rows: 2_048,
                shared: 2_048,
                cols: 2_048,
            },
        },
    ];

    if include_large {
        cases.push(BenchCase {
            name: "mul-2048x12288x512",
            dims: Dims {
                rows: 2_048,
                shared: 12_288,
                cols: 512,
            },
        });
        cases.push(BenchCase {
            name: "mul-512x12288x2048",
            dims: Dims {
                rows: 512,
                shared: 12_288,
                cols: 2_048,
            },
        });
    }

    cases
}

fn inverse_cases(include_large: bool, include_huge_inverse: bool) -> Vec<BenchCase> {
    let mut cases = vec![
        BenchCase {
            name: "inv-24",
            dims: Dims {
                rows: 24,
                cols: 24,
                shared: 24,
            },
        },
        BenchCase {
            name: "inv-96",
            dims: Dims {
                rows: 96,
                cols: 96,
                shared: 96,
            },
        },
        BenchCase {
            name: "inv-384",
            dims: Dims {
                rows: 384,
                cols: 384,
                shared: 384,
            },
        },
        BenchCase {
            name: "inv-1024",
            dims: Dims {
                rows: 1_024,
                cols: 1_024,
                shared: 1_024,
            },
        },
    ];

    if include_large {
        cases.push(BenchCase {
            name: "inv-2048",
            dims: Dims {
                rows: 2_048,
                cols: 2_048,
                shared: 2_048,
            },
        });
        cases.push(BenchCase {
            name: "inv-4096",
            dims: Dims {
                rows: 4_096,
                cols: 4_096,
                shared: 4_096,
            },
        });
    }

    if include_huge_inverse {
        cases.push(BenchCase {
            name: "inv-12288",
            dims: Dims {
                rows: 12_288,
                cols: 12_288,
                shared: 12_288,
            },
        });
    }

    cases
}

fn patterned_matrix(rows: usize, cols: usize, seed: u64) -> BitMatrix {
    BitMatrix::from_fn(rows, cols, |row, col| {
        let mixed = mix64(seed ^ ((row as u64) << 32) ^ col as u64);
        ((mixed ^ (mixed >> 11) ^ (mixed >> 29)) & 1) != 0
    })
}

fn invertible_matrix(n: usize, seed: u64) -> BitMatrix {
    let lower = BitMatrix::from_fn(n, n, |row, col| {
        if row == col {
            true
        } else if row > col {
            ((mix64(seed ^ ((row as u64) << 32) ^ col as u64) >> 7) & 1) != 0
        } else {
            false
        }
    });
    let upper = BitMatrix::from_fn(n, n, |row, col| {
        if row == col {
            true
        } else if row < col {
            ((mix64(seed.rotate_left(13) ^ ((row as u64) << 32) ^ col as u64) >> 19) & 1) != 0
        } else {
            false
        }
    });
    lower.matmul(&upper)
}

fn verify_transpose(matrix: &BitMatrix) {
    let transposed = matrix.transposed();
    assert_eq!(transposed.dimensions(), (matrix.cols(), matrix.rows()));
    let checkpoints = [
        (0, 0),
        (matrix.rows() / 2, matrix.cols() / 2),
        (
            matrix.rows().saturating_sub(1),
            matrix.cols().saturating_sub(1),
        ),
    ];
    for (row, col) in checkpoints {
        if row < matrix.rows() && col < matrix.cols() {
            assert_eq!(matrix.get(row, col), transposed.get(col, row));
        }
    }
}

fn verify_matmul(lhs: &BitMatrix, rhs: &BitMatrix) {
    let product = lhs.matmul(rhs);
    let rows = lhs.rows().min(3);
    let cols = rhs.cols().min(3);
    let shared = lhs.cols();

    for row in 0..rows {
        for col in 0..cols {
            let mut bit = 0;
            for index in 0..shared {
                bit ^= lhs.get(row, index) & rhs.get(index, col);
            }
            assert_eq!(
                bit,
                product.get(row, col),
                "matmul mismatch at ({row}, {col})"
            );
        }
    }
}

fn verify_matmul_or(lhs: &BitMatrix, rhs: &BitMatrix) {
    let product = lhs.matmul_or(rhs);
    let rows = lhs.rows().min(3);
    let cols = rhs.cols().min(3);
    let shared = lhs.cols();

    for row in 0..rows {
        for col in 0..cols {
            let mut bit = 0;
            for index in 0..shared {
                bit |= lhs.get(row, index) & rhs.get(index, col);
            }
            assert_eq!(
                bit,
                product.get(row, col),
                "matmul_or mismatch at ({row}, {col})"
            );
        }
    }
}

fn verify_inverse(matrix: &BitMatrix) {
    let inverse = matrix
        .try_inverse()
        .expect("generated matrix should invert");
    let identity = BitMatrix::identity(matrix.rows());
    assert_eq!(matrix.matmul(&inverse), identity);
}

fn density(matrix: &BitMatrix) -> f64 {
    let bits = matrix.rows() as f64 * matrix.cols() as f64;
    if bits == 0.0 {
        0.0
    } else {
        matrix.count_ones() as f64 / bits
    }
}

fn millis(seconds: f64) -> f64 {
    seconds * 1_000.0
}

fn mib_per_second(bits: u128, seconds: f64) -> f64 {
    let bytes = bits as f64 / 8.0;
    bytes / seconds / (1024.0 * 1024.0)
}

fn giga_units_per_second(work: u128, seconds: f64) -> f64 {
    work as f64 / seconds / 1_000_000_000.0
}

fn mix64(mut value: u64) -> u64 {
    value ^= value >> 30;
    value = value.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    value ^= value >> 27;
    value = value.wrapping_mul(0x94D0_49BB_1331_11EB);
    value ^ (value >> 31)
}
