use std::array;

const MAGIC_INCR: u64 = 0x_A076_1D64_78BD_642F;
const MAGIC_XOR: u64 = 0x_E703_7ED1_A0B4_28DB;

/// Shuffles the elements of the slice `data` using the Fisher-Yates algorithm and the given
/// random number generator.
pub fn shuffle<T, G>(data: &mut [T], rng: &mut G)
where
    usize: BoundedRandom<G>,
{
    let mut index = data.len();
    while index > 1 {
        let other = usize::bounded_random(rng, 0, index);
        data.swap(index - 1, other);
        index -= 1;
    }
}

#[inline]
/// Returns a `u64` using `x` as the seed for the wyrand algorithm.
pub fn wyhash(x: u64) -> u64 {
    let mut a = x;
    let mut b = x ^ MAGIC_XOR; // 0x_E703_7ED1_A0B4_28DB
    let r = (a as u128) * (b as u128);
    a = r as u64;
    b = (r >> 64) as u64;
    a ^ b
}

pub trait U64Generator {
    fn u64(&mut self) -> u64;
}

pub trait Random<R> {
    fn random(rng: &mut R) -> Self;
}

pub trait BoundedRandom<R> {
    fn bounded_random(rng: &mut R, low: Self, high: Self) -> Self;
}

#[derive(Clone)]
/// A pseudorandom number generator that uses the wyrand algorithm.
pub struct WyRng {
    /// The current state of the RNG.
    state: u64,
}

impl WyRng {
    pub fn bounded<T>(&mut self, low: T, high: T) -> T
    where
        T: BoundedRandom<Self>,
    {
        T::bounded_random(self, low, high)
    }

    /// Returns a new PRNG initialized with the given seed. If the seed is set to 0, the seed is
    /// based on the address of the PRNG. This should yield an unique sequence for each run of
    /// the program.
    pub fn new(mut seed: u64) -> Self {
        let mut rng = Self { state: MAGIC_INCR };
        #[cfg(not(debug_assertions))]
        if seed == 0 {
            use std::time::{SystemTime, UNIX_EPOCH};
            seed = &rng as *const Self as u64;
            if let Ok(elapsed) = SystemTime::now().duration_since(UNIX_EPOCH) {
                seed ^= ((elapsed.as_nanos() << 64) >> 64) as u64;
            };
        }
        #[cfg(debug_assertions)]
        if seed == 0 {
            seed = 123456789123456789;
        }
        rng.state += seed;
        rng
    }

    /// Returns a random value of type `T`. For integers, the value is in the range `[T::MIN,
    /// T::MAX]`. For floating-point numbers, the value is in the range `[0, 1)`.
    pub fn random<T>(&mut self) -> T
    where
        T: Random<Self>,
    {
        T::random(self)
    }

    /// Sets the seed of the PRNG.
    pub fn seed(&mut self, x: u64) {
        let state = MAGIC_INCR.wrapping_add(x);
        self.state = state;
    }

    /// Returns a `u64`.
    fn u64(&mut self) -> u64 {
        self.state = self.state.wrapping_add(MAGIC_INCR);
        wyhash(self.state)
    }
}

impl Default for WyRng {
    fn default() -> Self {
        Self::new(0)
    }
}

impl U64Generator for WyRng {
    fn u64(&mut self) -> u64 {
        self.u64()
    }
}

impl<G> Random<G> for f32
where
    G: U64Generator,
{
    fn random(rng: &mut G) -> Self {
        ((rng.u64() >> 40) as f32) * (-24_f32).exp2()
    }
}

impl<G> BoundedRandom<G> for f32
where
    G: U64Generator,
{
    fn bounded_random(rng: &mut G, low: Self, high: Self) -> Self {
        assert!(low < high);
        let value = f32::random(rng);
        let range = high - low;
        assert!(range > 0.0);
        value * range + low
    }
}

impl<G> Random<G> for f64
where
    G: U64Generator,
{
    fn random(rng: &mut G) -> Self {
        ((rng.u64() >> 11) as f64) * (-53_f64).exp2()

        // // Ported from http://mumble.net/~campbell/tmp/random_real.c
        // let mut exponent = -64;
        // let mut significand;
        
        // loop {
        //     significand = rng.u64();
        //     if significand != 0 {
        //         break;
        //     }
        //     exponent -= 64;
        //     if exponent < -1074 {
        //         return 0.0;
        //     }
        // }
    
        // let shift = significand.leading_zeros();
        // if shift != 0 {
        //     exponent -= shift as i32;
        //     significand <<= shift;
        //     significand |= rng.u64() >> (64 - shift);
        // }
    
        // significand |= 1;
    
        // (significand as f64) * (2.0f64).powi(exponent)
    }
}

impl<G> BoundedRandom<G> for f64
where
    G: U64Generator,
{
    fn bounded_random(rng: &mut G, low: Self, high: Self) -> Self {
        assert!(low < high);
        let value = f64::random(rng);
        let range = high - low;
        assert!(range > 0.0);
        value * range + low
    }
}

impl<G> Random<G> for i8
where
    G: U64Generator,
{
    fn random(rng: &mut G) -> Self {
        rng.u64() as _
    }
}

impl<G> BoundedRandom<G> for i8
where
    G: U64Generator,
{
    fn bounded_random(rng: &mut G, low: Self, high: Self) -> Self {
        assert!(low < high);
        let range = high.abs_diff(low);
        let x = u8::bounded_random(rng, 0, range);
        if x > i8::MAX as u8 {
            (x - low as u8) as _
        } else {
            (x + low as u8) as _
        }
    }
}

impl<G> Random<G> for i16
where
    G: U64Generator,
{
    fn random(rng: &mut G) -> Self {
        rng.u64() as _
    }
}

impl<G> BoundedRandom<G> for i16
where
    G: U64Generator,
{
    fn bounded_random(rng: &mut G, low: Self, high: Self) -> Self {
        assert!(low < high);
        let range = high.abs_diff(low);
        let x = u16::bounded_random(rng, 0, range);
        if x > i16::MAX as u16 {
            (x - low as u16) as _
        } else {
            (x + low as u16) as _
        }
    }
}

impl<G> Random<G> for i32
where
    G: U64Generator,
{
    fn random(rng: &mut G) -> Self {
        rng.u64() as _
    }
}

impl<G> BoundedRandom<G> for i32
where
    G: U64Generator,
{
    fn bounded_random(rng: &mut G, low: Self, high: Self) -> Self {
        assert!(low < high);
        let range = high.abs_diff(low);
        let x = u32::bounded_random(rng, 0, range);
        if x > i32::MAX as u32 {
            (x - low as u32) as _
        } else {
            (x + low as u32) as _
        }
    }
}

impl<G> Random<G> for i64
where
    G: U64Generator,
{
    fn random(rng: &mut G) -> Self {
        rng.u64() as _
    }
}

impl<G> BoundedRandom<G> for i64
where
    G: U64Generator,
{
    fn bounded_random(rng: &mut G, low: Self, high: Self) -> Self {
        assert!(low < high);
        let range = high.abs_diff(low);
        let x = u64::bounded_random(rng, 0, range);
        if x > i64::MAX as u64 {
            (x - low as u64) as _
        } else {
            (x + low as u64) as _
        }
    }
}

impl<G> Random<G> for i128
where
    G: U64Generator,
{
    fn random(rng: &mut G) -> Self {
        ((rng.u64() as u128) << 64 | rng.u64() as u128) as _
    }
}

impl<G> BoundedRandom<G> for i128
where
    G: U64Generator,
{
    fn bounded_random(rng: &mut G, low: Self, high: Self) -> Self {
        assert!(low < high);
        let range = high.abs_diff(low);
        let x = u128::bounded_random(rng, 0, range);
        if x > i128::MAX as u128 {
            (x - low as u128) as _
        } else {
            (x + low as u128) as _
        }
    }
}

impl<G> Random<G> for isize
where
    G: U64Generator,
{
    fn random(rng: &mut G) -> Self {
        match core::mem::size_of::<usize>() {
            4 => (rng.u64() >> 32) as _,
            8 => rng.u64() as _,
            16 => ((rng.u64() as u128) << 64 | rng.u64() as u128) as _,
            _ => panic!("Unsupported usize size"),
        }
    }
}

impl<G> BoundedRandom<G> for isize
where
    G: U64Generator,
{
    fn bounded_random(rng: &mut G, low: Self, high: Self) -> Self {
        assert!(low < high);
        let range = high.abs_diff(low);
        let x = usize::bounded_random(rng, 0, range);
        if x > isize::MAX as usize {
            (x - low as usize) as _
        } else {
            (x + low as usize) as _
        }
    }
}

impl<G> Random<G> for u8
where
    G: U64Generator,
{
    fn random(rng: &mut G) -> Self {
        rng.u64() as _
    }
}

impl<G> BoundedRandom<G> for u8
where
    G: U64Generator,
{
    fn bounded_random(rng: &mut G, low: Self, high: Self) -> Self {
        assert!(low < high);
        let range = high - low;
        let mut x = u8::random(rng);
        let mut m = (x as u16) * (range as u16);
        let mut l = m as u8;
        if l < range {
            let mut t = u8::MAX - range;
            if t >= range {
                t -= range;
                if t >= range {
                    t %= range;
                }
            }
            while l < t {
                x = u8::random(rng);
                m = (x as u16) * (range as u16);
                l = m as u8;
            }
        }
        (m >> 8) as u8 + low
    }
}

impl<G> Random<G> for u16
where
    G: U64Generator,
{
    fn random(rng: &mut G) -> Self {
        rng.u64() as _
    }
}

impl<G> BoundedRandom<G> for u16
where
    G: U64Generator,
{
    fn bounded_random(rng: &mut G, low: Self, high: Self) -> Self {
        assert!(low < high);
        let range = high - low;
        let mut x = u16::random(rng);
        let mut m = (x as u32) * (range as u32);
        let mut l = m as u16;
        if l < range {
            let mut t = u16::MAX - range;
            if t >= range {
                t -= range;
                if t >= range {
                    t %= range;
                }
            }
            while l < t {
                x = u16::random(rng);
                m = (x as u32) * (range as u32);
                l = m as u16;
            }
        }
        (m >> 16) as u16 + low
    }
}

impl<G> Random<G> for u32
where
    G: U64Generator,
{
    fn random(rng: &mut G) -> Self {
        rng.u64() as _
    }
}

impl<G> BoundedRandom<G> for u32
where
    G: U64Generator,
{
    fn bounded_random(rng: &mut G, low: Self, high: Self) -> Self {
        assert!(low < high);
        let range = high - low;
        let mut x = u32::random(rng);
        let mut m = (x as u64) * (range as u64);
        let mut l = m as u32;
        if l < range {
            let mut t = u32::MAX - range;
            if t >= range {
                t -= range;
                if t >= range {
                    t %= range;
                }
            }
            while l < t {
                x = u32::random(rng);
                m = (x as u64) * (range as u64);
                l = m as u32;
            }
        }
        (m >> 32) as u32 + low
    }
}

impl<G> Random<G> for u64
where
    G: U64Generator,
{
    fn random(rng: &mut G) -> Self {
        rng.u64()
    }
}

impl<G> BoundedRandom<G> for u64
where
    G: U64Generator,
{
    fn bounded_random(rng: &mut G, low: Self, high: Self) -> Self {
        assert!(low < high);
        let range = high - low;
        let mut x = rng.u64();
        let mut m = (x as u128) * (range as u128);
        let mut l = m as u64;
        if l < range {
            let mut t = u64::MAX - range;
            if t >= range {
                t -= range;
                if t >= range {
                    t %= range;
                }
            }
            while l < t {
                x = rng.u64();
                m = (x as u128) * (range as u128);
                l = m as u64;
            }
        }
        (m >> 64) as u64 + low
    }
}

impl<G> Random<G> for u128
where
    G: U64Generator,
{
    fn random(rng: &mut G) -> Self {
        (rng.u64() as u128) << 64 | rng.u64() as u128
    }
}

impl<G> BoundedRandom<G> for u128
where
    G: U64Generator,
{
    fn bounded_random(rng: &mut G, low: Self, high: Self) -> Self {
        assert!(low < high);
        let range = high - low;
        let mask = u128::MAX >> range.leading_zeros();
        loop {
            let value = u128::random(rng);
            let x = value & mask;
            if x < range {
                return x + low;
            }
        }
    }
}

impl<G> Random<G> for usize
where
    G: U64Generator,
{
    fn random(rng: &mut G) -> Self {
        match core::mem::size_of::<usize>() {
            4 => (rng.u64() >> 32) as _,
            8 => rng.u64() as _,
            16 => ((rng.u64() as u128) << 64 | rng.u64() as u128) as _,
            _ => panic!("Unsupported usize size"),
        }
    }
}

impl<G> BoundedRandom<G> for usize
where
    G: U64Generator,
{
    fn bounded_random(rng: &mut G, low: Self, high: Self) -> Self {
        assert!(low < high);
        match core::mem::size_of::<usize>() {
            4 => u32::bounded_random(rng, low as u32, high as u32) as usize,
            8 => u64::bounded_random(rng, low as u64, high as u64) as usize,
            16 => u128::bounded_random(rng, low as u128, high as u128) as usize,
            _ => panic!("Unsupported usize size"),
        }
    }
}

impl<G, T, const N: usize> Random<G> for [T; N]
where
    T: Copy + Random<G>,
{
    fn random(rng: &'_ mut G) -> Self {
        array::from_fn(|_| T::random(rng))
    }
}

impl<G, T, const N: usize> BoundedRandom<G> for [T; N]
where
    T: Copy + BoundedRandom<G>,
{
    fn bounded_random(rng: &'_ mut G, low: Self, high: Self) -> Self {
        array::from_fn(|i| T::bounded_random(rng, low[i], high[i]))
    }
}