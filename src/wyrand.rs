/// A pseudorandom number generator that uses the WyRand algorithm.
pub struct WyRng {
    /// The current state of the RNG.
    state: u64,
}

impl WyRng {
    /// Returns a `u8` in the range `[low, high)`.
    pub fn bounded_u8(&mut self, low: u8, high: u8) -> u8 {
        let range = high - low;
        let mut x = self.u8();
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
                x = self.u8();
                m = (x as u16) * (range as u16);
                l = m as u8;
            }
        }
        (m >> 8) as u8 + low
    }

    /// Returns a `u16` in the range `[low, high)`.
    pub fn bounded_u16(&mut self, low: u16, high: u16) -> u16 {
        let range = high - low;
        let mut x = self.u16();
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
                x = self.u16();
                m = (x as u32) * (range as u32);
                l = m as u16;
            }
        }
        (m >> 16) as u16 + low
    }

    /// Returns a `u32` in the range `[low, high)`.
    pub fn bounded_u32(&mut self, low: u32, high: u32) -> u32 {
        let range = high - low;
        let mut x = self.u32();
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
                x = self.u32();
                m = (x as u64) * (range as u64);
                l = m as u32;
            }
        }
        (m >> 32) as u32 + low
    }

    /// Returns a `u64` in the range `[low, high)`.
    pub fn bounded_u64(&mut self, low: u64, high: u64) -> u64 {
        debug_assert!(low < high);

        let range = high - low;
        let mut x = self.u64();
        let mut m = (x as u128) * (range as u128);
        let mut l = m as u64;
        if l < range {
            let mut t = u64::MAX - range;
            t -= range * (t >= range) as u64;
            if t >= range {
                t %= range;
            }
            while l < t {
                x = self.u64();
                m = (x as u128) * (range as u128);
                l = m as u64;
            }
        }
        (m >> 64) as u64 + low
    }

    /// Returns a `u128` in the range `[low, high)`.
    pub fn bounded_u128(&mut self, low: u128, high: u128) -> u128 {
        let range = high - low;
        let mask = u128::MAX >> range.leading_zeros();
        loop {
            let x = self.u128() & mask;
            if x < range {
                return x + low;
            }
        }
    }

    /// Returns an array of `N` distinct `usize` values in the range `[0, bound)`.
    pub fn distinct_usizes<const N: usize>(&mut self, max: usize) -> [usize; N] {
        let mut result = [0; N];
        for i in 0..N {
            result[i] = self.bounded_usize(0, max);
            while result[..i].iter().any(|&x| x == result[i]) {
                result[i] = self.bounded_usize(0, max);
            }
        }
        result
    }

    /// Returns a `usize` in the range `[0, bound)`.
    pub fn bounded_usize(&mut self, low: usize, high: usize) -> usize {
        match core::mem::size_of::<usize>() {
            4 => self.bounded_u32(low as u32, high as u32) as usize,
            8 => self.bounded_u64(low as u64, high as u64) as usize,
            16 => self.bounded_u128(low as u128, high as u128) as usize,
            _ => panic!("Unsupported usize size"),
        }
    }

    /// Returns a `f64` in the range `[0, 1)`.
    pub fn f64(&mut self) -> f64 {
        ((self.u64() >> 11) as f64) * (-53_f64).exp2()
    }

    /// Returns a new PRNG initialized with the given seed. If the seed is set to 0, the seed is
    /// based on the address of the PRNG. This should yield an unique sequence for each run of
    /// the program.
    pub fn new(mut seed: u64) -> Self {
        let mut rng = Self { state: 0 };
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
        rng.state = rng.state.wrapping_add(seed);
        rng
    }

    /// Shuffles the elements of the slice `data` using the Fisher-Yates algorithm.
    pub fn shuffle<T>(&mut self, data: &mut [T]) {
        let mut index = data.len();
        while index > 1 {
            let other = self.bounded_usize(0, index);
            data.swap(index - 1, other);
            index -= 1;
        }
    }

    /// Returns a `u8`.
    pub fn u8(&mut self) -> u8 {
        (self.u64() >> 56) as u8
    }

    /// Returns a `u16`.
    pub fn u16(&mut self) -> u16 {
        (self.u64() >> 48) as u16
    }

    /// Returns a `u32`.
    pub fn u32(&mut self) -> u32 {
        (self.u64() >> 32) as u32
    }

    /// Returns a `u64`.
    pub fn u64(&mut self) -> u64 {
        self.state = self.state.wrapping_add(0x_a076_1d64_78bd_642f);
        Self::wyhash(self.state)
    }

    /// Returns a `u128`.
    pub fn u128(&mut self) -> u128 {
        (self.u64() as u128) << 64 | self.u64() as u128
    }

    /// Returns a `usize`.
    pub fn usize(&mut self) -> usize {
        match core::mem::size_of::<usize>() {
            4 => self.u32() as usize,
            8 => self.u64() as usize,
            16 => self.u128() as usize,
            _ => panic!("Unsupported usize size"),
        }
    }

    /// Returns a `u64` using `x` as the seed for the wyrand algorithm.
    fn wyhash(x: u64) -> u64 {
        let mut a = x;
        let mut b = x ^ 0x_e703_7ed1_a0b4_28db;
        let r = (a as u128) * (b as u128);
        a = r as u64;
        b = (r >> 64) as u64;
        a ^ b
    }
}

#[allow(dead_code)]
/// Trait for generating pseudorandom numbers.
pub trait PRng<T> {
    /// Returns a pseudorandom number.
    fn next(&mut self) -> T;

    /// Returns an iterator that generates pseudorandom numbers.
    fn iter(&mut self) -> impl Iterator<Item = T> + '_ {
        core::iter::from_fn(|| Some(self.next()))
    }

    /// Returns a pseudorandom number in the range `[low, high)`. Panics if `low >= high`.
    fn next_bounded(&mut self, low: T, high: T) -> T;

    /// Returns an iterator that generates pseudorandom numbers in the range `[low, high)`. Panics
    /// if `low >= high`.
    fn iter_bounded<'a>(&'a mut self, low: T, high: T) -> impl Iterator<Item = T> + '_
    where
        T: Copy + 'a,
    {
        core::iter::from_fn(move || Some(self.next_bounded(low, high)))
    }
}

impl Default for WyRng {
    fn default() -> Self {
        Self::new(0)
    }
}

impl PRng<u8> for WyRng {
    fn next(&mut self) -> u8 {
        self.u8()
    }

    fn next_bounded(&mut self, low: u8, high: u8) -> u8 {
        self.bounded_u8(low, high)
    }
}

impl PRng<u16> for WyRng {
    fn next(&mut self) -> u16 {
        self.u16()
    }

    fn next_bounded(&mut self, low: u16, high: u16) -> u16 {
        self.bounded_u16(low, high)
    }
}

impl PRng<u32> for WyRng {
    fn next(&mut self) -> u32 {
        self.u32()
    }

    fn next_bounded(&mut self, low: u32, high: u32) -> u32 {
        self.bounded_u32(low, high)
    }
}

impl PRng<u64> for WyRng {
    fn next(&mut self) -> u64 {
        self.u64()
    }

    fn next_bounded(&mut self, low: u64, high: u64) -> u64 {
        self.bounded_u64(low, high)
    }
}

impl PRng<u128> for WyRng {
    fn next(&mut self) -> u128 {
        self.u128()
    }

    fn next_bounded(&mut self, low: u128, high: u128) -> u128 {
        self.bounded_u128(low, high)
    }
}

impl PRng<usize> for WyRng {
    fn next(&mut self) -> usize {
        self.usize()
    }

    fn next_bounded(&mut self, low: usize, high: usize) -> usize {
        self.bounded_usize(low, high)
    }
}

impl<T, const N: usize> PRng<[T; N]> for WyRng
where
    WyRng: PRng<T>,
    T: Copy,
{
    fn next(&mut self) -> [T; N] {
        let mut result = core::array::from_fn(|_| self.next());
        self.shuffle(&mut result);
        result
    }

    fn next_bounded(&mut self, low: [T; N], high: [T; N]) -> [T; N] {
        let mut result = [low[0]; N];
        for i in 0..N {
            result[i] = self.next_bounded(low[i], high[i]);
        }
        result
    }
}