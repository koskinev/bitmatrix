use std::ops::BitOr;

pub trait BitShuffle {
    /// Moves the bits of the value to the right according to `mask`.
    ///
    /// ```text
    ///   x <- abcd_efgh_ijkl_mnop
    ///   m <- 1100_1100_0011_0011
    ///   c <- x.compress(m)
    ///   c == 0000_0000_abef_klop
    /// ```
    ///
    /// This operation is also known as "gather". The inverse is [`BitShuffle::expand`].
    fn compress(self, mask: Self) -> Self;

    /// Moves the bits of the value to the left according to `mask`.
    ///
    /// ```text
    ///   x <- abcd_efgh_ijkl_mnop
    ///   m <- 1100_1100_0011_0011
    ///   c <- x.compress(m)
    ///   c == abef_klop_0000_0000
    /// ```
    fn compress_left(self, mask: Self) -> Self;

    /// Moves the bits of the value to the right according to `mask`.
    /// ```text
    ///   x <- 0000_0000_abcd_efgh
    ///   m <- 1100_1100_0011_0011
    ///   e <- x.expand(m)
    ///   e == ab00_cd00_ef00_gh00
    /// ```
    ///
    /// The inverse of this operation is [`BitShuffle::compress`].
    fn expand(self, mask: Self) -> Self;

    /// Moves all bits marked with `1`s in the mask to the right and all bits marked with `0`s to
    /// the left.
    /// ```text
    ///   x <- abcd_efgh_ijkl_mnop
    ///   m <- 1100_1100_0011_0011
    ///   c <- x.unshuffle(m)
    ///   c == abef_klop_cdgh_ijmn
    /// ```
    ///
    /// This is also known as the "sheeps and goats" operation.
    fn unshuffle(self, mask: Self) -> Self
    where
        Self: Copy + BitOr<Output = Self>,
    {
        self.compress_left(mask) | self.compress(mask)
    }
}

impl BitShuffle for u8 {
    fn compress(mut self, mask: Self) -> Self {
        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
        {
            if is_x86_feature_detected!("bmi2") {
                unsafe {
                    #[cfg(target_arch = "x86")]
                    use core::arch::x86::_pext_u32;
                    #[cfg(target_arch = "x86_64")]
                    use core::arch::x86_64::_pext_u32;

                    return _pext_u32(self as u32, mask as u32) as u8;
                }
            }
        }

        let mut m = mask;
        let mut mk: Self;
        let mut mp: Self;
        let mut mv: Self;
        let mut t: Self;

        self &= m;
        mk = !m << 1;

        for i in 0..3 {
            mp = mk ^ (mk << 1);
            mp = mp ^ (mp << 2);
            mp = mp ^ (mp << 4);
            mv = mp & m;
            m = (m ^ mv) | (mv >> (1 << i));
            t = self & mv;
            self = self ^ t | (t >> (1 << i));
            mk &= !mp;
        }
        self
    }

    fn compress_left(mut self, mask: Self) -> Self {
        self = self.compress(mask);
        self << mask.count_ones()
    }

    fn expand(self, mask: Self) -> Self {
        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
        {
            if is_x86_feature_detected!("bmi2") {
                unsafe {
                    #[cfg(target_arch = "x86")]
                    use core::arch::x86::_pdep_u32;
                    #[cfg(target_arch = "x86_64")]
                    use core::arch::x86_64::_pdep_u32;

                    return _pdep_u32(self as u32, mask as u32) as Self;
                }
            }
        }

        use std::mem::MaybeUninit;

        let mut x = self;
        let mut m = mask;

        let mut mk: Self;
        let mut mp: Self;
        let mut mv: Self;
        let mut t: Self;

        mk = !m << 1;

        let mut array = [MaybeUninit::<Self>::uninit(); 3];
        for (i, elem) in array.iter_mut().enumerate() {
            mp = mk ^ (mk << 1);
            mp = mp ^ (mp << 2);
            mp = mp ^ (mp << 4);

            mv = mp & m;
            elem.write(mv);
            m = (m ^ mv) | (mv >> (1 << i));
            mk &= !mp;
        }
        for (i, elem) in array.iter().enumerate().rev() {
            mv = unsafe { elem.assume_init() };
            t = x << (1 << i);
            x = (x & !mv) | (t & mv);
        }
        x & mask
    }
}

impl BitShuffle for u16 {
    fn compress(mut self, mask: Self) -> Self {
        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
        {
            if is_x86_feature_detected!("bmi2") {
                unsafe {
                    #[cfg(target_arch = "x86")]
                    use core::arch::x86::_pext_u32;
                    #[cfg(target_arch = "x86_64")]
                    use core::arch::x86_64::_pext_u32;

                    return _pext_u32(self as u32, mask as u32) as u16;
                }
            }
        }

        let mut m = mask;
        let mut mk: Self;
        let mut mp: Self;
        let mut mv: Self;
        let mut t: Self;

        self &= m;
        mk = !m << 1;

        for i in 0..4 {
            mp = mk ^ (mk << 1);
            mp = mp ^ (mp << 2);
            mp = mp ^ (mp << 4);
            mp = mp ^ (mp << 8);
            mv = mp & m;
            m = (m ^ mv) | (mv >> (1 << i));
            t = self & mv;
            self = self ^ t | (t >> (1 << i));
            mk &= !mp;
        }
        self
    }

    fn compress_left(mut self, mask: Self) -> Self {
        self = self.compress(mask);
        self << mask.count_zeros()
    }

    fn expand(self, mask: Self) -> Self {
        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
        {
            if is_x86_feature_detected!("bmi2") {
                unsafe {
                    #[cfg(target_arch = "x86")]
                    use core::arch::x86::_pdep_u32;
                    #[cfg(target_arch = "x86_64")]
                    use core::arch::x86_64::_pdep_u32;

                    return _pdep_u32(self as u32, mask as u32) as Self;
                }
            }
        }

        use std::mem::MaybeUninit;

        let mut x = self;
        let mut m = mask;

        let mut mk: Self;
        let mut mp: Self;
        let mut mv: Self;
        let mut t: Self;

        mk = !m << 1;

        let mut array = [MaybeUninit::<Self>::uninit(); 4];
        for (i, elem) in array.iter_mut().enumerate() {
            mp = mk ^ (mk << 1);
            mp = mp ^ (mp << 2);
            mp = mp ^ (mp << 4);
            mp = mp ^ (mp << 8);

            mv = mp & m;
            elem.write(mv);
            m = (m ^ mv) | (mv >> (1 << i));
            mk &= !mp;
        }
        for (i, elem) in array.iter().enumerate().rev() {
            mv = unsafe { elem.assume_init() };
            t = x << (1 << i);
            x = (x & !mv) | (t & mv);
        }
        x & mask
    }
}

impl BitShuffle for u32 {
    fn compress(mut self, mask: Self) -> Self {
        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
        {
            if is_x86_feature_detected!("bmi2") {
                unsafe {
                    #[cfg(target_arch = "x86")]
                    use core::arch::x86::_pext_u32;
                    #[cfg(target_arch = "x86_64")]
                    use core::arch::x86_64::_pext_u32;

                    return _pext_u32(self, mask);
                }
            }
        }

        let mut m = mask;
        let mut mk: Self;
        let mut mp: Self;
        let mut mv: Self;
        let mut t: Self;

        self &= m;
        mk = !m << 1;

        for i in 0..5 {
            mp = mk ^ (mk << 1);
            mp = mp ^ (mp << 2);
            mp = mp ^ (mp << 4);
            mp = mp ^ (mp << 8);
            mp = mp ^ (mp << 16);
            mv = mp & m;
            m = (m ^ mv) | (mv >> (1 << i));
            t = self & mv;
            self = self ^ t | (t >> (1 << i));
            mk &= !mp;
        }
        self
    }

    fn compress_left(mut self, mask: Self) -> Self {
        self = self.compress(mask);
        self << mask.count_ones()
    }

    fn expand(self, mask: Self) -> Self {
        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
        {
            if is_x86_feature_detected!("bmi2") {
                unsafe {
                    #[cfg(target_arch = "x86")]
                    use core::arch::x86::_pdep_u32;
                    #[cfg(target_arch = "x86_64")]
                    use core::arch::x86_64::_pdep_u32;

                    return _pdep_u32(self, mask) as Self;
                }
            }
        }

        use std::mem::MaybeUninit;

        let mut x = self;
        let mut m = mask;

        let mut mk: Self;
        let mut mp: Self;
        let mut mv: Self;
        let mut t: Self;

        mk = !m << 1;

        let mut array = [MaybeUninit::<Self>::uninit(); 4];
        for (i, elem) in array.iter_mut().enumerate() {
            mp = mk ^ (mk << 1);
            mp = mp ^ (mp << 2);
            mp = mp ^ (mp << 4);
            mp = mp ^ (mp << 8);

            mv = mp & m;
            elem.write(mv);
            m = (m ^ mv) | (mv >> (1 << i));
            mk &= !mp;
        }
        for (i, elem) in array.iter().enumerate().rev() {
            mv = unsafe { elem.assume_init() };
            t = x << (1 << i);
            x = (x & !mv) | (t & mv);
        }
        x & mask
    }
}

impl BitShuffle for u64 {
    fn compress(mut self, mask: Self) -> Self {
        #[cfg(target_arch = "x86_64")]
        {
            if is_x86_feature_detected!("bmi2") {
                unsafe {
                    use core::arch::x86_64::_pext_u64;
                    return _pext_u64(self, mask);
                }
            }
        }

        let mut m = mask;
        let mut mk: Self;
        let mut mp: Self;
        let mut mv: Self;
        let mut t: Self;

        self &= m;
        mk = !m << 1;

        for i in 0..6 {
            mp = mk ^ (mk << 1);
            mp = mp ^ (mp << 2);
            mp = mp ^ (mp << 4);
            mp = mp ^ (mp << 8);
            mp = mp ^ (mp << 16);
            mp = mp ^ (mp << 32);
            mv = mp & m;
            m = (m ^ mv) | (mv >> (1 << i));
            t = self & mv;
            self = self ^ t | (t >> (1 << i));
            mk &= !mp;
        }
        self
    }

    fn compress_left(mut self, mask: Self) -> Self {
        self = self.compress(mask);
        self << mask.count_ones()
    }

    fn expand(self, mask: Self) -> Self {
        #[cfg(target_arch = "x86_64")]
        {
            if is_x86_feature_detected!("bmi2") {
                unsafe {
                    #[cfg(target_arch = "x86_64")]
                    use core::arch::x86_64::_pdep_u64;

                    return _pdep_u64(self, mask);
                }
            }
        }

        use std::mem::MaybeUninit;

        let mut x = self;
        let mut m = mask;

        let mut mk: Self;
        let mut mp: Self;
        let mut mv: Self;
        let mut t: Self;

        mk = !m << 1;

        let mut array = [MaybeUninit::<Self>::uninit(); 6];
        for (i, elem) in array.iter_mut().enumerate() {
            mp = mk ^ (mk << 1);
            mp = mp ^ (mp << 2);
            mp = mp ^ (mp << 4);
            mp = mp ^ (mp << 8);
            mp = mp ^ (mp << 16);
            mp = mp ^ (mp << 32);

            mv = mp & m;
            elem.write(mv);
            m = (m ^ mv) | (mv >> (1 << i));
            mk &= !mp;
        }

        for (i, elem) in array.iter().enumerate().rev() {
            mv = unsafe { elem.assume_init() };
            t = x << (1 << i);
            x = (x & !mv) | (t & mv);
        }
        x & mask
    }
}

impl BitShuffle for u128 {
    fn compress(mut self, mask: Self) -> Self {
        #[cfg(target_arch = "x86_64")]
        {
            if is_x86_feature_detected!("bmi2") && is_x86_feature_detected!("popcnt") {
                unsafe {
                    use core::arch::x86_64::{_pext_u64, _popcnt64};

                    //       |   hi | lo
                    // ------+------+--------------------------------------
                    // self  | hgfe | dcba
                    // mask  | 0111 | 0110
                    // LOW   | 0000 | 1111
                    // lo    |      | dcba (self & LOW)
                    // hi    |      | hgfe (self >> 64)
                    // m_lo  |      | 0110
                    // m_hi  |      | 0111 (mask >> 64)
                    // r_lo  |      | 00ba (_pext_u64(lo, m_lo))
                    // r_hi  | 000g | fe00 (_pext_u64(hi, m_hi) << _popcnt64(m_lo))
                    // r     | 000g | feba r_hi | r_lo

                    const LOW: u128 = u128::MAX >> 64;
                    let (lo, hi) = ((self & LOW) as u64, (self >> 64) as u64);
                    let (m_lo, m_hi) = ((mask & LOW) as u64, (mask >> 64) as u64);

                    let r_lo = _pext_u64(lo, m_lo) as u128;
                    let r_hi = (_pext_u64(hi, m_hi) as u128) << _popcnt64(m_lo as i64);

                    return r_hi | r_lo;
                }
            }
        }

        let mut m = mask;
        let mut mk: Self;
        let mut mp: Self;
        let mut mv: Self;
        let mut t: Self;

        self &= m;
        mk = !m << 1;

        for i in 0..7 {
            mp = mk ^ (mk << 1);
            mp = mp ^ (mp << 2);
            mp = mp ^ (mp << 4);
            mp = mp ^ (mp << 8);
            mp = mp ^ (mp << 16);
            mp = mp ^ (mp << 32);
            mp = mp ^ (mp << 64);
            mv = mp & m;
            m = (m ^ mv) | (mv >> (1 << i));
            t = self & mv;
            self = self ^ t | (t >> (1 << i));
            mk &= !mp;
        }
        self
    }

    fn compress_left(mut self, mask: Self) -> Self {
        self = self.compress(mask);
        self << mask.count_ones()
    }

    fn expand(self, mask: Self) -> Self {
        #[cfg(target_arch = "x86_64")]
        {
            if is_x86_feature_detected!("bmi2") && is_x86_feature_detected!("popcnt") {
                unsafe {
                    use core::arch::x86_64::{_pdep_u64, _popcnt64};

                    //       |   hi | lo
                    // ------+------+--------------------------------------
                    // self  | hgfe | dcba
                    // mask  | 0111 | 0110
                    // LOW   | 0000 | 1111
                    // m_lo  |      | 0110
                    // r_lo  |      | 0ba0 (_pdep_u64(a & LOW, m_lo))
                    // shl   | 00hg | fedc (a >> _popcnt64(mask & LOW))
                    // r_hi  |     | 0gfe (_pdep_u64(a_shl & LOW, mask >> 64))
                    // r     | 0gfe | 0ba0 (r_hi << 64 | r_lo)

                    const LOW: u128 = u128::MAX >> 64;
                    let m_lo = (mask & LOW) as u64;
                    let r_lo = _pdep_u64((self & LOW) as u64, m_lo) as u128;
                    let shl = self >> _popcnt64(m_lo as i64);
                    let r_hi = _pdep_u64((shl & LOW) as u64, m_lo) as u128;
                    return (r_hi << 64) | r_lo;
                }
            }
        }

        use std::mem::MaybeUninit;

        let mut x = self;
        let mut m = mask;

        let mut mk: Self;
        let mut mp: Self;
        let mut mv: Self;
        let mut t: Self;

        mk = !m << 1;

        if size_of::<Self>() == 1 {
            // 3 steps
            let mut array = [MaybeUninit::<Self>::uninit(); 3];
            for (i, elem) in array.iter_mut().enumerate() {
                mp = mk ^ (mk << 1);
                mp = mp ^ (mp << 2);
                mp = mp ^ (mp << 4);

                mv = mp & m;
                elem.write(mv);
                m = (m ^ mv) | (mv >> (1 << i));
                mk &= !mp;
            }
            for (i, elem) in array.iter().enumerate().rev() {
                mv = unsafe { elem.assume_init() };
                t = x << (1 << i);
                x = (x & !mv) | (t & mv);
            }
            x & mask
        } else if size_of::<Self>() == 2 {
            // 4 steps
            let mut array = [MaybeUninit::<Self>::uninit(); 4];
            for (i, elem) in array.iter_mut().enumerate() {
                mp = mk ^ (mk << 1);
                mp = mp ^ (mp << 2);
                mp = mp ^ (mp << 4);
                mp = mp ^ (mp << 8);

                mv = mp & m;
                elem.write(mv);
                m = (m ^ mv) | (mv >> (1 << i));
                mk &= !mp;
            }
            for (i, elem) in array.iter().enumerate().rev() {
                mv = unsafe { elem.assume_init() };
                t = x << (1 << i);
                x = (x & !mv) | (t & mv);
            }
            x & mask
        } else if size_of::<Self>() == 4 {
            // 5 steps
            let mut array = [MaybeUninit::<Self>::uninit(); 5];
            for (i, elem) in array.iter_mut().enumerate() {
                mp = mk ^ (mk << 1);
                mp = mp ^ (mp << 2);
                mp = mp ^ (mp << 4);
                mp = mp ^ (mp << 8);
                mp = mp ^ (mp << 16);

                mv = mp & m;
                elem.write(mv);
                m = (m ^ mv) | (mv >> (1 << i));
                mk &= !mp;
            }
            for (i, elem) in array.iter().enumerate().rev() {
                mv = unsafe { elem.assume_init() };
                t = x << (1 << i);
                x = (x & !mv) | (t & mv);
            }
            x & mask
        } else if size_of::<Self>() == 8 {
            // 6 steps
            let mut array = [MaybeUninit::<Self>::uninit(); 6];
            for (i, elem) in array.iter_mut().enumerate() {
                mp = mk ^ (mk << 1);
                mp = mp ^ (mp << 2);
                mp = mp ^ (mp << 4);
                mp = mp ^ (mp << 8);
                mp = mp ^ (mp << 16);
                mp = mp ^ (mp << 32);

                mv = mp & m;
                elem.write(mv);
                m = (m ^ mv) | (mv >> (1 << i));
                mk &= !mp;
            }

            for (i, elem) in array.iter().enumerate().rev() {
                mv = unsafe { elem.assume_init() };
                t = x << (1 << i);
                x = (x & !mv) | (t & mv);
            }
            x & mask
        } else if size_of::<Self>() == 16 {
            // 7 steps
            let mut array = [MaybeUninit::<Self>::uninit(); 7];
            for (i, elem) in array.iter_mut().enumerate() {
                mp = mk ^ (mk << 1);
                mp = mp ^ (mp << 2);
                mp = mp ^ (mp << 4);
                mp = mp ^ (mp << 8);
                mp = mp ^ (mp << 16);
                mp = mp ^ (mp << 32);
                mp = mp ^ (mp << 64);

                mv = mp & m;
                elem.write(mv);
                m = (m ^ mv) | (mv >> (1 << i));
                mk &= !mp;
            }
            for (i, elem) in array.iter().enumerate().rev() {
                mv = unsafe { elem.assume_init() };
                t = x << (1 << i);
                x = (x & !mv) | (t & mv);
            }
            x & mask
        } else {
            std::unimplemented!("not implemented for bit size {}", size_of::<Self>());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::BitShuffle;

    #[test]
    fn test_features() {
        #[cfg(target_arch = "x86")]
        {
            print!("x86 ");
        }
        #[cfg(target_arch = "x86_64")]
        {
            print!("x86_64 ");
        }

        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
        {
            if is_x86_feature_detected!("bmi2") {
                print!("bmi2");
            } else {
                print!("no bmi2");
            }
        }
        println!()
    }

    #[test]
    fn test_compress() {
        // Test u8
        let u: u8 = 0b1010_1010;
        let m: u8 = 0b1100_1100;

        let expect: u8 = 0b0000_1010;
        let expect_left: u8 = 0b1010_0000;
        assert_eq!(u.compress(m), expect);
        assert_eq!(u.compress_left(m), expect_left);

        // Test u16
        let u: u16 = 0b1010_1010_1010_1010;
        let m: u16 = 0b1100_1100_1100_1100;

        let expect: u16 = 0b0000_0000_1010_1010;
        let expect_left: u16 = 0b1010_1010_0000_0000;
        assert_eq!(u.compress(m), expect);
        assert_eq!(u.compress_left(m), expect_left);

        // Test u32
        let u: u32 = 0b1010_1010_1010_1010_1010_1010_1010_1010;
        let m: u32 = 0b1100_1100_1100_1100_1100_1100_1100_1100;

        let expect: u32 = 0b0000_0000_0000_0000_1010_1010_1010_1010;
        let expect_left: u32 = 0b1010_1010_1010_1010_0000_0000_0000_0000;
        assert_eq!(u.compress(m), expect);
        assert_eq!(u.compress_left(m), expect_left);

        // Test u64
        let u: u64 = 0xAAAA_AAAA_AAAA_AAAA;
        let m: u64 = 0xCCCC_CCCC_CCCC_CCCC;

        let expect: u64 = 0x0000_0000_AAAA_AAAA;
        let expect_left: u64 = 0xAAAA_AAAA_0000_0000;
        assert_eq!(u.compress(m), expect);
        assert_eq!(u.compress_left(m), expect_left);

        // Test u128
        let u: u128 = 0xAAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAAA;
        let m: u128 = 0xCCCC_CCCC_CCCC_CCCC_CCCC_CCCC_CCCC_CCCC;

        let expect: u128 = 0x0000_0000_0000_0000_AAAA_AAAA_AAAA_AAAA;
        let expect_left: u128 = 0xAAAA_AAAA_AAAA_AAAA_0000_0000_0000_0000;
        assert_eq!(u.compress(m), expect);
        assert_eq!(u.compress_left(m), expect_left);
    }

    #[test]
    fn uint_expand() {
        // Test u8
        let u: u8 = 0b0000_1010;
        let m: u8 = 0b1010_1010;
        let expect: u8 = 0b1000_1000;
        assert_eq!(u.expand(m), expect);

        // Test u16
        let u: u16 = 0b0000_0000_1010_1010;
        let m: u16 = 0b1010_1010_1010_1010;
        let expect: u16 = 0b1000_1000_1000_1000;
        assert_eq!(u.expand(m), expect);

        // Test u32
        let u: u32 = 0b0000_0000_0000_0000_1010_1010_1010_1010;
        let m: u32 = 0b1010_1010_1010_1010_1010_1010_1010_1010;
        let expect: u32 = 0b1000_1000_1000_1000_1000_1000_1000_1000;
        assert_eq!(u.expand(m), expect);

        // Test u64
        let u: u64 = 0x0000_0000_AAAA_AAAA;
        let m: u64 = 0xAAAA_AAAA_AAAA_AAAA;
        let expect: u64 = 0x8888_8888_8888_8888;
        assert_eq!(u.expand(m), expect);

        // Test u128
        let u: u128 = 0x0000_0000_0000_0000_AAAA_AAAA_AAAA_AAAA;
        let m: u128 = 0xAAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAAA;
        let expect: u128 = 0x8888_8888_8888_8888_8888_8888_8888_8888;
        assert_eq!(u.expand(m), expect);
    }

    #[test]
    fn uint_unshuffle() {
        // Test u8
        let u: u8 = 0b0000_1111;
        let m: u8 = 0b1010_1010;
        let expect: u8 = 0b0011_0011;
        let got = u.unshuffle(m);
        assert_eq!(got, expect);

        // Test u16
        let u: u16 = 0b0000_0000_1111_1111;
        let m: u16 = 0b1010_1010_1010_1010;
        let expect: u16 = 0b0000_1111_0000_1111;
        let got = u.unshuffle(m);
        assert_eq!(got, expect);

        // Test u32
        let u: u32 = 0b0000_0000_0000_0000_1111_1111_1111_1111;
        let m: u32 = 0b1010_1010_1010_1010_1010_1010_1010_1010;
        let expect: u32 = 0b0000_0000_1111_1111_0000_0000_1111_1111;
        let got = u.unshuffle(m);
        assert_eq!(got, expect);

        // Test u64
        let u: u64 = 0x0000_0000_FFFF_FFFF;
        let m: u64 = 0xAAAA_AAAA_AAAA_AAAA;
        let expect: u64 = 0x0000_FFFF_0000_FFFF;
        let got = u.unshuffle(m);
        assert_eq!(got, expect);

        // Test u128
        let u: u128 = 0x0000_0000_0000_0000_FFFF_FFFF_FFFF_FFFF;
        let m: u128 = 0xAAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAAA;
        let expect: u128 = 0x0000_0000_FFFF_FFFF_0000_0000_FFFF_FFFF;
        let got = u.unshuffle(m);
        assert_eq!(got, expect);
    }
}
