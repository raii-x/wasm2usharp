use float_cmp::approx_eq;
use wasm2usharp::util::bit_mask;
use wast::{
    core::{NanPattern, WastRetCore},
    token::{Float32, Float64},
    WastRet,
};

pub fn f32_to_wasm_ret_core<'a>(bits: u32) -> WastRetCore<'a> {
    WastRetCore::F32(
        match f_to_wasm_ret_core(bits as u64, 32, (f32::MANTISSA_DIGITS - 1) as u64) {
            NanPattern::CanonicalNan => NanPattern::CanonicalNan,
            NanPattern::ArithmeticNan => NanPattern::ArithmeticNan,
            NanPattern::Value(bits) => NanPattern::Value(Float32 { bits: bits as u32 }),
        },
    )
}

pub fn f64_to_wasm_ret_core<'a>(bits: u64) -> WastRetCore<'a> {
    WastRetCore::F64(
        match f_to_wasm_ret_core(bits, 64, (f64::MANTISSA_DIGITS - 1) as u64) {
            NanPattern::CanonicalNan => NanPattern::CanonicalNan,
            NanPattern::ArithmeticNan => NanPattern::ArithmeticNan,
            NanPattern::Value(bits) => NanPattern::Value(Float64 { bits }),
        },
    )
}

fn f_to_wasm_ret_core(bits: u64, size_bits: u64, frac_bits: u64) -> NanPattern<u64> {
    let expo_bits = size_bits - 1 - frac_bits;
    let expo_mask = bit_mask(expo_bits) << frac_bits;

    if bits & expo_mask == expo_mask {
        let frac = bits & bit_mask(frac_bits);
        match frac {
            0 => NanPattern::Value(bits), // 無限大
            x if x == 1 << (frac_bits - 1) => NanPattern::CanonicalNan,
            _ => NanPattern::ArithmeticNan,
        }
    } else {
        NanPattern::Value(bits)
    }
}

#[derive(Debug)]
pub struct WastRetEq<'a>(pub WastRet<'a>);

impl<'a> WastRetEq<'a> {
    pub fn into_canonical_nan(self) -> Self {
        Self(match self.0 {
            WastRet::Core(x) => WastRet::Core(match x {
                WastRetCore::F32(x) => {
                    WastRetCore::F32(nan_pattern(x, |x| f32::from_bits(x.bits).is_nan()))
                }
                WastRetCore::F64(x) => {
                    WastRetCore::F64(nan_pattern(x, |x| f64::from_bits(x.bits).is_nan()))
                }
                _other => _other,
            }),
            _other => _other,
        })
    }
}

fn nan_pattern<T, F>(pattern: NanPattern<T>, is_nan: F) -> NanPattern<T>
where
    T: Copy,
    F: FnOnce(T) -> bool,
{
    match pattern {
        NanPattern::CanonicalNan => NanPattern::CanonicalNan,
        NanPattern::ArithmeticNan => NanPattern::CanonicalNan,
        NanPattern::Value(value) => {
            if is_nan(value) {
                NanPattern::CanonicalNan
            } else {
                NanPattern::Value(value)
            }
        }
    }
}

impl<'a> PartialEq for WastRetEq<'a> {
    fn eq(&self, other: &Self) -> bool {
        wast_ret_eq(&self.0, &other.0)
    }
}

fn wast_ret_eq(lhs: &WastRet, rhs: &WastRet) -> bool {
    use WastRet::*;
    match (lhs, rhs) {
        (Core(l0), Core(r0)) => wast_ret_core_eq(l0, r0),
        (Component(_), Component(_)) => {
            panic!("component-model is not supported")
        }
        _ => false,
    }
}

fn wast_ret_core_eq(lhs: &WastRetCore, rhs: &WastRetCore) -> bool {
    use WastRetCore::*;
    match (lhs, rhs) {
        (I32(l0), I32(r0)) => l0 == r0,
        (I64(l0), I64(r0)) => l0 == r0,
        (F32(l0), F32(r0)) => nan_pattern_eq(l0, r0, |l, r| {
            approx_eq!(
                f32,
                f32::from_bits(l.bits),
                f32::from_bits(r.bits),
                ulps = 2
            )
        }),
        (F64(l0), F64(r0)) => nan_pattern_eq(l0, r0, |l, r| {
            approx_eq!(
                f64,
                f64::from_bits(l.bits),
                f64::from_bits(r.bits),
                ulps = 2
            )
        }),
        _ => false,
    }
}

fn nan_pattern_eq<T, F>(lhs: &NanPattern<T>, rhs: &NanPattern<T>, eq: F) -> bool
where
    T: Copy,
    F: FnOnce(T, T) -> bool,
{
    use NanPattern::*;
    match (lhs, rhs) {
        (CanonicalNan, CanonicalNan) => true,
        (ArithmeticNan, ArithmeticNan) => true,
        (Value(l0), Value(r0)) => eq(*l0, *r0),
        _ => false,
    }
}
