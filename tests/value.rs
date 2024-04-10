use float_cmp::approx_eq;
use wasm2usharp::util::bit_mask;
use wast::{
    core::{NanPattern, WastArgCore, WastRetCore},
    token::{Float32, Float64},
};

pub fn wast_ret_core_contains<'a>(ret: &WastRetCore<'a>, arg: &WastArgCore<'a>) -> bool {
    use {WastArgCore as Arg, WastRetCore as Ret};
    match (ret, arg) {
        (Ret::I32(l), Arg::I32(r)) => l == r,
        (Ret::I64(l), Arg::I64(r)) => l == r,
        (Ret::F32(ret), Arg::F32(arg)) => nan_pattern_contains(
            ret,
            arg,
            |l, r| {
                approx_eq!(
                    f32,
                    f32::from_bits(l.bits),
                    f32::from_bits(r.bits),
                    ulps = 1
                )
            },
            f32_to_nan_pattern,
        ),
        (Ret::F64(l), Arg::F64(r)) => nan_pattern_contains(
            l,
            r,
            |l, r| {
                approx_eq!(
                    f64,
                    f64::from_bits(l.bits),
                    f64::from_bits(r.bits),
                    ulps = 1
                )
            },
            f64_to_nan_pattern,
        ),
        _ => false,
    }
}

fn nan_pattern_contains<T, F, F2>(ret: &NanPattern<T>, arg: &T, eq: F, to_nan_pattern: F2) -> bool
where
    T: Copy,
    F: FnOnce(T, T) -> bool,
    F2: FnOnce(T) -> NanPattern<T>,
{
    use NanPattern::*;
    match ret {
        CanonicalNan => {
            matches!(to_nan_pattern(*arg), NanPattern::CanonicalNan)
        }
        ArithmeticNan => {
            matches!(
                to_nan_pattern(*arg),
                NanPattern::ArithmeticNan | NanPattern::CanonicalNan
            )
        }
        Value(val) => eq(*val, *arg),
    }
}

fn f32_to_nan_pattern(float: Float32) -> NanPattern<Float32> {
    match f_to_nan_pattern(float.bits as u64, 32, (f32::MANTISSA_DIGITS - 1) as u64) {
        NanPattern::CanonicalNan => NanPattern::CanonicalNan,
        NanPattern::ArithmeticNan => NanPattern::ArithmeticNan,
        NanPattern::Value(bits) => NanPattern::Value(Float32 { bits: bits as u32 }),
    }
}

fn f64_to_nan_pattern(float: Float64) -> NanPattern<Float64> {
    match f_to_nan_pattern(float.bits, 64, (f64::MANTISSA_DIGITS - 1) as u64) {
        NanPattern::CanonicalNan => NanPattern::CanonicalNan,
        NanPattern::ArithmeticNan => NanPattern::ArithmeticNan,
        NanPattern::Value(bits) => NanPattern::Value(Float64 { bits }),
    }
}

fn f_to_nan_pattern(bits: u64, size_bits: u64, frac_bits: u64) -> NanPattern<u64> {
    let expo_bits = size_bits - 1 - frac_bits;
    let expo_mask = bit_mask(expo_bits) << frac_bits;

    if bits & expo_mask == expo_mask {
        // Arithmetic NaNの定義は、仮数部のMSBが1であるNaNだが、
        // 生成されるC#コードでArithmetic NaNを生成すべき箇所で生成しない場合があるため、
        // ここではCanonical NaN以外の全てのNaNをArithmetic NaNとしている。
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
