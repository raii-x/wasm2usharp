use std::fmt;

use num_traits::Float;
use wasmparser::ValType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CsType {
    Void,
    Bool,
    Byte,
    Int,
    UInt,
    Long,
    ULong,
    Float,
    Double,
}

impl CsType {
    pub fn get(ty: ValType) -> Self {
        match ty {
            ValType::I32 => Self::Int,
            ValType::I64 => Self::Long,
            ValType::F32 => Self::Float,
            ValType::F64 => Self::Double,
            _ => panic!("Unsupported type"),
        }
    }

    pub fn val_type(&self) -> ValType {
        match self {
            Self::Int => ValType::I32,
            Self::Long => ValType::I64,
            Self::Float => ValType::F32,
            Self::Double => ValType::F64,
            _ => panic!("Unsupported type"),
        }
    }

    pub fn to_unsigned(&self) -> Self {
        match self {
            Self::Int => Self::UInt,
            Self::Long => Self::ULong,
            _ => panic!("Can't convert to unsigned"),
        }
    }

    pub fn signed(&self) -> bool {
        match self {
            Self::Int | Self::Long | Self::Float | Self::Double => true,
            Self::Byte | Self::UInt | Self::ULong => false,
            _ => panic!("Unsupported type"),
        }
    }

    pub fn to_i(&self) -> Self {
        match self {
            Self::Float => Self::Int,
            Self::Double => Self::Long,
            _ => panic!("Specify float type as argument"),
        }
    }

    pub fn int_bits(&self) -> u32 {
        match self {
            Self::Int | Self::UInt => i32::BITS,
            Self::Long | Self::ULong => i64::BITS,
            _ => panic!("Specify integer type as argument"),
        }
    }

    pub fn frac_bits(&self) -> u32 {
        match self {
            Self::Float => f32::MANTISSA_DIGITS - 1,
            Self::Double => f64::MANTISSA_DIGITS - 1,
            _ => panic!("Specify float type as argument"),
        }
    }

    pub fn cast(&self) -> &'static str {
        match self {
            Self::Void => panic!("Unsupported conversion"),
            Self::Bool => "Convert.ToBoolean",
            Self::Byte => "Convert.ToByte",
            Self::Int => "Convert.ToInt32",
            Self::UInt => "Convert.ToUInt32",
            Self::Long => "Convert.ToInt64",
            Self::ULong => "Convert.ToUInt64",
            Self::Float => "Convert.ToSingle",
            Self::Double => "Convert.ToDouble",
        }
    }

    pub fn default(&self) -> Const {
        match self {
            Self::Void => panic!("Unsupported type"),
            Self::Bool => Const::Bool(false),
            Self::Byte => Const::Byte(0),
            Self::Int => Const::Int(0),
            Self::UInt => Const::UInt(0),
            Self::Long => Const::Long(0),
            Self::ULong => Const::ULong(0),
            Self::Float => Const::Float(0.),
            Self::Double => Const::Double(0.),
        }
    }
}

impl fmt::Display for CsType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Void => "void",
                Self::Bool => "bool",
                Self::Byte => "byte",
                Self::Int => "int",
                Self::UInt => "uint",
                Self::Long => "long",
                Self::ULong => "ulong",
                Self::Float => "float",
                Self::Double => "double",
            }
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Const {
    Bool(bool),
    Byte(u8),
    Int(i32),
    UInt(u32),
    Long(i64),
    ULong(u64),
    Float(f32),
    Double(f64),
}

impl Const {
    pub fn ty(&self) -> CsType {
        match self {
            Self::Bool(_) => CsType::Bool,
            Self::Byte(_) => CsType::Byte,
            Self::Int(_) => CsType::Int,
            Self::UInt(_) => CsType::UInt,
            Self::Long(_) => CsType::Long,
            Self::ULong(_) => CsType::ULong,
            Self::Float(_) => CsType::Float,
            Self::Double(_) => CsType::Double,
        }
    }
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(x) => write!(f, "{}", x),
            Self::Byte(x) => write!(f, "{}", x),
            Self::Int(x) => match *x {
                i32::MIN => write!(f, "({} - 1)", i32::MIN + 1),
                _x => write!(f, "{}", _x),
            },
            Self::UInt(x) => write!(f, "{}u", x),
            Self::Long(x) => match *x {
                i64::MIN => write!(f, "({}L - 1L)", i64::MIN + 1),
                _x => write!(f, "{}L", _x),
            },
            Self::ULong(x) => write!(f, "{}ul", x),
            Self::Float(x) => {
                fmt_float(*x, CsType::Float, f).unwrap_or_else(|| write!(f, "{:e}f", *x))
            }
            Self::Double(x) => {
                fmt_float(*x, CsType::Float, f).unwrap_or_else(|| write!(f, "{:e}", *x))
            }
        }
    }
}

fn fmt_float(value: impl Float, ty: CsType, f: &mut fmt::Formatter<'_>) -> Option<fmt::Result> {
    if value.is_infinite() {
        Some(if value.is_sign_positive() {
            write!(f, "{ty}.PositiveInfinity")
        } else {
            write!(f, "{ty}.NegativeInfinity")
        })
    } else if value.is_nan() {
        Some(write!(f, "{ty}.NaN"))
    } else {
        None
    }
}
