use std::fmt;

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

    pub fn to_i(&self) -> CsType {
        match self {
            CsType::Float => CsType::Int,
            CsType::Double => CsType::Long,
            _ => panic!("Specify float type as argument"),
        }
    }

    pub fn int_bits(&self) -> u32 {
        match self {
            CsType::Int | CsType::UInt => i32::BITS,
            CsType::Long | CsType::ULong => i64::BITS,
            _ => panic!("Specify integer type as argument"),
        }
    }

    pub fn frac_bits(&self) -> u32 {
        match self {
            CsType::Float => f32::MANTISSA_DIGITS - 1,
            CsType::Double => f64::MANTISSA_DIGITS - 1,
            _ => panic!("Specify float type as argument"),
        }
    }

    pub fn cast(&self) -> &'static str {
        match self {
            CsType::Void => panic!("Unsupported conversion"),
            CsType::Bool => "Convert.ToBoolean",
            CsType::Byte => "Convert.ToByte",
            CsType::Int => "Convert.ToInt32",
            CsType::UInt => "Convert.ToUInt32",
            CsType::Long => "Convert.ToInt64",
            CsType::ULong => "Convert.ToUInt64",
            CsType::Float => "Convert.ToSingle",
            CsType::Double => "Convert.ToDouble",
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
