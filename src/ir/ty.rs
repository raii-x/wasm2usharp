use std::fmt;

use wasmparser::ValType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CsType {
    Void,
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
}

impl fmt::Display for CsType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Void => "void",
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
