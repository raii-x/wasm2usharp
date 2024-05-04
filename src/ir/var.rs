use std::fmt;

use cranelift_entity::{entity_impl, EntityRef, PrimaryMap};

use super::ty::{Const, CsType};

pub type Vars = PrimaryMap<VarId, Var>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VarId(u32);
entity_impl!(VarId);

#[derive(Clone, Copy)]
pub struct Var {
    pub ty: CsType,
    pub local: bool,
    pub default: Option<Const>,
    pub used: bool,
}

impl Default for Var {
    fn default() -> Self {
        Self {
            ty: CsType::Int,
            local: false,
            default: None,
            used: true,
        }
    }
}

impl fmt::Display for VarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "var{}", self.index())
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum Primary {
    Var(VarId),
    Const(Const),
}

impl Primary {
    pub fn ty(&self, vars: &Vars) -> CsType {
        match self {
            Self::Var(x) => vars[*x].ty,
            Self::Const(x) => x.ty(),
        }
    }
}

impl fmt::Display for Primary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(x) => x.fmt(f),
            Self::Const(x) => x.fmt(f),
        }
    }
}

impl From<VarId> for Primary {
    fn from(value: VarId) -> Self {
        Self::Var(value)
    }
}

impl From<Const> for Primary {
    fn from(value: Const) -> Self {
        Self::Const(value)
    }
}
