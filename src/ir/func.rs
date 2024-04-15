use std::fmt;

use wasmparser::FuncType;

use super::{
    instr::InstrNode,
    ty::{Const, CsType},
};

pub struct Func {
    pub header: FuncHeader,
    pub code: Option<Code>,
    pub in_table: bool,
}

pub struct FuncHeader {
    pub name: String,
    pub ty: FuncType,
    pub import: bool,
    pub export: bool,
}

pub struct Code {
    pub instr_nodes: Vec<InstrNode>,
    pub vars: FuncVars,
}

pub struct FuncVars {
    pub var_decls: Vec<VarDecl>,
    pub loop_var_count: usize,
}

impl FuncVars {
    pub fn new(header: &FuncHeader) -> Self {
        let mut this = Self {
            var_decls: Vec::new(),
            loop_var_count: 0,
        };

        for &ty in header.ty.params() {
            this.new_var_with_local(CsType::get(ty), None, true);
        }

        this
    }

    fn new_var_with_local(&mut self, ty: CsType, default: Option<Const>, local: bool) -> Var {
        let var = Var {
            index: self.var_decls.len(),
            ty,
            local,
        };
        self.var_decls.push(VarDecl { var, default });
        var
    }

    pub fn new_var(&mut self, ty: CsType, default: Option<Const>) -> Var {
        self.new_var_with_local(ty, default, false)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct VarDecl {
    pub var: Var,
    pub default: Option<Const>,
}

#[derive(Debug, Clone, Copy)]
pub struct Var {
    pub index: usize,
    pub ty: CsType,
    pub local: bool,
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "var{}", self.index)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Primary {
    Var(Var),
    Const(Const),
}

impl Primary {
    pub fn ty(&self) -> CsType {
        match self {
            Self::Var(x) => x.ty,
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

impl From<Var> for Primary {
    fn from(value: Var) -> Self {
        Self::Var(value)
    }
}

impl From<Const> for Primary {
    fn from(value: Const) -> Self {
        Self::Const(value)
    }
}
