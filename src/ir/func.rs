use std::{fmt, io};

use wasmparser::FuncType;

use super::{
    func_header,
    instr::Instr,
    module::Module,
    result_cs_ty,
    ty::{Const, CsType},
    BREAK_DEPTH, LOOP,
};

pub struct Func {
    pub header: FuncHeader,
    pub code: Option<Code>,
    pub in_table: bool,
}

impl Func {
    pub fn write(&self, f: &mut dyn io::Write, module: &Module<'_>) -> io::Result<()> {
        // 関数ヘッダ
        let func_ty = self.header.ty.clone();

        if self.header.export {
            write!(f, "public ")?;
        }

        let code = self.code.as_ref().unwrap();

        let params: Vec<(CsType, &Var)> = func_ty
            .params()
            .iter()
            .map(|&ty| CsType::get(ty))
            .zip(code.var_decls.iter().map(|x| &x.var))
            .collect();
        write!(
            f,
            "{}",
            func_header(&self.header.name, result_cs_ty(func_ty.results()), &params)
        )?;

        writeln!(f, " {{")?;

        writeln!(f, "int {BREAK_DEPTH} = 0;")?;

        // ループ変数
        for i in 0..code.loop_var_count {
            writeln!(f, "bool {LOOP}{i};")?;
        }

        // 一時変数
        for decl in code.var_decls.iter().skip(func_ty.params().len()) {
            match decl.default {
                Some(def) => writeln!(f, "{} {} = {def};", decl.var.ty, decl.var)?,
                None => writeln!(f, "{} {};", decl.var.ty, decl.var)?,
            }
        }

        // 本体
        for instr in &code.instrs {
            instr.write(f, module)?;
        }

        writeln!(f, "}}")?;

        Ok(())
    }
}

pub struct FuncHeader {
    pub name: String,
    pub ty: FuncType,
    pub import: bool,
    pub export: bool,
}

pub struct Code {
    pub instrs: Vec<Instr>,
    pub var_decls: Vec<VarDecl>,
    pub loop_var_count: usize,
}

impl Code {
    pub fn new(header: &FuncHeader) -> Self {
        let mut this = Self {
            instrs: Vec::new(),
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
