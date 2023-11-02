use std::fmt;

use wasmparser::{FuncType, ValType};

use super::{func_header, get_cs_ty, result_cs_ty, BREAK_DEPTH, LOOP};

pub struct Func {
    pub header: FuncHeader,
    pub code: Option<Code>,
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // 関数ヘッダ
        let func_ty = self.header.ty.clone();

        if self.header.export {
            write!(f, "public ")?;
        }

        let code = self.code.as_ref().unwrap();

        let params: Vec<(&str, &Var)> = func_ty
            .params()
            .iter()
            .map(|&ty| get_cs_ty(ty))
            .zip(code.vars.iter())
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
        for var in code.vars.iter().skip(func_ty.params().len()) {
            writeln!(f, "{} {var} = 0;", get_cs_ty(var.ty))?;
        }

        // 本体
        for instr in &code.instrs {
            writeln!(f, "{instr}")?;
        }

        writeln!(f, "}}")?;

        Ok(())
    }
}

pub struct FuncHeader {
    pub name: String,
    pub ty: FuncType,
    pub export: bool,
}

pub struct Code {
    pub instrs: Vec<Instr>,
    pub vars: Vec<Var>,
    pub loop_var_count: usize,
}

impl Code {
    pub fn new(header: &FuncHeader) -> Self {
        let mut this = Self {
            instrs: Vec::new(),
            vars: Vec::new(),
            loop_var_count: 0,
        };

        for &ty in header.ty.params() {
            this.new_var(ty);
        }

        this
    }

    pub fn new_var(&mut self, ty: ValType) -> Var {
        let var = Var {
            index: self.vars.len(),
            ty,
        };
        self.vars.push(var);
        var
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Var {
    pub index: usize,
    pub ty: ValType,
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "var{}", self.index)
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    Line(String),
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Line(x) => write!(f, "{}", x),
        }
    }
}