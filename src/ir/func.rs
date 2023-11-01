use std::fmt;

use std::io::Write;

use anyhow::Result;
use wasmparser::{FuncType, ValType};

use super::{func_header, get_cs_ty, result_cs_ty, BREAK_DEPTH, LOOP};

pub struct Func {
    pub header: FuncHeader,
    pub code: Option<Code>,
}

impl Func {
    pub fn write(&self, out_file: &mut impl Write) -> Result<()> {
        // 関数ヘッダ
        let func_ty = self.header.ty.clone();

        if self.header.export {
            write!(out_file, "public ")?;
        }

        let code = self.code.as_ref().unwrap();

        let params: Vec<(&str, &Var)> = func_ty
            .params()
            .iter()
            .map(|&ty| get_cs_ty(ty))
            .zip(code.vars.iter())
            .collect();
        write!(
            out_file,
            "{}",
            func_header(&self.header.name, result_cs_ty(func_ty.results()), &params)
        )?;

        writeln!(out_file, " {{")?;

        writeln!(out_file, "int {BREAK_DEPTH} = 0;")?;

        // ループ変数
        for i in 0..code.loop_var_count {
            writeln!(out_file, "bool {LOOP}{i};")?;
        }

        // 一時変数
        for var in code.vars.iter().skip(func_ty.params().len()) {
            writeln!(out_file, "{} {var} = 0;", get_cs_ty(var.ty))?;
        }

        // 本体
        for stmt in &code.stmts {
            writeln!(out_file, "{stmt}")?;
        }

        writeln!(out_file, "}}")?;

        Ok(())
    }
}

pub struct FuncHeader {
    pub name: String,
    pub ty: FuncType,
    pub export: bool,
}

pub struct Code {
    pub stmts: Vec<String>,
    pub vars: Vec<Var>,
    pub loop_var_count: usize,
}

impl Code {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            stmts: Vec::new(),
            vars: Vec::new(),
            loop_var_count: 0,
        }
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
