use std::{fmt, io};

use wasmparser::FuncType;

use crate::ir::{STACK, STACK_TOP};

use super::{
    func_header,
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
            this.new_var(CsType::get(ty), None);
        }

        this
    }

    pub fn new_var(&mut self, ty: CsType, default: Option<Const>) -> Var {
        let var = Var {
            index: self.var_decls.len(),
            ty,
        };
        self.var_decls.push(VarDecl { var, default });
        var
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
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "var{}", self.index)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Expr {
    Var(Var),
    Const(Const),
}

impl Expr {
    pub fn ty(&self) -> CsType {
        match self {
            Self::Var(x) => x.ty,
            Self::Const(x) => x.ty(),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(x) => x.fmt(f),
            Self::Const(x) => x.fmt(f),
        }
    }
}

impl From<Var> for Expr {
    fn from(value: Var) -> Self {
        Self::Var(value)
    }
}

impl From<Const> for Expr {
    fn from(value: Const) -> Self {
        Self::Const(value)
    }
}

pub enum Instr {
    Line(String),
    Call {
        func: usize,
        params: Vec<Expr>,
        result: Option<Var>,
        recursive: bool,
        save_vars: Vec<Var>,
        save_loop_vars: Vec<usize>,
    },
}

impl Instr {
    pub fn write(&self, f: &mut dyn io::Write, module: &Module<'_>) -> io::Result<()> {
        match self {
            Self::Line(x) => writeln!(f, "{}", x),
            Self::Call {
                func,
                params,
                result,
                recursive,
                save_vars,
                save_loop_vars,
            } => {
                if *recursive {
                    // ローカル変数保存用のスタックにプッシュ
                    for (i, &var) in save_vars.iter().enumerate() {
                        write!(f, "{STACK}[{STACK_TOP}")?;
                        if i != 0 {
                            write!(f, " + {i}")?;
                        }
                        writeln!(f, "] = {var};")?;
                    }
                    writeln!(f, "{STACK_TOP} += {};", save_vars.len())?;
                }

                if let Some(result) = result {
                    write!(f, "{result} = ")?;
                }

                write!(f, "{}(", module.all_funcs[*func].header.name)?;

                let params = params
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{params}")?;

                writeln!(f, ");")?;

                if *recursive {
                    writeln!(f, "{STACK_TOP} -= {};", save_vars.len())?;
                    // ローカル変数保存用のスタックからポップ
                    for (i, &var) in save_vars.iter().enumerate() {
                        write!(f, "{var} = ({}){STACK}[{STACK_TOP}", var.ty)?;
                        if i != 0 {
                            write!(f, " + {i}")?;
                        }
                        writeln!(f, "];")?;
                    }

                    // ループ変数を元に戻す
                    for i in save_loop_vars {
                        writeln!(f, "{LOOP}{i} = true;")?;
                    }
                }

                Ok(())
            }
        }
    }
}
