use std::{fmt, io};

use num_traits::Float;
use wasmparser::FuncType;

use super::{func_header, module::Module, result_cs_ty, ty::CsType, BREAK_DEPTH, LOOP};

pub struct Func {
    pub header: FuncHeader,
    pub code: Option<Code>,
    pub recursive: bool,
    pub in_table: bool,
}

impl Func {
    pub fn write(&self, f: &mut dyn io::Write, module: &Module<'_>) -> io::Result<()> {
        // 関数ヘッダ
        let func_ty = self.header.ty.clone();

        if !module.test && self.recursive {
            write!(f, "[RecursiveMethod] ")?;
        }

        if self.header.export {
            write!(f, "public ")?;
        }

        let code = self.code.as_ref().unwrap();

        let params: Vec<(CsType, &Var)> = func_ty
            .params()
            .iter()
            .map(|&ty| CsType::get(ty))
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
            let def_val = match var.ty {
                CsType::Bool => "false",
                _ => "0",
            };
            writeln!(f, "{} {var} = {def_val};", var.ty)?;
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
            this.new_var(CsType::get(ty));
        }

        this
    }

    pub fn new_var(&mut self, ty: CsType) -> Var {
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
    pub ty: CsType,
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "var{}", self.index)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Const {
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
}

impl Const {
    pub fn ty(&self) -> CsType {
        match self {
            Self::Int(_) => CsType::Int,
            Self::Long(_) => CsType::Long,
            Self::Float(_) => CsType::Float,
            Self::Double(_) => CsType::Double,
        }
    }
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(x) => match *x {
                i32::MIN => write!(f, "({} - 1)", i32::MIN + 1),
                _x => write!(f, "{}", _x),
            },
            Self::Long(x) => match *x {
                i64::MIN => write!(f, "({} - 1)", i64::MIN + 1),
                _x => write!(f, "{}", _x),
            },
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
            } => {
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

                writeln!(f, ");")
            }
        }
    }
}
