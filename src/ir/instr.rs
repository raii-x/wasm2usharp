use std::io;

use crate::ir::{STACK, STACK_TOP};

use super::{
    func::{Primary, Var},
    module::Module,
    LOOP,
};

pub enum Instr {
    Line(String),
    Call(Call),
    Set { lhs: Var, rhs: Expr },
}

pub struct Call {
    pub func: usize,
    pub params: Vec<Primary>,
    pub recursive: bool,
    pub save_vars: Vec<Var>,
    pub save_loop_vars: Vec<usize>,
}

impl Call {
    pub fn into_instr(self, result: Option<Var>) -> Instr {
        match result {
            Some(result) => Instr::Set {
                lhs: result,
                rhs: Expr::Call(self),
            },
            None => Instr::Call(self),
        }
    }
}

pub enum Expr {
    Primary(Primary),
    Call(Call),
    Line(String),
}

impl Instr {
    pub fn write(&self, f: &mut dyn io::Write, module: &Module<'_>) -> io::Result<()> {
        match self {
            Self::Line(x) => writeln!(f, "{}", x),
            Self::Call(x) => call(f, module, x, None),
            Self::Set { lhs, rhs } => match rhs {
                Expr::Primary(rhs) => writeln!(f, "{lhs} = {rhs};"),
                Expr::Call(rhs) => call(f, module, rhs, Some(*lhs)),
                Expr::Line(rhs) => writeln!(f, "{lhs} = {rhs};"),
            },
        }
    }
}

fn call(
    f: &mut dyn io::Write,
    module: &Module<'_>,
    call: &Call,
    result: Option<Var>,
) -> io::Result<()> {
    if call.recursive && !call.save_vars.is_empty() {
        // ローカル変数保存用のスタックにプッシュ
        for (i, &var) in call.save_vars.iter().enumerate() {
            write!(f, "{STACK}[{STACK_TOP}")?;
            if i != 0 {
                write!(f, " + {i}")?;
            }
            writeln!(f, "] = {var};")?;
        }
        writeln!(f, "{STACK_TOP} += {};", call.save_vars.len())?;
    }

    if let Some(result) = result {
        write!(f, "{result} = ")?;
    }

    write!(f, "{}(", module.all_funcs[call.func].header.name)?;

    let params = call
        .params
        .iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    write!(f, "{params}")?;

    writeln!(f, ");")?;

    if call.recursive && !call.save_vars.is_empty() {
        writeln!(f, "{STACK_TOP} -= {};", call.save_vars.len())?;
        // ローカル変数保存用のスタックからポップ
        for (i, &var) in call.save_vars.iter().enumerate() {
            write!(f, "{var} = ({}){STACK}[{STACK_TOP}", var.ty)?;
            if i != 0 {
                write!(f, " + {i}")?;
            }
            writeln!(f, "];")?;
        }

        // ループ変数を元に戻す
        for i in &call.save_loop_vars {
            writeln!(f, "{LOOP}{i} = true;")?;
        }
    }

    Ok(())
}
