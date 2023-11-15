use std::io;

use crate::ir::{STACK, STACK_TOP};

use super::{
    func::{Expr, Var},
    module::Module,
    LOOP,
};

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
                if *recursive && !save_vars.is_empty() {
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

                if *recursive && !save_vars.is_empty() {
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
