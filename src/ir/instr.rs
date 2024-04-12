use std::io;

use crate::ir::{STACK, STACK_TOP};

use super::{
    func::{Primary, Var},
    module::Module,
    LOOP,
};

/// Instruction
#[derive(Debug, Default)]
pub struct Instr {
    pub kind: InstrKind,
    /// 文字列内の以下のパターンが置換される  
    /// 引数: $p0, $p1, ...  
    /// 戻り値: $r  
    /// 呼び出す関数: $c
    pub pattern: String,
    pub params: Vec<Primary>,
    pub result: Option<Var>,
    pub call: Option<Call>,
}

#[derive(Debug, Default, PartialEq, Eq)]
pub enum InstrKind {
    #[default]
    Stmt,
    Expr,
    Return,
}

#[derive(Debug)]
pub struct Call {
    pub func: usize,
    pub recursive: bool,
    pub save_vars: Vec<Var>,
    pub save_loop_vars: Vec<usize>,
}

impl Instr {
    pub fn line(line: impl Into<String>) -> Self {
        Self {
            pattern: line.into(),
            ..Default::default()
        }
    }

    pub fn return_(param: Option<Primary>) -> Self {
        Self {
            kind: InstrKind::Return,
            pattern: match param {
                Some(_) => "$p0",
                None => "",
            }
            .to_string(),
            params: match param {
                Some(param) => vec![param],
                None => vec![],
            },
            ..Default::default()
        }
    }

    pub fn set(lhs: Var, rhs: Primary) -> Self {
        Self {
            kind: InstrKind::Expr,
            pattern: "$p0".to_string(),
            params: vec![rhs],
            result: Some(lhs),
            ..Default::default()
        }
    }

    pub fn set_pattern(lhs: Var, pattern: impl Into<String>, params: Vec<Primary>) -> Self {
        Self {
            kind: InstrKind::Expr,
            pattern: pattern.into(),
            params,
            result: Some(lhs),
            ..Default::default()
        }
    }

    pub fn call(call: Call, params: Vec<Primary>, result: Option<Var>) -> Self {
        let mut pattern = "$c(".to_string();
        pattern += &(0..params.len())
            .map(|i| format!("$p{}", i))
            .collect::<Vec<_>>()
            .join(", ");
        pattern += ")";

        Self {
            kind: InstrKind::Expr,
            pattern,
            params,
            result,
            call: Some(call),
        }
    }

    pub fn write(&self, f: &mut dyn io::Write, module: &Module<'_>) -> io::Result<()> {
        let mut pattern = self.pattern.clone();

        // $p1などが$p10を置換しないように、番号の大きい側から置換する
        for (i, param) in self.params.iter().enumerate().rev() {
            pattern = pattern.replace(&format!("$p{}", i), &param.to_string());
        }

        match self.kind {
            InstrKind::Expr => {
                if self.result.is_some() {
                    pattern = format!("$r = {pattern};");
                } else {
                    pattern += ";";
                }
            }
            InstrKind::Stmt => (),
            InstrKind::Return => pattern = format!("return {pattern};"),
        }

        if let Some(result) = self.result {
            pattern = pattern.replace("$r", &result.to_string());
        }

        if let Some(call) = &self.call {
            call.write_pattern(f, pattern, module)
        } else {
            writeln!(f, "{}", pattern)
        }
    }
}

impl Call {
    fn write_pattern(
        &self,
        f: &mut dyn io::Write,
        pattern: String,
        module: &Module<'_>,
    ) -> io::Result<()> {
        if self.recursive && !self.save_vars.is_empty() {
            // ローカル変数保存用のスタックにプッシュ
            for (i, &var) in self.save_vars.iter().enumerate() {
                write!(f, "{STACK}[{STACK_TOP}")?;
                if i != 0 {
                    write!(f, " + {i}")?;
                }
                writeln!(f, "] = {var};")?;
            }
            writeln!(f, "{STACK_TOP} += {};", self.save_vars.len())?;
        }

        // 関数呼び出しを書き込む
        let pattern = pattern.replace("$c", &module.all_funcs[self.func].header.name);
        writeln!(f, "{}", pattern)?;

        if self.recursive && !self.save_vars.is_empty() {
            writeln!(f, "{STACK_TOP} -= {};", self.save_vars.len())?;
            // ローカル変数保存用のスタックからポップ
            for (i, &var) in self.save_vars.iter().enumerate() {
                write!(f, "{var} = ({}){STACK}[{STACK_TOP}", var.ty)?;
                if i != 0 {
                    write!(f, " + {i}")?;
                }
                writeln!(f, "];")?;
            }

            // ループ変数を元に戻す
            for i in &self.save_loop_vars {
                writeln!(f, "{LOOP}{i} = true;")?;
            }
        }

        Ok(())
    }
}
