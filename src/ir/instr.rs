use std::io;

use crate::ir::{BREAK_DEPTH, STACK, STACK_TOP};

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

#[derive(Debug, Default)]
pub enum InstrKind {
    #[default]
    Stmt,
    Expr,
    Return,
    Block(Vec<Instr>),
    Loop {
        loop_var: usize,
        block: Vec<Instr>,
    },
    If {
        then: Vec<Instr>,
        else_: Option<Vec<Instr>>,
    },
    Br(u32),
    BrIf(u32),
    Switch(Vec<Case>),
}

#[derive(Debug)]
pub struct Case {
    /// Noneならdefault
    pub value: Option<u32>,
    pub block: Vec<Instr>,
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
                None => Vec::new(),
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
        self.write_inner(f, module, false)
    }

    fn write_inner(
        &self,
        f: &mut dyn io::Write,
        module: &Module<'_>,
        in_block: bool,
    ) -> io::Result<()> {
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
            InstrKind::Return => pattern = format!("return {pattern};"),
            _ => (),
        }

        if let Some(result) = self.result {
            pattern = pattern.replace("$r", &result.to_string());
        }

        if let Some(call) = &self.call {
            call.write_pattern(f, pattern, module)
        } else {
            match &self.kind {
                InstrKind::Block(block) => {
                    writeln!(f, "do {{")?;
                    for instr in block {
                        instr.write_inner(f, module, true)?;
                    }
                    writeln!(f, "}} while (false);")?;
                    write_multi_break(f, in_block)
                }
                InstrKind::Loop { loop_var, block } => {
                    writeln!(f, "{LOOP}{loop_var} = true;")?;
                    writeln!(f, "do {{")?;
                    writeln!(f, "do {{")?;

                    for instr in block {
                        instr.write_inner(f, module, true)?;
                    }

                    writeln!(f, "{LOOP}{loop_var} = false;")?;
                    writeln!(f, "}} while (false);")?;
                    writeln!(f, "if ({BREAK_DEPTH} > 0) break;")?;
                    writeln!(f, "}} while ({LOOP}{});", loop_var)?;
                    write_multi_break(f, in_block)
                }
                InstrKind::If { then, else_ } => {
                    writeln!(f, "do if ({pattern}) {{")?;
                    for instr in then {
                        instr.write_inner(f, module, true)?;
                    }

                    if let Some(else_) = else_ {
                        writeln!(f, "}} else {{")?;
                        for instr in else_ {
                            instr.write_inner(f, module, true)?;
                        }
                    }

                    writeln!(f, "}} while (false);")?;
                    write_multi_break(f, in_block)
                }
                InstrKind::Br(depth) => write_break(f, *depth),
                InstrKind::BrIf(depth) => {
                    writeln!(f, "if ({pattern}) {{")?;
                    write_break(f, *depth)?;
                    writeln!(f, "}}")
                }
                InstrKind::Switch(cases) => {
                    writeln!(f, "switch ({pattern}) {{")?;

                    for case in cases {
                        if let Some(value) = case.value {
                            writeln!(f, "case {value}:")?;
                        } else {
                            writeln!(f, "default:")?;
                        }

                        for instr in &case.block {
                            instr.write_inner(f, module, in_block)?;
                        }

                        // 命令がない、または最後の命令がbrではない場合にbreak
                        if case
                            .block
                            .last()
                            .map(|i| !matches!(i.kind, InstrKind::Br(..)))
                            .unwrap_or(true)
                        {
                            writeln!(f, "break;")?;
                        }
                    }

                    writeln!(f, "}}")
                }
                _ => writeln!(f, "{}", pattern),
            }
        }
    }
}

fn write_multi_break(f: &mut dyn io::Write, in_block: bool) -> io::Result<()> {
    // 最も外側のブロックのendでない場合のみ
    if in_block {
        // 多重break
        writeln!(f, "if ({BREAK_DEPTH} > 0) {{ {BREAK_DEPTH}--; break; }}")?;
    }
    Ok(())
}

fn write_break(f: &mut dyn io::Write, depth: u32) -> io::Result<()> {
    if depth > 0 {
        writeln!(f, "{BREAK_DEPTH} = {depth};")?;
    }
    writeln!(f, "break;")
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

pub struct Builder {
    instr_blocks: Vec<Vec<Instr>>,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            instr_blocks: vec![Vec::new()],
        }
    }

    pub fn push(&mut self, instr: Instr) {
        let create_block = matches!(
            instr.kind,
            InstrKind::Block(_)
                | InstrKind::Loop { .. }
                | InstrKind::If { .. }
                | InstrKind::Switch(_)
        );

        self.instr_blocks.last_mut().unwrap().push(instr);

        if create_block {
            self.instr_blocks.push(Vec::new());
        }
    }

    pub fn else_(&mut self) {
        let then = self.instr_blocks.pop().unwrap();
        let instr = self.instr_blocks.last_mut().unwrap().last_mut().unwrap();

        instr.kind = InstrKind::If {
            then,
            else_: Some(Vec::new()),
        };

        self.instr_blocks.push(Vec::new());
    }

    pub fn end(&mut self) {
        let block = self.instr_blocks.pop().unwrap();
        let instr = self.instr_blocks.last_mut().unwrap().last_mut().unwrap();

        match &mut instr.kind {
            InstrKind::Block(x)
            | InstrKind::Loop { block: x, .. }
            | InstrKind::If {
                then: x,
                else_: None,
            }
            | InstrKind::If { else_: Some(x), .. } => *x = block,
            InstrKind::Switch(_) => (),
            _ => unreachable!(),
        }
    }

    pub fn end_case(&mut self, value: Option<u32>) {
        let block = self.instr_blocks.pop().unwrap();
        let InstrKind::Switch(cases) = &mut self
            .instr_blocks
            .last_mut()
            .unwrap()
            .last_mut()
            .unwrap()
            .kind
        else {
            unreachable!()
        };

        cases.push(Case { value, block });
        self.instr_blocks.push(Vec::new());
    }

    pub fn build(self) -> Vec<Instr> {
        assert!(self.instr_blocks.len() == 1);
        self.instr_blocks.into_iter().next().unwrap()
    }
}

impl Default for Builder {
    fn default() -> Self {
        Self::new()
    }
}
