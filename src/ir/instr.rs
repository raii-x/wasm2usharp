use std::io;

use crate::ir::{BREAK_DEPTH, STACK, STACK_TOP};

use super::{
    func::{Primary, Var},
    module::Module,
    LOOP,
};

pub struct InstrNode {
    pub instr: Instr,
    pub child: Option<InstrChild>,
}

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
    Block,
    /// ループ変数のインデックス
    Loop(usize),
    If,
    Br(u32),
    BrIf(u32),
    /// 各caseの値、Noneならdefault
    Switch(Vec<Option<u32>>),
}

#[derive(Debug)]
pub struct Call {
    pub func: usize,
    pub recursive: bool,
    pub save_vars: Vec<Var>,
    pub save_loop_vars: Vec<usize>,
}

pub struct InstrChild {
    pub blocks: Vec<Vec<InstrNode>>,
    pub breakable: bool,
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

    fn expand_pattern(&self, module: &Module<'_>) -> String {
        let mut pattern = self.pattern.clone();

        // $p1などが$p10を置換しないように、番号の大きい側から置換する
        for (i, param) in self.params.iter().enumerate().rev() {
            pattern = pattern.replace(&format!("$p{}", i), &param.to_string());
        }

        if let Some(result) = self.result {
            pattern = pattern.replace("$r", &result.to_string());
        }

        if let Some(call) = &self.call {
            pattern = pattern.replace("$c", &module.all_funcs[call.func].header.name);
        }

        pattern
    }
}

impl InstrNode {
    pub fn write(&self, f: &mut dyn io::Write, module: &Module<'_>) -> io::Result<()> {
        self.write_inner(f, module, false)
    }

    fn write_inner(
        &self,
        f: &mut dyn io::Write,
        module: &Module<'_>,
        in_block: bool,
    ) -> io::Result<()> {
        let pattern = self.instr.expand_pattern(module);

        if let Some(call) = &self.instr.call {
            call.push_save_vars(f)?;
        }

        match &self.instr.kind {
            InstrKind::Stmt => {
                writeln!(f, "{}", pattern)?;
            }
            InstrKind::Expr => {
                if let Some(result) = self.instr.result {
                    writeln!(f, "{result} = {pattern};")?;
                } else {
                    writeln!(f, "{pattern};")?;
                }
            }
            InstrKind::Return => {
                writeln!(f, "return {pattern};")?;
            }
            InstrKind::Block => {
                writeln!(f, "do {{")?;
                for instr in &self.child.as_ref().unwrap().blocks[0] {
                    instr.write_inner(f, module, true)?;
                }
                writeln!(f, "}} while (false);")?;
            }
            InstrKind::Loop(loop_var) => {
                writeln!(f, "{LOOP}{loop_var} = true;")?;
                writeln!(f, "do {{")?;
                writeln!(f, "do {{")?;

                for instr in &self.child.as_ref().unwrap().blocks[0] {
                    instr.write_inner(f, module, true)?;
                }

                writeln!(f, "{LOOP}{loop_var} = false;")?;
                writeln!(f, "}} while (false);")?;
                writeln!(f, "if ({BREAK_DEPTH} > 0) break;")?;
                writeln!(f, "}} while ({LOOP}{});", loop_var)?;
            }
            InstrKind::If => {
                let child = self.child.as_ref().unwrap();

                writeln!(f, "do if ({pattern}) {{")?;
                for instr in &child.blocks[0] {
                    instr.write_inner(f, module, true)?;
                }

                if let Some(else_) = child.blocks.get(1) {
                    writeln!(f, "}} else {{")?;
                    for instr in else_ {
                        instr.write_inner(f, module, true)?;
                    }
                }

                writeln!(f, "}} while (false);")?;
            }
            InstrKind::Br(depth) => {
                write_break(f, *depth)?;
            }
            InstrKind::BrIf(depth) => {
                writeln!(f, "if ({pattern}) {{")?;
                write_break(f, *depth)?;
                writeln!(f, "}}")?;
            }
            InstrKind::Switch(cases) => {
                writeln!(f, "switch ({pattern}) {{")?;

                let blocks = &self.child.as_ref().unwrap().blocks;
                assert_eq!(cases.len(), blocks.len());

                for (case, block) in cases.iter().zip(blocks) {
                    if let Some(case) = case {
                        writeln!(f, "case {case}:")?;
                    } else {
                        writeln!(f, "default:")?;
                    }

                    for instr in block {
                        instr.write_inner(f, module, in_block)?;
                    }

                    // 命令がない、または最後の命令がbrではない場合にbreak
                    if block
                        .last()
                        .map_or(true, |n| !matches!(n.instr.kind, InstrKind::Br(..)))
                    {
                        writeln!(f, "break;")?;
                    }
                }

                writeln!(f, "}}")?;
            }
        }

        if self.child.as_ref().map_or(false, |c| c.breakable) {
            write_multi_break(f, in_block)?;
        }

        if let Some(call) = &self.instr.call {
            call.pop_save_vars(f)?;
        }
        Ok(())
    }

    pub fn all_calls(&mut self) -> Vec<&mut Call> {
        let mut calls = Vec::new();
        self.all_calls_inner(&mut calls);
        calls
    }

    fn all_calls_inner<'a>(&'a mut self, calls: &mut Vec<&'a mut Call>) {
        if let Some(call) = &mut self.instr.call {
            calls.push(call);
        };

        let Some(child) = &mut self.child else { return };

        for block in &mut child.blocks {
            for instr in block {
                instr.all_calls_inner(calls);
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
    fn push_save_vars(&self, f: &mut dyn io::Write) -> io::Result<()> {
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
        Ok(())
    }

    fn pop_save_vars(&self, f: &mut dyn io::Write) -> io::Result<()> {
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
    blocks: Vec<Vec<InstrNode>>,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            blocks: vec![Vec::new()],
        }
    }

    pub fn push(&mut self, instr: Instr) {
        let breakable = match instr.kind {
            InstrKind::Block | InstrKind::Loop { .. } | InstrKind::If { .. } => Some(true),
            InstrKind::Switch(_) => Some(false),
            _ => None,
        };

        self.blocks.last_mut().unwrap().push(InstrNode {
            instr,
            child: breakable.map(|breakable| InstrChild {
                blocks: Vec::new(),
                breakable,
            }),
        });
    }

    pub fn start_block(&mut self) {
        self.blocks.push(Vec::new());
    }

    pub fn end_block(&mut self) {
        let block = self.blocks.pop().unwrap();
        let instr = self.blocks.last_mut().unwrap().last_mut().unwrap();

        instr.child.as_mut().unwrap().blocks.push(block);
    }

    pub fn build(self) -> Vec<InstrNode> {
        assert!(self.blocks.len() == 1);
        self.blocks.into_iter().next().unwrap()
    }
}

impl Default for Builder {
    fn default() -> Self {
        Self::new()
    }
}
