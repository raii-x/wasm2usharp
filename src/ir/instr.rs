use super::{
    func::{Primary, Var},
    module::Module,
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
    pub fn expand_pattern(&self, module: &Module<'_>) -> String {
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
    pub fn all_instrs(&mut self) -> Vec<&mut Instr> {
        let mut instrs = Vec::new();
        self.all_instrs_inner(&mut instrs);
        instrs
    }

    fn all_instrs_inner<'a>(&'a mut self, instrs: &mut Vec<&'a mut Instr>) {
        instrs.push(&mut self.instr);

        let Some(child) = &mut self.child else { return };

        for block in &mut child.blocks {
            for instr in block {
                instr.all_instrs_inner(instrs);
            }
        }
    }
}
