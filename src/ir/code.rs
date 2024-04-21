use cranelift_entity::{entity_impl, packed_option::PackedOption, PrimaryMap};

use super::{
    func::{Primary, Var},
    module::Module,
};

pub type Insts = PrimaryMap<InstId, Inst>;

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InstId(u32);
entity_impl!(InstId);

/// Instruction
#[derive(Default)]
pub struct Inst {
    /// この命令を含むブロック、`None`なら命令は挿入されていない
    pub parent: PackedOption<BlockId>,
    pub prev: PackedOption<InstId>,
    pub next: PackedOption<InstId>,
    pub first_block: PackedOption<BlockId>,
    pub last_block: PackedOption<BlockId>,
    pub kind: InstKind,
    /// 文字列内の以下のパターンが置換される  
    /// 引数: $p0, $p1, ...  
    /// 戻り値: $r  
    /// 呼び出す関数: $c
    pub pattern: String,
    pub params: Vec<Primary>,
    pub result: Option<Var>,
    pub call: Option<Call>,
    pub breakable: Breakable,
}

#[derive(Default)]
pub enum InstKind {
    #[default]
    Stmt,
    Expr,
    Return,
    Block,
    /// ループ変数のインデックス
    Loop(usize),
    If,
    Br(u32),
    /// 各caseの値、Noneならdefault
    Switch(Vec<Option<u32>>),
}

pub struct Call {
    pub func: usize,
    pub recursive: bool,
    pub save_vars: Vec<Var>,
    pub save_loop_vars: Vec<usize>,
}

#[derive(PartialEq, Eq, Default)]
pub enum Breakable {
    #[default]
    /// breakが不可能
    No,
    /// 1段階のbreakのみ可能
    Single,
    /// 複数段階のbreakが可能
    Multi,
}

impl Inst {
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

pub type Blocks = PrimaryMap<BlockId, Block>;

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(u32);
entity_impl!(BlockId, "block");

#[derive(Default)]
pub struct Block {
    pub parent: PackedOption<InstId>,
    pub prev: PackedOption<BlockId>,
    pub next: PackedOption<BlockId>,
    pub first_inst: PackedOption<InstId>,
    pub last_inst: PackedOption<InstId>,
}
