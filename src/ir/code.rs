use cranelift_entity::{entity_impl, packed_option::PackedOption, PrimaryMap, SecondaryMap};

use super::{
    module::Module,
    node::{node_def, Node},
    var::{Primary, VarId, Vars},
};

pub struct Code {
    pub blocks: PrimaryMap<BlockId, Block>,
    pub block_nodes: SecondaryMap<BlockId, BlockNode>,
    pub insts: PrimaryMap<InstId, Inst>,
    pub inst_nodes: SecondaryMap<InstId, InstNode>,
    pub root: BlockId,
    pub vars: Vars,
    pub loop_var_count: usize,
    pub break_depth_used: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(u32);
entity_impl!(BlockId, "block");

pub struct Block;

node_def!(BlockNode, BlockId, InstId, InstId);

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InstId(u32);
entity_impl!(InstId);

/// Instruction
#[derive(Default)]
pub struct Inst {
    pub kind: InstKind,
    /// 文字列内の以下のパターンが置換される  
    /// 引数: $p0, $p1, ...  
    /// 戻り値: $r  
    /// 呼び出す関数: $c
    pub pattern: String,
    pub params: Vec<Primary>,
    pub result: Option<VarId>,
    pub call: Option<Call>,
    pub breakable: Breakable,
}

node_def!(InstNode, InstId, BlockId, BlockId);

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
    pub save_vars: Vec<VarId>,
    pub save_loop_vars: Vec<usize>,
}

#[derive(Clone, Copy, PartialEq, Eq, Default)]
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
