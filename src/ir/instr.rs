use super::{
    func::{Primary, Var},
    module::Module,
};

pub struct InstrTree {
    nodes: Vec<Option<InstrNode>>,
    pub root: Vec<InstrNodeId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InstrNodeId(usize);

#[derive(Debug)]
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

#[derive(Debug)]
pub struct InstrChild {
    pub blocks: Vec<Vec<InstrNodeId>>,
    pub breakable: Breakable,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Breakable {
    /// breakが不可能
    No,
    /// 1段階のbreakのみ可能
    Single,
    /// 複数段階のbreakが可能
    Multi,
}

impl InstrTree {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            root: Vec::new(),
        }
    }

    pub fn push(&mut self, node: InstrNode) -> InstrNodeId {
        let id = InstrNodeId(self.nodes.len());
        self.nodes.push(Some(node));
        id
    }

    pub fn remove(&mut self, id: InstrNodeId) -> Option<InstrNode> {
        self.nodes[id.0].take()
    }

    pub fn get(&self, id: InstrNodeId) -> Option<&InstrNode> {
        self.nodes.get(id.0).and_then(|node| node.as_ref())
    }

    pub fn get_mut(&mut self, id: InstrNodeId) -> Option<&mut InstrNode> {
        self.nodes.get_mut(id.0).and_then(|node| node.as_mut())
    }

    pub fn iter(&self) -> impl Iterator<Item = (InstrNodeId, &InstrNode)> {
        self.nodes
            .iter()
            .enumerate()
            .filter_map(|(i, node)| node.as_ref().map(|node| (InstrNodeId(i), node)))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (InstrNodeId, &mut InstrNode)> {
        self.nodes
            .iter_mut()
            .enumerate()
            .filter_map(|(i, node)| node.as_mut().map(|node| (InstrNodeId(i), node)))
    }
}

impl Default for InstrTree {
    fn default() -> Self {
        Self::new()
    }
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
