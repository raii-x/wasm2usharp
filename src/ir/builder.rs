use cranelift_entity::{PrimaryMap, SecondaryMap};
use wasmparser::ValType;

use super::{
    code::{Block, BlockId, BlockNode, Breakable, Call, Code, Inst, InstKind},
    ty::CsType,
    var::{Primary, Var, VarId, Vars},
};

pub struct Builder {
    pub code: Code,
    cursor: BlockId,
}

impl Builder {
    pub fn new(params: &[ValType]) -> Self {
        let mut vars = Vars::new();

        for &ty in params {
            vars.push(Var {
                ty: CsType::get(ty),
                local: true,
                default: None,
                used: true,
            });
        }

        let mut blocks = PrimaryMap::new();
        let root = blocks.push(Block);

        Self {
            code: Code {
                blocks,
                block_nodes: SecondaryMap::new(),
                insts: PrimaryMap::new(),
                inst_nodes: SecondaryMap::new(),
                root,
                vars,
                loop_var_count: 0,
                break_depth_used: true,
            },
            cursor: root,
        }
    }

    pub fn new_var(&mut self, var: Var) -> VarId {
        self.code.vars.push(var)
    }

    /// カーソルのブロックの最後に命令を追加
    pub fn push(&mut self, inst: Inst) {
        let new_id = self.code.insts.push(inst);
        self.code.inst_nodes[new_id].parent = self.cursor.into();

        let block = &mut self.code.block_nodes[self.cursor];
        if let Some(last_id) = block.last_child.expand() {
            self.code.inst_nodes[last_id].next = new_id.into();
            self.code.inst_nodes[new_id].prev = last_id.into();
        } else {
            block.first_child = new_id.into();
        }
        block.last_child = new_id.into();
    }

    pub fn push_line(&mut self, line: impl Into<String>) {
        self.push(Inst {
            pattern: line.into(),
            ..Default::default()
        });
    }

    pub fn push_return(&mut self, param: Option<Primary>) {
        self.push(Inst {
            kind: InstKind::Return,
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
        });
    }

    pub fn push_set(&mut self, lhs: VarId, rhs: Primary) {
        self.push(Inst {
            kind: InstKind::Expr,
            pattern: "$p0".to_string(),
            params: vec![rhs],
            result: Some(lhs),
            ..Default::default()
        });
    }

    pub fn push_set_pattern(
        &mut self,
        lhs: VarId,
        pattern: impl Into<String>,
        params: Vec<Primary>,
    ) {
        self.push(Inst {
            kind: InstKind::Expr,
            pattern: pattern.into(),
            params,
            result: Some(lhs),
            ..Default::default()
        });
    }

    pub fn push_call(&mut self, call: Call, params: Vec<Primary>, result: Option<VarId>) {
        let mut pattern = "$c(".to_string();
        pattern += &(0..params.len())
            .map(|i| format!("$p{}", i))
            .collect::<Vec<_>>()
            .join(", ");
        pattern += ")";

        self.push(Inst {
            kind: InstKind::Expr,
            pattern,
            params,
            result,
            call: Some(call),
            ..Default::default()
        });
    }

    pub fn push_block(&mut self, breakable: Breakable) {
        self.push(Inst {
            kind: InstKind::Block,
            breakable,
            ..Default::default()
        });
    }

    pub fn push_if(&mut self, cond: Primary, breakable: Breakable) {
        self.push(Inst {
            kind: InstKind::If,
            pattern: "$p0 != 0".to_string(),
            params: vec![cond],
            breakable,
            ..Default::default()
        });
    }

    pub fn push_br(&mut self, depth: u32) {
        self.push(Inst {
            kind: InstKind::Br(depth),
            ..Default::default()
        });
    }

    /// カーソルのブロックの最後の命令の子としてブロックを追加し、カーソルを追加したブロックに移動
    pub fn start_block(&mut self) {
        let inst_id = self.code.block_nodes[self.cursor].last_child.unwrap();
        let new_id = self.code.blocks.push(Block);
        self.code.block_nodes[new_id] = BlockNode {
            parent: inst_id.into(),
            ..Default::default()
        };

        let node = &mut self.code.inst_nodes[inst_id];
        if let Some(last_id) = node.last_child.expand() {
            self.code.block_nodes[last_id].next = new_id.into();
            self.code.block_nodes[new_id].prev = last_id.into();
        } else {
            node.first_child = new_id.into();
        }
        node.last_child = new_id.into();

        self.cursor = new_id;
    }

    /// カーソルを現在のカーソルのブロックを持つ命令を含むブロックに移動
    pub fn end_block(&mut self) {
        let inst_id = self.code.block_nodes[self.cursor].parent.unwrap();
        self.cursor = self.code.inst_nodes[inst_id].parent.unwrap();
    }

    pub fn build(self) -> Code {
        assert!(self.code.block_nodes[self.cursor].parent.is_none());
        self.code
    }
}
