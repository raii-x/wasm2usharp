use super::{
    func::{Code, FuncVars, Primary, Var},
    instr::{Block, BlockId, Blocks, Breakable, Call, Inst, InstKind, Insts},
};

pub struct Builder {
    pub code: Code,
    cursor: BlockId,
}

impl Builder {
    pub fn new(vars: FuncVars) -> Self {
        let mut blocks = Blocks::new();
        let root = blocks.push(Default::default());

        Self {
            code: Code {
                blocks,
                insts: Insts::new(),
                root,
                vars,
            },
            cursor: root,
        }
    }

    /// カーソルのブロックの最後に命令を追加
    pub fn push(&mut self, mut inst: Inst) {
        inst.parent = self.cursor.into();
        let new_id = self.code.insts.push(inst);

        let block = &mut self.code.blocks[self.cursor];
        if let Some(last_id) = block.last_inst.expand() {
            self.code.insts[last_id].next = new_id.into();
            self.code.insts[new_id].prev = last_id.into();
        } else {
            block.first_inst = new_id.into();
        }
        block.last_inst = new_id.into();
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

    pub fn push_set(&mut self, lhs: Var, rhs: Primary) {
        self.push(Inst {
            kind: InstKind::Expr,
            pattern: "$p0".to_string(),
            params: vec![rhs],
            result: Some(lhs),
            ..Default::default()
        });
    }

    pub fn push_set_pattern(&mut self, lhs: Var, pattern: impl Into<String>, params: Vec<Primary>) {
        self.push(Inst {
            kind: InstKind::Expr,
            pattern: pattern.into(),
            params,
            result: Some(lhs),
            ..Default::default()
        });
    }

    pub fn push_call(&mut self, call: Call, params: Vec<Primary>, result: Option<Var>) {
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
        let inst_id = self.code.blocks[self.cursor].last_inst.unwrap();
        let new_id = self.code.blocks.push(Block {
            parent: inst_id.into(),
            ..Default::default()
        });

        let inst = &mut self.code.insts[inst_id];
        if let Some(last_id) = inst.last_block.expand() {
            self.code.blocks[last_id].next = new_id.into();
            self.code.blocks[new_id].prev = last_id.into();
        } else {
            inst.first_block = new_id.into();
        }
        inst.last_block = new_id.into();

        self.cursor = new_id;
    }

    /// カーソルを現在のカーソルのブロックを持つ命令を含むブロックに移動
    pub fn end_block(&mut self) {
        let inst_id = self.code.blocks[self.cursor].parent.unwrap();
        self.cursor = self.code.insts[inst_id].parent.unwrap();
    }

    pub fn build(self) -> Code {
        assert!(self.code.blocks[self.cursor].parent.is_none());
        self.code
    }
}
