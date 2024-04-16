use super::{
    func::{Primary, Var},
    instr::{Breakable, Call, Instr, InstrChild, InstrKind, InstrNode, InstrNodeId, InstrTree},
};

pub struct Builder {
    tree: InstrTree,
    blocks: Vec<Vec<InstrNodeId>>,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            tree: InstrTree::new(),
            blocks: vec![Vec::new()],
        }
    }

    pub fn push(&mut self, instr: Instr) {
        let id = self.tree.push(InstrNode { instr, child: None });
        self.blocks.last_mut().unwrap().push(id);
    }

    pub fn push_with_child(&mut self, instr: Instr, breakable: Breakable) {
        let id = self.tree.push(InstrNode {
            instr,
            child: Some(InstrChild {
                blocks: Vec::new(),
                breakable,
            }),
        });
        self.blocks.last_mut().unwrap().push(id);
    }

    pub fn push_line(&mut self, line: impl Into<String>) {
        self.push(Instr {
            pattern: line.into(),
            ..Default::default()
        });
    }

    pub fn push_return(&mut self, param: Option<Primary>) {
        self.push(Instr {
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
        });
    }

    pub fn push_set(&mut self, lhs: Var, rhs: Primary) {
        self.push(Instr {
            kind: InstrKind::Expr,
            pattern: "$p0".to_string(),
            params: vec![rhs],
            result: Some(lhs),
            ..Default::default()
        });
    }

    pub fn push_set_pattern(&mut self, lhs: Var, pattern: impl Into<String>, params: Vec<Primary>) {
        self.push(Instr {
            kind: InstrKind::Expr,
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

        self.push(Instr {
            kind: InstrKind::Expr,
            pattern,
            params,
            result,
            call: Some(call),
        });
    }

    pub fn push_if(&mut self, cond: Primary, breakable: Breakable) {
        self.push_with_child(
            Instr {
                kind: InstrKind::If,
                pattern: "$p0 != 0".to_string(),
                params: vec![cond],
                ..Default::default()
            },
            breakable,
        );
    }

    pub fn push_br(&mut self, depth: u32) {
        self.push(Instr {
            kind: InstrKind::Br(depth),
            ..Default::default()
        });
    }

    pub fn start_block(&mut self) {
        self.blocks.push(Vec::new());
    }

    pub fn end_block(&mut self) {
        let block = self.blocks.pop().unwrap();
        let id = self.blocks.last().unwrap().last().unwrap();
        let instr = self.tree.get_mut(*id).unwrap();

        instr.child.as_mut().unwrap().blocks.push(block);
    }

    pub fn build(mut self) -> InstrTree {
        assert_eq!(self.blocks.len(), 1);
        self.tree.root = self.blocks.into_iter().next().unwrap();
        self.tree
    }
}

impl Default for Builder {
    fn default() -> Self {
        Self::new()
    }
}
