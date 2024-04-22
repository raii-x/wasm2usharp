use crate::ir::code::{BlockId, Breakable, Code, InstId, InstKind};

/// ブロックを作る命令のbreakableを最適化する
pub fn breakable(code: &mut Code) {
    BreakablePass::new(code).run();
}

struct BreakablePass<'a> {
    code: &'a mut Code,
}

impl<'a> BreakablePass<'a> {
    fn new(code: &'a mut Code) -> Self {
        Self { code }
    }

    fn run(&mut self) {
        self.run_block(self.code.root);
    }

    /// ブロックの最大break段階数を返す
    fn run_block(&mut self, id: BlockId) -> u32 {
        let block = &mut self.code.blocks[id];

        let mut max_depth = 0;
        let mut next_inst_id = block.first_inst;

        // ブロック内の命令の最大のbreak段階数を求める
        while let Some(inst_id) = next_inst_id.expand() {
            let depth = self.run_inst(inst_id);
            max_depth = max_depth.max(depth);

            next_inst_id = self.code.insts[inst_id].next;
        }

        max_depth
    }

    /// 命令の最大break段階数を返す
    fn run_inst(&mut self, id: InstId) -> u32 {
        // br命令なら段階数+1
        if let InstKind::Br(depth) = self.code.insts[id].kind {
            return depth + 1;
        }

        let mut max_depth = 0;
        let mut next_block_id = self.code.insts[id].first_block;

        // 命令の子ブロックの最大のbreak段階数を求める
        while let Some(block_id) = next_block_id.expand() {
            let depth = self.run_block(block_id);
            max_depth = max_depth.max(depth);

            next_block_id = self.code.blocks[block_id].next;
        }

        let inst = &mut self.code.insts[id];
        if inst.breakable == Breakable::No {
            // break可能でなければ子のbreak段階数を命令のbreak段階数とする
            max_depth
        } else {
            // max_depthによってBreakableを最適化
            inst.breakable = match max_depth {
                0 => Breakable::No,
                1 => Breakable::Single,
                _ => Breakable::Multi,
            };
            // break可能なら子のbreak段階数から1を引いて命令のbreak段階数とする
            max_depth.saturating_sub(1)
        }
    }
}
