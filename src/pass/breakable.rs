use cranelift_entity::EntitySet;

use crate::ir::{
    code::{BlockId, Breakable, Code, InstId, InstKind},
    node::Node,
};

/// ブロックを作る命令のbreakableを最適化する
pub fn breakable(code: &mut Code) {
    BreakablePass::new(code).run();
}

struct BreakablePass<'a> {
    code: &'a mut Code,
    /// break先となっている命令の集合
    targets: EntitySet<InstId>,
    inst_stack: Vec<InstId>,
    reduction_stack: Vec<u32>,
}

impl<'a> BreakablePass<'a> {
    fn new(code: &'a mut Code) -> Self {
        let capacity = code.insts.len();
        Self {
            code,
            targets: EntitySet::with_capacity(capacity),
            inst_stack: Vec::new(),
            reduction_stack: Vec::new(),
        }
    }

    fn run(&mut self) {
        self.mark_target(self.code.root);
        self.reduce_depth(self.code.root);
        self.set_breakable_block(self.code.root);
    }

    /// break先の命令をtargetsに追加
    fn mark_target(&mut self, block_id: BlockId) {
        let mut inst_iter = self.code.block_nodes[block_id].iter();
        while let Some(inst_id) = inst_iter.next(&self.code.inst_nodes) {
            let inst = &self.code.insts[inst_id];
            if let InstKind::Br(depth) = inst.kind {
                // break先となる命令のIDをtargetsに追加
                let target_id = self.inst_stack[self.inst_stack.len() - 1 - depth as usize];
                self.targets.insert(target_id);
            }

            let breakable = inst.breakable != Breakable::No;
            if breakable {
                self.inst_stack.push(inst_id);
            }

            let mut block_iter = self.code.inst_nodes[inst_id].iter();
            while let Some(block_id) = block_iter.next(&self.code.block_nodes) {
                self.mark_target(block_id);
            }

            if breakable {
                self.inst_stack.pop();
            }
        }
    }

    /// br命令のbreak量を減らす
    fn reduce_depth(&mut self, block_id: BlockId) {
        let mut inst_iter = self.code.block_nodes[block_id].iter();
        while let Some(inst_id) = inst_iter.next(&self.code.inst_nodes) {
            let inst = &mut self.code.insts[inst_id];
            if let InstKind::Br(depth) = &mut inst.kind {
                // reduction_stackの (最後の値 - 最後からdepth番目の値) がbreakの減少量となる
                *depth -= self.reduction_stack.last().unwrap()
                    - self.reduction_stack[self.reduction_stack.len() - 1 - *depth as usize];
            }

            let breakable = inst.breakable != Breakable::No;
            if breakable {
                let mut reduction = if let Some(last) = self.reduction_stack.last() {
                    *last
                } else {
                    0
                };

                if !matches!(inst.kind, InstKind::Loop(_) | InstKind::Switch(_))
                    && !self.targets.contains(inst_id)
                {
                    // この命令の子孫のbreakを1減らす
                    inst.breakable = Breakable::No;
                    reduction += 1;
                }

                self.reduction_stack.push(reduction);
            }

            // 命令の子ブロックに対して処理
            let mut block_iter = self.code.inst_nodes[inst_id].iter();
            while let Some(block_id) = block_iter.next(&self.code.block_nodes) {
                self.reduce_depth(block_id);
            }

            if breakable {
                self.reduction_stack.pop();
            }
        }
    }

    /// breakableを最適化し、ブロックの最大break段階数を返す
    fn set_breakable_block(&mut self, block_id: BlockId) -> u32 {
        let mut max_depth = 0;

        // ブロック内の命令の最大のbreak段階数を求める
        let mut inst_iter = self.code.block_nodes[block_id].iter();
        while let Some(inst_id) = inst_iter.next(&self.code.inst_nodes) {
            let depth = self.set_breakable_inst(inst_id);
            max_depth = max_depth.max(depth);
        }

        max_depth
    }

    /// breakableを最適化し、命令の最大break段階数を返す
    fn set_breakable_inst(&mut self, inst_id: InstId) -> u32 {
        // br命令なら段階数+1
        if let InstKind::Br(depth) = self.code.insts[inst_id].kind {
            return depth + 1;
        }

        let mut max_depth = 0;

        // 命令の子ブロックの最大のbreak段階数を求める
        let mut block_iter = self.code.inst_nodes[inst_id].iter();
        while let Some(block_id) = block_iter.next(&self.code.block_nodes) {
            let depth = self.set_breakable_block(block_id);
            max_depth = max_depth.max(depth);
        }

        let inst = &mut self.code.insts[inst_id];
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
