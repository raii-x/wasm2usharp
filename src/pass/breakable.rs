use cranelift_entity::EntitySet;

use crate::ir::code::{BlockId, Breakable, Code, InstId, InstKind};

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
        self.mark_target_block(self.code.root);
        self.reduce_depth_block(self.code.root);
        self.set_breakable_block(self.code.root);
    }

    /// break先の命令をtargetsに追加
    fn mark_target_block(&mut self, id: BlockId) {
        let mut next_inst_id = self.code.blocks[id].first_inst;
        while let Some(inst_id) = next_inst_id.expand() {
            self.mark_target_inst(inst_id);
            next_inst_id = self.code.inst_nodes[inst_id].next;
        }
    }

    /// break先の命令をtargetsに追加
    fn mark_target_inst(&mut self, id: InstId) {
        let inst = &self.code.insts[id];
        if let InstKind::Br(depth) = inst.kind {
            // break先となる命令のIDをtargetsに追加
            let target_id = self.inst_stack[self.inst_stack.len() - 1 - depth as usize];
            self.targets.insert(target_id);
        }

        let breakable = inst.breakable != Breakable::No;
        if breakable {
            self.inst_stack.push(id);
        }

        let mut next_block_id = self.code.inst_nodes[id].first_block;
        while let Some(block_id) = next_block_id.expand() {
            self.mark_target_block(block_id);
            next_block_id = self.code.blocks[block_id].next;
        }

        if breakable {
            self.inst_stack.pop();
        }
    }

    /// br命令のbreak量を減らす
    fn reduce_depth_block(&mut self, id: BlockId) {
        let mut next_inst_id = self.code.blocks[id].first_inst;
        while let Some(inst_id) = next_inst_id.expand() {
            self.reduce_depth_inst(inst_id);
            next_inst_id = self.code.inst_nodes[inst_id].next;
        }
    }

    /// br命令のbreak量を減らす
    fn reduce_depth_inst(&mut self, id: InstId) {
        let inst = &mut self.code.insts[id];
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
                && !self.targets.contains(id)
            {
                // この命令の子孫のbreakを1減らす
                inst.breakable = Breakable::No;
                reduction += 1;
            }

            self.reduction_stack.push(reduction);
        }

        // 命令の子ブロックに対して処理
        let mut next_block_id = self.code.inst_nodes[id].first_block;
        while let Some(block_id) = next_block_id.expand() {
            self.reduce_depth_block(block_id);
            next_block_id = self.code.blocks[block_id].next;
        }

        if breakable {
            self.reduction_stack.pop();
        }
    }

    /// breakableを最適化し、ブロックの最大break段階数を返す
    fn set_breakable_block(&mut self, id: BlockId) -> u32 {
        let block = &mut self.code.blocks[id];

        let mut max_depth = 0;
        let mut next_inst_id = block.first_inst;

        // ブロック内の命令の最大のbreak段階数を求める
        while let Some(inst_id) = next_inst_id.expand() {
            let depth = self.set_breakable_inst(inst_id);
            max_depth = max_depth.max(depth);

            next_inst_id = self.code.inst_nodes[inst_id].next;
        }

        max_depth
    }

    /// breakableを最適化し、命令の最大break段階数を返す
    fn set_breakable_inst(&mut self, id: InstId) -> u32 {
        // br命令なら段階数+1
        if let InstKind::Br(depth) = self.code.insts[id].kind {
            return depth + 1;
        }

        let mut max_depth = 0;
        let mut next_block_id = self.code.inst_nodes[id].first_block;

        // 命令の子ブロックの最大のbreak段階数を求める
        while let Some(block_id) = next_block_id.expand() {
            let depth = self.set_breakable_block(block_id);
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
