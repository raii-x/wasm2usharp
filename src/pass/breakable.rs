use cranelift_entity::EntitySet;

use crate::ir::code::{BlockId, Breakable, Code, InstId, InstKind};

/// ブロックを作る命令のbreakableを最適化する
pub fn breakable(code: &mut Code) {
    let mut targets = EntitySet::with_capacity(code.insts.len());
    mark_target(code, &mut targets, &mut Vec::new(), code.root);
    reduce_depth(code, &targets, &mut Vec::new(), code.root);
    set_breakable_block(code, code.root);
}

/// break先の命令をtargetsに追加
fn mark_target(
    code: &Code,
    targets: &mut EntitySet<InstId>,
    inst_stack: &mut Vec<InstId>,
    block_id: BlockId,
) {
    let mut inst_iter = code.block_nodes[block_id].iter();
    while let Some(inst_id) = inst_iter.next(&code.inst_nodes) {
        let inst = &code.insts[inst_id];
        if let InstKind::Br(depth) = inst.kind {
            // break先となる命令のIDをtargetsに追加
            let target_id = inst_stack[inst_stack.len() - 1 - depth as usize];
            targets.insert(target_id);
        }

        let breakable = inst.breakable != Breakable::No;
        if breakable {
            inst_stack.push(inst_id);
        }

        let mut block_iter = code.inst_nodes[inst_id].iter();
        while let Some(block_id) = block_iter.next(&code.block_nodes) {
            mark_target(code, targets, inst_stack, block_id);
        }

        if breakable {
            inst_stack.pop();
        }
    }
}

/// br命令のbreak量を減らす
fn reduce_depth(
    code: &mut Code,
    targets: &EntitySet<InstId>,
    reduction_stack: &mut Vec<u32>,
    block_id: BlockId,
) {
    let mut inst_iter = code.block_nodes[block_id].iter();
    while let Some(inst_id) = inst_iter.next(&code.inst_nodes) {
        let inst = &mut code.insts[inst_id];
        if let InstKind::Br(depth) = &mut inst.kind {
            // reduction_stackの (最後の値 - 最後からdepth番目の値) がbreakの減少量となる
            *depth -= reduction_stack.last().unwrap()
                - reduction_stack[reduction_stack.len() - 1 - *depth as usize];
        }

        let breakable = inst.breakable != Breakable::No;
        if breakable {
            let mut reduction = if let Some(last) = reduction_stack.last() {
                *last
            } else {
                0
            };

            if !matches!(inst.kind, InstKind::Loop(_) | InstKind::Switch)
                && !targets.contains(inst_id)
            {
                // この命令の子孫のbreakを1減らす
                inst.breakable = Breakable::No;
                reduction += 1;
            }

            reduction_stack.push(reduction);
        }

        // 命令の子ブロックに対して処理
        let mut block_iter = code.inst_nodes[inst_id].iter();
        while let Some(block_id) = block_iter.next(&code.block_nodes) {
            reduce_depth(code, targets, reduction_stack, block_id);
        }

        if breakable {
            reduction_stack.pop();
        }
    }
}

/// breakableを最適化し、ブロックの最大break段階数を返す
fn set_breakable_block(code: &mut Code, block_id: BlockId) -> u32 {
    let mut max_depth = 0;

    // ブロック内の命令の最大のbreak段階数を求める
    let mut inst_iter = code.block_nodes[block_id].iter();
    while let Some(inst_id) = inst_iter.next(&code.inst_nodes) {
        let depth = set_breakable_inst(code, inst_id);
        max_depth = max_depth.max(depth);
    }

    max_depth
}

/// breakableを最適化し、命令の最大break段階数を返す
fn set_breakable_inst(code: &mut Code, inst_id: InstId) -> u32 {
    // br命令なら段階数+1
    if let InstKind::Br(depth) = code.insts[inst_id].kind {
        return depth + 1;
    }

    let mut max_depth = 0;

    // 命令の子ブロックの最大のbreak段階数を求める
    let mut block_iter = code.inst_nodes[inst_id].iter();
    while let Some(block_id) = block_iter.next(&code.block_nodes) {
        let depth = set_breakable_block(code, block_id);
        max_depth = max_depth.max(depth);
    }

    let inst = &mut code.insts[inst_id];
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
