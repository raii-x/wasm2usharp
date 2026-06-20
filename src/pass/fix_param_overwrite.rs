use cranelift_entity::EntityRef;

use crate::ir::{
    code::{BlockId, Code, Inst, InstKind},
    var::{Primary, Var},
};

// UdonSharpの関数呼び出しでは、最初の引数から変数に代入していくため、自分自身を再帰呼び出しする場合に
// 後の引数で先の引数に対応する変数を使用している場合、後の引数は先の引数で上書きされた値になる。
// これを避けるため、該当する場合は別の変数を使用する。
pub fn fix_param_overwrite(code: &mut Code, func: usize) {
    fix_param_overwrite_impl(code, func, code.root);
}

fn fix_param_overwrite_impl(code: &mut Code, func: usize, block_id: BlockId) {
    let mut inst_iter = code.block_nodes[block_id].iter();
    while let Some(inst_id) = inst_iter.next(&code.inst_nodes) {
        let mut set_insts = Vec::new();
        let inst = &mut code.insts[inst_id];

        if let Some(call) = &mut inst.call {
            if func == call.func {
                // 自身を呼び出す関数
                for (i, param) in inst.params.iter_mut().enumerate() {
                    let ty = param.ty(&code.vars);
                    if let Primary::Var(param_id) = param {
                        if param_id.index() < i {
                            // 新しい変数を作成して引数を置き換える
                            let new_var = code.vars.push(Var {
                                ty,
                                local: false,
                                ..Default::default()
                            });

                            set_insts.push(Inst {
                                kind: InstKind::Expr,
                                pattern: "$p0".to_string(),
                                params: vec![(*param_id).into()],
                                result: Some(new_var),
                                ..Default::default()
                            });

                            *param = Primary::Var(new_var);
                        }
                    }
                }
            }
        }

        // 変数を代入する命令を追加
        let set_insts = set_insts
            .into_iter()
            .map(|inst| code.insts.push(inst))
            .collect::<Vec<_>>();

        if !set_insts.is_empty() {
            for (i, &set_inst_id) in set_insts.iter().enumerate() {
                code.inst_nodes[set_inst_id].parent = block_id.into();

                if i == 0 {
                    // 最初の命令の場合
                    if let Some(prev_inst_id) = code.inst_nodes[inst_id].prev.take() {
                        // 前の命令の次にする
                        code.inst_nodes[prev_inst_id].next = set_inst_id.into();
                    } else {
                        // 親ブロックの最初の子にする
                        code.block_nodes[block_id].first_child = set_insts[0].into();
                    }
                } else {
                    code.inst_nodes[set_inst_id].prev = set_insts[i - 1].into();
                }

                if i == set_insts.len() - 1 {
                    // 最後の命令の場合、関数呼び出し命令の前にする
                    code.inst_nodes[set_inst_id].next = inst_id.into();
                    code.inst_nodes[inst_id].prev = set_inst_id.into();
                } else {
                    code.inst_nodes[set_inst_id].next = set_insts[i + 1].into();
                }
            }
        }

        // 命令の子ブロックに対して処理
        let mut block_iter = code.inst_nodes[inst_id].iter();
        while let Some(block_id) = block_iter.next(&code.block_nodes) {
            fix_param_overwrite_impl(code, func, block_id);
        }
    }
}
