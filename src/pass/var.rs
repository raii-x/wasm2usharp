use cranelift_entity::EntitySet;

use crate::ir::{code::Code, var::Primary};

/// 未使用の変数を削除する
pub fn remove_unused_vars(code: &mut Code) {
    let mut used_vars = EntitySet::with_capacity(code.vars.len());

    // 使用している変数を解析
    for (_, inst) in code.insts.iter() {
        for param in &inst.params {
            if let Primary::Var(var_id) = param {
                used_vars.insert(*var_id);
            }
        }
        if let Some(var_id) = inst.result {
            used_vars.insert(var_id);
        }
    }

    // 未使用の変数を削除
    for var_id in code.vars.keys() {
        if !used_vars.contains(var_id) {
            code.vars[var_id].used = false;
        }
    }

    // 未使用の変数を保存しないようにする
    for (_, inst) in code.insts.iter_mut() {
        if let Some(call) = &mut inst.call {
            call.save_vars.retain(|var_id| used_vars.contains(*var_id));
        }
    }
}
