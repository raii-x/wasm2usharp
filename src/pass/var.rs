use cranelift_entity::EntitySet;

use crate::ir::{
    code::{Breakable, Code, InstKind},
    var::Primary,
};

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

/// 未使用のBREAK_DEPTH変数を削除する
pub fn remove_unused_break_depth(code: &mut Code) {
    // break_depthを使用する命令を使用していればreturn
    for (_, inst) in code.insts.iter() {
        if inst.breakable == Breakable::Multi {
            return;
        }
        if let InstKind::Br(depth) = &inst.kind {
            if *depth > 0 {
                return;
            }
        }
    }

    code.break_depth_used = false;
}

#[cfg(test)]
mod tests {
    use cranelift_entity::EntityRef;

    use crate::ir::{
        builder::Builder,
        code::{Call, InstId},
        ty::Const,
        var::Var,
    };

    use super::remove_unused_vars;

    #[test]
    fn test_remove_unused_vars() {
        // 未使用のvar0が削除され、使用されているvar1とvar2は削除されない
        let mut builder = Builder::new(&[]);

        let var0 = builder.new_var(Var {
            ..Default::default()
        });
        let var1 = builder.new_var(Var {
            ..Default::default()
        });
        let var2 = builder.new_var(Var {
            ..Default::default()
        });

        builder.push_call(
            Call {
                func: 0,
                recursive: true,
                save_vars: vec![var0, var1, var2],
                save_loop_vars: vec![],
            },
            vec![],
            None,
        );
        builder.push_set(var1, Const::Int(1).into());
        builder.push_return(Some(var2.into()));

        let mut code = builder.build();

        remove_unused_vars(&mut code);

        assert!(!code.vars[var0].used);
        assert!(code.vars[var1].used);
        assert!(code.vars[var2].used);
        assert_eq!(
            code.insts[InstId::new(0)].call.as_ref().unwrap().save_vars,
            vec![var1, var2]
        );
    }
}
