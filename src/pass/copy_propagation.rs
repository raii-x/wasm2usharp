use std::collections::HashSet;

use cranelift_entity::SecondaryMap;

use crate::{
    ir::{
        code::{BlockId, Code, InstId, InstKind},
        node::Node,
        var::VarId,
    },
    util::HashSetExt,
};

#[derive(Clone, Default)]
struct Sets {
    gen: HashSet<Def>,
    kill: HashSet<Def>,
    in_: HashSet<Def>,
    out: HashSet<Def>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum Def {
    /// 引数やローカル変数の初期値の場合
    Header(VarId),
    /// 命令の場合
    Inst(InstId),
}

pub fn copy_propagation(code: &mut Code) {
    copy_propagation_inner(code);
}

fn copy_propagation_inner(code: &mut Code) -> SecondaryMap<InstId, Sets> {
    // 全ての変数について、その変数への代入文の集合を求める
    let mut var_def = SecondaryMap::<_, HashSet<_>>::with_capacity(code.vars.len());

    // ローカル変数の初期値
    for (var_id, var) in code.vars.iter() {
        if var.local {
            var_def[var_id].insert(Def::Header(var_id));
        }
    }

    // 代入文
    for (inst_id, inst) in code.insts.iter() {
        if let Some(result) = inst.result {
            var_def[result].insert(Def::Inst(inst_id));
        }
    }

    let mut sets = SecondaryMap::with_capacity(code.insts.len());
    reaching_def_gen_kill(code, &var_def, &mut sets);

    for (_, Sets { gen, in_, .. }) in sets.iter_mut() {
        *in_ = gen.clone();
    }

    // ローカル変数の初期値をprev_outに設定
    let mut prev_out = HashSet::new();
    for (var_id, var) in code.vars.iter() {
        if var.local {
            prev_out.insert(Def::Header(var_id));
        }
    }

    reaching_def_in_out(code, &mut sets, &mut Vec::new(), prev_out, code.root);

    sets
}

fn reaching_def_gen_kill(
    code: &mut Code,
    var_def: &SecondaryMap<VarId, HashSet<Def>>,
    sets: &mut SecondaryMap<InstId, Sets>,
) {
    for (inst_id, inst) in code.insts.iter_mut() {
        if let Some(result) = inst.result {
            // この命令をgenに追加
            sets[inst_id].gen.insert(Def::Inst(inst_id));

            // この命令以外の同じ変数への代入文をkillに追加
            let mut inst_kill = var_def[result].clone();
            inst_kill.remove(&Def::Inst(inst_id));
            sets[inst_id].kill = inst_kill;
        }
    }
}

fn reaching_def_in_out(
    code: &mut Code,
    sets: &mut SecondaryMap<InstId, Sets>,
    inst_stack: &mut Vec<InstId>,
    mut prev_out: HashSet<Def>,
    block_id: BlockId,
) -> Option<InstId> {
    let mut iter = code.block_nodes[block_id].iter();
    while let Some(inst_id) = iter.next(&code.inst_nodes) {
        sets[inst_id].in_ = sets[inst_id].out.or(&prev_out);

        // 子ブロック
        if code.inst_nodes[inst_id].first_child.is_some() {
            inst_stack.push(inst_id);

            let mut iter = code.inst_nodes[inst_id].iter();
            while let Some(block_id) = iter.next(&code.block_nodes) {
                if let Some(last_inst) =
                    reaching_def_in_out(code, sets, inst_stack, prev_out.clone(), block_id)
                {
                    // out_stackのトップと最後の命令のoutとの和集合をとる
                    let i = *inst_stack.last().unwrap();
                    let out = sets[i].out.or(&sets[last_inst].out);
                    sets[i].out = out;
                }
            }

            // out = gen ∪ (in ∖ kill)
            sets[inst_id].out = sets[inst_id]
                .gen
                .or(&sets[inst_stack.pop().unwrap()].out.sub(&sets[inst_id].kill));
        } else {
            // out = gen ∪ (in ∖ kill)
            sets[inst_id].out = sets[inst_id]
                .gen
                .or(&sets[inst_id].in_.sub(&sets[inst_id].kill));
        }

        // brなら対応するブロック命令のoutとの和集合をとる
        if let InstKind::Br(depth) = code.insts[inst_id].kind {
            let i = inst_stack.len() - 1 - depth as usize;
            let out = sets[inst_stack[i]].out.or(&sets[inst_id].out);
            sets[inst_stack[i]].out = out;
        }

        prev_out = sets[inst_id].out.clone();
    }

    code.block_nodes[block_id]
        .last_child()
        .and_then(|id| match code.insts[id].kind {
            InstKind::Return | InstKind::Br(_) => None,
            _ => Some(id),
        })
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use cranelift_entity::{EntityRef, SecondaryMap};

    use crate::{
        ir::{
            builder::Builder,
            code::{Inst, InstId, InstKind},
            ty::Const,
            var::{Var, VarId},
        },
        pass::copy_propagation::Sets,
    };

    use super::{copy_propagation_inner, Def};

    #[test]
    fn test_copy_propagation() {
        // func(v1, v2)
        // 0: v1 = 1
        // 1: block
        // 2:   v2 = 2
        // 3:   v1 = v2
        // 4: v2 = 3

        let mut builder = Builder::new(&[]);

        let v1 = builder.new_var(Var {
            local: true,
            ..Default::default()
        });
        let v2 = builder.new_var(Var {
            local: true,
            ..Default::default()
        });

        builder.push_set(v1, Const::Int(1).into()); // 0

        // 1
        builder.push(Inst {
            kind: InstKind::Block,
            ..Default::default()
        });

        builder.start_block();
        builder.push_set(v2, Const::Int(2).into()); // 2
        builder.push_set(v1, v2.into()); // 3
        builder.end_block();

        builder.push_set(v2, Const::Int(3).into()); // 4

        let mut code = builder.build();

        let sets = copy_propagation_inner(&mut code);
        sets_eq(
            &sets,
            &[
                (0, (&[0], &[-1, 3], &[-1, -2], &[0, -2])),
                (1, (&[], &[], &[0, -2], &[2, 3])),
                (2, (&[2], &[-2, 4], &[0, -2], &[0, 2])),
                (3, (&[3], &[-1, 0], &[0, 2], &[3, 2])),
                (4, (&[4], &[-2, 2], &[3, 2], &[3, 4])),
            ],
        );
    }

    /// (gen, kill, in, out)
    /// -1以下ならDef::Header(-i + 1)となる
    type SetArrs<'a> = (&'a [i32], &'a [i32], &'a [i32], &'a [i32]);

    /// Setsの各集合が等しいか確認する
    fn sets_eq(actual: &SecondaryMap<InstId, Sets>, expected: &[(usize, SetArrs<'_>)]) {
        for (i, expected) in expected.iter() {
            let actual = &actual[InstId::new(*i)];
            set_eq(&actual.gen, expected.0, *i, "gen");
            set_eq(&actual.kill, expected.1, *i, "kill");
            set_eq(&actual.in_, expected.2, *i, "in");
            set_eq(&actual.out, expected.3, *i, "out");
        }
        assert_eq!(actual.keys().len(), expected.len());
    }

    /// 集合が等しいか確認する
    fn set_eq(actual: &HashSet<Def>, expected: &[i32], i: usize, label: &str) {
        let mut actual = actual.iter().copied().collect::<Vec<_>>();
        actual.sort();

        let mut expected = expected
            .iter()
            .map(|&i| {
                if i < 0 {
                    Def::Header(VarId::new(-(i + 1) as usize))
                } else {
                    Def::Inst(InstId::new(i as usize))
                }
            })
            .collect::<Vec<_>>();
        expected.sort();

        assert!(
            actual == expected,
            "assertion failed ([{}].{})\n   actual: {:?}\n expected: {:?}",
            i,
            label,
            actual,
            expected
        );
    }
}
