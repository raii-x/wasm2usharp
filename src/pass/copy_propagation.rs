use std::collections::HashSet;

use cranelift_entity::SecondaryMap;

use crate::{
    ir::{
        code::{BlockId, Breakable, Code, InstId, InstKind},
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
    reaching_def(code);
}

fn reaching_def(code: &mut Code) -> SecondaryMap<InstId, Sets> {
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

    let mut sets = SecondaryMap::<_, Sets>::with_capacity(code.insts.len());

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

fn reaching_def_in_out(
    code: &mut Code,
    sets: &mut SecondaryMap<InstId, Sets>,
    br_stack: &mut Vec<HashSet<Def>>,
    mut prev_out: HashSet<Def>,
    block_id: BlockId,
) -> Option<InstId> {
    let mut iter = code.block_nodes[block_id].iter();
    while let Some(inst_id) = iter.next(&code.inst_nodes) {
        if matches!(code.insts[inst_id].kind, InstKind::Loop(_)) {
            loop {
                reaching_def_in_out_inst(code, sets, br_stack, &mut prev_out, inst_id);

                // ループ文ならoutにブロックのinを含め、outが前と等しくなるまで繰り返す
                let out = prev_out.or(&sets[inst_id].in_);
                if out == prev_out {
                    // outが更新されなければ終了
                    break;
                }
                prev_out = out;
            }
        } else {
            reaching_def_in_out_inst(code, sets, br_stack, &mut prev_out, inst_id);
        }
    }

    code.block_nodes[block_id]
        .last_child()
        .and_then(|id| match code.insts[id].kind {
            InstKind::Return | InstKind::Br(_) => None,
            _ => Some(id),
        })
}

fn reaching_def_in_out_inst(
    code: &mut Code,
    sets: &mut SecondaryMap<InstId, Sets>,
    br_stack: &mut Vec<HashSet<Def>>,
    prev_out: &mut HashSet<Def>,
    inst_id: InstId,
) {
    sets[inst_id].in_ = prev_out.clone();

    // 子ブロック
    if code.inst_nodes[inst_id].first_child.is_some() {
        let breakable = code.insts[inst_id].breakable != Breakable::No;
        if breakable {
            br_stack.push(HashSet::new());
        }

        let mut block_out = HashSet::new();

        let mut iter = code.inst_nodes[inst_id].iter();
        while let Some(block_id) = iter.next(&code.block_nodes) {
            if let Some(last_inst) =
                reaching_def_in_out(code, sets, br_stack, prev_out.clone(), block_id)
            {
                // ブロックのoutと最後の命令のoutとの和集合をとる
                block_out.or_assign(&sets[last_inst].out);
            }
        }

        // if文でelseが無い場合
        let node = &code.inst_nodes[inst_id];
        if matches!(code.insts[inst_id].kind, InstKind::If) && node.first_child == node.last_child {
            // ブロックのoutとifブロックのinとの和集合をとる
            block_out.or_assign(&sets[inst_id].in_);
        }

        if breakable {
            if matches!(code.insts[inst_id].kind, InstKind::Loop(_)) {
                // ループならスタックブロックのoutをinに含める
                let poped = br_stack.pop().unwrap();
                sets[inst_id].in_.or_assign(&poped);
            } else {
                block_out.or_assign(&br_stack.pop().unwrap());
            }
        }

        // out = gen ∪ (in ∖ kill)
        sets[inst_id].out = sets[inst_id].gen.or(&block_out.sub(&sets[inst_id].kill));
    } else {
        // out = gen ∪ (in ∖ kill)
        sets[inst_id].out = sets[inst_id]
            .gen
            .or(&sets[inst_id].in_.sub(&sets[inst_id].kill));
    }

    // brなら対応するブロック命令のoutとの和集合をとる
    if let InstKind::Br(depth) = code.insts[inst_id].kind {
        let i = br_stack.len() - 1 - depth as usize;
        br_stack[i].or_assign(&sets[inst_id].out);
    }

    *prev_out = sets[inst_id].out.clone();
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use cranelift_entity::{EntityRef, SecondaryMap};

    use crate::{
        ir::{
            builder::Builder,
            code::{Breakable, InstId},
            ty::Const,
            var::{Var, VarId},
        },
        pass::copy_propagation::Sets,
    };

    use super::{reaching_def, Def};

    #[test]
    fn copy_propagation_block() {
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

        builder.push_block(Breakable::No); // 1

        builder.start_block();
        builder.push_set(v2, Const::Int(2).into()); // 2
        builder.push_set(v1, v2.into()); // 3
        builder.end_block();

        builder.push_set(v2, Const::Int(3).into()); // 4

        let mut code = builder.build();

        let sets = reaching_def(&mut code);
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

    #[test]
    fn copy_propagation_if_else() {
        // func(v1, v2)
        // 0: if v1
        // 1:   then: v1 = 1
        // 2:         v2 = 2
        // 3:   else: v2 = 3
        // 4: v1 = 3

        let mut builder = Builder::new(&[]);

        let v1 = builder.new_var(Var {
            local: true,
            ..Default::default()
        });
        let v2 = builder.new_var(Var {
            local: true,
            ..Default::default()
        });

        builder.push_if(v1.into(), Breakable::No); // 0

        builder.start_block();
        builder.push_set(v1, Const::Int(1).into()); // 1
        builder.push_set(v2, Const::Int(2).into()); // 2
        builder.end_block();

        builder.start_block();
        builder.push_set(v2, Const::Int(3).into()); // 3
        builder.end_block();

        builder.push_set(v1, Const::Int(4).into()); // 4

        let mut code = builder.build();

        let sets = reaching_def(&mut code);
        sets_eq(
            &sets,
            &[
                (0, (&[], &[], &[-1, -2], &[-1, 1, 2, 3])),
                (1, (&[1], &[-1, 4], &[-1, -2], &[1, -2])),
                (2, (&[2], &[-2, 3], &[1, -2], &[1, 2])),
                (3, (&[3], &[-2, 2], &[-1, -2], &[-1, 3])),
                (4, (&[4], &[-1, 1], &[-1, 1, 2, 3], &[2, 3, 4])),
            ],
        );
    }

    #[test]
    fn copy_propagation_if() {
        // func(v1)
        // 0: if v1
        // 1:   v1 = 1
        // 2: v1 = 2

        let mut builder = Builder::new(&[]);

        let v1 = builder.new_var(Var {
            local: true,
            ..Default::default()
        });

        builder.push_if(v1.into(), Breakable::No); // 0

        builder.start_block();
        builder.push_set(v1, Const::Int(1).into()); // 1
        builder.end_block();

        builder.push_set(v1, Const::Int(2).into()); // 2

        let mut code = builder.build();

        let sets = reaching_def(&mut code);
        sets_eq(
            &sets,
            &[
                (0, (&[], &[], &[-1], &[-1, 1])),
                (1, (&[1], &[-1, 2], &[-1], &[1])),
                (2, (&[2], &[-1, 1], &[-1, 1], &[2])),
            ],
        );
    }

    #[test]
    fn copy_propagation_br() {
        // func(v1)
        // 0: block (breakable)
        // 1:   block
        // 2:     if v1
        // 3:       v1 = 1
        // 4:       br 0
        // 5:   v1 = 2
        // 6: v1 = 3

        let mut builder = Builder::new(&[]);

        let v1 = builder.new_var(Var {
            local: true,
            ..Default::default()
        });

        builder.push_block(Breakable::Single); // 0
        builder.start_block();

        builder.push_block(Breakable::No); // 1
        builder.start_block();

        builder.push_if(v1.into(), Breakable::No); // 2
        builder.start_block();

        builder.push_set(v1, Const::Int(1).into()); // 3
        builder.push_br(0); // 4

        builder.end_block();
        builder.end_block();

        builder.push_set(v1, Const::Int(2).into()); // 5

        builder.end_block();

        builder.push_set(v1, Const::Int(3).into()); // 6

        let mut code = builder.build();

        let sets = reaching_def(&mut code);
        sets_eq(
            &sets,
            &[
                (0, (&[], &[], &[-1], &[3, 5])),
                (1, (&[], &[], &[-1], &[-1])),
                (2, (&[], &[], &[-1], &[-1])),
                (3, (&[3], &[-1, 5, 6], &[-1], &[3])),
                (4, (&[], &[], &[3], &[3])),
                (5, (&[5], &[-1, 3, 6], &[-1], &[5])),
                (6, (&[6], &[-1, 3, 5], &[3, 5], &[6])),
            ],
        );
    }

    #[test]
    fn copy_propagation_loop() {
        // func(v1)
        // 0: loop (breakable)
        // 1:   if v1 (breakable)
        // 2:     v1 = 1
        // 3:     br 1
        // 4: v1 = 2

        let mut builder = Builder::new(&[]);

        let v1 = builder.new_var(Var {
            local: true,
            ..Default::default()
        });

        builder.push_loop(0, Breakable::Single); // 0
        builder.start_block();

        builder.push_if(v1.into(), Breakable::Multi); // 1
        builder.start_block();

        builder.push_set(v1, Const::Int(1).into()); // 2
        builder.push_br(1); // 3

        builder.end_block();
        builder.end_block();

        builder.push_set(v1, Const::Int(2).into()); // 4

        let mut code = builder.build();

        let sets = reaching_def(&mut code);
        sets_eq(
            &sets,
            &[
                (0, (&[], &[], &[-1, 2], &[-1, 2])),
                (1, (&[], &[], &[-1, 2], &[-1, 2])),
                (2, (&[2], &[-1, 4], &[-1, 2], &[2])),
                (3, (&[], &[], &[2], &[2])),
                (4, (&[4], &[-1, 2], &[-1, 2], &[4])),
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
