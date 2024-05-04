use std::collections::HashSet;

use cranelift_entity::SecondaryMap;

use crate::{
    ir::{
        code::{BlockId, Breakable, Code, Inst, InstId, InstKind},
        var::{Primary, VarId},
    },
    util::HashSetExt,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum Def {
    /// 引数やローカル変数の初期値の場合
    Header(VarId),
    /// 命令の場合
    Inst(InstId),
}

pub fn copy_propagation(code: &mut Code) {
    let reach_sets = reaching_def(code);
    let copy_sets = copy(code);
    replace_copy(code, &reach_sets, &copy_sets);
}

fn reaching_def(code: &Code) -> SecondaryMap<InstId, HashSet<Def>> {
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

    let mut in_sets = SecondaryMap::<_, HashSet<Def>>::with_capacity(code.insts.len());

    let gen = |inst_id| code.insts[inst_id].result.is_some();
    let kill = |inst_id, set: &mut HashSet<Def>| {
        if let Some(result) = code.insts[inst_id].result {
            for def in &var_def[result] {
                match def {
                    Def::Inst(id) if *id == inst_id => {}
                    _ => {
                        // この命令以外の同じ変数への代入文を削除する
                        set.remove(def);
                    }
                }
            }
        }
    };

    for (inst_id, in_) in in_sets.iter_mut() {
        if gen(inst_id) {
            in_.insert(Def::Inst(inst_id));
        }
    }

    // ローカル変数の初期値をprev_outに設定
    let mut prev_out = HashSet::new();
    for (var_id, var) in code.vars.iter() {
        if var.local {
            prev_out.insert(Def::Header(var_id));
        }
    }

    let builder = SetsBuilder::new(code, in_sets, false, gen, kill);
    builder.build(prev_out)
}

fn copy(code: &Code) -> SecondaryMap<InstId, HashSet<Def>> {
    // 変数が含まれるコピー文の集合
    let mut copies = SecondaryMap::<_, HashSet<_>>::with_capacity(code.vars.len());

    let in_sets = SecondaryMap::<_, HashSet<Def>>::with_capacity(code.insts.len());

    for (inst_id, inst) in code.insts.iter() {
        if let Some(result) = inst.result {
            // コピー文
            if is_copy(inst) {
                // 全ての変数について、その変数を使用するコピー文の集合を求める
                match inst.params[0] {
                    Primary::Var(var_id) => {
                        copies[result].insert(Def::Inst(inst_id));
                        copies[var_id].insert(Def::Inst(inst_id));
                    }
                    Primary::Const(_) => (),
                }
            }
        }
    }

    let gen = |inst_id| is_copy(&code.insts[inst_id]);
    let kill = |inst_id, set: &mut HashSet<Def>| {
        if let Some(result) = code.insts[inst_id].result {
            for def in &copies[result] {
                match def {
                    Def::Inst(id) if *id == inst_id => {}
                    _ => {
                        // この命令以外の同じ変数へのコピー文を削除する
                        set.remove(def);
                    }
                }
            }
        }
    };
    let builder = SetsBuilder::new(code, in_sets, true, gen, kill);
    builder.build(HashSet::new())
}

struct SetsBuilder<'a, F1, F2> {
    code: &'a Code,
    in_sets: SecondaryMap<InstId, HashSet<Def>>,
    br_stack: Vec<Option<HashSet<Def>>>,
    intersection: bool,
    /// 引数のInstIdがgenに含まれるかどうかを返すクロージャ
    gen: F1,
    /// 引数の集合からInstIdのkillの要素を削除するクロージャ
    kill: F2,
}

impl<'a, F1, F2> SetsBuilder<'a, F1, F2>
where
    F1: Fn(InstId) -> bool,
    F2: Fn(InstId, &mut HashSet<Def>),
{
    fn new(
        code: &'a Code,
        in_sets: SecondaryMap<InstId, HashSet<Def>>,
        intersection: bool,
        gen: F1,
        kill: F2,
    ) -> Self {
        Self {
            code,
            in_sets,
            br_stack: Vec::new(),
            intersection,
            gen,
            kill,
        }
    }

    fn build(mut self, prev_out: HashSet<Def>) -> SecondaryMap<InstId, HashSet<Def>> {
        self.in_out_block(prev_out, self.code.root);
        self.in_sets
    }

    fn in_out_block(
        &mut self,
        mut prev_out: HashSet<Def>,
        block_id: BlockId,
    ) -> Option<HashSet<Def>> {
        let mut iter = self.code.block_nodes[block_id].iter();
        while let Some(inst_id) = iter.next(&self.code.inst_nodes) {
            if matches!(self.code.insts[inst_id].kind, InstKind::Loop(_)) {
                loop {
                    let next_out = self.in_out_inst(&prev_out, inst_id);

                    // ループ文ならinが前と等しくなるまで繰り返す
                    if self.in_sets[inst_id] == prev_out {
                        // inが更新されなければ終了
                        prev_out = next_out;
                        break;
                    }
                    prev_out = self.in_sets[inst_id].clone();
                }
            } else {
                prev_out = self.in_out_inst(&prev_out, inst_id);
            }
        }

        self.code.block_nodes[block_id]
            .last_child
            .expand()
            .and_then(|id| match self.code.insts[id].kind {
                InstKind::Return | InstKind::Br(_) => None,
                _ => Some(prev_out),
            })
    }

    fn in_out_inst(&mut self, prev_out: &HashSet<Def>, inst_id: InstId) -> HashSet<Def> {
        let mut out;
        self.in_sets[inst_id] = prev_out.clone();

        // 子ブロック
        if self.code.inst_nodes[inst_id].first_child.is_some() {
            let breakable = self.code.insts[inst_id].breakable != Breakable::No;
            if breakable {
                self.br_stack.push(None);
            }

            let mut block_out: Option<HashSet<Def>> = None;

            let mut iter = self.code.inst_nodes[inst_id].iter();
            while let Some(block_id) = iter.next(&self.code.block_nodes) {
                // ブロックのoutと最後の命令のoutを統合する
                if let Some(child_out) = self.in_out_block(prev_out.clone(), block_id) {
                    block_out = self.merge_set_option(&block_out, &child_out);
                } else {
                    // ブロックが空の場合
                    block_out = self.merge_set_option(&block_out, prev_out);
                }
            }

            // if文でelseが無い場合
            let node = &self.code.inst_nodes[inst_id];
            if matches!(self.code.insts[inst_id].kind, InstKind::If)
                && node.first_child == node.last_child
            {
                // ブロックのoutとifブロックのinを統合する
                // brやreturnで終わる場合はblock_outはNoneとなる
                block_out = self.merge_set_option(&block_out, &self.in_sets[inst_id]);
            }

            if breakable {
                if let Some(poped) = self.br_stack.pop().unwrap() {
                    if matches!(self.code.insts[inst_id].kind, InstKind::Loop(_)) {
                        // ループならスタックブロックのoutをinに含める
                        self.in_sets[inst_id] = self.merge_set(&self.in_sets[inst_id], &poped);
                    } else {
                        let block_out = block_out.as_mut().unwrap();
                        *block_out = self.merge_set(block_out, &poped);
                    }
                }
            }

            // out = gen ∪ (in ∖ kill)
            if let Some(block_out) = block_out {
                out = block_out;
                (self.kill)(inst_id, &mut out); // in ∖ kill
            } else {
                out = HashSet::new();
            }
        } else {
            // out = gen ∪ (in ∖ kill)
            out = self.in_sets[inst_id].clone();
            (self.kill)(inst_id, &mut out); // in ∖ kill
        }

        if (self.gen)(inst_id) {
            out.insert(Def::Inst(inst_id)); // ∪ gen
        }

        // brなら対応するブロック命令のoutと集合を統合する
        if let InstKind::Br(depth) = self.code.insts[inst_id].kind {
            let i = self.br_stack.len() - 1 - depth as usize;
            self.br_stack[i] = self.merge_set_option(&self.br_stack[i], &out);
        }

        out
    }

    fn merge_set(&self, x: &HashSet<Def>, y: &HashSet<Def>) -> HashSet<Def> {
        if self.intersection {
            x.and(y)
        } else {
            x.or(y)
        }
    }

    fn merge_set_option(&self, x: &Option<HashSet<Def>>, y: &HashSet<Def>) -> Option<HashSet<Def>> {
        Some(match x {
            Some(x) => self.merge_set(x, y),
            None => y.clone(),
        })
    }
}

fn is_copy(inst: &Inst) -> bool {
    matches!(inst.kind, InstKind::Expr)
        && inst.result.is_some()
        && inst.pattern == "$p0"
        && matches! {inst.params[0], Primary::Var(_)}
}

/// コピー先の変数をコピー元に置き換え、不要なコピー文を削除できるなら削除する
fn replace_copy(
    code: &mut Code,
    reach_sets: &SecondaryMap<InstId, HashSet<Def>>,
    copy_sets: &SecondaryMap<InstId, HashSet<Def>>,
) {
    for (copy_inst_id, _) in copy_sets.iter() {
        if !is_copy(&code.insts[copy_inst_id]) {
            // inst_idがコピーではない
            continue;
        }

        let Primary::Var(src) = code.insts[copy_inst_id].params[0] else {
            unreachable!()
        };
        let dst = code.insts[copy_inst_id].result.unwrap();

        let mut removable = true;

        for (reach_inst_id, inst_reach_sets) in reach_sets.iter() {
            if !inst_reach_sets.contains(&Def::Inst(copy_inst_id)) {
                // コピー文がreach_inst_idの文に到達可能ではない
                continue;
            }

            let inst = &mut code.insts[reach_inst_id];

            // reach_inst_idの文でコピー先の変数を使用しているかどうか
            if inst.params.contains(&dst.into())
                || inst
                    .call
                    .as_ref()
                    .is_some_and(|c| c.recursive && c.save_vars.contains(&dst))
            {
                if copy_sets[reach_inst_id].contains(&Def::Inst(copy_inst_id)) {
                    // paramsをコピー元の変数に書き換え
                    for p in &mut inst.params {
                        match p {
                            Primary::Var(p) if *p == dst => *p = src,
                            _ => (),
                        }
                    }

                    // save_varをコピー元の変数に書き換え
                    if let Some(call) = &mut inst.call {
                        if call.recursive {
                            for save_var in &mut call.save_vars {
                                if *save_var == dst {
                                    *save_var = src;
                                }
                            }
                        }
                    }
                } else {
                    // copyのinの集合に含まれないものがある場合は、コピー文の削除を行わない
                    removable = false;
                }
            }
        }

        if removable {
            // コピー文を削除
            code.insts[copy_inst_id] = Inst {
                kind: InstKind::Nop,
                ..Default::default()
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use cranelift_entity::{EntityRef, SecondaryMap};

    use crate::ir::{
        builder::Builder,
        code::{Breakable, Code, InstId, InstKind},
        ty::Const,
        var::{Var, VarId},
    };

    use super::{copy, reaching_def, replace_copy, Def};

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

        let code = builder.build();

        let reach_sets = reaching_def(&code);
        set_eq(
            &reach_sets,
            &[
                (0, &[-1, -2]),
                (1, &[0, -2]),
                (2, &[0, -2]),
                (3, &[0, 2]),
                (4, &[3, 2]),
            ],
            "reach",
        );

        let copy_sets = copy(&code);
        set_eq(
            &copy_sets,
            &[(0, &[]), (1, &[]), (2, &[]), (3, &[]), (4, &[3])],
            "copy",
        );
    }

    #[test]
    fn copy_propagation_if_else() {
        // func(v1, v2)
        // 0: v3 = v1
        // 1: if v1
        // 2:   then: v1 = 1
        // 3:         v2 = 2
        // 4:   else: v2 = 3
        // 5: v1 = 3

        let mut builder = Builder::new(&[]);

        let v1 = builder.new_var(Var {
            local: true,
            ..Default::default()
        });
        let v2 = builder.new_var(Var {
            local: true,
            ..Default::default()
        });
        let v3 = builder.new_var(Var {
            ..Default::default()
        });

        builder.push_set(v3, v1.into()); // 0
        builder.push_if(v1.into(), Breakable::No); // 1

        builder.start_block();
        builder.push_set(v1, Const::Int(1).into()); // 2
        builder.push_set(v2, Const::Int(2).into()); // 3
        builder.end_block();

        builder.start_block();
        builder.push_set(v2, Const::Int(3).into()); // 4
        builder.end_block();

        builder.push_set(v1, Const::Int(4).into()); // 5

        let code = builder.build();

        let reach_sets = reaching_def(&code);
        set_eq(
            &reach_sets,
            &[
                (0, &[-1, -2]),
                (1, &[-1, -2, 0]),
                (2, &[-1, -2, 0]),
                (3, &[2, -2, 0]),
                (4, &[-1, -2, 0]),
                (5, &[-1, 2, 3, 4, 0]),
            ],
            "reach",
        );

        let copy_sets = copy(&code);
        set_eq(
            &copy_sets,
            &[
                (0, &[]),
                (1, &[0]),
                (2, &[0]),
                (3, &[]),
                (4, &[0]),
                (5, &[]),
            ],
            "copy",
        );
    }

    #[test]
    fn copy_propagation_if() {
        // func(v1, v2)
        // 0: if v1
        // 1:   v1 = v2
        // 2: v1 = 2

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
        builder.push_set(v1, v2.into()); // 1
        builder.end_block();

        builder.push_set(v1, Const::Int(2).into()); // 2

        let code = builder.build();

        let reach_sets = reaching_def(&code);
        set_eq(
            &reach_sets,
            &[(0, &[-1, -2]), (1, &[-1, -2]), (2, &[-1, 1, -2])],
            "reach",
        );

        let copy_sets = copy(&code);
        set_eq(&copy_sets, &[(0, &[]), (1, &[]), (2, &[])], "copy");
    }

    #[test]
    fn copy_propagation_br() {
        // func(v1, v2)
        // 0: block (breakable)
        // 1:   block
        // 2:     if v1
        // 3:       v1 = 1
        // 4:       br 0
        // 5:   v1 = v2
        // 6: v1 = 3

        let mut builder = Builder::new(&[]);

        let v1 = builder.new_var(Var {
            local: true,
            ..Default::default()
        });
        let v2 = builder.new_var(Var {
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

        builder.push_set(v1, v2.into()); // 5

        builder.end_block();

        builder.push_set(v1, Const::Int(3).into()); // 6

        let code = builder.build();

        let reach_sets = reaching_def(&code);
        set_eq(
            &reach_sets,
            &[
                (0, &[-1, -2]),
                (1, &[-1, -2]),
                (2, &[-1, -2]),
                (3, &[-1, -2]),
                (4, &[3, -2]),
                (5, &[-1, -2]),
                (6, &[3, 5, -2]),
            ],
            "reach",
        );

        let copy_sets = copy(&code);
        set_eq(
            &copy_sets,
            &[
                (0, &[]),
                (1, &[]),
                (2, &[]),
                (3, &[]),
                (4, &[]),
                (5, &[]),
                (6, &[]),
            ],
            "copy",
        );
    }

    #[test]
    fn copy_propagation_loop() {
        // func(v1, v2)
        // 0: loop (breakable)
        // 1:   if v1 (breakable)
        // 2:     v1 = v2
        // 3:     br 1
        // 4: v1 = 2

        let mut builder = Builder::new(&[]);

        let v1 = builder.new_var(Var {
            local: true,
            ..Default::default()
        });
        let v2 = builder.new_var(Var {
            local: true,
            ..Default::default()
        });

        builder.push_loop(0, Breakable::Single); // 0
        builder.start_block();

        builder.push_if(v1.into(), Breakable::Multi); // 1
        builder.start_block();

        builder.push_set(v1, v2.into()); // 2
        builder.push_br(1); // 3

        builder.end_block();
        builder.end_block();

        builder.push_set(v1, Const::Int(2).into()); // 4

        let code = builder.build();

        let reach_sets = reaching_def(&code);
        set_eq(
            &reach_sets,
            &[
                (0, &[-1, 2, -2]),
                (1, &[-1, 2, -2]),
                (2, &[-1, 2, -2]),
                (3, &[2, -2]),
                (4, &[-1, 2, -2]),
            ],
            "reach",
        );

        let copy_sets = copy(&code);
        set_eq(
            &copy_sets,
            &[(0, &[]), (1, &[]), (2, &[]), (3, &[2]), (4, &[])],
            "copy",
        );
    }

    #[test]
    fn copy_propagation_loop_no_br() {
        // 0: loop
        // 1:   v1 = 1
        // 2:   v2 = v1
        // 3: v3 = v2

        let mut builder = Builder::new(&[]);

        let v1 = builder.new_var(Var {
            ..Default::default()
        });
        let v2 = builder.new_var(Var {
            ..Default::default()
        });
        let v3 = builder.new_var(Var {
            ..Default::default()
        });

        builder.push_loop(0, Breakable::Single); // 0
        builder.start_block();
        builder.push_set(v1, Const::Int(1).into()); // 1
        builder.push_set(v2, v1.into()); // 2
        builder.end_block();
        builder.push_set(v3, v2.into()); // 3

        let code = builder.build();

        reaching_def(&code);
        copy(&code);

        let reach_sets = reaching_def(&code);
        set_eq(
            &reach_sets,
            &[(0, &[]), (1, &[]), (2, &[1]), (3, &[1, 2])],
            "reach",
        );

        let copy_sets = copy(&code);
        set_eq(
            &copy_sets,
            &[(0, &[]), (1, &[]), (2, &[]), (3, &[2])],
            "copy",
        );
    }

    #[test]
    fn copy_propagation_copy() {
        // func(v1)
        // 0: v2 = v1
        // 1: v3 = v2;
        // 2: v2 = v1;
        // 3: v3 = 1;
        // 4: v2 = 2;

        let mut builder = Builder::new(&[]);

        let v1 = builder.new_var(Var {
            local: true,
            ..Default::default()
        });
        let v2 = builder.new_var(Var {
            ..Default::default()
        });
        let v3 = builder.new_var(Var {
            ..Default::default()
        });

        builder.push_set(v2, v1.into()); // 0
        builder.push_set(v3, v2.into()); // 1
        builder.push_set(v2, v1.into()); // 2
        builder.push_set(v3, Const::Int(1).into()); // 3
        builder.push_set(v2, Const::Int(2).into()); // 4

        let code = builder.build();

        let reach_sets = reaching_def(&code);
        set_eq(
            &reach_sets,
            &[
                (0, &[-1]),
                (1, &[-1, 0]),
                (2, &[-1, 0, 1]),
                (3, &[-1, 2, 1]),
                (4, &[-1, 2, 3]),
            ],
            "reach",
        );

        let copy_sets = copy(&code);
        set_eq(
            &copy_sets,
            &[(0, &[]), (1, &[0]), (2, &[0, 1]), (3, &[2]), (4, &[2])],
            "copy",
        );
    }

    #[test]
    fn copy_propagation_empty_block() {
        // func(v1)
        // 0: v2 = v1
        // 1: block
        // 2: v1 = 1

        let mut builder = Builder::new(&[]);

        let v1 = builder.new_var(Var {
            local: true,
            ..Default::default()
        });
        let v2 = builder.new_var(Var {
            ..Default::default()
        });

        builder.push_set(v2, v1.into());

        builder.push_block(Breakable::No);
        builder.start_block();
        builder.end_block();

        builder.push_set(v1, Const::Int(1).into());

        let code = builder.build();

        let reach_sets = reaching_def(&code);
        set_eq(
            &reach_sets,
            &[(0, &[-1]), (1, &[-1, 0]), (2, &[-1, 0])],
            "reach",
        );

        let copy_sets = copy(&code);
        set_eq(&copy_sets, &[(0, &[]), (1, &[0]), (2, &[0])], "copy");
    }

    #[test]
    fn copy_propagation_add() {
        // func(v1, v2)
        // 0: v3 = v1
        // 1: v4 = v2;
        // 2: v5 = v3 + v4;

        let mut builder = Builder::new(&[]);

        let v1 = builder.new_var(Var {
            local: true,
            ..Default::default()
        });
        let v2 = builder.new_var(Var {
            local: true,
            ..Default::default()
        });
        let v3 = builder.new_var(Var {
            ..Default::default()
        });
        let v4 = builder.new_var(Var {
            ..Default::default()
        });
        let v5 = builder.new_var(Var {
            ..Default::default()
        });

        builder.push_set(v3, v1.into()); // 0
        builder.push_set(v4, v2.into()); // 1
        builder.push_set_pattern(v5, "$p0 + $p1", vec![v3.into(), v4.into()]); // 2

        let mut code = builder.build();

        let reach_sets = reaching_def(&code);
        let copy_sets = copy(&code);
        replace_copy(&mut code, &reach_sets, &copy_sets);

        code_eq_nop(&code, &[true, true, false]);
    }

    #[test]
    fn copy_propagation_tee() {
        // func(x)
        // 0: v1 = x
        // 1: tmp = v1
        // 2: x = v1 / 2
        // 3: v2 = x + tmp
        // 4: return v2

        let mut builder = Builder::new(&[]);

        let x = builder.new_var(Var {
            local: true,
            ..Default::default()
        });
        let tmp = builder.new_var(Var {
            ..Default::default()
        });
        let v1 = builder.new_var(Var {
            ..Default::default()
        });
        let v2 = builder.new_var(Var {
            ..Default::default()
        });

        builder.push_set(v1, x.into()); // 0
        builder.push_set(tmp, v1.into()); // 1
        builder.push_set_pattern(x, "$p0 / $p1", vec![v1.into(), Const::Int(2).into()]); // 2
        builder.push_set_pattern(v2, "$p0 + $p1", vec![x.into(), tmp.into()]); // 3
        builder.push_return(Some(v2.into())); // 4

        let mut code = builder.build();

        let reach_sets = reaching_def(&code);
        set_eq(
            &reach_sets,
            &[
                (0, &[-1]),
                (1, &[-1, 0]),
                (2, &[-1, 0, 1]),
                (3, &[2, 0, 1]),
                (4, &[2, 0, 1, 3]),
            ],
            "reach",
        );

        let copy_sets = copy(&code);
        set_eq(
            &copy_sets,
            &[(0, &[]), (1, &[0]), (2, &[0, 1]), (3, &[1]), (4, &[1])],
            "copy",
        );

        replace_copy(&mut code, &reach_sets, &copy_sets);

        // TODO: このテストを通るようにする
        // code_eq_nop(&code, &[true, false, false, false, false]);
    }

    /// 集合が等しいか確認する
    /// expectedの右側は-1以下ならDef::Header(-i + 1)となる
    fn set_eq(
        actual: &SecondaryMap<InstId, HashSet<Def>>,
        expected: &[(usize, &[i32])],
        label: &str,
    ) {
        for (i, expected) in expected.iter() {
            let actual = &actual[InstId::new(*i)];
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
                "assertion failed ({}[{}])\n   actual: {:?}\n expected: {:?}",
                label,
                i,
                actual,
                expected
            );
        }
        assert_eq!(actual.keys().len(), expected.len());
    }

    /// コードの各命令がnopか確認する
    fn code_eq_nop(code: &Code, expected: &[bool]) {
        let actual = code
            .insts
            .iter()
            .map(|(_, inst)| matches!(inst.kind, InstKind::Nop))
            .collect::<Vec<_>>();
        assert_eq!(actual, expected);
    }
}
