use std::collections::HashSet;

use cranelift_entity::SecondaryMap;

use crate::{
    ir::{
        code::{BlockId, Breakable, Code, Inst, InstId, InstKind},
        var::{Primary, VarId},
    },
    util::HashSetExt,
};

#[derive(Clone, Default)]
struct Sets {
    gen: HashSet<Def>,
    kill: HashSet<Def>,
    in_: HashSet<Def>,
}

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

fn reaching_def(code: &Code) -> SecondaryMap<InstId, Sets> {
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

    for (inst_id, inst) in code.insts.iter() {
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

    let builder = SetsBuilder::new(code, sets, false);
    builder.build(prev_out)
}

fn copy(code: &Code) -> SecondaryMap<InstId, Sets> {
    // 変数が含まれるコピー文の集合
    let mut copies = SecondaryMap::<_, HashSet<_>>::with_capacity(code.vars.len());

    let mut sets = SecondaryMap::<_, Sets>::with_capacity(code.insts.len());

    for (inst_id, inst) in code.insts.iter() {
        if let Some(result) = inst.result {
            // コピー文
            if inst.pattern == "$p0" {
                // 全ての変数について、その変数を使用するコピー文の集合を求める
                match inst.params[0] {
                    Primary::Var(var_id) => {
                        copies[result].insert(Def::Inst(inst_id));
                        copies[var_id].insert(Def::Inst(inst_id));

                        // この命令をgenに追加
                        sets[inst_id].gen.insert(Def::Inst(inst_id));
                    }
                    Primary::Const(_) => (),
                }
            }
        }
    }

    for (inst_id, inst) in code.insts.iter() {
        if let Some(result) = inst.result {
            // この命令以外の同じ変数へのコピー文をkillに追加
            let mut inst_kill = copies[result].clone();
            inst_kill.remove(&Def::Inst(inst_id));
            sets[inst_id].kill = inst_kill;
        }
    }

    let builder = SetsBuilder::new(code, sets, true);
    builder.build(HashSet::new())
}

struct SetsBuilder<'a> {
    code: &'a Code,
    sets: SecondaryMap<InstId, Sets>,
    br_stack: Vec<Option<HashSet<Def>>>,
    intersection: bool,
}

impl<'a> SetsBuilder<'a> {
    fn new(code: &'a Code, sets: SecondaryMap<InstId, Sets>, intersection: bool) -> Self {
        Self {
            code,
            sets,
            br_stack: Vec::new(),
            intersection,
        }
    }

    fn build(mut self, prev_out: HashSet<Def>) -> SecondaryMap<InstId, Sets> {
        self.in_out_block(prev_out, self.code.root);
        self.sets
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
                    if self.sets[inst_id].in_ == prev_out {
                        // inが更新されなければ終了
                        prev_out = next_out;
                        break;
                    }
                    prev_out = self.sets[inst_id].in_.clone();
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
        let out;
        self.sets[inst_id].in_ = prev_out.clone();

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
                block_out = self.merge_set_option(&block_out, &self.sets[inst_id].in_);
            }

            if breakable {
                if let Some(poped) = self.br_stack.pop().unwrap() {
                    if matches!(self.code.insts[inst_id].kind, InstKind::Loop(_)) {
                        // ループならスタックブロックのoutをinに含める
                        self.sets[inst_id].in_ = self.merge_set(&self.sets[inst_id].in_, &poped);
                    } else {
                        let block_out = block_out.as_mut().unwrap();
                        *block_out = self.merge_set(block_out, &poped);
                    }
                }
            }

            // out = gen ∪ (in ∖ kill)
            if let Some(block_out) = block_out {
                out = self.sets[inst_id]
                    .gen
                    .or(&block_out.sub(&self.sets[inst_id].kill));
            } else {
                out = self.sets[inst_id].gen.clone();
            }
        } else {
            // out = gen ∪ (in ∖ kill)
            out = self.sets[inst_id]
                .gen
                .or(&self.sets[inst_id].in_.sub(&self.sets[inst_id].kill));
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

/// コピー先の変数をコピー元に置き換え、不要なコピー文を削除できるなら削除する
fn replace_copy(
    code: &mut Code,
    reach_sets: &SecondaryMap<InstId, Sets>,
    copy_sets: &SecondaryMap<InstId, Sets>,
) {
    for (copy_inst_id, inst_copy_sets) in copy_sets.iter() {
        if inst_copy_sets.gen.is_empty() {
            // inst_idがコピーではない
            continue;
        }

        let Primary::Var(src) = code.insts[copy_inst_id].params[0] else {
            unreachable!()
        };
        let dst = code.insts[copy_inst_id].result.unwrap();

        let mut removable = true;

        for (reach_inst_id, inst_reach_sets) in reach_sets.iter() {
            if !inst_reach_sets.in_.contains(&Def::Inst(copy_inst_id)) {
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
                if copy_sets[reach_inst_id]
                    .in_
                    .contains(&Def::Inst(copy_inst_id))
                {
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

    use super::{copy, reaching_def, replace_copy, Def, Sets};

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
        sets_eq(
            &reach_sets,
            &[
                (0, (&[0], &[-1, 3], &[-1, -2])),
                (1, (&[], &[], &[0, -2])),
                (2, (&[2], &[-2, 4], &[0, -2])),
                (3, (&[3], &[-1, 0], &[0, 2])),
                (4, (&[4], &[-2, 2], &[3, 2])),
            ],
        );

        let copy_sets = copy(&code);
        sets_eq(
            &copy_sets,
            &[
                (0, (&[], &[3], &[])),
                (1, (&[], &[], &[])),
                (2, (&[], &[3], &[])),
                (3, (&[3], &[], &[])),
                (4, (&[], &[3], &[3])),
            ],
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
        sets_eq(
            &reach_sets,
            &[
                (0, (&[0], &[], &[-1, -2])),
                (1, (&[], &[], &[-1, -2, 0])),
                (2, (&[2], &[-1, 5], &[-1, -2, 0])),
                (3, (&[3], &[-2, 4], &[2, -2, 0])),
                (4, (&[4], &[-2, 3], &[-1, -2, 0])),
                (5, (&[5], &[-1, 2], &[-1, 2, 3, 4, 0])),
            ],
        );

        let copy_sets = copy(&code);
        sets_eq(
            &copy_sets,
            &[
                (0, (&[0], &[], &[])),
                (1, (&[], &[], &[0])),
                (2, (&[], &[0], &[0])),
                (3, (&[], &[], &[])),
                (4, (&[], &[], &[0])),
                (5, (&[], &[0], &[])),
            ],
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
        sets_eq(
            &reach_sets,
            &[
                (0, (&[], &[], &[-1, -2])),
                (1, (&[1], &[-1, 2], &[-1, -2])),
                (2, (&[2], &[-1, 1], &[-1, 1, -2])),
            ],
        );

        let copy_sets = copy(&code);
        sets_eq(
            &copy_sets,
            &[
                (0, (&[], &[], &[])),
                (1, (&[1], &[], &[])),
                (2, (&[], &[1], &[])),
            ],
        );
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
        sets_eq(
            &reach_sets,
            &[
                (0, (&[], &[], &[-1, -2])),
                (1, (&[], &[], &[-1, -2])),
                (2, (&[], &[], &[-1, -2])),
                (3, (&[3], &[-1, 5, 6], &[-1, -2])),
                (4, (&[], &[], &[3, -2])),
                (5, (&[5], &[-1, 3, 6], &[-1, -2])),
                (6, (&[6], &[-1, 3, 5], &[3, 5, -2])),
            ],
        );

        let copy_sets = copy(&code);
        sets_eq(
            &copy_sets,
            &[
                (0, (&[], &[], &[])),
                (1, (&[], &[], &[])),
                (2, (&[], &[], &[])),
                (3, (&[], &[5], &[])),
                (4, (&[], &[], &[])),
                (5, (&[5], &[], &[])),
                (6, (&[], &[5], &[])),
            ],
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
        sets_eq(
            &reach_sets,
            &[
                (0, (&[], &[], &[-1, 2, -2])),
                (1, (&[], &[], &[-1, 2, -2])),
                (2, (&[2], &[-1, 4], &[-1, 2, -2])),
                (3, (&[], &[], &[2, -2])),
                (4, (&[4], &[-1, 2], &[-1, 2, -2])),
            ],
        );

        let copy_sets = copy(&code);
        sets_eq(
            &copy_sets,
            &[
                (0, (&[], &[], &[])),
                (1, (&[], &[], &[])),
                (2, (&[2], &[], &[])),
                (3, (&[], &[], &[2])),
                (4, (&[], &[2], &[])),
            ],
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
        sets_eq(
            &reach_sets,
            &[
                (0, (&[], &[], &[])),
                (1, (&[1], &[], &[])),
                (2, (&[2], &[], &[1])),
                (3, (&[3], &[], &[1, 2])),
            ],
        );

        let copy_sets = copy(&code);
        sets_eq(
            &copy_sets,
            &[
                (0, (&[], &[], &[])),
                (1, (&[], &[2], &[])),
                (2, (&[2], &[3], &[])),
                (3, (&[3], &[], &[2])),
            ],
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
        sets_eq(
            &reach_sets,
            &[
                (0, (&[0], &[2, 4], &[-1])),
                (1, (&[1], &[3], &[-1, 0])),
                (2, (&[2], &[0, 4], &[-1, 0, 1])),
                (3, (&[3], &[1], &[-1, 2, 1])),
                (4, (&[4], &[0, 2], &[-1, 2, 3])),
            ],
        );

        let copy_sets = copy(&code);
        sets_eq(
            &copy_sets,
            &[
                (0, (&[0], &[1, 2], &[])),
                (1, (&[1], &[], &[0])),
                (2, (&[2], &[0, 1], &[0, 1])),
                (3, (&[], &[1], &[2])),
                (4, (&[], &[0, 1, 2], &[2])),
            ],
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
        sets_eq(
            &reach_sets,
            &[
                (0, (&[0], &[], &[-1])),
                (1, (&[], &[], &[-1, 0])),
                (2, (&[2], &[-1], &[-1, 0])),
            ],
        );

        let copy_sets = copy(&code);
        sets_eq(
            &copy_sets,
            &[
                (0, (&[0], &[], &[])),
                (1, (&[], &[], &[0])),
                (2, (&[], &[0], &[0])),
            ],
        );
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
        sets_eq(
            &reach_sets,
            &[
                (0, (&[0], &[], &[-1])),
                (1, (&[1], &[], &[-1, 0])),
                (2, (&[2], &[-1], &[-1, 0, 1])),
                (3, (&[3], &[], &[2, 0, 1])),
                (4, (&[], &[], &[2, 0, 1, 3])),
            ],
        );

        let copy_sets = copy(&code);
        sets_eq(
            &copy_sets,
            &[
                (0, (&[0], &[1], &[])),
                (1, (&[1], &[], &[0])),
                (2, (&[], &[0], &[0, 1])),
                (3, (&[], &[], &[1])),
                (4, (&[], &[], &[1])),
            ],
        );

        replace_copy(&mut code, &reach_sets, &copy_sets);

        // TODO: このテストを通るようにする
        // code_eq_nop(&code, &[true, false, false, false, false]);
    }

    /// (gen, kill, in, out)
    /// -1以下ならDef::Header(-i + 1)となる
    type SetArrs<'a> = (&'a [i32], &'a [i32], &'a [i32]);

    /// Setsの各集合が等しいか確認する
    fn sets_eq(actual: &SecondaryMap<InstId, Sets>, expected: &[(usize, SetArrs<'_>)]) {
        for (i, expected) in expected.iter() {
            let actual = &actual[InstId::new(*i)];
            set_eq(&actual.gen, expected.0, *i, "gen");
            set_eq(&actual.kill, expected.1, *i, "kill");
            set_eq(&actual.in_, expected.2, *i, "in");
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
            "assertion failed (reach[{}].{})\n   actual: {:?}\n expected: {:?}",
            i,
            label,
            actual,
            expected
        );
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
