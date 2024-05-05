use std::collections::{HashMap, HashSet};

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
    let (use_defs, def_uses) = reaching_def(
        code,
        #[cfg(test)]
        None,
    );
    let copy_in = copy(code);
    replace_copy(code, &use_defs, &def_uses, &copy_in);
}

/// テスト時には引数のin_setsにReaching definitionのinを出力する
fn reaching_def(
    code: &Code,
    #[cfg(test)] mut in_sets: Option<&mut SecondaryMap<InstId, HashSet<Def>>>,
) -> (
    SecondaryMap<InstId, HashSet<Def>>,
    HashMap<Def, HashSet<InstId>>,
) {
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

    let mut use_defs: SecondaryMap<InstId, HashSet<Def>> =
        SecondaryMap::with_capacity(code.insts.len());
    let mut def_uses: HashMap<Def, HashSet<InstId>> = HashMap::with_capacity(code.insts.len());

    let mut in_add_var = |inst_id, out: &HashSet<Def>, var_id| {
        for def in &var_def[var_id] {
            if out.contains(def) {
                use_defs[inst_id].insert(*def);
                def_uses.entry(*def).or_default().insert(inst_id);
            }
        }
    };

    let in_ = |inst_id, out: &HashSet<Def>| {
        #[cfg(test)]
        if let Some(in_sets) = &mut in_sets {
            // Reaching definitionのinを出力
            in_sets[inst_id] = out.clone();
        }

        let inst = &code.insts[inst_id];

        // outにparamsの変数が含まれているかチェック
        for param in &inst.params {
            let Primary::Var(var_id) = param else {
                continue;
            };
            in_add_var(inst_id, out, *var_id);
        }

        // outにsave_varsの変数が含まれているかチェック
        if let Some(call) = &inst.call {
            if call.recursive {
                for var_id in &call.save_vars {
                    in_add_var(inst_id, out, *var_id);
                }
            }
        }
    };

    // ローカル変数の初期値をprev_outに設定
    let mut prev_out = HashSet::new();
    for (var_id, var) in code.vars.iter() {
        if var.local {
            prev_out.insert(Def::Header(var_id));
        }
    }

    let builder = SetsBuilder::new(code, Merge::Union, gen, kill, in_);
    builder.build(prev_out);
    (use_defs, def_uses)
}

fn copy(code: &Code) -> SecondaryMap<InstId, HashSet<Def>> {
    // 変数が含まれるコピー文の集合
    let mut copies = SecondaryMap::<_, HashSet<_>>::with_capacity(code.vars.len());

    let mut in_sets = SecondaryMap::<_, HashSet<Def>>::with_capacity(code.insts.len());

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
    let in_ = |inst_id, out: &HashSet<_>| in_sets[inst_id] = out.clone();

    let builder = SetsBuilder::new(code, Merge::Intersection, gen, kill, in_);
    builder.build(HashSet::new());
    in_sets
}

struct SetsBuilder<'a, F1, F2, F3> {
    code: &'a Code,
    br_stack: Vec<Option<HashSet<Def>>>,
    merge: Merge,
    /// 引数のInstIdがgenに含まれるかどうかを返すクロージャ
    gen: F1,
    /// 引数の集合からInstIdのkillの要素を削除するクロージャ
    kill: F2,
    /// in_setsを更新するクロージャ
    in_: F3,
}

impl<'a, F1, F2, F3> SetsBuilder<'a, F1, F2, F3>
where
    F1: Fn(InstId) -> bool,
    F2: Fn(InstId, &mut HashSet<Def>),
    F3: FnMut(InstId, &HashSet<Def>),
{
    fn new(code: &'a Code, merge: Merge, gen: F1, kill: F2, in_: F3) -> Self {
        Self {
            code,
            br_stack: Vec::new(),
            merge,
            gen,
            kill,
            in_,
        }
    }

    fn build(mut self, prev_out: HashSet<Def>) {
        self.in_out_block(prev_out, self.code.root);
    }

    fn in_out_block(&mut self, out: HashSet<Def>, block_id: BlockId) -> Option<HashSet<Def>> {
        let mut out_opt = Some(out);
        let mut iter = self.code.block_nodes[block_id].iter();
        while let Some(inst_id) = iter.next(&self.code.inst_nodes) {
            let Some(mut out) = out_opt else { break };

            if matches!(self.code.insts[inst_id].kind, InstKind::Loop(_)) {
                loop {
                    let loop_in;
                    (out_opt, loop_in) = self.in_out_inst(out.clone(), inst_id);

                    // ループ文ならinが前と等しくなるまで繰り返す
                    if let Some(loop_in) = loop_in {
                        if loop_in == out {
                            // inが更新されなければ終了
                            break;
                        } else {
                            // 次の入力をループのinとする
                            out = loop_in;
                        }
                    } else {
                        // brでループ先頭に戻らなければ終了
                        break;
                    }
                }
            } else {
                (out_opt, _) = self.in_out_inst(out, inst_id);
            }
        }

        out_opt
    }

    /// (この命令のout、更新されたループ文のin)を返す
    /// 命令のoutは、brかreturn命令の際はNoneとなる
    fn in_out_inst(
        &mut self,
        mut out: HashSet<Def>,
        inst_id: InstId,
    ) -> (Option<HashSet<Def>>, Option<HashSet<Def>>) {
        (self.in_)(inst_id, &mut out);

        let mut loop_in = None;

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
                if let Some(child_out) = self.in_out_block(out.clone(), block_id) {
                    self.merge.merge_option(&mut block_out, &child_out);
                } else {
                    // ブロックが空の場合
                    self.merge.merge_option(&mut block_out, &out);
                }
            }

            // if文でelseが無い場合
            let node = &self.code.inst_nodes[inst_id];
            if matches!(self.code.insts[inst_id].kind, InstKind::If)
                && node.first_child == node.last_child
            {
                // ブロックのoutとifブロックのinを統合する
                // brやreturnで終わる場合はblock_outはNoneとなる
                self.merge.merge_option(&mut block_out, &out);
            }

            if breakable {
                if let Some(poped) = self.br_stack.pop().unwrap() {
                    if matches!(self.code.insts[inst_id].kind, InstKind::Loop(_)) {
                        // ループならスタックブロックのoutをinに含める
                        loop_in = Some(poped);
                        self.merge.merge_option(&mut loop_in, &out);
                    } else {
                        let block_out = block_out.as_mut().unwrap();
                        self.merge.merge(block_out, &poped);
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
            (self.kill)(inst_id, &mut out); // in ∖ kill
        }

        if (self.gen)(inst_id) {
            out.insert(Def::Inst(inst_id)); // ∪ gen
        }

        let out_opt = match self.code.insts[inst_id].kind {
            InstKind::Br(depth) => {
                // brなら対応するブロック命令のoutと集合を統合する
                let i = self.br_stack.len() - 1 - depth as usize;
                self.merge.merge_option_move(&mut self.br_stack[i], out);
                None
            }
            InstKind::Return => None,
            _ => Some(out),
        };

        (out_opt, loop_in)
    }
}

/// 集合のマージ方法を指定する
enum Merge {
    Union,
    Intersection,
}

impl Merge {
    fn merge(&self, x: &mut HashSet<Def>, y: &HashSet<Def>) {
        match self {
            Self::Union => x.extend(y),
            Self::Intersection => *x = x.and(y),
        }
    }

    fn merge_option(&self, x: &mut Option<HashSet<Def>>, y: &HashSet<Def>) {
        match x {
            Some(x) => self.merge(x, y),
            None => *x = Some(y.clone()),
        }
    }

    fn merge_option_move(&self, x: &mut Option<HashSet<Def>>, y: HashSet<Def>) {
        match x {
            Some(x) => self.merge(x, &y),
            None => *x = Some(y),
        }
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
    _use_defs: &SecondaryMap<InstId, HashSet<Def>>,
    def_uses: &HashMap<Def, HashSet<InstId>>,
    copy_in: &SecondaryMap<InstId, HashSet<Def>>,
) {
    // コピー文がキー、そのコピーが使用されている文の集合が値
    let mut copy_uses: SecondaryMap<InstId, HashSet<InstId>> =
        SecondaryMap::with_capacity(code.insts.len());

    for copy_inst_id in code.insts.keys() {
        if let Some(uses) = def_uses.get(&Def::Inst(copy_inst_id)) {
            for &use_inst_id in uses {
                if copy_in[use_inst_id].contains(&Def::Inst(copy_inst_id)) {
                    copy_uses[copy_inst_id].insert(use_inst_id);
                }
            }
        }
    }

    for copy_inst_id in code.insts.keys() {
        if !is_copy(&code.insts[copy_inst_id]) {
            // inst_idがコピーではない
            continue;
        }

        let Primary::Var(src) = code.insts[copy_inst_id].params[0] else {
            unreachable!()
        };
        let dst = code.insts[copy_inst_id].result.unwrap();

        if let Some(uses) = def_uses.get(&Def::Inst(copy_inst_id)) {
            if uses.len() != copy_uses[copy_inst_id].len() {
                // 変数の全ての使用が置き換え可能でなければ置き換えを行わない
                continue;
            }

            for &use_inst_id in uses {
                let use_inst = &mut code.insts[use_inst_id];

                // paramsをコピー元の変数に書き換え
                for p in &mut use_inst.params {
                    match p {
                        Primary::Var(p) if *p == dst => *p = src,
                        _ => (),
                    }
                }

                // save_varをコピー元の変数に書き換え
                if let Some(call) = &mut use_inst.call {
                    if call.recursive {
                        for save_var in &mut call.save_vars {
                            if *save_var == dst {
                                *save_var = src;
                            }
                        }
                    }
                }

                if is_copy(use_inst) {
                    // コピー文の右辺が置き換えられる場合はcopy_usesを更新する
                    copy_uses[use_inst_id] = copy_uses[use_inst_id].and(&copy_uses[copy_inst_id]);
                }
            }

            // コピー文を削除
            code.insts[copy_inst_id] = Inst {
                kind: InstKind::Nop,
                ..Default::default()
            };

            // 2つの変数のdefaultを合わせる
            if code.vars[src].default.is_none() && code.vars[dst].default.is_some() {
                code.vars[src].default = code.vars[dst].default;
            }
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

        let mut reach_sets = SecondaryMap::new();
        reaching_def(&code, Some(&mut reach_sets));
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

        let mut reach_sets = SecondaryMap::new();
        reaching_def(&code, Some(&mut reach_sets));
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

        let mut reach_sets = SecondaryMap::new();
        reaching_def(&code, Some(&mut reach_sets));
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

        let mut reach_sets = SecondaryMap::new();
        reaching_def(&code, Some(&mut reach_sets));
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

        let mut reach_sets = SecondaryMap::new();
        reaching_def(&code, Some(&mut reach_sets));
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

        let mut reach_sets = SecondaryMap::new();
        reaching_def(&code, Some(&mut reach_sets));
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

        let mut reach_sets = SecondaryMap::new();
        reaching_def(&code, Some(&mut reach_sets));
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

        let mut reach_sets = SecondaryMap::new();
        reaching_def(&code, Some(&mut reach_sets));
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

        let mut reach_sets = SecondaryMap::new();
        let (use_defs, def_uses) = reaching_def(&code, Some(&mut reach_sets));
        let copy_sets = copy(&code);
        replace_copy(&mut code, &use_defs, &def_uses, &copy_sets);

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

        let mut reach_sets = SecondaryMap::new();
        let (use_defs, def_uses) = reaching_def(&code, Some(&mut reach_sets));
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

        replace_copy(&mut code, &use_defs, &def_uses, &copy_sets);
        code_eq_nop(&code, &[true, false, false, false, false]);
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
