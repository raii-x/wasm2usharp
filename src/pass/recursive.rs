use std::collections::HashSet;

use crate::ir::{
    instr::{Call, InstrTree},
    module::Module,
};

use petgraph::{algo::tarjan_scc, graph::Graph};

/// 関数のコールグラフがサイクルを含むか判定し、
/// 含むなら関数のrecursiveをtrue、含まないならfalseにする
pub fn recursive(module: &mut Module<'_>) {
    // 関数呼び出しに対応するグラフ
    let mut graph = Graph::<(), ()>::new();

    // 関数をノードとして追加
    let nodes = (0..module.all_funcs.len())
        .map(|_| graph.add_node(()))
        .collect::<Vec<_>>();

    // 関数呼び出しに対応するエッジを追加
    for (i_caller, caller) in module.all_funcs.iter_mut().enumerate() {
        if let Some(code) = &mut caller.code {
            iterate_call(&mut code.instr_tree, |call| {
                graph.add_edge(nodes[i_caller], nodes[call.func], ());
            });
        }
    }

    // 強連結成分を求める
    let scc = tarjan_scc(&graph);

    for nodes in scc {
        let nodes_set: HashSet<usize> = HashSet::from_iter(nodes.iter().map(|x| x.index()));
        for node in nodes {
            if let Some(code) = &mut module.all_funcs[node.index()].code {
                iterate_call(&mut code.instr_tree, |call| {
                    if nodes_set.contains(&call.func) {
                        // 同じ強連結成分に含まれる関数の呼び出しをrecursiveとする
                        call.recursive = true;
                    }
                });
            }
        }
    }
}

fn iterate_call(tree: &mut InstrTree, mut f: impl FnMut(&mut Call)) {
    for (_, node) in tree.iter_mut() {
        if let Some(call) = &mut node.instr.call {
            f(call);
        }
    }
}
