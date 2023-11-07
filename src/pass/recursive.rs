use std::collections::HashSet;

use crate::ir::{func::Instr, module::Module};

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
            for instr in &mut code.instrs {
                if let Instr::Call { func, .. } = instr {
                    graph.add_edge(nodes[i_caller], nodes[*func], ());
                }
            }
        }
    }

    // 強連結成分を求める
    let scc = tarjan_scc(&graph);

    for nodes in scc {
        let nodes_set: HashSet<usize> = HashSet::from_iter(nodes.iter().map(|x| x.index()));
        for node in nodes {
            if let Some(code) = &mut module.all_funcs[node.index()].code {
                for instr in &mut code.instrs {
                    if let Instr::Call {
                        func, recursive, ..
                    } = instr
                    {
                        if nodes_set.contains(func) {
                            // 同じ強連結成分に含まれる関数の呼び出しをrecursiveとする
                            *recursive = true;
                        }
                    }
                }
            }
        }
    }
}
