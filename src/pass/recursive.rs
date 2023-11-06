use crate::ir::{func::Instr, module::Module};

use petgraph::{algo::tarjan_scc, graph::Graph};

/// 関数のコールグラフがサイクルを含むか判定し、
/// 含むなら関数のrecursiveをtrue、含まないならfalseにする
pub fn recursive(module: &mut Module<'_>) {
    let mut recursives = vec![false; module.all_funcs.len()];

    // 関数呼び出しに対応するグラフ
    let mut graph = Graph::<(), ()>::new();

    // 関数をノードとして追加
    let nodes = (0..module.all_funcs.len())
        .map(|_| graph.add_node(()))
        .collect::<Vec<_>>();

    // 関数呼び出しに対応するエッジを追加
    for (i_caller, caller) in module.all_funcs.iter().enumerate() {
        if let Some(code) = &caller.code {
            for instr in &code.instrs {
                if let Instr::Call { func, .. } = instr {
                    if *func == i_caller {
                        // 自身を再帰的に呼び出すならrecursiveとする
                        recursives[i_caller] = true;
                    } else {
                        graph.add_edge(nodes[i_caller], nodes[*func], ());
                    }
                }
            }
        }
    }

    // 強連結成分を求める
    let scc = tarjan_scc(&graph);

    for nodes in scc {
        if nodes.len() >= 2 {
            // 強連結成分のノード数が2以上ならrecursiveとする
            for node in nodes {
                recursives[node.index()] = true;
            }
        }
    }

    for (i, rec) in recursives.into_iter().enumerate() {
        module.all_funcs[i].recursive = rec;
    }
}
