use std::io;

use crate::ir::{
    code::{BlockId, Breakable, Call, Code, InstId, InstKind},
    module::Module,
    var::VarId,
    BREAK_DEPTH, LOOP, STACK, STACK_TOP,
};

pub fn codegen_code(f: &mut dyn io::Write, code: &Code, module: &Module<'_>) -> io::Result<()> {
    codegen_block(f, code, code.root, module, false)
}

fn codegen_block(
    f: &mut dyn io::Write,
    code: &Code,
    id: BlockId,
    module: &Module<'_>,
    in_block: bool,
) -> io::Result<()> {
    let mut next_inst_id = code.block_nodes[id].first_child;
    while let Some(inst_id) = next_inst_id.expand() {
        codegen_inst(f, code, inst_id, module, in_block)?;

        next_inst_id = code.inst_nodes[inst_id].next;
    }
    Ok(())
}

fn codegen_inst(
    f: &mut dyn io::Write,
    code: &Code,
    id: InstId,
    module: &Module<'_>,
    in_block: bool,
) -> io::Result<()> {
    let inst = &code.insts[id];
    let node = &code.inst_nodes[id];
    let pattern = inst.expand_pattern(module);

    if let Some(call) = &inst.call {
        push_save_vars(f, call, inst.result)?;
    }

    match &inst.kind {
        InstKind::Nop => {}
        InstKind::Stmt => {
            writeln!(f, "{}", pattern)?;
        }
        InstKind::Expr => {
            if let Some(result) = inst.result {
                writeln!(f, "{result} = {pattern};")?;
            } else {
                writeln!(f, "{pattern};")?;
            }
        }
        InstKind::Return => {
            writeln!(f, "return {pattern};")?;
        }
        InstKind::Block => {
            if inst.breakable != Breakable::No {
                writeln!(f, "do {{")?;
            }
            codegen_block(f, code, node.first_child.unwrap(), module, true)?;
            if inst.breakable != Breakable::No {
                writeln!(f, "}} while (false);")?;
            }
        }
        InstKind::Loop(loop_var) => {
            writeln!(f, "{LOOP}{loop_var} = true;")?;
            writeln!(f, "do {{")?;
            writeln!(f, "do {{")?;

            codegen_block(f, code, node.first_child.unwrap(), module, true)?;

            writeln!(f, "{LOOP}{loop_var} = false;")?;
            writeln!(f, "}} while (false);")?;
            if inst.breakable == Breakable::Multi {
                writeln!(f, "if ({BREAK_DEPTH} > 0) break;")?;
            }
            writeln!(f, "}} while ({LOOP}{});", loop_var)?;
        }
        InstKind::If => {
            if inst.breakable == Breakable::No {
                writeln!(f, "if ({pattern}) {{")?;
            } else {
                writeln!(f, "do if ({pattern}) {{")?;
            }

            let then = node.first_child.unwrap();
            codegen_block(f, code, then, module, true)?;

            if let Some(else_) = code.block_nodes[then].next.expand() {
                writeln!(f, "}} else {{")?;
                codegen_block(f, code, else_, module, true)?;
            }

            if inst.breakable == Breakable::No {
                writeln!(f, "}}")?;
            } else {
                writeln!(f, "}} while (false);")?;
            }
        }
        InstKind::Br(depth) => {
            if *depth > 0 {
                writeln!(f, "{BREAK_DEPTH} = {depth};")?;
            }
            writeln!(f, "break;")?;
        }
        InstKind::Switch => {
            writeln!(f, "switch ({pattern}) {{")?;

            let mut iter = node.iter();
            while let Some(block_id) = iter.next(&code.block_nodes) {
                codegen_block(f, code, block_id, module, in_block)?;
            }

            writeln!(f, "}}")?;
        }
        InstKind::Case => {
            writeln!(f, "case {pattern}:")?;
        }
        InstKind::Default => {
            writeln!(f, "default:")?;
        }
    }

    // 最も外側のブロックのendでない、かつ多重breakが可能な場合のみ
    if in_block && inst.breakable == Breakable::Multi {
        writeln!(f, "if ({BREAK_DEPTH} > 0) {{ {BREAK_DEPTH}--; break; }}")?;
    }

    if let Some(call) = &inst.call {
        pop_save_vars(f, call, code, inst.result)?;
    }
    Ok(())
}

fn filtered_save_vars(call: &Call, result: Option<VarId>) -> Vec<VarId> {
    if !call.recursive {
        return Vec::new();
    }

    let mut save_vars = call
        .save_vars
        .iter()
        .copied()
        .filter(|&var_id| Some(var_id) != result)
        .collect::<Vec<_>>();
    save_vars.sort();
    save_vars
}

fn push_save_vars(f: &mut dyn io::Write, call: &Call, result: Option<VarId>) -> io::Result<()> {
    let save_vars = filtered_save_vars(call, result);
    if !save_vars.is_empty() {
        // ローカル変数保存用のスタックにプッシュ
        for (i, &var) in save_vars.iter().enumerate() {
            write!(f, "{STACK}[{STACK_TOP}")?;
            if i != 0 {
                write!(f, " + {i}")?;
            }
            writeln!(f, "] = {var};")?;
        }
        writeln!(f, "{STACK_TOP} += {};", save_vars.len())?;
    }
    Ok(())
}

fn pop_save_vars(
    f: &mut dyn io::Write,
    call: &Call,
    code: &Code,
    result: Option<VarId>,
) -> io::Result<()> {
    let save_vars = filtered_save_vars(call, result);
    if !save_vars.is_empty() {
        writeln!(f, "{STACK_TOP} -= {};", save_vars.len())?;

        // ローカル変数保存用のスタックからポップ
        for (i, &var_id) in save_vars.iter().enumerate() {
            write!(
                f,
                "{var_id} = ({}){STACK}[{STACK_TOP}",
                code.vars[var_id].ty
            )?;
            if i != 0 {
                write!(f, " + {i}")?;
            }
            writeln!(f, "];")?;
        }

        let mut save_loop_vars = call.save_loop_vars.iter().copied().collect::<Vec<_>>();
        save_loop_vars.sort();

        // ループ変数を元に戻す
        for i in &save_loop_vars {
            writeln!(f, "{LOOP}{i} = true;")?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::ir::{builder::Builder, code::Call, var::Var};

    use super::filtered_save_vars;

    #[test]
    fn excludes_recursive_call_result_from_saved_vars() {
        let mut builder = Builder::new(&[]);
        let var0 = builder.new_var(Var::default());
        let var1 = builder.new_var(Var::default());

        let call = Call {
            func: 0,
            recursive: true,
            save_vars: HashSet::from([var0, var1]),
            save_loop_vars: Default::default(),
        };

        assert_eq!(filtered_save_vars(&call, Some(var0)), vec![var1]);
        assert_eq!(filtered_save_vars(&call, Some(var1)), vec![var0]);
        assert_eq!(filtered_save_vars(&call, None), vec![var0, var1]);
    }
}
