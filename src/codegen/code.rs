use std::io;

use crate::ir::{
    code::{BlockId, Breakable, Call, Code, InstId, InstKind},
    module::Module,
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
        push_save_vars(f, call)?;
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
        pop_save_vars(f, call, code)?;
    }
    Ok(())
}

fn push_save_vars(f: &mut dyn io::Write, call: &Call) -> io::Result<()> {
    if call.recursive && !call.save_vars.is_empty() {
        let mut save_vars = call.save_vars.iter().copied().collect::<Vec<_>>();
        save_vars.sort();

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

fn pop_save_vars(f: &mut dyn io::Write, call: &Call, code: &Code) -> io::Result<()> {
    if call.recursive && !call.save_vars.is_empty() {
        let mut save_vars = call.save_vars.iter().copied().collect::<Vec<_>>();
        save_vars.sort();

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
