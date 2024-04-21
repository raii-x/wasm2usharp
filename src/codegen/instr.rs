use std::io;

use crate::ir::{
    func::Code,
    instr::{BlockId, Breakable, Call, InstId, InstKind},
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
    let mut inst_id = code.blocks[id].first_inst;
    loop {
        if inst_id.is_none() {
            break;
        }
        codegen_inst(f, code, inst_id.unwrap(), module, in_block)?;

        inst_id = code.insts[inst_id.unwrap()].next;
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
    let pattern = inst.expand_pattern(module);

    if let Some(call) = &inst.call {
        push_save_vars(call, f)?;
    }

    match &inst.kind {
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
            writeln!(f, "do {{")?;
            codegen_block(f, code, inst.first_block.unwrap(), module, true)?;
            writeln!(f, "}} while (false);")?;
        }
        InstKind::Loop(loop_var) => {
            writeln!(f, "{LOOP}{loop_var} = true;")?;
            writeln!(f, "do {{")?;
            writeln!(f, "do {{")?;

            codegen_block(f, code, inst.first_block.unwrap(), module, true)?;

            writeln!(f, "{LOOP}{loop_var} = false;")?;
            writeln!(f, "}} while (false);")?;
            writeln!(f, "if ({BREAK_DEPTH} > 0) break;")?;
            writeln!(f, "}} while ({LOOP}{});", loop_var)?;
        }
        InstKind::If => {
            if inst.breakable == Breakable::No {
                writeln!(f, "if ({pattern}) {{")?;
            } else {
                writeln!(f, "do if ({pattern}) {{")?;
            }

            let then = inst.first_block.unwrap();
            codegen_block(f, code, then, module, true)?;

            if let Some(else_) = code.blocks[then].next.expand() {
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
        InstKind::Switch(cases) => {
            writeln!(f, "switch ({pattern}) {{")?;

            let mut block_id = inst.first_block;
            let mut i = 0;
            loop {
                if block_id.is_none() {
                    break;
                }

                if let Some(case) = cases[i] {
                    writeln!(f, "case {case}:")?;
                } else {
                    writeln!(f, "default:")?;
                }

                codegen_block(f, code, block_id.unwrap(), module, in_block)?;

                let block = &code.blocks[block_id.unwrap()];

                // 命令がない、または最後の命令がbrではない場合にbreak
                if block
                    .last_inst
                    .expand()
                    .map_or(true, |id| !matches!(code.insts[id].kind, InstKind::Br(..)))
                {
                    writeln!(f, "break;")?;
                }

                block_id = block.next;
                i += 1;
            }

            writeln!(f, "}}")?;
        }
    }

    // 最も外側のブロックのendでない、かつ多重breakが可能な場合のみ
    if in_block && inst.breakable == Breakable::Multi {
        writeln!(f, "if ({BREAK_DEPTH} > 0) {{ {BREAK_DEPTH}--; break; }}")?;
    }

    if let Some(call) = &inst.call {
        pop_save_vars(call, f)?;
    }
    Ok(())
}

fn push_save_vars(call: &Call, f: &mut dyn io::Write) -> io::Result<()> {
    if call.recursive && !call.save_vars.is_empty() {
        // ローカル変数保存用のスタックにプッシュ
        for (i, &var) in call.save_vars.iter().enumerate() {
            write!(f, "{STACK}[{STACK_TOP}")?;
            if i != 0 {
                write!(f, " + {i}")?;
            }
            writeln!(f, "] = {var};")?;
        }
        writeln!(f, "{STACK_TOP} += {};", call.save_vars.len())?;
    }
    Ok(())
}

fn pop_save_vars(call: &Call, f: &mut dyn io::Write) -> io::Result<()> {
    if call.recursive && !call.save_vars.is_empty() {
        writeln!(f, "{STACK_TOP} -= {};", call.save_vars.len())?;
        // ローカル変数保存用のスタックからポップ
        for (i, &var) in call.save_vars.iter().enumerate() {
            write!(f, "{var} = ({}){STACK}[{STACK_TOP}", var.ty)?;
            if i != 0 {
                write!(f, " + {i}")?;
            }
            writeln!(f, "];")?;
        }

        // ループ変数を元に戻す
        for i in &call.save_loop_vars {
            writeln!(f, "{LOOP}{i} = true;")?;
        }
    }
    Ok(())
}
