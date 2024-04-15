use std::io;

use crate::ir::{
    instr::{Call, InstrKind, InstrNode},
    module::Module,
    BREAK_DEPTH, LOOP, STACK, STACK_TOP,
};

pub fn codegen_instr_node(
    node: &InstrNode,
    f: &mut dyn io::Write,
    module: &Module<'_>,
) -> io::Result<()> {
    codegen_inner(node, f, module, false)
}

fn codegen_inner(
    node: &InstrNode,
    f: &mut dyn io::Write,
    module: &Module<'_>,
    in_block: bool,
) -> io::Result<()> {
    let pattern = node.instr.expand_pattern(module);

    if let Some(call) = &node.instr.call {
        push_save_vars(call, f)?;
    }

    match &node.instr.kind {
        InstrKind::Stmt => {
            writeln!(f, "{}", pattern)?;
        }
        InstrKind::Expr => {
            if let Some(result) = node.instr.result {
                writeln!(f, "{result} = {pattern};")?;
            } else {
                writeln!(f, "{pattern};")?;
            }
        }
        InstrKind::Return => {
            writeln!(f, "return {pattern};")?;
        }
        InstrKind::Block => {
            writeln!(f, "do {{")?;
            for instr in &node.child.as_ref().unwrap().blocks[0] {
                codegen_inner(instr, f, module, true)?;
            }
            writeln!(f, "}} while (false);")?;
        }
        InstrKind::Loop(loop_var) => {
            writeln!(f, "{LOOP}{loop_var} = true;")?;
            writeln!(f, "do {{")?;
            writeln!(f, "do {{")?;

            for instr in &node.child.as_ref().unwrap().blocks[0] {
                codegen_inner(instr, f, module, true)?;
            }

            writeln!(f, "{LOOP}{loop_var} = false;")?;
            writeln!(f, "}} while (false);")?;
            writeln!(f, "if ({BREAK_DEPTH} > 0) break;")?;
            writeln!(f, "}} while ({LOOP}{});", loop_var)?;
        }
        InstrKind::If => {
            let child = node.child.as_ref().unwrap();

            if child.breakable {
                writeln!(f, "do if ({pattern}) {{")?;
            } else {
                writeln!(f, "if ({pattern}) {{")?;
            }

            for instr in &child.blocks[0] {
                codegen_inner(instr, f, module, true)?;
            }

            if let Some(else_) = child.blocks.get(1) {
                writeln!(f, "}} else {{")?;
                for instr in else_ {
                    codegen_inner(instr, f, module, true)?;
                }
            }

            if child.breakable {
                writeln!(f, "}} while (false);")?;
            } else {
                writeln!(f, "}}")?;
            }
        }
        InstrKind::Br(depth) => {
            write_break(f, *depth)?;
        }
        InstrKind::Switch(cases) => {
            writeln!(f, "switch ({pattern}) {{")?;

            let blocks = &node.child.as_ref().unwrap().blocks;
            assert_eq!(cases.len(), blocks.len());

            for (case, block) in cases.iter().zip(blocks) {
                if let Some(case) = case {
                    writeln!(f, "case {case}:")?;
                } else {
                    writeln!(f, "default:")?;
                }

                for instr in block {
                    codegen_inner(instr, f, module, in_block)?;
                }

                // 命令がない、または最後の命令がbrではない場合にbreak
                if block
                    .last()
                    .map_or(true, |n| !matches!(n.instr.kind, InstrKind::Br(..)))
                {
                    writeln!(f, "break;")?;
                }
            }

            writeln!(f, "}}")?;
        }
    }

    if node.child.as_ref().map_or(false, |c| c.breakable) {
        write_multi_break(f, in_block)?;
    }

    if let Some(call) = &node.instr.call {
        pop_save_vars(call, f)?;
    }
    Ok(())
}

fn write_multi_break(f: &mut dyn io::Write, in_block: bool) -> io::Result<()> {
    // 最も外側のブロックのendでない場合のみ
    if in_block {
        // 多重break
        writeln!(f, "if ({BREAK_DEPTH} > 0) {{ {BREAK_DEPTH}--; break; }}")?;
    }
    Ok(())
}

fn write_break(f: &mut dyn io::Write, depth: u32) -> io::Result<()> {
    if depth > 0 {
        writeln!(f, "{BREAK_DEPTH} = {depth};")?;
    }
    writeln!(f, "break;")
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
