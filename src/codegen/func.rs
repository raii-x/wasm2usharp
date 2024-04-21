use std::io;

use crate::ir::{
    func::{Func, Var},
    func_header,
    module::Module,
    result_cs_ty,
    ty::CsType,
    BREAK_DEPTH, LOOP,
};

use super::code::codegen_code;

pub fn codegen_func(func: &Func, f: &mut dyn io::Write, module: &Module<'_>) -> io::Result<()> {
    // 関数ヘッダ
    let func_ty = func.header.ty.clone();

    if func.header.export {
        write!(f, "public ")?;
    }

    let code = func.code.as_ref().unwrap();

    let params: Vec<(CsType, &Var)> = func_ty
        .params()
        .iter()
        .map(|&ty| CsType::get(ty))
        .zip(code.vars.var_decls.iter().map(|x| &x.var))
        .collect();
    write!(
        f,
        "{}",
        func_header(&func.header.name, result_cs_ty(func_ty.results()), &params)
    )?;

    writeln!(f, " {{")?;

    writeln!(f, "int {BREAK_DEPTH} = 0;")?;

    // ループ変数
    for i in 0..code.vars.loop_var_count {
        writeln!(f, "bool {LOOP}{i};")?;
    }

    // 一時変数
    for decl in code.vars.var_decls.iter().skip(func_ty.params().len()) {
        match decl.default {
            Some(def) => writeln!(f, "{} {} = {def};", decl.var.ty, decl.var)?,
            None => writeln!(f, "{} {};", decl.var.ty, decl.var)?,
        }
    }

    // 本体
    codegen_code(f, code, module)?;

    writeln!(f, "}}")?;

    Ok(())
}
