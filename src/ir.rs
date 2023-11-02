use std::fmt;

use wasmparser::ValType;

use self::module::Module;

pub mod func;
pub mod module;

pub const PAGE_SIZE: u32 = 65536;
pub const MAX_PARAMS: usize = 16;
pub const W2US_PREFIX: &str = "w2us_";
pub const MEMORY: &str = "w2us_memory";
pub const TABLE: &str = "w2us_table";
pub const DATA: &str = "w2us_data";
pub const ELEMENT: &str = "w2us_element";
pub const INIT: &str = "w2us_init";
pub const CALL_INDIRECT: &str = "w2us_call_indirect";
pub const BREAK_DEPTH: &str = "w2us_break_depth";
pub const LOOP: &str = "w2us_loop";

pub fn get_cs_ty(ty: ValType) -> &'static str {
    match ty {
        ValType::I32 => "int",
        ValType::I64 => "long",
        ValType::F32 => "float",
        ValType::F64 => "double",
        _ => unreachable!(),
    }
}

pub fn func_header(
    name: impl fmt::Display,
    result: impl fmt::Display,
    params: &[(impl fmt::Display, impl fmt::Display)],
) -> String {
    let mut header = format!("{result} {name}(");

    for (i, param) in params.iter().enumerate() {
        if i != 0 {
            header += ", ";
        }
        header += &format!("{} {}", param.0, param.1);
    }

    header += ")";
    header
}

pub fn result_cs_ty(results: &[ValType]) -> &str {
    match results.len() {
        0 => "void",
        1 => get_cs_ty(results[0]),
        _ => unreachable!(),
    }
}

pub fn trap(module: &Module<'_>, message: &str) -> String {
    if module.test {
        format!("throw new Exception(\"{message}\");")
    } else {
        format!("Debug.LogError(\"{message}\");")
    }
}