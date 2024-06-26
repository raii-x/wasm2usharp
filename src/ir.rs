use std::fmt;

use self::module::Module;

pub mod builder;
pub mod code;
pub mod module;
pub mod node;
pub mod ty;
pub mod var;

pub const PAGE_SIZE: u32 = 65536;
pub const MAX_PARAMS: usize = 16;
pub const STACK_SIZE: usize = 65536;
pub const FUNC: &str = "w2us_func";
pub const GLOBAL: &str = "w2us_global";
pub const MEMORY: &str = "w2us_memory";
pub const TABLE: &str = "w2us_table";
pub const DATA: &str = "w2us_data";
pub const ELEMENT: &str = "w2us_element";
pub const INIT: &str = "w2us_init";
pub const CALL_INDIRECT: &str = "w2us_call_indirect";
pub const BREAK_DEPTH: &str = "w2us_break_depth";
pub const VAR: &str = "w2us_v";
pub const LOOP: &str = "w2us_loop";
pub const STACK: &str = "w2us_stack";
pub const STACK_TOP: &str = "w2us_stack_top";
pub const NULL: &str = "w2us_null";
pub const START: &str = "w2us_start";

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

pub fn trap(module: &Module<'_>, message: &str) -> String {
    if module.test {
        format!(r#"throw new Exception("{message}");"#)
    } else {
        // エラー時にはnullのメソッドを呼び出すことで例外を出して停止する
        format!(r#"Debug.LogError("{message}"); {NULL}.RequestSerialization();"#)
    }
}
