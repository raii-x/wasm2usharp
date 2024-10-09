use std::collections::HashMap;

use wasmparser::{FuncType, GlobalType, MemoryType, TableType};

use super::{code::Code, ty::CsType};

pub struct Module<'input> {
    pub buf: &'input [u8],
    pub class_name: String,
    pub namespace: Option<String>,
    pub test: bool,
    pub types: Vec<FuncType>,
    /// 出力のU#に含まれる全ての関数。
    /// 元のwasmに存在していた関数は同じインデックスで参照できる
    pub all_funcs: Vec<Func>,
    /// 元のwasmに存在していた関数の数
    pub wasm_func_count: usize,
    /// call_indirect命令に対応する関数
    pub call_indirects: HashMap<usize, usize>,
    pub table: Option<Table>,
    pub memory: Option<Memory>,
    pub globals: Vec<Global>,
    pub elements: Vec<Element>,
    pub datas: Vec<Data<'input>>,
    pub start_func: Option<usize>,
    pub import_modules: HashMap<String, String>,
}

impl<'input> Module<'input> {
    pub fn new(
        buf: &'input [u8],
        class_name: String,
        namespace: Option<String>,
        test: bool,
    ) -> Self {
        Self {
            buf,
            class_name,
            namespace,
            test,
            types: Vec::new(),
            all_funcs: Vec::new(),
            wasm_func_count: 0,
            call_indirects: HashMap::new(),
            table: None,
            memory: None,
            globals: Vec::new(),
            elements: Vec::new(),
            datas: Vec::new(),
            start_func: None,
            import_modules: HashMap::new(),
        }
    }
}

impl<'input> Module<'input> {
    pub fn math_class(&self, ty: CsType) -> &'static str {
        match ty {
            CsType::Float => {
                if self.test {
                    "MathF"
                } else {
                    "Mathf"
                }
            }
            CsType::Double => "Math",
            _ => panic!("Specify float type as argument"),
        }
    }
}

pub struct Table {
    pub name: String,
    pub ty: TableType,
    pub import: bool,
    pub export: bool,
}

pub struct Memory {
    pub name: String,
    pub ty: MemoryType,
    pub import: bool,
    pub export: bool,
}

pub struct Global {
    pub name: String,
    pub ty: GlobalType,
    pub init_expr: Option<String>,
    pub import: bool,
    pub export: bool,
}

pub struct Element {
    pub offset_expr: String,
    pub items: Vec<u32>,
}

pub struct Data<'a> {
    pub offset_expr: String,
    pub data: &'a [u8],
}

pub struct Func {
    pub header: FuncHeader,
    pub code: Option<Code>,
    pub in_table: bool,
}

pub struct FuncHeader {
    pub name: String,
    pub ty: FuncType,
    pub import: bool,
    pub export: bool,
}
