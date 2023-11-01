use wasmparser::{FuncType, GlobalType, MemoryType, TableType};

use super::func::Func;

pub struct Module<'input> {
    pub buf: &'input [u8],
    pub class_name: &'input str,
    pub test: bool,
    pub types: Vec<FuncType>,
    pub funcs: Vec<Func>,
    pub table: Option<Table>,
    pub memory: Option<Memory>,
    pub globals: Vec<Global>,
    pub elements: Vec<Element>,
    pub datas: Vec<Data<'input>>,
    pub start_func: Option<u32>,
}

impl<'input> Module<'input> {
    pub fn new(buf: &'input [u8], class_name: &'input str, test: bool) -> Self {
        Self {
            buf,
            class_name,
            test,
            types: Vec::new(),
            funcs: Vec::new(),
            table: None,
            memory: None,
            globals: Vec::new(),
            elements: Vec::new(),
            datas: Vec::new(),
            start_func: None,
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
