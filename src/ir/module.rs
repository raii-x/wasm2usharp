use core::fmt;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use wasmparser::{FuncType, GlobalType, MemoryType, TableType};

use super::{func::Func, get_cs_ty, DATA, ELEMENT, PAGE_SIZE};

pub struct Module<'input> {
    pub buf: &'input [u8],
    pub class_name: &'input str,
    pub test: bool,
    pub types: Vec<FuncType>,
    pub funcs: Vec<Rc<RefCell<Func>>>,
    pub table: Option<Table>,
    pub memory: Option<Memory>,
    pub globals: Vec<Global>,
    pub elements: Vec<Element>,
    pub datas: Vec<Data<'input>>,
    pub start_func: Option<u32>,
    pub import_modules: HashMap<String, String>,
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
            import_modules: HashMap::new(),
        }
    }
}

impl<'input> fmt::Display for Module<'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "using System;")?;
        if !self.test {
            writeln!(f, "using UdonSharp;")?;
            writeln!(f, "using UnityEngine;")?;
        }

        write!(f, "public class {} ", self.class_name)?;
        if self.test {
            writeln!(f, "{{")?;
        } else {
            writeln!(f, ": UdonSharpBehaviour {{")?;
        }

        for (i, elem) in self.elements.iter().enumerate() {
            // エレメント配列
            // テストの際、使用するテーブルを外部からインポートするモジュールの場合、
            // エレメント配列内にC#メソッドのデリゲートを格納する。
            // それ以外の場合は、uintで関数のインデックス+1を表す
            let use_delegate = self.test && self.table.as_ref().unwrap().import;
            let cs_ty = if use_delegate { "object" } else { "uint" };
            let eq = if use_delegate { "=>" } else { "=" };
            write!(f, "{cs_ty}[] {ELEMENT}{i} {eq} new {cs_ty}[] {{ ",)?;

            for &item in elem.items.iter() {
                if use_delegate {
                    write!(f, "{},", self.funcs[item as usize].borrow().header.name)?;
                } else {
                    // テーブルに格納される関数インデックスは元のインデックスに1を足したもの
                    // (配列の初期値の0でnullを表現するため)
                    write!(f, "{},", item + 1)?;
                }
            }

            writeln!(f, " }};")?;
        }

        // コード
        for func in &self.funcs {
            let func = func.borrow();
            if func.code.is_some() {
                write!(f, "{}", func)?;
            }
        }

        // データ配列
        for (i, data) in self.datas.iter().enumerate() {
            write!(f, "byte[] {DATA}{i} = new byte[] {{ ")?;
            for byte in data.data {
                write!(f, "{byte},")?;
            }
            writeln!(f, " }};")?;
        }

        // インポートするモジュールの宣言
        for (module_var, module_ty) in &self.import_modules {
            writeln!(f, "public {module_ty} {module_var};")?;
        }

        // グローバル変数宣言
        for global in &self.globals {
            if global.import {
                continue;
            }

            if global.export {
                write!(f, "[NonSerialized] public ")?
            }

            let cs_ty = get_cs_ty(global.ty.content_type);
            writeln!(f, "{cs_ty} {};", global.name)?;
        }

        // テーブル宣言
        if let Some(table) = &self.table {
            if !table.import {
                if table.export {
                    write!(f, "[NonSerialized] public ")?
                }

                // テストの場合はuintとAction/Funcを混在させるため
                // テーブルはobjectの配列で表す
                let elem_cs_ty = if self.test { "object" } else { "uint" };

                writeln!(
                    f,
                    "{elem_cs_ty}[] {} = new {elem_cs_ty}[{}];",
                    table.name, table.ty.initial
                )?;
            }
        }

        // メモリー宣言
        if let Some(memory) = &self.memory {
            if !memory.import {
                if memory.export {
                    write!(f, "[NonSerialized] public ")?
                }
                writeln!(
                    f,
                    "byte[] {} = new byte[{}];",
                    memory.name,
                    memory.ty.initial * PAGE_SIZE as u64
                )?;
            }
        }

        writeln!(f, "}}")?;

        Ok(())
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
