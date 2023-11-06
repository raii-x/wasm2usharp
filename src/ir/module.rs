use std::{collections::HashMap, io};

use wasmparser::{FuncType, GlobalType, MemoryType, TableType};

use super::{func::Func, ty::CsType, DATA, ELEMENT, PAGE_SIZE};

pub struct Module<'input> {
    pub buf: &'input [u8],
    pub class_name: &'input str,
    pub test: bool,
    pub types: Vec<FuncType>,
    /// 出力のU#に含まれる全ての関数。
    /// 元のwasmに存在していた関数は同じインデックスで参照できる
    pub all_funcs: Vec<Func>,
    /// 元のwasmに存在していた関数の数
    pub wasm_func_count: usize,
    /// call_indirect命令に対応する関数
    pub call_indirects: Vec<usize>,
    pub table: Option<Table>,
    pub memory: Option<Memory>,
    pub globals: Vec<Global>,
    pub elements: Vec<Element>,
    pub datas: Vec<Data<'input>>,
    pub start_func: Option<usize>,
    pub import_modules: HashMap<String, String>,
}

impl<'input> Module<'input> {
    pub fn new(buf: &'input [u8], class_name: &'input str, test: bool) -> Self {
        Self {
            buf,
            class_name,
            test,
            types: Vec::new(),
            all_funcs: Vec::new(),
            wasm_func_count: 0,
            call_indirects: Vec::new(),
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
    pub fn write(&self, f: &mut dyn io::Write) -> io::Result<()> {
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
                    write!(f, "{},", self.all_funcs[item as usize].header.name)?;
                } else {
                    // テーブルに格納される関数インデックスは元のインデックスに1を足したもの
                    // (配列の初期値の0でnullを表現するため)
                    write!(f, "{},", item + 1)?;
                }
            }

            writeln!(f, " }};")?;
        }

        // コード
        for func in &self.all_funcs {
            if func.code.is_some() {
                func.write(f, self)?;
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

            let cs_ty = CsType::get(global.ty.content_type);
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
