use core::fmt;
use std::collections::HashMap;

use wasmparser::{FuncType, GlobalType, MemoryType, TableType, ValType};

use super::{
    func::Func, func_header, get_cs_ty, result_cs_ty, trap, CALL_INDIRECT, DATA, ELEMENT, INIT,
    MAX_PARAMS, PAGE_SIZE,
};

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

    fn write_call_indirects(&self, out_file: &mut fmt::Formatter<'_>) -> fmt::Result {
        let table = match &self.table {
            Some(x) => x,
            None => return Ok(()), // テーブルが無ければreturn
        };

        for (i_ty, ty) in self.types.iter().enumerate() {
            let name = format!("{CALL_INDIRECT}{i_ty}");

            // call_indirect関数が受け取る引数リスト
            let mut params = params_cs_ty_name(ty);

            // 関数呼び出し用の引数リスト
            let call_params = params
                .iter()
                .map(|(_, y)| y.as_str())
                .collect::<Vec<_>>()
                .join(", ");

            params.push((get_cs_ty(ValType::I32), "index".to_string()));

            // call_indirect用の関数定義
            writeln!(
                out_file,
                "{} {{",
                func_header(&name, result_cs_ty(ty.results()), &params)
            )?;

            let table_name = &table.name;
            let use_delegate = self.test && ty.params().len() <= MAX_PARAMS;

            if use_delegate {
                // テストの際はuintの他にdelegateが含まれることがある
                writeln!(out_file, "if ({table_name}[index] is uint) {{")?;
            }

            writeln!(
                out_file,
                "switch ({}{table_name}[index]) {{",
                if self.test { "(uint)" } else { "" }, // テストの際はobjectをuintに変換
            )?;
            for (i, func) in self.funcs.iter().enumerate() {
                if func.header.ty != *ty {
                    continue;
                }

                if ty.results().is_empty() {
                    writeln!(
                        out_file,
                        "case {}: {}({call_params}); return;",
                        i + 1,
                        func.header.name
                    )?;
                } else {
                    writeln!(
                        out_file,
                        "case {}: return {}({call_params});",
                        i + 1,
                        func.header.name
                    )?;
                }
            }
            write!(out_file, "default: ")?;
            write!(out_file, "{}", trap(self, "invalid table value"))?;
            write!(out_file, "return")?;
            if ty.results().is_empty() {
                writeln!(out_file, ";")?;
            } else {
                writeln!(out_file, " 0;")?;
            }
            writeln!(out_file, "}}")?;

            if use_delegate {
                writeln!(out_file, "}} else {{")?;
                // delegateに変換して呼び出し
                let del = func_delegate(ty);
                if ty.results().is_empty() {
                    writeln!(
                        out_file,
                        "(({del}){table_name}[index])({call_params}); return;",
                    )?;
                } else {
                    writeln!(
                        out_file,
                        "return (({del}){table_name}[index])({call_params});",
                    )?;
                }
                writeln!(out_file, "}}")?;
            }

            writeln!(out_file, "}}")?;
        }
        Ok(())
    }

    fn write_init_method(&self, out_file: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(out_file, "public void {INIT}() {{")?;
        for global in &self.globals {
            if global.import {
                continue;
            }

            // グローバル変数の初期値を代入
            if let Some(init_expr) = &global.init_expr {
                writeln!(out_file, "{} = {init_expr};", global.name)?;
            }
        }
        for (i, Element { offset_expr, items }) in self.elements.iter().enumerate() {
            // テーブルへのエレメントのコピー
            writeln!(
                out_file,
                "Array.Copy({ELEMENT}{i}, 0, {}, {offset_expr}, {});",
                self.table.as_ref().unwrap().name,
                items.len()
            )?;
        }
        for (i, Data { offset_expr, data }) in self.datas.iter().enumerate() {
            // メモリへのデータのコピー
            writeln!(
                out_file,
                "Array.Copy({DATA}{i}, 0, {}, {offset_expr}, {});",
                self.memory.as_ref().unwrap().name,
                data.len()
            )?;
        }
        if let Some(start_func) = self.start_func {
            // start関数の呼び出し
            writeln!(
                out_file,
                "{}();",
                self.funcs[start_func as usize].header.name
            )?;
        }
        writeln!(out_file, "}}")?;
        Ok(())
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
                    write!(f, "{},", self.funcs[item as usize].header.name)?;
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

        self.write_call_indirects(f)?;

        self.write_init_method(f)?;

        writeln!(f, "}}")?;

        Ok(())
    }
}

/// 引数の関数型と対応するC#でのAction/Funcの型を表す文字列を取得 (テスト用)
fn func_delegate(ty: &FuncType) -> String {
    let cs_ty = if ty.results().is_empty() {
        "Action".to_string()
    } else {
        "Func".to_string()
    };

    let params: Vec<&str> = ty
        .params()
        .iter()
        .map(|&x| get_cs_ty(x))
        .chain(ty.results().iter().map(|&x| get_cs_ty(x)))
        .collect();

    if params.is_empty() {
        cs_ty
    } else {
        cs_ty + "<" + &params.join(", ") + ">"
    }
}

fn params_cs_ty_name(ty: &FuncType) -> Vec<(&'static str, String)> {
    ty.params()
        .iter()
        .enumerate()
        .map(|(i, param)| (get_cs_ty(*param), format!("param{i}")))
        .collect::<Vec<_>>()
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
