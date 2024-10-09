use std::io;

use crate::ir::{module::Module, ty::CsType, DATA, ELEMENT, NULL, STACK, STACK_SIZE, STACK_TOP};

use super::func::codegen_func;

pub fn codegen_module(module: &Module<'_>, f: &mut dyn io::Write) -> io::Result<()> {
    writeln!(
        f,
        "// Converted from WebAssembly with {} {}",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION"),
    )?;
    writeln!(f, "using System;")?;
    if !module.test {
        writeln!(f, "using UdonSharp;")?;
        writeln!(f, "using UnityEngine;")?;
    }

    writeln!(f, "#pragma warning disable")?;

    if let Some(namespace) = &module.namespace {
        writeln!(f, "namespace {} {{", namespace)?;
    }

    write!(f, "public class {} ", module.class_name)?;
    if module.test {
        writeln!(f, "{{")?;
    } else {
        writeln!(f, ": UdonSharpBehaviour {{")?;
    }

    for (i, elem) in module.elements.iter().enumerate() {
        // エレメント配列
        // テストの際、使用するテーブルを外部からインポートするモジュールの場合、
        // エレメント配列内にC#メソッドのデリゲートを格納する。
        // それ以外の場合は、intで関数のインデックス+1を表す
        let use_delegate = module.test && module.table.as_ref().unwrap().import;
        let cs_ty = if use_delegate { "object" } else { "int" };
        let eq = if use_delegate { "=>" } else { "=" };
        write!(f, "{cs_ty}[] {ELEMENT}{i} {eq} new {cs_ty}[] {{ ",)?;

        for &item in elem.items.iter() {
            if use_delegate {
                write!(f, "{},", module.all_funcs[item as usize].header.name)?;
            } else {
                // テーブルに格納される関数インデックスは元のインデックスに1を足したもの
                // (配列の初期値の0でnullを表現するため)
                write!(f, "{},", item + 1)?;
            }
        }

        writeln!(f, " }};")?;
    }

    // コード
    for func in &module.all_funcs {
        if func.code.is_some() {
            codegen_func(func, f, module)?;
        }
    }

    // データ配列
    for (i, data) in module.datas.iter().enumerate() {
        write!(f, "byte[] {DATA}{i} = new byte[] {{ ")?;
        for byte in data.data {
            write!(f, "{byte},")?;
        }
        writeln!(f, " }};")?;
    }

    // インポートするモジュールの宣言
    for (module_var, module_ty) in &module.import_modules {
        writeln!(f, "public {module_ty} {module_var};")?;
    }

    // グローバル変数宣言
    for global in &module.globals {
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
    if let Some(table) = &module.table {
        if !table.import {
            if table.export {
                write!(f, "[NonSerialized] public ")?
            }

            // テストの場合はintとAction/Funcを混在させるため
            // テーブルはobjectの配列で表す
            let elem_cs_ty = if module.test { "object" } else { "int" };

            writeln!(f, "{elem_cs_ty}[] {};", table.name)?;
        }
    }

    // メモリ宣言
    if let Some(memory) = &module.memory {
        if !memory.import {
            if memory.export {
                write!(f, "[NonSerialized] public ")?
            }
            writeln!(f, "byte[] {};", memory.name)?;
        }
    }

    // 再帰呼び出しで保存するローカル変数用のスタック
    writeln!(f, "object[] {STACK} = new object[{STACK_SIZE}];")?;
    writeln!(f, "int {STACK_TOP} = 0;")?;

    // trap用のnull変数
    if !module.test {
        writeln!(f, "UdonSharpBehaviour {NULL} = null;")?;
    }

    // classの終了
    writeln!(f, "}}")?;

    if module.namespace.is_some() {
        writeln!(f, "}}")?;
    }

    Ok(())
}
