use std::{collections::HashSet, io::Write};

use anyhow::Result;
use wasmparser::{
    ConstExpr, DataKind, ElementItems, ElementKind, FuncType, GlobalType, MemoryType, Parser,
    RecGroup, StructuralType, TableType, ValType,
};

use crate::{
    convert::code::CodeConverter,
    ir::{
        func::{Func, FuncHeader},
        func_header, get_cs_ty,
        module::{Data, Element, Global, Memory, Module, Table},
        result_cs_ty, CALL_INDIRECT, DATA, ELEMENT, INIT, MAX_PARAMS, MEMORY, PAGE_SIZE, TABLE,
        W2US_PREFIX,
    },
};

use super::{convert_to_ident, trap};

pub struct Converter<'input, 'module> {
    module: &'module mut Module<'input>,
    code_idx: usize,
}

impl<'input, 'module> Converter<'input, 'module> {
    pub fn new(module: &'module mut Module<'input>) -> Self {
        Self {
            module,
            code_idx: 0,
        }
    }

    /// import_mapはモジュール名をモジュールと対応するクラス型の名前に変換する関数
    pub fn convert(
        &mut self,
        out_file: &mut impl Write,
        import_map: impl Fn(&str) -> String,
    ) -> Result<HashSet<&'input str>> {
        writeln!(out_file, "using System;")?;
        if !self.module.test {
            writeln!(out_file, "using UdonSharp;")?;
            writeln!(out_file, "using UnityEngine;")?;
        }

        write!(out_file, "public class {} ", self.module.class_name)?;
        if self.module.test {
            writeln!(out_file, "{{")?;
        } else {
            writeln!(out_file, ": UdonSharpBehaviour {{")?;
        }

        let mut import_modules = HashSet::new();

        for payload in Parser::new(0).parse_all(self.module.buf) {
            self.convert_payload(out_file, payload?, &mut import_modules)?;
        }

        // インポートするモジュールの宣言
        for module in import_modules.iter() {
            let module_ident = convert_to_ident(module);
            writeln!(out_file, "public {} {module_ident};", import_map(module))?;
        }

        // グローバル変数宣言
        for global in &self.module.globals {
            if global.import {
                continue;
            }

            if global.export {
                write!(out_file, "[NonSerialized] public ")?
            }

            let cs_ty = get_cs_ty(global.ty.content_type);
            writeln!(out_file, "{cs_ty} {};", global.name)?;
        }

        // テーブル宣言
        if let Some(table) = &self.module.table {
            if !table.import {
                if table.export {
                    write!(out_file, "[NonSerialized] public ")?
                }

                // テストの場合はuintとAction/Funcを混在させるため
                // テーブルはobjectの配列で表す
                let elem_cs_ty = if self.module.test { "object" } else { "uint" };

                writeln!(
                    out_file,
                    "{elem_cs_ty}[] {} = new {elem_cs_ty}[{}];",
                    table.name, table.ty.initial
                )?;
            }
        }

        // メモリー宣言
        if let Some(memory) = &self.module.memory {
            if !memory.import {
                if memory.export {
                    write!(out_file, "[NonSerialized] public ")?
                }
                writeln!(
                    out_file,
                    "byte[] {} = new byte[{}];",
                    memory.name,
                    memory.ty.initial * PAGE_SIZE as u64
                )?;
            }
        }

        self.write_call_indirects(out_file)?;

        self.write_init_method(out_file)?;

        writeln!(out_file, "}}")?;

        Ok(import_modules)
    }

    fn convert_payload(
        &mut self,
        out_file: &mut impl Write,
        payload: wasmparser::Payload<'input>,
        import_modules: &mut HashSet<&'input str>,
    ) -> Result<()> {
        use wasmparser::Payload::*;
        match payload {
            Version { .. } => {
                // println!("====== Module");
            }
            TypeSection(s) => {
                for ty in s {
                    match ty? {
                        RecGroup::Single(ty) => match ty.structural_type {
                            StructuralType::Func(ty) => self.module.types.push(ty),
                            _ => panic!("Non-function types are not supported"),
                        },
                        _ => panic!("Non-function types are not supported"),
                    }
                }
            }
            ImportSection(s) => {
                for import in s {
                    use wasmparser::TypeRef::*;
                    let import = import?;

                    import_modules.insert(import.module);

                    let name = convert_to_ident(import.name);
                    let full_name = format!("{}.{name}", convert_to_ident(import.module));

                    match import.ty {
                        Func(ty) => self.add_func(ty, Some(full_name.clone())),
                        Table(ty) => self.add_table(ty, Some(full_name.clone())),
                        Memory(ty) => self.add_memory(ty, Some(full_name.clone())),
                        Global(ty) => self.add_global(ty, None, Some(full_name.clone())),
                        Tag(_) => panic!("Tag import is not supported"),
                    }
                }
            }
            FunctionSection(s) => {
                for func in s {
                    let func = func?;
                    self.add_func(func, None);
                }
            }
            TableSection(s) => {
                for table in s {
                    let table = table?;
                    self.add_table(table.ty, None);
                }
            }
            MemorySection(s) => {
                for memory in s {
                    let memory = memory?;
                    self.add_memory(memory, None);
                }
            }
            GlobalSection(s) => {
                for global in s {
                    let global = global?;
                    self.add_global(global.ty, Some(global.init_expr), None);
                }
            }
            ExportSection(s) => {
                for export in s {
                    use wasmparser::ExternalKind::*;

                    let export = export?;
                    let name = convert_to_ident(export.name);
                    match export.kind {
                        Func => {
                            let func = &mut self.module.funcs[export.index as usize];
                            func.header.name = name;
                            func.header.export = true;
                        }
                        Table => {
                            let table = self.module.table.as_mut().unwrap();
                            table.name = name;
                            table.export = true;
                        }
                        Memory => {
                            let memory = self.module.memory.as_mut().unwrap();
                            memory.name = name;
                            memory.export = true;
                        }
                        Global => {
                            let global = &mut self.module.globals[export.index as usize];
                            global.name = name;
                            global.export = true;
                        }
                        Tag => {
                            panic!("Tag export is not supported");
                        }
                    }
                }
            }
            StartSection { func, .. } => {
                self.module.start_func = Some(func);
            }
            ElementSection(s) => {
                for (i, elem) in s.into_iter().enumerate() {
                    let elem = elem?;

                    let offset_expr = match elem.kind {
                        ElementKind::Active {
                            table_index,
                            offset_expr,
                        } => {
                            assert!(table_index.is_none());
                            self.convert_const_expr(&offset_expr)
                        }
                        _ => panic!("Not supported element kind"),
                    };

                    let items = if let ElementItems::Functions(s) = elem.items {
                        s.into_iter().collect::<Result<Vec<_>, _>>()?
                    } else {
                        panic!("Non function elements are not supported");
                    };

                    // エレメント配列
                    // テストの際、使用するテーブルを外部からインポートするモジュールの場合、
                    // エレメント配列内にC#メソッドのデリゲートを格納する。
                    // それ以外の場合は、uintで関数のインデックス+1を表す
                    let use_delegate =
                        self.module.test && self.module.table.as_ref().unwrap().import;
                    let cs_ty = if use_delegate { "object" } else { "uint" };
                    let eq = if use_delegate { "=>" } else { "=" };
                    write!(out_file, "{cs_ty}[] {ELEMENT}{i} {eq} new {cs_ty}[] {{ ",)?;

                    for &item in items.iter() {
                        if use_delegate {
                            write!(
                                out_file,
                                "{},",
                                self.module.funcs[item as usize].header.name
                            )?;
                        } else {
                            // テーブルに格納される関数インデックスは元のインデックスに1を足したもの
                            // (配列の初期値の0でnullを表現するため)
                            write!(out_file, "{},", item + 1)?;
                        }
                    }

                    writeln!(out_file, " }};")?;

                    self.module.elements.push(Element { offset_expr, items });
                }
            }
            DataSection(s) => {
                for (i, data) in s.into_iter().enumerate() {
                    let data = data?;
                    let offset_expr = match data.kind {
                        DataKind::Active {
                            memory_index,
                            offset_expr,
                        } => {
                            if memory_index != 0 {
                                panic!("Multi memory is not supported");
                            }
                            self.convert_const_expr(&offset_expr)
                        }
                        DataKind::Passive => panic!("Passive data segment is not supported"),
                    };
                    self.module.datas.push(Data {
                        offset_expr,
                        data: data.data,
                    });

                    // データ配列
                    write!(out_file, "byte[] {DATA}{i} = new byte[] {{ ")?;
                    for byte in data.data {
                        write!(out_file, "{byte},")?;
                    }
                    writeln!(out_file, " }};")?;
                }
            }
            CodeSectionEntry(s) => {
                let code_conv = CodeConverter::new(self.module, self.code_idx);
                let code = code_conv.convert(s)?;

                let func = &mut self.module.funcs[self.code_idx];
                func.code = Some(code);
                func.write(out_file)?;

                self.code_idx += 1;
            }
            _other => {
                // println!("found payload {:?}", _other);
            }
        }
        Ok(())
    }

    fn write_call_indirects(&self, out_file: &mut impl Write) -> Result<()> {
        let table = match &self.module.table {
            Some(x) => x,
            None => return Ok(()), // テーブルが無ければreturn
        };

        for (i_ty, ty) in self.module.types.iter().enumerate() {
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
            let use_delegate = self.module.test && ty.params().len() <= MAX_PARAMS;

            if use_delegate {
                // テストの際はuintの他にdelegateが含まれることがある
                writeln!(out_file, "if ({table_name}[index] is uint) {{")?;
            }

            writeln!(
                out_file,
                "switch ({}{table_name}[index]) {{",
                if self.module.test { "(uint)" } else { "" }, // テストの際はobjectをuintに変換
            )?;
            for (i, func) in self.module.funcs.iter().enumerate() {
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
            write!(out_file, "{}", trap(self.module, "invalid table value"))?;
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

    fn write_init_method(&self, out_file: &mut impl Write) -> Result<()> {
        writeln!(out_file, "public void {INIT}() {{")?;
        for global in &self.module.globals {
            if global.import {
                continue;
            }

            // グローバル変数の初期値を代入
            if let Some(init_expr) = &global.init_expr {
                writeln!(out_file, "{} = {init_expr};", global.name)?;
            }
        }
        for (i, Element { offset_expr, items }) in self.module.elements.iter().enumerate() {
            // テーブルへのエレメントのコピー
            writeln!(
                out_file,
                "Array.Copy({ELEMENT}{i}, 0, {}, {offset_expr}, {});",
                self.module.table.as_ref().unwrap().name,
                items.len()
            )?;
        }
        for (i, Data { offset_expr, data }) in self.module.datas.iter().enumerate() {
            // メモリへのデータのコピー
            writeln!(
                out_file,
                "Array.Copy({DATA}{i}, 0, {}, {offset_expr}, {});",
                self.module.memory.as_ref().unwrap().name,
                data.len()
            )?;
        }
        if let Some(start_func) = self.module.start_func {
            // start関数の呼び出し
            writeln!(
                out_file,
                "{}();",
                self.module.funcs[start_func as usize].header.name
            )?;
        }
        writeln!(out_file, "}}")?;
        Ok(())
    }

    fn add_func(&mut self, ty: u32, name: Option<String>) {
        let import = name.is_some();
        let name = match name {
            Some(x) => x,
            None => format!("{W2US_PREFIX}func{}", self.module.funcs.len()),
        };
        let ty = self.module.types[ty as usize].clone();

        if import {
            self.code_idx += 1;
        }

        self.module.funcs.push(Func {
            header: FuncHeader {
                name,
                ty,
                export: false,
            },
            code: None,
        });
    }

    fn add_table(&mut self, ty: TableType, name: Option<String>) {
        assert!(self.module.table.is_none());
        assert!(ty.element_type.is_func_ref());

        let import = name.is_some();
        let name = match name {
            Some(x) => x,
            None => TABLE.to_string(),
        };

        self.module.table = Some(Table {
            name,
            ty,
            import,
            export: false,
        });
    }

    fn add_memory(&mut self, ty: MemoryType, name: Option<String>) {
        assert!(self.module.memory.is_none());

        let import = name.is_some();
        let name = match name {
            Some(x) => x,
            None => MEMORY.to_string(),
        };

        self.module.memory = Some(Memory {
            name,
            ty,
            import,
            export: false,
        });
    }

    fn add_global(
        &mut self,
        ty: GlobalType,
        init_expr: Option<ConstExpr<'input>>,
        name: Option<String>,
    ) {
        let import = name.is_some();
        let name = match name {
            Some(x) => x,
            None => format!("{W2US_PREFIX}global{}", self.module.globals.len()),
        };
        let init_expr = init_expr.map(|x| self.convert_const_expr(&x));

        self.module.globals.push(Global {
            ty,
            name,
            init_expr,
            import,
            export: false,
        });
    }

    fn convert_const_expr(&self, expr: &ConstExpr<'_>) -> String {
        use wasmparser::Operator::*;
        let mut op_iter = expr.get_operators_reader().into_iter();
        let value = match op_iter.next().unwrap().unwrap() {
            I32Const { value } => match value {
                i32::MIN => format!("{} - 1", i32::MIN + 1),
                _ => format!("{}", value),
            },
            I64Const { value } => match value {
                i64::MIN => format!("{} - 1", i64::MIN + 1),
                _ => format!("{}", value),
            },
            F32Const { value } => format!("{:e}f", f32::from_bits(value.bits())),
            F64Const { value } => format!("{:e}", f64::from_bits(value.bits())),
            GlobalGet { global_index } => {
                self.module.globals[global_index as usize].name.to_string()
            }
            _ => panic!("Not supported const expr operator"),
        };

        if let End = op_iter.next().unwrap().unwrap() {
        } else {
            panic!("Not supported const expr operator")
        };
        assert!(op_iter.next().is_none());

        value
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
