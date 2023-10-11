mod code;

use std::{fmt, io::Write};

use anyhow::Result;
use once_cell::sync::Lazy;
use regex::Regex;
use wasmparser::{
    ConstExpr, DataKind, ElementItems, ElementKind, FuncType, GlobalType, MemoryType, Parser,
    RecGroup, StructuralType, TableType, ValType,
};

use crate::converter::code::CodeConverter;

pub struct Converter<'input> {
    buf: &'input [u8],
    class_name: &'input str,
    test: bool,
    types: Vec<FuncType>,
    funcs: Vec<Func>,
    table: Option<TableType>,
    memory: u32,
    globals: Vec<Global>,
    elements: Vec<Element>,
    datas: Vec<Data<'input>>,
    code_idx: usize,
    start_func: Option<u32>,
}

const PAGE_SIZE: u32 = 65536;
const W2US_PREFIX: &str = "w2us_";
const MEMORY: &str = "w2us_memory";
const TABLE: &str = "w2us_table";
const DATA: &str = "w2us_data";
const ELEMENT: &str = "w2us_element";
pub const INIT: &str = "w2us_init";
const CALL_INDIRECT: &str = "w2us_call_indirect";

impl<'input> Converter<'input> {
    pub fn new(buf: &'input [u8], class_name: &'input str, test: bool) -> Self {
        Self {
            buf,
            class_name,
            test,
            types: Vec::new(),
            funcs: Vec::new(),
            table: None,
            memory: 0,
            globals: Vec::new(),
            elements: Vec::new(),
            datas: Vec::new(),
            code_idx: 0,
            start_func: None,
        }
    }

    pub fn convert(&mut self, out_file: &mut impl Write) -> Result<()> {
        writeln!(out_file, "using System;")?;
        if !self.test {
            writeln!(out_file, "using UdonSharp;")?;
            writeln!(out_file, "using UnityEngine;")?;
        }

        write!(out_file, "class {} ", self.class_name)?;
        if self.test {
            writeln!(out_file, "{{")?;
        } else {
            writeln!(out_file, ": UdonSharpBehaviour {{")?;
        }

        for payload in Parser::new(0).parse_all(self.buf) {
            use wasmparser::Payload::*;
            match payload? {
                Version { .. } => {
                    // println!("====== Module");
                }
                TypeSection(s) => {
                    for ty in s {
                        match ty? {
                            RecGroup::Single(ty) => match ty.structural_type {
                                StructuralType::Func(ty) => self.types.push(ty),
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

                        let name = format!(
                            "{}__{}",
                            convert_to_ident(import.module),
                            convert_to_ident(import.name)
                        );
                        match import.ty {
                            Func(ty) => self.add_func(ty, Some(name)),
                            Table(ty) => self.add_table(ty),
                            Memory(ty) => self.add_memory(ty, out_file)?,
                            Global(ty) => self.add_global(ty, None, Some(name)),
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
                        self.add_table(table.ty);
                    }
                }
                MemorySection(s) => {
                    for memory in s {
                        let memory = memory?;
                        self.add_memory(memory, out_file)?;
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
                        match export.kind {
                            Func => {
                                let func = &mut self.funcs[export.index as usize];
                                func.name = convert_to_ident(export.name);
                                func.export = true;
                            }
                            Table => {}
                            Memory => {}
                            Global => {}
                            Tag => {}
                        }
                        // println!("  Export {} {:?}", export.name, export.kind);
                    }
                }
                StartSection { func, .. } => {
                    self.start_func = Some(func);
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
                        write!(out_file, "uint[] {ELEMENT}{i} = new uint[] {{ ")?;
                        for item in items.iter() {
                            write!(out_file, "{item},")?;
                        }
                        writeln!(out_file, " }};")?;

                        self.elements.push(Element { offset_expr, items });
                    }

                    for (i_ty, ty) in self.types.iter().enumerate() {
                        let name = format!("{CALL_INDIRECT}{i_ty}");

                        // call_indirect関数が受け取る引数リスト
                        let mut params = ty
                            .params()
                            .iter()
                            .enumerate()
                            .map(|(i, param)| (get_cs_ty(*param), format!("param{i}")))
                            .collect::<Vec<_>>();

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

                        writeln!(out_file, "switch ({TABLE}[index]) {{")?;
                        for (i, func) in self.funcs.iter().enumerate() {
                            if func.ty != *ty {
                                continue;
                            }

                            if ty.results().is_empty() {
                                writeln!(
                                    out_file,
                                    "case {i}: {}({}); return;",
                                    func.name, call_params
                                )?;
                            } else {
                                writeln!(
                                    out_file,
                                    "case {i}: return {}({});",
                                    func.name, call_params
                                )?;
                            }
                        }
                        write!(out_file, "default: ")?;
                        write!(out_file, "{}", self.trap("invalid table index"))?;
                        write!(out_file, "return")?;
                        if ty.results().is_empty() {
                            writeln!(out_file, ";")?;
                        } else {
                            writeln!(out_file, " 0;")?;
                        }
                        writeln!(out_file, "}}")?;
                        writeln!(out_file, "}}")?;
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
                        self.datas.push(Data {
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
                    let mut code_conv = CodeConverter::new(self, self.code_idx);
                    code_conv.convert(s, out_file)?;
                    self.code_idx += 1;
                }
                _other => {
                    // println!("found payload {:?}", _other);
                }
            }
        }

        // グローバル変数宣言
        for global in &self.globals {
            writeln!(
                out_file,
                "{} {};",
                get_cs_ty(global.ty.content_type),
                global.name
            )?;
        }

        // テーブル宣言
        if let Some(table) = &self.table {
            writeln!(out_file, "uint[] {TABLE} = new uint[{}];", table.initial)?;
        }

        // 初期化用関数
        writeln!(out_file, "public void {INIT}() {{")?;
        for global in &self.globals {
            // グローバル変数の初期値を代入
            if let Some(init_expr) = &global.init_expr {
                writeln!(out_file, "{} = {init_expr};", global.name)?;
            }
        }
        if self.table.is_some() {
            // テーブルに無効値を割り当て
            writeln!(out_file, "Array.Fill({TABLE}, 0xffffffff);")?;
        }
        for (i, Element { offset_expr, items }) in self.elements.iter().enumerate() {
            // テーブルへのエレメントのコピー
            writeln!(
                out_file,
                "Array.Copy({ELEMENT}{i}, 0, {TABLE}, {offset_expr}, {});",
                items.len()
            )?;
        }
        for (i, Data { offset_expr, data }) in self.datas.iter().enumerate() {
            // メモリへのデータのコピー
            writeln!(
                out_file,
                "Array.Copy({DATA}{i}, 0, {MEMORY}, {offset_expr}, {});",
                data.len()
            )?;
        }
        if let Some(start_func) = self.start_func {
            // start関数の呼び出し
            writeln!(out_file, "{}();", self.funcs[start_func as usize].name)?;
        }
        writeln!(out_file, "}}")?;

        writeln!(out_file, "}}")?;
        Ok(())
    }

    fn add_func(&mut self, ty: u32, name: Option<String>) {
        let name = match name {
            Some(x) => x,
            None => format!("{W2US_PREFIX}func{}", self.funcs.len()),
        };
        let ty = self.types[ty as usize].clone();
        self.funcs.push(Func {
            name,
            ty,
            export: false,
        });
    }

    fn add_table(&mut self, ty: TableType) {
        assert!(self.table.is_none());
        assert!(ty.element_type.is_func_ref());
        self.table = Some(ty);
    }

    fn add_memory(&mut self, ty: MemoryType, out_file: &mut impl Write) -> Result<()> {
        assert_eq!(self.memory, 0);
        self.memory = ty.initial as u32;

        writeln!(
            out_file,
            "byte[] {MEMORY} = new byte[{}];",
            self.memory * PAGE_SIZE
        )?;
        Ok(())
    }

    fn add_global(
        &mut self,
        ty: GlobalType,
        init_expr: Option<ConstExpr<'input>>,
        name: Option<String>,
    ) {
        let name = match name {
            Some(x) => x,
            None => format!("{W2US_PREFIX}global{}", self.globals.len()),
        };
        let init_expr = init_expr.map(|x| self.convert_const_expr(&x));

        self.globals.push(Global {
            ty,
            name,
            init_expr,
        });
    }

    fn trap(&self, message: &str) -> String {
        if self.test {
            format!("throw new Exception(\"{message}\");")
        } else {
            format!("Debug.LogError(\"{message}\");")
        }
    }

    fn convert_const_expr(&self, expr: &ConstExpr<'_>) -> String {
        use wasmparser::Operator::*;
        let mut op_iter = expr.get_operators_reader().into_iter();
        let value = match op_iter.next().unwrap().unwrap() {
            I32Const { value } => format!("{}", value as u32),
            I64Const { value } => format!("{}", value as u64),
            F32Const { value } => format!("{:e}f", f32::from_bits(value.bits())),
            F64Const { value } => format!("{:e}", f64::from_bits(value.bits())),
            GlobalGet { global_index } => self.globals[global_index as usize].name.to_string(),
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

struct Func {
    name: String,
    ty: FuncType,
    export: bool,
}

struct Global {
    name: String,
    ty: GlobalType,
    init_expr: Option<String>,
}

struct Element {
    offset_expr: String,
    items: Vec<u32>,
}

struct Data<'a> {
    offset_expr: String,
    data: &'a [u8],
}

fn func_header(
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

fn result_cs_ty(results: &[ValType]) -> &str {
    match results.len() {
        0 => "void",
        1 => get_cs_ty(results[0]),
        _ => unreachable!(),
    }
}

fn get_cs_ty(ty: ValType) -> &'static str {
    match ty {
        ValType::I32 => "uint",
        ValType::I64 => "ulong",
        ValType::F32 => "float",
        ValType::F64 => "double",
        _ => unreachable!(),
    }
}

pub fn convert_to_ident(name: &str) -> String {
    let prefix = if name.chars().next().unwrap().is_ascii_digit() {
        "_"
    } else {
        ""
    };

    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\W").unwrap());
    let ident = RE.replace_all(name, "_");

    prefix.to_string() + ident.as_ref()
}
