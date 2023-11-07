use std::{collections::HashSet, iter::once};

use anyhow::Result;
use wasmparser::{
    ConstExpr, DataKind, ElementItems, ElementKind, FuncType, GlobalType, MemoryType, Parser,
    RecGroup, StructuralType, TableType, ValType,
};

use crate::{
    convert::{builtin_func::add_builtin_funcs, code::CodeConverter},
    ir::{
        func::{Code, Expr, Func, FuncHeader, Instr},
        module::{Data, Element, Global, Memory, Module, Table},
        trap,
        ty::{Const, CsType},
        CALL_INDIRECT, DATA, ELEMENT, INIT, MAX_PARAMS, MEMORY, TABLE, W2US_PREFIX,
    },
};

use super::convert_to_ident;

pub struct ModuleConverter<'input, 'module> {
    module: &'module mut Module<'input>,
    code_idx: usize,
}

impl<'input, 'module> ModuleConverter<'input, 'module> {
    pub fn new(module: &'module mut Module<'input>) -> Self {
        Self {
            module,
            code_idx: 0,
        }
    }

    /// import_mapはモジュール名をモジュールと対応するクラス型の名前に変換する関数
    pub fn convert(&mut self, import_map: &dyn Fn(&str) -> String) -> Result<HashSet<&'input str>> {
        let mut import_modules = HashSet::new();

        for payload in Parser::new(0).parse_all(self.module.buf) {
            self.convert_payload(payload?, &mut import_modules, &import_map)?;
        }

        self.add_init_method();

        Ok(import_modules)
    }

    fn convert_payload(
        &mut self,
        payload: wasmparser::Payload<'input>,
        import_modules: &mut HashSet<&'input str>,
        import_map: &dyn Fn(&str) -> String,
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

                    let module = convert_to_ident(import.module);
                    let name = convert_to_ident(import.name);
                    let full_name = format!("{module}.{name}");

                    import_modules.insert(import.module);
                    self.module
                        .import_modules
                        .entry(module)
                        .or_insert(import_map(import.module));

                    match import.ty {
                        Func(ty) => self.add_wasm_func(ty, Some(full_name.clone())),
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
                    self.add_wasm_func(func, None);
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
                            let func = &mut self.module.all_funcs[export.index as usize];
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
                self.module.start_func = Some(func as usize);
            }
            ElementSection(s) => {
                for elem in s.into_iter() {
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

                    for &i in &items {
                        // elementに存在するなら関数がテーブルにあるとする
                        self.module.all_funcs[i as usize].in_table = true;
                    }

                    self.module.elements.push(Element { offset_expr, items });
                }
            }
            DataSection(s) => {
                for data in s.into_iter() {
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
                }
            }
            CodeSectionStart { .. } => {
                add_builtin_funcs(self.module);
                self.add_call_indirects();
            }
            CodeSectionEntry(s) => {
                let code = {
                    let code_conv = CodeConverter::new(self.module, self.code_idx);
                    code_conv.convert(s)?
                };

                self.module.all_funcs[self.code_idx].code = Some(code);

                self.code_idx += 1;
            }
            _other => {
                // println!("found payload {:?}", _other);
            }
        }
        Ok(())
    }

    /// Wasmに元々存在している全ての関数をadd_wasm_funcで追加するまでは
    /// 他の関数を追加してはならない
    fn add_wasm_func(&mut self, ty: u32, name: Option<String>) {
        let import = name.is_some();
        let name = match name {
            Some(x) => x,
            None => format!("{W2US_PREFIX}func{}", self.module.all_funcs.len()),
        };
        let ty = self.module.types[ty as usize].clone();

        if import {
            self.code_idx += 1;
        }

        let func = Func {
            header: FuncHeader {
                name,
                ty,
                export: false,
            },
            code: None,
            in_table: false,
        };

        self.module.wasm_func_count += 1;
        self.module.all_funcs.push(func);
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
            I32Const { value } => Const::Int(value).to_string(),
            I64Const { value } => Const::Long(value).to_string(),
            F32Const { value } => Const::Float(f32::from_bits(value.bits())).to_string(),
            F64Const { value } => Const::Double(f64::from_bits(value.bits())).to_string(),
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

    fn add_call_indirects(&mut self) {
        let table = match &self.module.table {
            Some(x) => x,
            None => return, // テーブルが無ければreturn
        };

        for (i_ty, ty) in self.module.types.iter().enumerate() {
            let name = format!("{CALL_INDIRECT}{i_ty}");

            let call_ind_ty = FuncType::new(
                ty.params()
                    .iter()
                    .copied()
                    .chain(once(ValType::I32))
                    .collect::<Vec<_>>(),
                ty.results().to_vec(),
            );

            // call_indirect用の関数定義
            let header = FuncHeader {
                name,
                ty: call_ind_ty,
                export: false,
            };

            let mut code = Code::new(&header);

            let table_name = &table.name;
            let use_delegate = self.module.test && ty.params().len() <= MAX_PARAMS;

            let index_var = code.var_decls.last().unwrap().var;

            if use_delegate {
                // テストの際はuintの他にdelegateが含まれることがある
                code.instrs.push(Instr::Line(format!(
                    "if ({table_name}[{index_var}] is uint) {{"
                )));
            }

            code.instrs.push(Instr::Line(format!(
                "switch ({}{table_name}[{index_var}]) {{",
                if self.module.test { "(uint)" } else { "" }, // テストの際はobjectをuintに変換
            )));

            // 関数呼び出し用の引数リスト
            let call_params: Vec<Expr> = code.var_decls[0..code.var_decls.len() - 1]
                .iter()
                .map(|x| x.var.into())
                .collect();

            let result = if ty.results().is_empty() {
                None
            } else {
                Some(code.new_var(CsType::get(ty.results()[0]), None))
            };

            for i in 0..self.module.wasm_func_count {
                let func = &self.module.all_funcs[i];
                if func.header.ty != *ty {
                    continue;
                }

                code.instrs.push(Instr::Line(format!("case {}:", i + 1)));
                code.instrs.push(Instr::Call {
                    func: i,
                    params: call_params.clone(),
                    result,
                    recursive: false,
                    save_vars: vec![],
                    save_loop_vars: vec![],
                });

                match result {
                    Some(x) => code.instrs.push(Instr::Line(format!("return {x};"))),
                    None => code.instrs.push(Instr::Line("return;".to_string())),
                }
            }

            code.instrs.push(Instr::Line("default:".to_string()));
            code.instrs
                .push(Instr::Line(trap(self.module, "invalid table value")));
            if ty.results().is_empty() {
                code.instrs.push(Instr::Line("return;".to_string()));
            } else {
                code.instrs.push(Instr::Line("return 0;".to_string()));
            }

            code.instrs.push(Instr::Line("}".to_string()));

            if use_delegate {
                code.instrs.push(Instr::Line("} else {".to_string()));
                // delegateに変換して呼び出し
                let del = func_delegate(ty);
                let call_params = call_params
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                if ty.results().is_empty() {
                    code.instrs.push(Instr::Line(format!(
                        "(({del}){table_name}[{index_var}])({call_params}); return;",
                    )));
                } else {
                    code.instrs.push(Instr::Line(format!(
                        "return (({del}){table_name}[{index_var}])({call_params});",
                    )));
                }
                code.instrs.push(Instr::Line("}".to_string()));
            }

            let index = self.module.all_funcs.len();
            let func = Func {
                header,
                code: Some(code),
                in_table: false,
            };

            self.module.call_indirects.push(index);
            self.module.all_funcs.push(func);
        }
    }

    fn add_init_method(&mut self) {
        let header = FuncHeader {
            name: INIT.to_string(),
            ty: FuncType::new(vec![], vec![]),
            export: true,
        };

        let mut code = Code::new(&header);

        for global in &self.module.globals {
            if global.import {
                continue;
            }

            // グローバル変数の初期値を代入
            if let Some(init_expr) = &global.init_expr {
                code.instrs
                    .push(Instr::Line(format!("{} = {init_expr};", global.name)));
            }
        }
        for (i, Element { offset_expr, items }) in self.module.elements.iter().enumerate() {
            // テーブルへのエレメントのコピー
            code.instrs.push(Instr::Line(format!(
                "Array.Copy({ELEMENT}{i}, 0, {}, {offset_expr}, {});",
                self.module.table.as_ref().unwrap().name,
                items.len()
            )));
        }
        for (i, Data { offset_expr, data }) in self.module.datas.iter().enumerate() {
            // メモリへのデータのコピー
            code.instrs.push(Instr::Line(format!(
                "Array.Copy({DATA}{i}, 0, {}, {offset_expr}, {});",
                self.module.memory.as_ref().unwrap().name,
                data.len()
            )));
        }
        if let Some(start_func) = self.module.start_func {
            // start関数の呼び出し
            code.instrs.push(Instr::Call {
                func: start_func,
                params: vec![],
                result: None,
                recursive: false,
                save_vars: vec![],
                save_loop_vars: vec![],
            });
        }

        self.module.all_funcs.push(Func {
            header,
            code: Some(code),
            in_table: false,
        });
    }
}

/// 引数の関数型と対応するC#でのAction/Funcの型を表す文字列を取得 (テスト用)
fn func_delegate(ty: &FuncType) -> String {
    let cs_ty = if ty.results().is_empty() {
        "Action".to_string()
    } else {
        "Func".to_string()
    };

    let params: Vec<String> = ty
        .params()
        .iter()
        .map(|&x| CsType::get(x))
        .chain(ty.results().iter().map(|&x| CsType::get(x)))
        .map(|x| x.to_string())
        .collect();

    if params.is_empty() {
        cs_ty
    } else {
        cs_ty + "<" + &params.join(", ") + ">"
    }
}
