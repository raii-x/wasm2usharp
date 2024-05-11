use std::{collections::HashSet, iter::once};

use anyhow::Result;
use wasmparser::{
    ConstExpr, DataKind, ElementItems, ElementKind, FuncType, GlobalType, MemoryType, Name,
    NameSectionReader, Naming, Parser, RecGroup, StructuralType, TableType, ValType,
};

use crate::{
    ir::{
        builder::Builder,
        code::{Breakable, Call, Inst, InstKind},
        module::{Data, Element, Func, FuncHeader, Global, Memory, Module, Table},
        trap,
        ty::{Const, CsType},
        var::{Primary, Var},
        CALL_INDIRECT, DATA, ELEMENT, FUNC, GLOBAL, INIT, MAX_PARAMS, MEMORY, PAGE_SIZE, STACK_TOP,
        TABLE,
    },
    parse::code::CodeParser,
};

use super::convert_to_ident;

pub struct ModuleParser<'input, 'module> {
    module: &'module mut Module<'input>,
    code_idx: usize,
}

impl<'input, 'module> ModuleParser<'input, 'module> {
    pub fn new(module: &'module mut Module<'input>) -> Self {
        Self {
            module,
            code_idx: 0,
        }
    }

    /// import_mapはモジュール名をモジュールと対応するクラス型の名前に変換する関数
    pub fn parse(&mut self, import_map: &dyn Fn(&str) -> String) -> Result<HashSet<&'input str>> {
        let mut import_modules = HashSet::new();

        for payload in Parser::new(0).parse_all(self.module.buf) {
            self.parse_payload(payload?, &mut import_modules, &import_map)?;
        }

        self.add_init_method();

        Ok(import_modules)
    }

    fn parse_payload(
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
                for elem in s {
                    let elem = elem?;

                    let offset_expr = match elem.kind {
                        ElementKind::Active {
                            table_index,
                            offset_expr,
                        } => {
                            assert!(table_index.is_none());
                            self.parse_const_expr(&offset_expr)
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
                for data in s {
                    let data = data?;
                    let offset_expr = match data.kind {
                        DataKind::Active {
                            memory_index,
                            offset_expr,
                        } => {
                            if memory_index != 0 {
                                panic!("Multi memory is not supported");
                            }
                            self.parse_const_expr(&offset_expr)
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
                self.add_call_indirects();
            }
            CodeSectionEntry(s) => {
                let code = {
                    let code_conv = CodeParser::new(self.module, self.code_idx);
                    code_conv.parse(s)?
                };

                self.module.all_funcs[self.code_idx].code = Some(code);

                self.code_idx += 1;
            }
            CustomSection(s) => {
                if s.name() == "name" {
                    self.apply_names(s)?;
                }
            }
            _other => {}
        }
        Ok(())
    }

    /// Wasmに元々存在している全ての関数をadd_wasm_funcで追加するまでは
    /// 他の関数を追加してはならない
    fn add_wasm_func(&mut self, ty: u32, name: Option<String>) {
        let import = name.is_some();
        let name = match name {
            Some(x) => x,
            None => format!("{FUNC}{}", self.module.all_funcs.len()),
        };
        let ty = self.module.types[ty as usize].clone();

        if import {
            self.code_idx += 1;
        }

        let func = Func {
            header: FuncHeader {
                name,
                ty,
                import,
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
            None => format!("{GLOBAL}{}", self.module.globals.len()),
        };
        let init_expr = init_expr.map(|x| self.parse_const_expr(&x));

        self.module.globals.push(Global {
            ty,
            name,
            init_expr,
            import,
            export: false,
        });
    }

    fn parse_const_expr(&self, expr: &ConstExpr<'_>) -> String {
        use wasmparser::Operator::*;
        let mut op_iter = expr.get_operators_reader().into_iter();
        let value = match op_iter.next().unwrap().unwrap() {
            I32Const { value } => Const::Int(value).to_string(),
            I64Const { value } => Const::Long(value).to_string(),
            F32Const { value } => Const::Float(value.bits()).to_string(),
            F64Const { value } => Const::Double(value.bits()).to_string(),
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
                import: false,
                export: false,
            };

            let mut builder = Builder::new(header.ty.params());

            let table_name = &table.name;
            let use_delegate = self.module.test && ty.params().len() <= MAX_PARAMS;

            let (index_var_id, _) = builder.code.vars.last().unwrap();

            if use_delegate {
                // テストの際はuintの他にdelegateが含まれることがある
                builder.push_if_pattern(
                    format!("{table_name}[$p0] is uint"),
                    vec![index_var_id.into()],
                    Breakable::No,
                );
                builder.start_block();
            }

            // 関数呼び出し用の引数リスト
            let params: Vec<Primary> = builder.code.vars.iter().map(|(id, _)| id.into()).collect();
            let mut params_no_index = params.clone();
            params_no_index.pop();

            // 呼び出される関数のインデックスのリスト
            let cases: Vec<usize> = (0..self.module.wasm_func_count)
                .filter(|&i| {
                    let func = &self.module.all_funcs[i];
                    func.header.ty == *ty && func.in_table
                })
                .collect();

            if cases.is_empty() {
                if use_delegate {
                    builder.push_line(trap(self.module, "invalid table value"));
                } else {
                    // call_indirect関数を生成しない
                    continue;
                }
            } else {
                builder.push(Inst {
                    kind: InstKind::Switch,
                    pattern: format!(
                        "{}{table_name}[$p0]",
                        if self.module.test { "(uint)" } else { "" }, // テストの際はobjectをuintに変換
                    ),
                    params: vec![index_var_id.into()],
                    breakable: Breakable::Single,
                    ..Default::default()
                });

                let result = if ty.results().is_empty() {
                    None
                } else {
                    Some(builder.new_var(Var {
                        ty: CsType::get(ty.results()[0]),
                        ..Default::default()
                    }))
                };

                for i in cases {
                    builder.start_block();
                    builder.push_case(Const::UInt(i as u32 + 1).into());
                    let call = Call {
                        func: i,
                        ..Default::default()
                    };
                    builder.push_call(call, params_no_index.clone(), result);

                    match result {
                        Some(id) => builder.push_return(Some(id.into())),
                        None => builder.push_return(None),
                    }
                    builder.end_block();
                }

                builder.start_block();
                builder.push_default();
                builder.push_line(trap(self.module, "invalid table value"));
                match result {
                    Some(id) => {
                        builder.push_return(Some(builder.code.vars[id].ty.default().into()))
                    }
                    None => builder.push_return(None),
                }
                builder.end_block();
            }

            if use_delegate {
                builder.end_block();
                builder.start_block();

                // delegateに変換して呼び出し
                let del = func_delegate(ty);
                let call_params_pat = (0..params_no_index.len())
                    .map(|i| format!("$p{}", i))
                    .collect::<Vec<_>>()
                    .join(", ");
                let pattern = format!(
                    "(({del}){table_name}[$p{}])({call_params_pat})",
                    params_no_index.len()
                );

                if ty.results().is_empty() {
                    builder.push(Inst {
                        kind: InstKind::Expr,
                        pattern,
                        params,
                        ..Default::default()
                    });
                    builder.push_return(None);
                } else {
                    builder.push(Inst {
                        kind: InstKind::Return,
                        pattern,
                        params,
                        ..Default::default()
                    });
                }

                builder.end_block();
            }

            let index = self.module.all_funcs.len();
            let func = Func {
                header,
                code: Some(builder.build()),
                in_table: false,
            };

            self.module.call_indirects.insert(i_ty, index);
            self.module.all_funcs.push(func);
        }
    }

    fn add_init_method(&mut self) {
        let header = FuncHeader {
            name: INIT.to_string(),
            ty: FuncType::new(vec![], vec![]),
            import: false,
            export: true,
        };

        let mut builder = Builder::new(header.ty.params());

        builder.push_line(format!("{STACK_TOP} = 0;"));

        for global in &self.module.globals {
            if global.import {
                continue;
            }

            // グローバル変数の初期値を代入
            if let Some(init_expr) = &global.init_expr {
                builder.push_line(format!("{} = {init_expr};", global.name));
            }
        }

        if let Some(table) = &self.module.table {
            if !table.import {
                // テーブル配列の作成
                let elem_cs_ty = if self.module.test { "object" } else { "uint" };
                builder.push_line(format!(
                    "{} = new {elem_cs_ty}[{}];",
                    table.name, table.ty.initial
                ));
            }
        }

        for (i, Element { offset_expr, .. }) in self.module.elements.iter().enumerate() {
            // テーブルへのエレメントのコピー
            builder.push_line(format!(
                "{ELEMENT}{i}.CopyTo({}, {offset_expr});",
                self.module.table.as_ref().unwrap().name
            ));
        }

        if let Some(memory) = &self.module.memory {
            if !memory.import {
                // メモリ配列の作成
                builder.push_line(format!(
                    "{} = new byte[{}];",
                    memory.name,
                    memory.ty.initial * PAGE_SIZE as u64
                ));
            }
        }

        for (i, Data { offset_expr, .. }) in self.module.datas.iter().enumerate() {
            // メモリへのデータのコピー
            builder.push_line(format!(
                "{DATA}{i}.CopyTo({}, {offset_expr});",
                self.module.memory.as_ref().unwrap().name
            ));
        }

        if let Some(start_func) = self.module.start_func {
            // start関数の呼び出し
            let call = Call {
                func: start_func,
                ..Default::default()
            };
            builder.push_call(call, vec![], None);
        }

        self.module.all_funcs.push(Func {
            header,
            code: Some(builder.build()),
            in_table: false,
        });
    }

    fn apply_names(
        &mut self,
        setcion: wasmparser::CustomSectionReader<'_>,
    ) -> Result<(), anyhow::Error> {
        let mut names = self.all_names();

        let reader = NameSectionReader::new(setcion.data(), setcion.data_offset());
        for name in reader {
            if let Name::Function(s) = name? {
                self.apply_func_names(s, &mut names)?;
            }
        }

        Ok(())
    }

    fn apply_func_names(
        &mut self,
        s: wasmparser::SectionLimited<'_, Naming<'_>>,
        names: &mut HashSet<String>,
    ) -> Result<(), anyhow::Error> {
        for naming in s {
            let naming = naming?;

            let header = &mut self.module.all_funcs[naming.index as usize].header;

            if !header.import && !header.export {
                let name: String = convert_to_ident(naming.name);
                for i in 0..100 {
                    let mut name = name.clone();
                    if i != 0 {
                        // 既に同じ名前があれば後ろに"_"と数字をつける
                        name += &format!("_{i}");
                    }

                    if names.insert(name.clone()) {
                        // 同名の関数がない場合のみに関数の名前を変更
                        header.name = name;
                        break;
                    }
                }
            }
        }

        Ok(())
    }

    fn all_names(&mut self) -> HashSet<String> {
        let class = once(self.module.class_name.clone());
        let funcs = self.module.all_funcs.iter().map(|x| x.header.name.clone());
        let table = self.module.table.iter().map(|x| x.name.clone());
        let memory = self.module.memory.iter().map(|x| x.name.clone());
        let globals = self.module.globals.iter().map(|x| x.name.clone());
        let imports = self.module.import_modules.keys().cloned(); // key(変数名)のみ

        HashSet::from_iter(
            class
                .chain(funcs)
                .chain(table)
                .chain(memory)
                .chain(globals)
                .chain(imports),
        )
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
