use std::collections::HashSet;

use anyhow::Result;
use wasmparser::{
    ConstExpr, DataKind, ElementItems, ElementKind, GlobalType, MemoryType, Parser, RecGroup,
    StructuralType, TableType,
};

use crate::{
    convert::code::CodeConverter,
    ir::{
        func::{Func, FuncHeader},
        module::{Data, Element, Global, Memory, Module, Table},
        MEMORY, TABLE, W2US_PREFIX,
    },
};

use super::convert_to_ident;

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
    pub fn convert(&mut self, import_map: impl Fn(&str) -> String) -> Result<HashSet<&'input str>> {
        let mut import_modules = HashSet::new();

        for payload in Parser::new(0).parse_all(self.module.buf) {
            self.convert_payload(payload?, &mut import_modules, &import_map)?;
        }

        Ok(import_modules)
    }

    fn convert_payload(
        &mut self,
        payload: wasmparser::Payload<'input>,
        import_modules: &mut HashSet<&'input str>,
        import_map: impl Fn(&str) -> String,
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
            CodeSectionEntry(s) => {
                let code_conv = CodeConverter::new(self.module, self.code_idx);
                let code = code_conv.convert(s)?;

                let func = &mut self.module.funcs[self.code_idx];
                func.code = Some(code);

                self.code_idx += 1;
            }
            _other => {
                // println!("found payload {:?}", _other);
            }
        }
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
