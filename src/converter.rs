use std::{fmt, io::Write};

use anyhow::Result;
use regex::Regex;
use wasmparser::{
    for_each_operator, BlockType, BrTable, ConstExpr, ElementItems, ElementKind, FuncType,
    FunctionBody, GlobalType, Ieee32, Ieee64, MemArg, Parser, RecGroup, StorageType,
    StructuralType, ValType, VisitOperator,
};

macro_rules! define_single_visit_operator {
    ( @mvp $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident) => {};
    ( @$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident) => {
        fn $visit(&mut self $($(,$arg: $argty)*)?) -> Self::Output {
            Err(OperatorError::NotSupported(stringify!($proposal)).into())
        }
    };
}

macro_rules! define_visit_operator {
    ($( @$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident)*) => {
        $(
            define_single_visit_operator!(@$proposal $op $({ $($arg: $argty),* })? => $visit);
        )*
    }
}

pub struct Converter<'input> {
    buf: &'input [u8],
    udon: bool,
    types: Vec<FuncType>,
    funcs: Vec<Func>,
    table: Vec<Option<u32>>,
    memory: u32,
    globals: Vec<GlobalType>,
    code_idx: usize,
    start_func: Option<u32>,
}

const PAGE_SIZE: u32 = 65536;
const CLASS_NAME: &str = "Wasm2USharp";
const W2US_PREFIX: &str = "w2us_";
const MEMORY: &str = "w2us_memory";
const INIT: &str = "w2us_init";
const BREAK_DEPTH: &str = "w2us_break_depth";
const CALL_INDIRECT: &str = "w2us_call_indirect";

impl<'input> Converter<'input> {
    pub fn new(buf: &'input [u8], udon: bool) -> Self {
        Self {
            buf,
            udon,
            types: Vec::new(),
            funcs: Vec::new(),
            table: Vec::new(),
            memory: 0,
            globals: Vec::new(),
            code_idx: 0,
            start_func: None,
        }
    }

    pub fn convert(&mut self, out_file: &mut impl Write) -> Result<()> {
        writeln!(out_file, "using System;")?;

        write!(out_file, "class {CLASS_NAME} ")?;
        if self.udon {
            writeln!(out_file, ": UdonSharpBehaviour {{")?;
        } else {
            writeln!(out_file, "{{")?;
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
                        let import = import?;
                        // println!("  Import {}::{}", import.module, import.name);
                    }
                }
                FunctionSection(s) => {
                    for func in s {
                        let func = func?;

                        let name = FuncName::Index(self.funcs.len() as u32);
                        let ty = self.types[func as usize].clone();
                        self.funcs.push(Func { name, ty });
                    }
                }
                TableSection(s) => {
                    for table in s {
                        let table = table?;
                        assert!(table.ty.element_type.is_func_ref());
                        self.table.resize(table.ty.initial as usize, None);
                    }
                }
                MemorySection(s) => {
                    for memory in s {
                        let memory = memory?;
                        self.memory = memory.initial as u32;

                        writeln!(
                            out_file,
                            "byte[] {MEMORY} = new byte[{}];",
                            self.memory * PAGE_SIZE
                        )?;
                    }
                }
                GlobalSection(s) => {
                    for global in s {
                        let global = global?;

                        writeln!(
                            out_file,
                            "{} global{};",
                            get_cs_ty(global.ty.content_type),
                            self.globals.len()
                        )?;

                        self.globals.push(global.ty);
                    }
                }
                ExportSection(s) => {
                    let re = Regex::new(r"\W").unwrap();
                    for export in s {
                        use wasmparser::ExternalKind::*;

                        let export = export?;
                        match export.kind {
                            Func => {
                                self.funcs[export.index as usize].name = FuncName::Exported(
                                    re.replace_all(export.name, "_").to_string(),
                                );
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
                    for elem in s {
                        let elem = elem?;

                        let offset = match elem.kind {
                            ElementKind::Active {
                                table_index,
                                offset_expr,
                            } => {
                                assert!(table_index.is_none());
                                read_i32_const_ops(&offset_expr)
                            }
                            _ => panic!("Not supported element kind"),
                        };

                        if let ElementItems::Functions(s) = elem.items {
                            for (i, item) in s.into_iter().enumerate() {
                                let item = item?;
                                self.table[offset as usize + i] = Some(item);
                            }
                        } else {
                            panic!("Non function elements are not supported");
                        }
                    }

                    for (i_ty, ty) in self.types.iter().enumerate() {
                        let name = format!("{CALL_INDIRECT}{i_ty}");
                        let mut params = ty
                            .params()
                            .iter()
                            .enumerate()
                            .map(|(i, param)| (get_cs_ty(*param), format!("param{i}")))
                            .collect::<Vec<_>>();

                        let call_params = params
                            .iter()
                            .map(|(_, y)| y.as_str())
                            .collect::<Vec<_>>()
                            .join(", ");

                        params.push((get_cs_ty(ValType::I32), "index".to_string()));

                        writeln!(
                            out_file,
                            "{} {{",
                            func_header(&name, result_cs_ty(ty.results()), &params)
                        )?;

                        writeln!(out_file, "switch (index) {{")?;
                        for (i, i_func) in self.table.iter().enumerate() {
                            if let Some(i_func) = i_func {
                                let func = &self.funcs[*i_func as usize];
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
                        }
                        write!(out_file, "default: ")?;
                        if self.udon {
                            write!(out_file, "Debug.Error(\"invalid table index\"); ")?;
                        }
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

        writeln!(out_file, "public void {INIT}() {{")?;
        if let Some(start_func) = self.start_func {
            writeln!(out_file, "{}();", self.funcs[start_func as usize].name)?;
        }
        writeln!(out_file, "}}")?;

        if !self.udon {
            writeln!(
                out_file,
                r#"static void Main(string[] args)
{{
    var instance = new {CLASS_NAME}();
    instance.{INIT}();
    if (args.Length != 0)
    {{
        var t = instance.GetType();
        var mi = t.GetMethod(args[0]);
        mi.Invoke(instance, null);
    }}
}}"#
            )?;
        }

        writeln!(out_file, "}}")?;

        Ok(())
    }
}

fn read_i32_const_ops(expr: &ConstExpr) -> i32 {
    let mut op_iter = expr.get_operators_reader().into_iter();
    let value = if let wasmparser::Operator::I32Const { value } = op_iter.next().unwrap().unwrap() {
        value
    } else {
        panic!("Not supported const expr operator")
    };

    if let wasmparser::Operator::End = op_iter.next().unwrap().unwrap() {
    } else {
        panic!("Not supported const expr operator")
    };

    value
}

struct Func {
    name: FuncName,
    ty: FuncType,
}

enum FuncName {
    Index(u32),
    Exported(String),
}

impl fmt::Display for FuncName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Index(i) => write!(f, "{W2US_PREFIX}func{i}"),
            Self::Exported(name) => write!(f, "{name}"),
        }
    }
}

struct CodeConverter<'input, 'conv> {
    conv: &'conv Converter<'input>,
    code_idx: usize,
    blocks: Vec<Block>,
    vars: Vec<Var>,
    local_count: u32,
    stmts: Vec<String>,
    /// brの後など、到達不可能なコードの処理時にtrue
    unreachable: bool,
}

#[derive(Debug)]
struct Block {
    stack: Vec<Var>,
    result: Option<Var>,
    do_while: bool,
}

#[derive(Debug, Clone, Copy)]
struct Var {
    index: u32,
    ty: ValType,
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "var{}", self.index)
    }
}

impl<'input, 'conv> CodeConverter<'input, 'conv> {
    fn new(conv: &'conv Converter<'input>, code_idx: usize) -> Self {
        Self {
            conv,
            code_idx,
            blocks: Vec::new(),
            vars: Vec::new(),
            local_count: 0,
            stmts: Vec::new(),
            unreachable: false,
        }
    }

    fn func(&self) -> &Func {
        &self.conv.funcs[self.code_idx]
    }

    fn convert(&mut self, body: FunctionBody<'_>, out_file: &mut impl Write) -> Result<()> {
        for &ty in self.conv.funcs[self.code_idx].ty.params() {
            let _ = self.new_var(ty);
            self.local_count += 1;
        }

        for local in body.get_locals_reader()? {
            let local = local?;
            let _ = self.new_var(local.1);
            self.local_count += 1;
        }

        let results = self.conv.funcs[self.code_idx].ty.results();

        // 関数の最上位のブロック
        self.visit_block(match results.len() {
            0 => BlockType::Empty,
            1 => BlockType::Type(results[0]),
            _ => panic!("multi value is not supported"),
        })?;
        let result_var = self.blocks[0].result;

        for op in body.get_operators_reader()? {
            let op = op?;

            if self.unreachable {
                use wasmparser::Operator::*;
                match &op {
                    End | Else => {}
                    _ => continue,
                }
            }
            self.visit_operator(&op)?;
        }

        if let Some(res) = result_var {
            self.stmts.push(format!("return {res};"));
        }

        self.write(out_file)?;

        Ok(())
    }

    fn write(&self, out_file: &mut impl Write) -> Result<()> {
        // 関数ヘッダ
        let func_ty = self.func().ty.clone();

        if let FuncName::Exported(_) = self.func().name {
            write!(out_file, "public ")?;
        }

        let params: Vec<(&str, &Var)> = func_ty
            .params()
            .iter()
            .map(|&ty| get_cs_ty(ty))
            .zip(self.vars.iter())
            .collect();
        print!(
            "{}",
            func_header(&self.func().name, result_cs_ty(func_ty.results()), &params)
        );

        writeln!(out_file, " {{")?;

        writeln!(out_file, "int {BREAK_DEPTH} = 0;")?;

        // 一時変数
        for var in self.vars.iter().skip(func_ty.params().len()) {
            writeln!(out_file, "{} {var} = 0;", get_cs_ty(var.ty))?;
        }

        // 本体
        for stmt in &self.stmts {
            writeln!(out_file, "{stmt}")?;
        }

        writeln!(out_file, "}}")?;

        Ok(())
    }

    fn new_var(&mut self, ty: ValType) -> Var {
        let var = Var {
            index: self.vars.len() as u32,
            ty,
        };
        self.vars.push(var);
        var
    }

    fn new_block(&mut self, blockty: BlockType, do_while: bool) {
        let result = match blockty {
            BlockType::Empty => None,
            BlockType::Type(ty) => Some(self.new_var(ty)),
            BlockType::FuncType(..) => panic!("func type blocks are not supported"),
        };

        self.blocks.push(Block {
            stack: Vec::new(),
            result,
            do_while,
        });
    }

    /// ブロックに戻り値があれば、stmtsに戻り値を代入する処理を追加する
    fn block_result(&mut self, relative_depth: u32) {
        let upper_block = &self.blocks[self.blocks.len() - 1 - relative_depth as usize];

        if let Some(result) = upper_block.result {
            let current_block = &self.blocks[self.blocks.len() - 1];
            let rhs = current_block.stack.last().unwrap();
            self.stmts.push(format!("{result} = {rhs};"));
        }
    }

    /// relative_depthが0より大きければBREAK_DEPTHに代入する処理を追加する
    fn set_break_depth(&mut self, relative_depth: u32) {
        if relative_depth > 0 {
            self.stmts
                .push(format!("{BREAK_DEPTH} = {relative_depth};"));
        }
    }

    fn push_stack(&mut self, var: Var) {
        self.blocks.last_mut().unwrap().stack.push(var)
    }

    fn pop_stack(&mut self) -> Var {
        self.blocks.last_mut().unwrap().stack.pop().unwrap()
    }

    fn last_stack(&self) -> Var {
        *self.blocks.last().unwrap().stack.last().unwrap()
    }

    fn visit_load(
        &mut self,
        memarg: MemArg,
        var_type: ValType,
        mem_type: StorageType,
        signed: bool,
    ) -> <Self as VisitOperator>::Output {
        use {StorageType::*, ValType::*};

        let idx = self.pop_stack();
        let var = self.new_var(var_type);
        self.push_stack(var);

        let mut load = format!("{var} = ");

        match mem_type {
            I8 => load += &format!("{MEMORY}[{0}+{1}];", idx, memarg.offset),
            I16 => load += &format!("{MEMORY}[{0}+{1}] | ({2}){MEMORY}[{0}+{1}+1]<<8;", idx, memarg.offset, get_cs_ty(var_type)),
            Val(I32) => load += &format!(
                "{MEMORY}[{0}+{1}] | ({2}){MEMORY}[{0}+{1}+1]<<8 | ({2}){MEMORY}[{0}+{1}+2]<<16 | ({2}){MEMORY}[{0}+{1}+3]<<24;",
                idx, memarg.offset, get_cs_ty(var_type)),
            Val(I64) => load += &format!(
                "{MEMORY}[{0}+{1}] | ({2}){MEMORY}[{0}+{1}+1]<<8 | ({2}){MEMORY}[{0}+{1}+2]<<16 | ({2}){MEMORY}[{0}+{1}+3]<<24 | ({2}){MEMORY}[{0}+{1}+4]<<32 | ({2}){MEMORY}[{0}+{1}+5]<<40 | ({2}){MEMORY}[{0}+{1}+6]<<48 | ({2}){MEMORY}[{0}+{1}+7]<<56;",
                idx, memarg.offset, get_cs_ty(var_type)),
            _ => unreachable!(),
        }
        self.stmts.push(load);

        if signed {
            let mut sign;
            match mem_type {
                I8 => sign = format!("if ({var} >= 0x80) "),
                I16 => sign = format!("if ({var} >= 0x8000) "),
                Val(I32) => sign = format!("if ({var} >= 0x80000000) "),
                _ => unreachable!(),
            }

            match (var_type, mem_type) {
                (I32, I8) => sign += &format!("{var} |= 0xffffff00;"),
                (I32, I16) => sign += &format!("{var} |= 0xffff0000;"),
                (I64, I8) => sign += &format!("{var} |= 0xffffffffffffff00;"),
                (I64, I16) => sign += &format!("{var} |= 0xffffffffffff0000;"),
                (I64, Val(I32)) => sign += &format!("{var} |= 0xffffffff00000000;"),
                _ => unreachable!(),
            }

            self.stmts.push(sign);
        }

        Ok(())
    }

    fn visit_store(
        &mut self,
        memarg: MemArg,
        mem_type: StorageType,
    ) -> <Self as VisitOperator>::Output {
        use {StorageType::*, ValType::*};

        let var = self.pop_stack();
        let idx = self.pop_stack();

        self.stmts.push(match mem_type {
            I8 => format!("{MEMORY}[{0}+{1}]=(byte)({2}&0xff);", idx, memarg.offset, var),
            I16 => {
                format!(
                    "{MEMORY}[{0}+{1}]=(byte)({2}&0xff); {MEMORY}[{0}+{1}+1]=(byte)(({2}>>8)&0xff);",
                    idx, memarg.offset, var
                )
            }
            Val(I32) => {
                format!(
                    "{MEMORY}[{0}+{1}]=(byte)({2}&0xff); {MEMORY}[{0}+{1}+1]=(byte)(({2}>>8)&0xff); {MEMORY}[{0}+{1}+2]=(byte)(({2}>>16)&0xff); {MEMORY}[{0}+{1}+3]=(byte)(({2}>>24)&0xff);",
                    idx, memarg.offset, var
                )
            }
            Val(I64) => {
                format!(
                    "{MEMORY}[{0}+{1}]=(byte)({2}&0xff); {MEMORY}[{0}+{1}+1]=(byte)(({2}>>8)&0xff); {MEMORY}[{0}+{1}+2]=(byte)(({2}>>16)&0xff); {MEMORY}[{0}+{1}+3]=(byte)(({2}>>24)&0xff); {MEMORY}[{0}+{1}+4]=(byte)(({2}>>32)&0xff); {MEMORY}[{0}+{1}+5]=(byte)(({2}>>40)&0xff); {MEMORY}[{0}+{1}+6]=(byte)(({2}>>48)&0xff); {MEMORY}[{0}+{1}+7]=(byte)(({2}>>56)&0xff);",
                    idx, memarg.offset, var
                )
            }
            _ => unreachable!(),
        });

        Ok(())
    }

    fn visit_const(
        &mut self,
        ty: ValType,
        value: impl fmt::Display,
    ) -> <Self as VisitOperator>::Output {
        let var = self.new_var(ty);
        self.push_stack(var);

        self.stmts.push(format!("{var} = {value};"));
        Ok(())
    }

    fn visit_eqz(&mut self) -> <Self as VisitOperator>::Output {
        let opnd = self.pop_stack();
        let result = self.new_var(ValType::I32);
        self.push_stack(result);

        self.stmts.push(format!("{result} = {opnd} == 0 ? 1 : 0;"));
        Ok(())
    }

    fn visit_bin_op(
        &mut self,
        ty: ValType,
        op: &str,
        logical: bool,
        signed: bool,
    ) -> <Self as VisitOperator>::Output {
        let rhs = self.pop_stack();
        let lhs = self.pop_stack();
        let result = self.new_var(if logical { ValType::I32 } else { ty });
        self.push_stack(result);

        let mut stmt = format!("{result} = ");

        if signed {
            let cs_ty = get_cs_ty(ty);
            // 先頭の`u`を取り除く
            let cs_ty_s = &cs_ty[1..];

            if !logical {
                stmt += &format!("({cs_ty})");
            }

            stmt += &format!("(({0}){lhs} {op} ({0}){rhs})", cs_ty_s)
        } else {
            stmt += &format!("{lhs} {op} {rhs}")
        };

        if logical {
            stmt += " ? 1 : 0";
        }

        stmt += ";";

        self.stmts.push(stmt);
        Ok(())
    }

    fn visit_rot_op(&mut self, ty: ValType, right: bool) -> <Self as VisitOperator>::Output {
        let rhs = self.pop_stack();
        let lhs = self.pop_stack();
        let result = self.new_var(ty);
        self.push_stack(result);

        let bits = get_int_bits(ty);

        if right {
            self.stmts.push(format!(
                "{result} = ({lhs} >> {rhs}) | ({lhs} << ({bits} - {rhs}))"
            ));
        } else {
            self.stmts.push(format!(
                "{result} = ({lhs} << {rhs}) | ({lhs} >> ({bits} - {rhs}));"
            ));
        }
        Ok(())
    }

    fn visit_clz(&mut self, ty: ValType) -> <Self as VisitOperator>::Output {
        let opnd = self.pop_stack();
        let result = self.new_var(ty);
        self.push_stack(result);

        let bits = get_int_bits(ty);

        self.stmts
            .push(format!("if ({opnd} == 0) {result} = {bits};"));
        let cs_ty = get_cs_ty(ty);
        // 2進で文字列化して文字数を数える
        self.stmts.push(format!(
            "else {result} = ({cs_ty}){bits} - Convert.ToString({opnd}, 2).Length;",
        ));
        Ok(())
    }

    fn visit_ctz(&mut self, ty: ValType) -> <Self as VisitOperator>::Output {
        let opnd = self.pop_stack();
        let result: Var = self.new_var(ty);
        self.push_stack(result);

        let bits = get_int_bits(ty);

        self.stmts
            .push(format!("if ({opnd} == 0) {result} = {bits};"));
        let cs_ty = get_cs_ty(ty);
        // 1. 文字数を揃えるため、MSBだけが1の数とopndのORをとる
        // 2. 2進で文字列化する
        // 3. 最後に1が出現するインデックスを求める
        self.stmts.push(format!(
            "else {result} = {} - ({cs_ty})Convert.ToString({opnd} | {}, 2).LastIndexOf('1');",
            bits - 1,
            1u64 << (bits - 1)
        ));
        Ok(())
    }

    fn visit_popcnt(&mut self, ty: ValType) -> <Self as VisitOperator>::Output {
        let opnd = self.pop_stack();
        let result: Var = self.new_var(ty);
        self.push_stack(result);

        let cs_ty = get_cs_ty(ty);
        // 2進で文字列化して、0を除去した後の文字数を数える
        self.stmts.push(format!(
            "{result} = ({cs_ty})Convert.ToString({opnd}, 2).Replace(\"0\", \"\").Length;"
        ));
        Ok(())
    }

    fn get_result(&mut self, ty: FuncType, stmt: &mut String) {
        match ty.results().len() {
            0 => {}
            1 => {
                let var = self.new_var(ty.results()[0]);
                *stmt += &format!("{var} = ");
                self.push_stack(var);
            }
            _ => {
                panic!("Multiple return values are not supported")
            }
        }
    }
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

fn get_int_bits(ty: ValType) -> i32 {
    match ty {
        ValType::I32 => 32,
        ValType::I64 => 64,
        _ => unreachable!(),
    }
}

impl<'a, 'input, 'conv> VisitOperator<'a> for CodeConverter<'input, 'conv> {
    type Output = Result<()>;

    for_each_operator!(define_visit_operator);

    fn visit_unreachable(&mut self) -> Self::Output {
        if self.conv.udon {
            self.stmts
                .push(r#"Debug.Error("unreachable");"#.to_string());
        }
        Ok(())
    }

    fn visit_nop(&mut self) -> Self::Output {
        self.stmts.push("// nop".to_string());
        Ok(())
    }

    fn visit_block(&mut self, blockty: BlockType) -> Self::Output {
        self.new_block(blockty, true);
        self.stmts.push("do {".to_string());
        Ok(())
    }

    fn visit_loop(&mut self, blockty: BlockType) -> Self::Output {
        self.new_block(blockty, false);
        self.stmts.push("while (true) {".to_string());
        Ok(())
    }

    fn visit_if(&mut self, blockty: BlockType) -> Self::Output {
        let var = self.pop_stack();
        self.new_block(blockty, true);
        self.stmts.push(format!("do if ({var} != 0) {{"));
        Ok(())
    }

    fn visit_else(&mut self) -> Self::Output {
        if self.unreachable {
            self.unreachable = false;
        } else {
            self.block_result(0);
        }

        self.blocks.last_mut().unwrap().stack.clear();

        self.stmts.push("} else {".to_string());
        Ok(())
    }

    fn visit_end(&mut self) -> Self::Output {
        if self.unreachable {
            self.unreachable = false;
        } else {
            self.block_result(0);
        }

        let block = self.blocks.pop().unwrap();

        if block.do_while {
            self.stmts.push("} while (false);".to_string());
        } else {
            self.stmts.push("}".to_string());
        }

        // 最も外側のブロックのendでない場合のみ
        if !self.blocks.is_empty() {
            if let Some(result) = block.result {
                self.push_stack(result);
            }

            // 多重break
            self.stmts.push(format!(
                "if ({BREAK_DEPTH} > 0) {{ {BREAK_DEPTH}--; break; }}"
            ));
        }

        Ok(())
    }

    fn visit_br(&mut self, relative_depth: u32) -> Self::Output {
        self.block_result(relative_depth);
        self.set_break_depth(relative_depth);
        self.unreachable = true;

        self.stmts.push("break;".to_string());
        Ok(())
    }

    fn visit_br_if(&mut self, relative_depth: u32) -> Self::Output {
        let var = self.pop_stack();
        self.stmts.push(format!("if ({var} != 0) {{"));

        self.block_result(relative_depth);
        self.set_break_depth(relative_depth);

        self.stmts.push("break;".to_string());

        self.stmts.push("}".to_string());
        Ok(())
    }

    fn visit_br_table(&mut self, targets: BrTable<'a>) -> Self::Output {
        let var = self.pop_stack();

        self.stmts.push(format!("switch ({var}) {{"));

        for target in targets.targets() {
            let target = target?;

            self.stmts.push(format!("case {var}:"));
            self.block_result(target);
            self.set_break_depth(target);
            self.stmts.push("break;".to_string());
        }

        {
            self.stmts.push("default:".to_string());
            self.block_result(targets.default());
            self.set_break_depth(targets.default());
            self.stmts.push("break;".to_string());
        }

        self.stmts.push("}".to_string());

        Ok(())
    }

    fn visit_return(&mut self) -> Self::Output {
        match self.func().ty.results().len() {
            0 => self.stmts.push("return;".to_string()),
            1 => {
                let var = self.last_stack();
                self.stmts.push(format!("return {var};"))
            }
            _ => {
                panic!("Multiple return values are not supported")
            }
        }
        Ok(())
    }

    fn visit_call(&mut self, function_index: u32) -> Self::Output {
        let mut call = String::new();

        let func = &self.conv.funcs[function_index as usize];

        let params: Vec<Var> = func.ty.params().iter().map(|_| self.pop_stack()).collect();

        self.get_result(func.ty.clone(), &mut call);
        call += &format!("{}(", func.name);

        for (i, arg) in params.iter().rev().enumerate() {
            if i != 0 {
                call += ", ";
            }
            call += &format!("{arg}");
        }

        call += ");";

        self.stmts.push(call);
        Ok(())
    }

    fn visit_call_indirect(
        &mut self,
        type_index: u32,
        table_index: u32,
        table_byte: u8,
    ) -> Self::Output {
        assert!(table_index == 0);
        assert!(table_byte == 0);

        let mut call = String::new();

        let ty = self.conv.types[type_index as usize].clone();

        let params: Vec<Var> = (0..ty.params().len() + 1)
            .map(|_| self.pop_stack())
            .collect();

        self.get_result(ty, &mut call);
        call += &format!("{CALL_INDIRECT}{}(", type_index);

        for (i, arg) in params.iter().rev().enumerate() {
            if i != 0 {
                call += ", ";
            }
            call += &format!("{arg}");
        }

        call += ");";

        self.stmts.push(call);
        Ok(())
    }

    fn visit_drop(&mut self) -> Self::Output {
        self.pop_stack();
        Ok(())
    }

    fn visit_select(&mut self) -> Self::Output {
        let c = self.pop_stack();
        let val2 = self.pop_stack();
        let val1 = self.pop_stack();

        let select = self.new_var(val1.ty);
        self.push_stack(select);

        self.stmts
            .push(format!("{select} = {c} != 0 ? {val1} : {val2};"));

        Ok(())
    }

    fn visit_local_get(&mut self, local_index: u32) -> Self::Output {
        let local = self.vars[local_index as usize];
        let var = self.new_var(local.ty);
        self.push_stack(var);

        self.stmts.push(format!("{var} = {local};"));
        Ok(())
    }

    fn visit_local_set(&mut self, local_index: u32) -> Self::Output {
        let local = self.vars[local_index as usize];
        let var = self.pop_stack();
        self.stmts.push(format!("{local} = {var};"));
        Ok(())
    }

    fn visit_local_tee(&mut self, local_index: u32) -> Self::Output {
        let local = self.vars[local_index as usize];
        self.stmts.push(format!("{local} = {};", self.last_stack()));
        Ok(())
    }

    fn visit_global_get(&mut self, global_index: u32) -> Self::Output {
        let global = self.conv.globals[global_index as usize];
        let var = self.new_var(global.content_type);
        self.push_stack(var);

        // TODO: エクスポートするグローバル変数名の対応
        self.stmts.push(format!("{var} = global{global_index};"));
        Ok(())
    }

    fn visit_global_set(&mut self, global_index: u32) -> Self::Output {
        // TODO: エクスポートするグローバル変数名の対応
        let var = self.pop_stack();
        self.stmts.push(format!("global{global_index} = {var};"));
        Ok(())
    }

    fn visit_i32_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, ValType::I32, StorageType::Val(ValType::I32), false)
    }

    fn visit_i64_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, ValType::I64, StorageType::Val(ValType::I64), false)
    }

    fn visit_f32_load(&mut self, memarg: MemArg) -> Self::Output {
        todo!()
    }

    fn visit_f64_load(&mut self, memarg: MemArg) -> Self::Output {
        todo!()
    }

    fn visit_i32_load8_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, ValType::I32, StorageType::I8, true)
    }

    fn visit_i32_load8_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, ValType::I32, StorageType::I8, false)
    }

    fn visit_i32_load16_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, ValType::I32, StorageType::I16, true)
    }

    fn visit_i32_load16_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, ValType::I32, StorageType::I16, false)
    }

    fn visit_i64_load8_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, ValType::I64, StorageType::I8, true)
    }

    fn visit_i64_load8_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, ValType::I64, StorageType::I8, false)
    }

    fn visit_i64_load16_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, ValType::I64, StorageType::I16, true)
    }

    fn visit_i64_load16_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, ValType::I64, StorageType::I16, false)
    }

    fn visit_i64_load32_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, ValType::I64, StorageType::Val(ValType::I32), true)
    }

    fn visit_i64_load32_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, ValType::I64, StorageType::Val(ValType::I32), false)
    }

    fn visit_i32_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, StorageType::Val(ValType::I32))
    }

    fn visit_i64_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, StorageType::Val(ValType::I64))
    }

    fn visit_f32_store(&mut self, memarg: MemArg) -> Self::Output {
        todo!()
    }

    fn visit_f64_store(&mut self, memarg: MemArg) -> Self::Output {
        todo!()
    }

    fn visit_i32_store8(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, StorageType::I8)
    }

    fn visit_i32_store16(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, StorageType::I16)
    }

    fn visit_i64_store8(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, StorageType::I8)
    }

    fn visit_i64_store16(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, StorageType::I16)
    }

    fn visit_i64_store32(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, StorageType::Val(ValType::I32))
    }

    fn visit_memory_size(&mut self, mem: u32, mem_byte: u8) -> Self::Output {
        if mem != 0 {
            panic!("Multi memory is not supported")
        }
        assert!(mem_byte == 0);

        let var = self.new_var(ValType::I32);
        self.push_stack(var);

        self.stmts.push(format!(
            "{var} = ({})({MEMORY}.Length / {PAGE_SIZE});",
            get_cs_ty(var.ty)
        ));
        Ok(())
    }

    fn visit_memory_grow(&mut self, mem: u32, mem_byte: u8) -> Self::Output {
        if mem != 0 {
            panic!("Multi memory is not supported")
        }
        assert!(mem_byte == 0);

        let size = self.pop_stack();
        let var = self.new_var(ValType::I32);
        self.push_stack(var);

        // 前のサイズを返す
        self.stmts
            .push(format!("{var} = {MEMORY}.Length / {PAGE_SIZE};"));

        // メモリをsizeだけ拡張
        self.stmts.push("{".to_string());
        self.stmts.push(format!("var old = {MEMORY};"));
        self.stmts.push(format!(
            "{MEMORY} = new byte[old.Length + {size} * {PAGE_SIZE}];"
        ));
        self.stmts
            .push(format!("Array.Copy(old, {MEMORY}, old.Length);"));
        self.stmts.push("}".to_string());
        Ok(())
    }

    fn visit_i32_const(&mut self, value: i32) -> Self::Output {
        self.visit_const(ValType::I32, value)
    }

    fn visit_i64_const(&mut self, value: i64) -> Self::Output {
        self.visit_const(ValType::I64, value)
    }

    fn visit_f32_const(&mut self, value: Ieee32) -> Self::Output {
        self.visit_const(ValType::F32, f32::from_bits(value.bits()))
    }

    fn visit_f64_const(&mut self, value: Ieee64) -> Self::Output {
        self.visit_const(ValType::F64, f64::from_bits(value.bits()))
    }

    fn visit_i32_eqz(&mut self) -> Self::Output {
        self.visit_eqz()
    }

    fn visit_i32_eq(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "==", true, false)
    }

    fn visit_i32_ne(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "!=", true, false)
    }

    fn visit_i32_lt_s(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "<", true, true)
    }

    fn visit_i32_lt_u(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "<", true, false)
    }

    fn visit_i32_gt_s(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, ">", true, true)
    }

    fn visit_i32_gt_u(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, ">", true, false)
    }

    fn visit_i32_le_s(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "<=", true, true)
    }

    fn visit_i32_le_u(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "<=", true, false)
    }

    fn visit_i32_ge_s(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, ">=", true, true)
    }

    fn visit_i32_ge_u(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, ">=", true, false)
    }

    fn visit_i64_eqz(&mut self) -> Self::Output {
        self.visit_eqz()
    }

    fn visit_i64_eq(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "==", true, false)
    }

    fn visit_i64_ne(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "!=", true, false)
    }

    fn visit_i64_lt_s(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "<", true, true)
    }

    fn visit_i64_lt_u(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "<", true, false)
    }

    fn visit_i64_gt_s(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, ">", true, true)
    }

    fn visit_i64_gt_u(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, ">", true, false)
    }

    fn visit_i64_le_s(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "<=", true, true)
    }

    fn visit_i64_le_u(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "<=", true, false)
    }

    fn visit_i64_ge_s(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, ">=", true, true)
    }

    fn visit_i64_ge_u(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, ">=", true, false)
    }

    fn visit_f32_eq(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F32, "==", true, false)
    }

    fn visit_f32_ne(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F32, "!=", true, false)
    }

    fn visit_f32_lt(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F32, "<", true, false)
    }

    fn visit_f32_gt(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F32, ">", true, false)
    }

    fn visit_f32_le(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F32, "<=", true, false)
    }

    fn visit_f32_ge(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F32, ">=", true, false)
    }

    fn visit_f64_eq(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F64, "==", true, false)
    }

    fn visit_f64_ne(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F64, "!=", true, false)
    }

    fn visit_f64_lt(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F64, "<", true, false)
    }

    fn visit_f64_gt(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F64, ">", true, false)
    }

    fn visit_f64_le(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F64, "<=", true, false)
    }

    fn visit_f64_ge(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F64, ">=", true, false)
    }

    fn visit_i32_clz(&mut self) -> Self::Output {
        self.visit_clz(ValType::I32)
    }

    fn visit_i32_ctz(&mut self) -> Self::Output {
        self.visit_ctz(ValType::I32)
    }

    fn visit_i32_popcnt(&mut self) -> Self::Output {
        self.visit_popcnt(ValType::I32)
    }

    fn visit_i32_add(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "+", false, false)
    }

    fn visit_i32_sub(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "-", false, false)
    }

    fn visit_i32_mul(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "*", false, false)
    }

    fn visit_i32_div_s(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "/", false, true)
    }

    fn visit_i32_div_u(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "/", false, false)
    }

    fn visit_i32_rem_s(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "%", false, true)
    }

    fn visit_i32_rem_u(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "%", false, false)
    }

    fn visit_i32_and(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "&", false, false)
    }

    fn visit_i32_or(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "|", false, false)
    }

    fn visit_i32_xor(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "^", false, false)
    }

    fn visit_i32_shl(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, "<<", false, false)
    }

    fn visit_i32_shr_s(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, ">>", false, true)
    }

    fn visit_i32_shr_u(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I32, ">>", false, false)
    }

    fn visit_i32_rotl(&mut self) -> Self::Output {
        self.visit_rot_op(ValType::I32, false)
    }

    fn visit_i32_rotr(&mut self) -> Self::Output {
        self.visit_rot_op(ValType::I32, true)
    }

    fn visit_i64_clz(&mut self) -> Self::Output {
        self.visit_clz(ValType::I64)
    }

    fn visit_i64_ctz(&mut self) -> Self::Output {
        self.visit_ctz(ValType::I64)
    }

    fn visit_i64_popcnt(&mut self) -> Self::Output {
        self.visit_popcnt(ValType::I64)
    }

    fn visit_i64_add(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "+", false, false)
    }

    fn visit_i64_sub(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "-", false, false)
    }

    fn visit_i64_mul(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "*", false, false)
    }

    fn visit_i64_div_s(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "/", false, true)
    }

    fn visit_i64_div_u(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "/", false, false)
    }

    fn visit_i64_rem_s(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "%", false, true)
    }

    fn visit_i64_rem_u(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "%", false, false)
    }

    fn visit_i64_and(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "&", false, false)
    }

    fn visit_i64_or(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "|", false, false)
    }

    fn visit_i64_xor(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "^", false, false)
    }

    fn visit_i64_shl(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, "<<", false, false)
    }

    fn visit_i64_shr_s(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, ">>", false, true)
    }

    fn visit_i64_shr_u(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::I64, ">>", false, false)
    }

    fn visit_i64_rotl(&mut self) -> Self::Output {
        self.visit_rot_op(ValType::I64, false)
    }

    fn visit_i64_rotr(&mut self) -> Self::Output {
        self.visit_rot_op(ValType::I64, true)
    }

    fn visit_f32_abs(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_neg(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_ceil(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_floor(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_trunc(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_nearest(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_sqrt(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_add(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_sub(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_mul(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_div(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_min(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_max(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_copysign(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_abs(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_neg(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_ceil(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_floor(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_trunc(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_nearest(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_sqrt(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_add(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_sub(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_mul(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_div(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_min(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_max(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_copysign(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_i32_wrap_i64(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_i32_trunc_f32_s(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_i32_trunc_f32_u(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_i32_trunc_f64_s(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_i32_trunc_f64_u(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_i64_extend_i32_s(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_i64_extend_i32_u(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_i64_trunc_f32_s(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_i64_trunc_f32_u(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_i64_trunc_f64_s(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_i64_trunc_f64_u(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_convert_i32_s(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_convert_i32_u(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_convert_i64_s(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_convert_i64_u(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_demote_f64(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_convert_i32_s(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_convert_i32_u(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_convert_i64_s(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_convert_i64_u(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_promote_f32(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_i32_reinterpret_f32(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_i64_reinterpret_f64(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f32_reinterpret_i32(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_f64_reinterpret_i64(&mut self) -> Self::Output {
        todo!()
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

#[derive(Debug, thiserror::Error)]
pub enum OperatorError {
    #[error("`{0}` proposal is not implemented")]
    NotSupported(&'static str),
}
