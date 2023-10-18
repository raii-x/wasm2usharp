use std::fmt;

use std::io::Write;

use anyhow::Result;
use num_traits::float::Float;
use wasmparser::{
    for_each_operator, BlockType, BrTable, FuncType, FunctionBody, Ieee32, Ieee64, MemArg,
    StorageType, ValType, VisitOperator,
};

use crate::util::bit_mask;

use super::{func_header, get_cs_ty, result_cs_ty, Converter, Func, CALL_INDIRECT, PAGE_SIZE};

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

pub struct CodeConverter<'input, 'conv> {
    conv: &'conv Converter<'input>,
    code_idx: usize,
    blocks: Vec<Block>,
    vars: Vec<Var>,
    local_count: u32,
    stmts: Vec<String>,
    /// brの後など、到達不可能なコードの処理時に1加算。
    /// unreachableが1以上の場合のブロックの出現ごとに1加算。
    /// ブロックの終了時に1減算。
    unreachable: i32,
    loop_var_count: usize,
}

#[derive(Debug)]
struct Block {
    stack: Vec<Var>,
    result: Option<Var>,
    loop_var: Option<usize>,
}

#[derive(Debug, Clone, Copy)]
struct Var {
    index: usize,
    ty: ValType,
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "var{}", self.index)
    }
}

const BREAK_DEPTH: &str = "w2us_break_depth";
const LOOP: &str = "w2us_loop";

impl<'input, 'conv> CodeConverter<'input, 'conv> {
    pub fn new(conv: &'conv Converter<'input>, code_idx: usize) -> Self {
        Self {
            conv,
            code_idx,
            blocks: Vec::new(),
            vars: Vec::new(),
            local_count: 0,
            stmts: Vec::new(),
            unreachable: 0,
            loop_var_count: 0,
        }
    }

    fn func(&self) -> &'conv Func {
        &self.conv.funcs[self.code_idx]
    }

    pub fn convert(&mut self, body: FunctionBody<'_>, out_file: &mut impl Write) -> Result<()> {
        for &ty in self.func().ty.params() {
            let _ = self.new_var(ty);
            self.local_count += 1;
        }

        for local in body.get_locals_reader()? {
            let (count, ty) = local?;
            for _ in 0..count {
                self.new_var(ty);
                self.local_count += 1;
            }
        }

        let results = self.func().ty.results();

        // 関数の最上位のブロック
        self.visit_block(match results.len() {
            0 => BlockType::Empty,
            1 => BlockType::Type(results[0]),
            _ => panic!("multi value is not supported"),
        })?;
        let result_var = self.blocks[0].result;

        for op in body.get_operators_reader()? {
            let op = op?;

            if self.unreachable > 0 {
                use wasmparser::Operator::*;
                match &op {
                    Block { .. } | Loop { .. } | If { .. } | Else | End => {}
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

        if self.func().export {
            write!(out_file, "public ")?;
        }

        let params: Vec<(&str, &Var)> = func_ty
            .params()
            .iter()
            .map(|&ty| get_cs_ty(ty))
            .zip(self.vars.iter())
            .collect();
        write!(
            out_file,
            "{}",
            func_header(&self.func().name, result_cs_ty(func_ty.results()), &params)
        )?;

        writeln!(out_file, " {{")?;

        writeln!(out_file, "int {BREAK_DEPTH} = 0;")?;

        // ループ変数
        for i in 0..self.loop_var_count {
            writeln!(out_file, "bool {LOOP}{i};")?;
        }

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
            index: self.vars.len(),
            ty,
        };
        self.vars.push(var);
        var
    }

    fn new_block(&mut self, blockty: BlockType, is_loop: bool) -> &Block {
        let result = match blockty {
            BlockType::Empty => None,
            BlockType::Type(ty) => Some(self.new_var(ty)),
            BlockType::FuncType(..) => panic!("func type blocks are not supported"),
        };

        let loop_var = if is_loop {
            Some({
                self.loop_var_count += 1;
                self.loop_var_count - 1
            })
        } else {
            None
        };

        self.blocks.push(Block {
            stack: Vec::new(),
            result,
            loop_var,
        });
        self.blocks.last().unwrap()
    }

    /// ブロックに戻り値があれば、stmtsに戻り値を代入する処理を追加する
    fn block_result(&mut self, relative_depth: u32, is_br: bool) {
        let upper_block = &self.blocks[self.blocks.len() - 1 - relative_depth as usize];

        // brでループを再開する場合は戻り値を取らない
        if is_br && upper_block.loop_var.is_some() {
            return;
        }

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

    fn visit_int_load(
        &mut self,
        memarg: MemArg,
        var_type: ValType,
        mem_type: StorageType,
        signed: bool,
    ) -> <Self as VisitOperator<'_>>::Output {
        let idx = self.pop_stack();
        let var = self.new_var(var_type);
        self.push_stack(var);

        self.int_load(memarg, var_type, mem_type, signed, idx, var);

        Ok(())
    }

    fn int_load(
        &mut self,
        memarg: MemArg,
        var_type: ValType,
        mem_type: StorageType,
        signed: bool,
        idx: Var,
        var: Var,
    ) {
        use {StorageType::*, ValType::*};

        let mut load = format!("{var} = ");

        let memory = &self.conv.memory.as_ref().unwrap().name;
        match mem_type {
            I8 => load += &format!("{memory}[{0}+{1}];", idx, memarg.offset),
            I16 => load += &format!("{memory}[{0}+{1}] | ({2}){memory}[{0}+{1}+1]<<8;", idx, memarg.offset, get_cs_ty(var_type)),
            Val(I32) => load += &format!(
                "{memory}[{0}+{1}] | ({2}){memory}[{0}+{1}+1]<<8 | ({2}){memory}[{0}+{1}+2]<<16 | ({2}){memory}[{0}+{1}+3]<<24;",
                idx, memarg.offset, get_cs_ty(var_type)),
            Val(I64) => load += &format!(
                "{memory}[{0}+{1}] | ({2}){memory}[{0}+{1}+1]<<8 | ({2}){memory}[{0}+{1}+2]<<16 | ({2}){memory}[{0}+{1}+3]<<24 | ({2}){memory}[{0}+{1}+4]<<32 | ({2}){memory}[{0}+{1}+5]<<40 | ({2}){memory}[{0}+{1}+6]<<48 | ({2}){memory}[{0}+{1}+7]<<56;",
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
    }

    fn visit_float_load(
        &mut self,
        memarg: MemArg,
        var_type: ValType,
    ) -> <Self as VisitOperator<'_>>::Output {
        let idx = self.pop_stack();
        let i_ty = ty_f_to_i(var_type);
        let i_var = self.new_var(i_ty);

        let var = self.new_var(var_type);
        self.push_stack(var);

        self.int_load(memarg, i_ty, StorageType::Val(i_ty), false, idx, i_var);

        self.bits_to_float(i_var, var);
        Ok(())
    }

    fn bits_to_float(&mut self, i_var: Var, f_var: Var) {
        let i_ty = i_var.ty;
        let f_ty = f_var.ty;
        let bits = get_int_bits(i_ty);
        let frac_bits = get_frac_bits(f_ty);
        let expo_bits = bits - 1 - frac_bits;
        let expo_bit_mask = bit_mask(expo_bits as u64);
        // LSBに寄せた後のビットマスク
        let expo_offset = (1 << (expo_bits - 1)) - 1;
        let cs_ty = get_cs_ty(f_ty);
        let class = self.math_class(f_ty);

        self.stmts.push("{".to_string());

        // ビット列の各データを抽出
        self.stmts
            .push(format!("var sign = {i_var} >> {};", bits - 1));
        self.stmts.push(format!(
            "var expo = ({i_var} >> {frac_bits}) & {expo_bit_mask};",
        ));
        self.stmts.push(format!(
            "var frac = {i_var} & {};",
            bit_mask(frac_bits as u64)
        ));

        let sign = format!("(1 - ({cs_ty})sign * 2)");

        self.stmts.push("if (expo == 0) {".to_string());
        {
            self.stmts.push("if (frac == 0) {".to_string());
            // 0の場合
            self.stmts.push(format!("{f_var} = sign == 0 ? 0f : -0f;"));
            self.stmts.push("} else {".to_string());
            // 非正規化数の場合
            self.stmts.push(format!(
                "{f_var} = ({cs_ty})frac * {cs_ty}.Epsilon * {sign};"
            ));
            self.stmts.push("}".to_string());
        }
        self.stmts
            .push(format!("}} else if (expo == {expo_bit_mask}) {{"));
        {
            self.stmts.push("if (frac == 0) {".to_string());
            // 無限大の場合
            self.stmts.push(format!(
                "{f_var} = sign == 0 ? {cs_ty}.PositiveInfinity : {cs_ty}.NegativeInfinity;"
            ));
            self.stmts.push("} else {".to_string());
            // NaNの場合
            self.stmts.push(format!("{f_var} = {cs_ty}.NaN;"));
            self.stmts.push("}".to_string());
        }
        self.stmts.push("} else {".to_string());
        {
            // 浮動小数点数の変数に代入
            let expo = format!("{class}.Pow(2, (int)expo - {})", expo_offset);
            let frac = format!("(({cs_ty})frac / {} + 1)", 1u64 << frac_bits);
            self.stmts
                .push(format!("{f_var} = {frac} * {expo} * {sign};"));
        }
        self.stmts.push("}".to_string());

        self.stmts.push("}".to_string());
    }

    fn visit_int_store(
        &mut self,
        memarg: MemArg,
        mem_type: StorageType,
    ) -> <Self as VisitOperator<'_>>::Output {
        let var = self.pop_stack();
        let idx = self.pop_stack();

        self.int_store(memarg, mem_type, idx, var);

        Ok(())
    }

    fn int_store(&mut self, memarg: MemArg, mem_type: StorageType, idx: Var, var: Var) {
        use {StorageType::*, ValType::*};

        let memory = &self.conv.memory.as_ref().unwrap().name;
        self.stmts.push(match mem_type {
            I8 => format!("{memory}[{0}+{1}]=(byte)({2}&0xff);", idx, memarg.offset, var),
            I16 => {
                format!(
                    "{memory}[{0}+{1}]=(byte)({2}&0xff); {memory}[{0}+{1}+1]=(byte)(({2}>>8)&0xff);",
                    idx, memarg.offset, var
                )
            }
            Val(I32) => {
                format!(
                    "{memory}[{0}+{1}]=(byte)({2}&0xff); {memory}[{0}+{1}+1]=(byte)(({2}>>8)&0xff); {memory}[{0}+{1}+2]=(byte)(({2}>>16)&0xff); {memory}[{0}+{1}+3]=(byte)(({2}>>24)&0xff);",
                    idx, memarg.offset, var
                )
            }
            Val(I64) => {
                format!(
                    "{memory}[{0}+{1}]=(byte)({2}&0xff); {memory}[{0}+{1}+1]=(byte)(({2}>>8)&0xff); {memory}[{0}+{1}+2]=(byte)(({2}>>16)&0xff); {memory}[{0}+{1}+3]=(byte)(({2}>>24)&0xff); {memory}[{0}+{1}+4]=(byte)(({2}>>32)&0xff); {memory}[{0}+{1}+5]=(byte)(({2}>>40)&0xff); {memory}[{0}+{1}+6]=(byte)(({2}>>48)&0xff); {memory}[{0}+{1}+7]=(byte)(({2}>>56)&0xff);",
                    idx, memarg.offset, var
                )
            }
            _ => unreachable!(),
        });
    }

    fn visit_float_store(
        &mut self,
        memarg: MemArg,
        mem_type: ValType,
    ) -> <Self as VisitOperator<'_>>::Output {
        let var = self.pop_stack();
        let idx = self.pop_stack();
        let i_ty = ty_f_to_i(mem_type);
        let i_var = self.new_var(i_ty);

        self.float_to_bits(var, i_var);

        self.int_store(memarg, StorageType::Val(i_ty), idx, i_var);

        Ok(())
    }

    fn float_to_bits(&mut self, f_var: Var, i_var: Var) {
        let f_ty = f_var.ty;
        let i_ty = i_var.ty;
        let bits = get_int_bits(i_ty);
        let frac_bits = get_frac_bits(f_ty);
        let expo_bits = bits - 1 - frac_bits;
        let expo_bit_mask = bit_mask(expo_bits as u64);
        // LSBに寄せた後のビットマスク
        let expo_offset = (1 << (expo_bits - 1)) - 1;
        let f_cs_ty = get_cs_ty(f_ty);
        let i_cs_ty = get_cs_ty(i_ty);
        let class = self.math_class(f_ty);

        self.stmts.push("{".to_string());
        self.stmts.push(format!("{i_cs_ty} sign;"));
        self.stmts.push(format!("{i_cs_ty} expo;"));
        self.stmts.push(format!("{i_cs_ty} frac;"));

        self.stmts.push(format!("if ({f_var} == 0) {{"));
        {
            // 0の場合
            // 1/0を計算することで+0と-0を区別
            self.stmts
                .push(format!("sign = 1 / {f_var} > 0 ? 0u : 1u;"));
            self.stmts.push("expo = 0;".to_string());
            self.stmts.push("frac = 0;".to_string());
        }
        self.stmts
            .push(format!("}} else if ({f_cs_ty}.IsInfinity({f_var})) {{"));
        {
            // 無限大の場合
            self.stmts.push(format!("sign = {f_var} > 0 ? 0u : 1u;"));
            self.stmts.push(format!("expo = {expo_bit_mask};"));
            self.stmts.push("frac = 0;".to_string());
        }
        self.stmts
            .push(format!("}} else if ({f_cs_ty}.IsNaN({f_var})) {{"));
        {
            // NaNの場合
            self.stmts.push("sign = 1;".to_string());
            self.stmts.push(format!("expo = {expo_bit_mask};"));
            // MSBだけが1
            self.stmts
                .push(format!("frac = {};", 1u64 << (frac_bits - 1)));
        }
        self.stmts.push("} else {".to_string());
        {
            self.stmts.push(format!("sign = {f_var} > 0 ? 0u : 1u;"));
            self.stmts
                .push(format!("var absVar = {class}.Abs({f_var});"));
            self.stmts
                .push(format!("var expoF = {class}.Floor({class}.Log2(absVar));"));
            self.stmts
                .push(format!("if (expoF <= {}) {{", -expo_offset));
            {
                // 非正規化数の場合
                self.stmts.push("expo = 0;".to_string());
                self.stmts
                    .push(format!("frac = ({i_cs_ty})(absVar / {f_cs_ty}.Epsilon);"));
            }
            self.stmts
                .push(format!("}} else if (expoF >= {}) {{", expo_offset + 1));
            {
                // Log2の誤差で無限大になった場合
                self.stmts.push(format!("expo = {expo_bit_mask};"));
                self.stmts.push("frac = 0;".to_string());
            }
            self.stmts.push("} else {".to_string());
            {
                // 通常の浮動小数点数の場合
                self.stmts
                    .push(format!("expo = ({i_cs_ty})expoF + {expo_offset};"));
                self.stmts.push(format!(
                    "frac = ({i_cs_ty})((absVar / {class}.Pow(2, expoF) - 1) * {});",
                    1u64 << frac_bits
                ));
            }
            self.stmts.push("}".to_string());
        }
        self.stmts.push("}".to_string());

        self.stmts.push(format!(
            "{i_var} = (sign << {}) | (expo << {}) | frac;",
            bits - 1,
            frac_bits
        ));

        self.stmts.push("}".to_string());
    }

    fn visit_const(
        &mut self,
        ty: ValType,
        value: impl fmt::Display,
    ) -> <Self as VisitOperator<'_>>::Output {
        let var = self.new_var(ty);
        self.push_stack(var);

        self.stmts.push(format!("{var} = {value};"));
        Ok(())
    }

    fn visit_float_const<T, F>(
        &mut self,
        value: T,
        ty: ValType,
        normal_display: F,
    ) -> std::result::Result<(), anyhow::Error>
    where
        T: Float,
        F: FnOnce() -> String,
    {
        let cs_ty = get_cs_ty(ty);

        let literal = if value.is_infinite() {
            if value.is_sign_positive() {
                format!("{cs_ty}.PositiveInfinity")
            } else {
                format!("{cs_ty}.NegativeInfinity")
            }
        } else if value.is_nan() {
            format!("{cs_ty}.NaN")
        } else {
            normal_display()
        };

        self.visit_const(ty, literal)
    }

    fn visit_eqz(&mut self) -> <Self as VisitOperator<'_>>::Output {
        let opnd = self.pop_stack();
        let result = self.new_var(ValType::I32);
        self.push_stack(result);

        self.stmts
            .push(format!("{result} = {opnd} == 0 ? 1u : 0u;"));
        Ok(())
    }

    fn visit_un_op(&mut self, ty: ValType, op: &str) -> <Self as VisitOperator<'_>>::Output {
        let opnd = self.pop_stack();
        let result = self.new_var(ty);
        self.push_stack(result);

        self.stmts.push(format!("{result} = {op}{opnd};"));
        Ok(())
    }

    fn visit_bin_op(
        &mut self,
        ty: ValType,
        op: &str,
        logical: bool,
        signed: bool,
    ) -> <Self as VisitOperator<'_>>::Output {
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
            stmt += " ? 1u : 0u";
        }

        stmt += ";";

        self.stmts.push(stmt);
        Ok(())
    }

    fn visit_shift_op(
        &mut self,
        ty: ValType,
        op: &str,
        signed: bool,
    ) -> <Self as VisitOperator<'_>>::Output {
        let rhs = self.pop_stack();
        let lhs = self.pop_stack();
        let result = self.new_var(ty);
        self.push_stack(result);

        let mut stmt = format!("{result} = ");

        let cs_ty = get_cs_ty(ty);

        if signed {
            // 先頭の`u`を取り除く
            let cs_ty_s = &cs_ty[1..];
            stmt += &format!("({cs_ty})(({cs_ty_s}){lhs} {op} (int){rhs})");
        } else {
            stmt += &format!("({cs_ty})({lhs} {op} (int){rhs})");
        };

        stmt += ";";

        self.stmts.push(stmt);
        Ok(())
    }

    fn visit_rot_op(&mut self, ty: ValType, right: bool) -> <Self as VisitOperator<'_>>::Output {
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

    fn visit_clz(&mut self, ty: ValType) -> <Self as VisitOperator<'_>>::Output {
        let opnd = self.pop_stack();
        let result = self.new_var(ty);
        self.push_stack(result);

        let bits = get_int_bits(ty);

        self.stmts
            .push(format!("if ({opnd} == 0) {result} = {bits};"));
        let cs_ty = get_cs_ty(ty);
        // 2進で文字列化して文字数を数える
        self.stmts.push(format!(
            "else {result} = ({cs_ty}){bits} - Convert.ToString((long){opnd}, 2).Length;",
        ));
        Ok(())
    }

    fn visit_ctz(&mut self, ty: ValType) -> <Self as VisitOperator<'_>>::Output {
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
            "else {result} = {} - ({cs_ty})Convert.ToString((long)({opnd} | {}), 2).LastIndexOf('1');",
            bits - 1,
            1u64 << (bits - 1)
        ));
        Ok(())
    }

    fn visit_popcnt(&mut self, ty: ValType) -> <Self as VisitOperator<'_>>::Output {
        let opnd = self.pop_stack();
        let result: Var = self.new_var(ty);
        self.push_stack(result);

        let cs_ty = get_cs_ty(ty);
        // 2進で文字列化して、0を除去した後の文字数を数える
        self.stmts.push(format!(
            "{result} = ({cs_ty})Convert.ToString((long){opnd}, 2).Replace(\"0\", \"\").Length;"
        ));
        Ok(())
    }

    fn visit_math_un_op(&mut self, ty: ValType, func: &str) -> <Self as VisitOperator<'_>>::Output {
        let opnd = self.pop_stack();
        let result = self.new_var(ty);
        self.push_stack(result);

        self.stmts.push(format!(
            "{result} = {}.{func}({opnd});",
            self.math_class(ty)
        ));
        Ok(())
    }

    fn visit_math_bin_op(
        &mut self,
        ty: ValType,
        func: &str,
    ) -> <Self as VisitOperator<'_>>::Output {
        let rhs = self.pop_stack();
        let lhs = self.pop_stack();
        let result = self.new_var(ty);
        self.push_stack(result);

        self.stmts.push(format!(
            "{result} = {}.{func}({lhs}, {rhs});",
            self.math_class(ty)
        ));
        Ok(())
    }

    fn visit_cast(
        &mut self,
        result_ty: ValType,
        mid_ty: Option<&'static str>,
    ) -> <Self as VisitOperator<'_>>::Output {
        let opnd = self.pop_stack();
        let result = self.new_var(result_ty);
        self.push_stack(result);

        let result_cs_ty = get_cs_ty(result_ty);

        self.stmts.push(match mid_ty {
            Some(cast_ty) => format!("{result} = ({result_cs_ty})({cast_ty}){opnd};"),
            None => format!("{result} = ({result_cs_ty}){opnd};"),
        });
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

    fn math_class(&self, ty: ValType) -> &'static str {
        match ty {
            ValType::F32 => {
                if self.conv.test {
                    "MathF"
                } else {
                    "Mathf"
                }
            }
            ValType::F64 => "Math",
            _ => panic!("Specify float type as argument"),
        }
    }
}

impl<'a, 'input, 'conv> VisitOperator<'a> for CodeConverter<'input, 'conv> {
    type Output = Result<()>;

    for_each_operator!(define_visit_operator);

    fn visit_unreachable(&mut self) -> Self::Output {
        self.stmts.push(self.conv.trap("unreachable"));
        Ok(())
    }

    fn visit_nop(&mut self) -> Self::Output {
        self.stmts.push("// nop".to_string());
        Ok(())
    }

    fn visit_block(&mut self, blockty: BlockType) -> Self::Output {
        if self.unreachable > 0 {
            self.unreachable += 1;
            return Ok(());
        }
        self.new_block(blockty, false);
        self.stmts.push("do {".to_string());
        Ok(())
    }

    fn visit_loop(&mut self, blockty: BlockType) -> Self::Output {
        if self.unreachable > 0 {
            self.unreachable += 1;
            return Ok(());
        }
        let block = self.new_block(blockty, true);
        let loop_var = block.loop_var.unwrap();
        self.stmts.push("do {".to_string());
        self.stmts.push("do {".to_string());
        self.stmts.push(format!("{LOOP}{} = true;", loop_var));
        Ok(())
    }

    fn visit_if(&mut self, blockty: BlockType) -> Self::Output {
        if self.unreachable > 0 {
            self.unreachable += 1;
            return Ok(());
        }
        let var = self.pop_stack();
        self.new_block(blockty, false);
        self.stmts.push(format!("do if ({var} != 0) {{"));
        Ok(())
    }

    fn visit_else(&mut self) -> Self::Output {
        match self.unreachable {
            x if x == 0 => self.block_result(0, false),
            x if x == 1 => self.unreachable -= 1,
            _ => return Ok(()),
        }

        self.blocks.last_mut().unwrap().stack.clear();

        self.stmts.push("} else {".to_string());
        Ok(())
    }

    fn visit_end(&mut self) -> Self::Output {
        match self.unreachable {
            x if x == 0 => self.block_result(0, false),
            x if x == 1 => self.unreachable -= 1,
            _ => {
                self.unreachable -= 1;
                return Ok(());
            }
        }

        let block = self.blocks.pop().unwrap();

        match block.loop_var {
            Some(loop_var) => {
                self.stmts.push(format!("{LOOP}{} = false;", loop_var));
                self.stmts.push("} while (false);".to_string());
                self.stmts.push(format!("if ({BREAK_DEPTH} > 0) break;"));
                self.stmts.push(format!("}} while ({LOOP}{});", loop_var));
            }
            None => {
                self.stmts.push("} while (false);".to_string());
            }
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
        self.block_result(relative_depth, true);
        self.set_break_depth(relative_depth);
        self.unreachable = 1;

        self.stmts.push("break;".to_string());
        Ok(())
    }

    fn visit_br_if(&mut self, relative_depth: u32) -> Self::Output {
        let var = self.pop_stack();
        self.stmts.push(format!("if ({var} != 0) {{"));

        self.block_result(relative_depth, true);
        self.set_break_depth(relative_depth);

        self.stmts.push("break;".to_string());

        self.stmts.push("}".to_string());
        Ok(())
    }

    fn visit_br_table(&mut self, targets: BrTable<'a>) -> Self::Output {
        self.unreachable = 1;
        let var = self.pop_stack();

        self.stmts.push(format!("switch ({var}) {{"));

        for (i, target) in targets.targets().enumerate() {
            let target = target?;

            self.stmts.push(format!("case {i}:"));
            self.block_result(target, true);
            self.set_break_depth(target);
            self.stmts.push("break;".to_string());
        }

        self.stmts.push("default:".to_string());
        self.block_result(targets.default(), true);
        self.set_break_depth(targets.default());
        self.stmts.push("break;".to_string());

        self.stmts.push("}".to_string());
        self.stmts.push("break;".to_string());

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
        let global = &self.conv.globals[global_index as usize];
        let var = self.new_var(global.ty.content_type);
        self.push_stack(var);

        self.stmts.push(if self.conv.test {
            format!("{var} = {}[0];", global.name)
        } else {
            format!("{var} = {};", global.name)
        });
        Ok(())
    }

    fn visit_global_set(&mut self, global_index: u32) -> Self::Output {
        let global = &self.conv.globals[global_index as usize];
        let var = self.pop_stack();

        self.stmts.push(if self.conv.test {
            format!("{}[0] = {var};", global.name)
        } else {
            format!("{} = {var};", global.name)
        });
        Ok(())
    }

    fn visit_i32_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, ValType::I32, StorageType::Val(ValType::I32), false)
    }

    fn visit_i64_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, ValType::I64, StorageType::Val(ValType::I64), false)
    }

    fn visit_f32_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_float_load(memarg, ValType::F32)
    }

    fn visit_f64_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_float_load(memarg, ValType::F64)
    }

    fn visit_i32_load8_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, ValType::I32, StorageType::I8, true)
    }

    fn visit_i32_load8_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, ValType::I32, StorageType::I8, false)
    }

    fn visit_i32_load16_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, ValType::I32, StorageType::I16, true)
    }

    fn visit_i32_load16_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, ValType::I32, StorageType::I16, false)
    }

    fn visit_i64_load8_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, ValType::I64, StorageType::I8, true)
    }

    fn visit_i64_load8_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, ValType::I64, StorageType::I8, false)
    }

    fn visit_i64_load16_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, ValType::I64, StorageType::I16, true)
    }

    fn visit_i64_load16_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, ValType::I64, StorageType::I16, false)
    }

    fn visit_i64_load32_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, ValType::I64, StorageType::Val(ValType::I32), true)
    }

    fn visit_i64_load32_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, ValType::I64, StorageType::Val(ValType::I32), false)
    }

    fn visit_i32_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_store(memarg, StorageType::Val(ValType::I32))
    }

    fn visit_i64_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_store(memarg, StorageType::Val(ValType::I64))
    }

    fn visit_f32_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_float_store(memarg, ValType::F32)
    }

    fn visit_f64_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_float_store(memarg, ValType::F64)
    }

    fn visit_i32_store8(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_store(memarg, StorageType::I8)
    }

    fn visit_i32_store16(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_store(memarg, StorageType::I16)
    }

    fn visit_i64_store8(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_store(memarg, StorageType::I8)
    }

    fn visit_i64_store16(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_store(memarg, StorageType::I16)
    }

    fn visit_i64_store32(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_store(memarg, StorageType::Val(ValType::I32))
    }

    fn visit_memory_size(&mut self, mem: u32, mem_byte: u8) -> Self::Output {
        if mem != 0 {
            panic!("Multi memory is not supported")
        }
        assert!(mem_byte == 0);

        let var = self.new_var(ValType::I32);
        self.push_stack(var);

        let memory = &self.conv.memory.as_ref().unwrap().name;

        let cs_ty = get_cs_ty(var.ty);
        self.stmts
            .push(format!("{var} = ({cs_ty})({memory}.Length / {PAGE_SIZE});"));
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

        let memory = &self.conv.memory.as_ref().unwrap().name;

        // 前のサイズを返す
        let cs_ty = get_cs_ty(var.ty);
        self.stmts
            .push(format!("{var} = ({cs_ty})({memory}.Length / {PAGE_SIZE});"));

        // メモリをsizeだけ拡張
        self.stmts.push("{".to_string());
        self.stmts.push(format!("var old = {memory};"));
        self.stmts.push(format!(
            "{memory} = new byte[old.Length + {size} * {PAGE_SIZE}];"
        ));
        self.stmts
            .push(format!("Array.Copy(old, {memory}, old.Length);"));
        self.stmts.push("}".to_string());
        Ok(())
    }

    fn visit_i32_const(&mut self, value: i32) -> Self::Output {
        self.visit_const(ValType::I32, value as u32)
    }

    fn visit_i64_const(&mut self, value: i64) -> Self::Output {
        self.visit_const(ValType::I64, value as u64)
    }

    fn visit_f32_const(&mut self, value: Ieee32) -> Self::Output {
        let value = f32::from_bits(value.bits());
        self.visit_float_const(value, ValType::F32, || format!("{:e}f", value))
    }

    fn visit_f64_const(&mut self, value: Ieee64) -> Self::Output {
        let value = f64::from_bits(value.bits());
        self.visit_float_const(value, ValType::F64, || format!("{:e}", value))
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
        self.visit_shift_op(ValType::I32, "<<", false)
    }

    fn visit_i32_shr_s(&mut self) -> Self::Output {
        self.visit_shift_op(ValType::I32, ">>", true)
    }

    fn visit_i32_shr_u(&mut self) -> Self::Output {
        self.visit_shift_op(ValType::I32, ">>", false)
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
        self.visit_shift_op(ValType::I64, "<<", false)
    }

    fn visit_i64_shr_s(&mut self) -> Self::Output {
        self.visit_shift_op(ValType::I64, ">>", true)
    }

    fn visit_i64_shr_u(&mut self) -> Self::Output {
        self.visit_shift_op(ValType::I64, ">>", false)
    }

    fn visit_i64_rotl(&mut self) -> Self::Output {
        self.visit_rot_op(ValType::I64, false)
    }

    fn visit_i64_rotr(&mut self) -> Self::Output {
        self.visit_rot_op(ValType::I64, true)
    }

    fn visit_f32_abs(&mut self) -> Self::Output {
        self.visit_math_un_op(ValType::F32, "Abs")
    }

    fn visit_f32_neg(&mut self) -> Self::Output {
        self.visit_un_op(ValType::F32, "-")
    }

    fn visit_f32_ceil(&mut self) -> Self::Output {
        self.visit_math_un_op(ValType::F32, "Ceiling")
    }

    fn visit_f32_floor(&mut self) -> Self::Output {
        self.visit_math_un_op(ValType::F32, "Floor")
    }

    fn visit_f32_trunc(&mut self) -> Self::Output {
        self.visit_math_un_op(ValType::F32, "Truncate")
    }

    fn visit_f32_nearest(&mut self) -> Self::Output {
        self.visit_math_un_op(ValType::F32, "Round")
    }

    fn visit_f32_sqrt(&mut self) -> Self::Output {
        self.visit_math_un_op(ValType::F32, "Sqrt")
    }

    fn visit_f32_add(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F32, "+", false, false)
    }

    fn visit_f32_sub(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F32, "-", false, false)
    }

    fn visit_f32_mul(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F32, "*", false, false)
    }

    fn visit_f32_div(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F32, "/", false, false)
    }

    fn visit_f32_min(&mut self) -> Self::Output {
        self.visit_math_bin_op(ValType::F32, "Min")
    }

    fn visit_f32_max(&mut self) -> Self::Output {
        self.visit_math_bin_op(ValType::F32, "Max")
    }

    fn visit_f32_copysign(&mut self) -> Self::Output {
        self.visit_math_bin_op(ValType::F32, "CopySign")
    }

    fn visit_f64_abs(&mut self) -> Self::Output {
        self.visit_math_un_op(ValType::F64, "Abs")
    }

    fn visit_f64_neg(&mut self) -> Self::Output {
        self.visit_un_op(ValType::F64, "-")
    }

    fn visit_f64_ceil(&mut self) -> Self::Output {
        self.visit_math_un_op(ValType::F64, "Ceiling")
    }

    fn visit_f64_floor(&mut self) -> Self::Output {
        self.visit_math_un_op(ValType::F64, "Floor")
    }

    fn visit_f64_trunc(&mut self) -> Self::Output {
        self.visit_math_un_op(ValType::F64, "Truncate")
    }

    fn visit_f64_nearest(&mut self) -> Self::Output {
        self.visit_math_un_op(ValType::F64, "Round")
    }

    fn visit_f64_sqrt(&mut self) -> Self::Output {
        self.visit_math_un_op(ValType::F64, "Sqrt")
    }

    fn visit_f64_add(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F64, "+", false, false)
    }

    fn visit_f64_sub(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F64, "-", false, false)
    }

    fn visit_f64_mul(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F64, "*", false, false)
    }

    fn visit_f64_div(&mut self) -> Self::Output {
        self.visit_bin_op(ValType::F64, "/", false, false)
    }

    fn visit_f64_min(&mut self) -> Self::Output {
        self.visit_math_bin_op(ValType::F64, "Min")
    }

    fn visit_f64_max(&mut self) -> Self::Output {
        self.visit_math_bin_op(ValType::F64, "Max")
    }

    fn visit_f64_copysign(&mut self) -> Self::Output {
        self.visit_math_bin_op(ValType::F64, "CopySign")
    }

    fn visit_i32_wrap_i64(&mut self) -> Self::Output {
        self.visit_cast(ValType::I32, None)
    }

    fn visit_i32_trunc_f32_s(&mut self) -> Self::Output {
        self.visit_cast(ValType::I32, Some("int"))
    }

    fn visit_i32_trunc_f32_u(&mut self) -> Self::Output {
        self.visit_cast(ValType::I32, None)
    }

    fn visit_i32_trunc_f64_s(&mut self) -> Self::Output {
        self.visit_cast(ValType::I32, Some("int"))
    }

    fn visit_i32_trunc_f64_u(&mut self) -> Self::Output {
        self.visit_cast(ValType::I32, None)
    }

    fn visit_i64_extend_i32_s(&mut self) -> Self::Output {
        self.visit_cast(ValType::I64, Some("int"))
    }

    fn visit_i64_extend_i32_u(&mut self) -> Self::Output {
        self.visit_cast(ValType::I64, None)
    }

    fn visit_i64_trunc_f32_s(&mut self) -> Self::Output {
        self.visit_cast(ValType::I64, Some("long"))
    }

    fn visit_i64_trunc_f32_u(&mut self) -> Self::Output {
        self.visit_cast(ValType::I64, None)
    }

    fn visit_i64_trunc_f64_s(&mut self) -> Self::Output {
        self.visit_cast(ValType::I64, Some("long"))
    }

    fn visit_i64_trunc_f64_u(&mut self) -> Self::Output {
        self.visit_cast(ValType::I64, None)
    }

    fn visit_f32_convert_i32_s(&mut self) -> Self::Output {
        self.visit_cast(ValType::F32, Some("int"))
    }

    fn visit_f32_convert_i32_u(&mut self) -> Self::Output {
        self.visit_cast(ValType::F32, None)
    }

    fn visit_f32_convert_i64_s(&mut self) -> Self::Output {
        self.visit_cast(ValType::F32, Some("long"))
    }

    fn visit_f32_convert_i64_u(&mut self) -> Self::Output {
        self.visit_cast(ValType::F32, None)
    }

    fn visit_f32_demote_f64(&mut self) -> Self::Output {
        self.visit_cast(ValType::F32, None)
    }

    fn visit_f64_convert_i32_s(&mut self) -> Self::Output {
        self.visit_cast(ValType::F64, Some("int"))
    }

    fn visit_f64_convert_i32_u(&mut self) -> Self::Output {
        self.visit_cast(ValType::F64, None)
    }

    fn visit_f64_convert_i64_s(&mut self) -> Self::Output {
        self.visit_cast(ValType::F64, Some("long"))
    }

    fn visit_f64_convert_i64_u(&mut self) -> Self::Output {
        self.visit_cast(ValType::F64, None)
    }

    fn visit_f64_promote_f32(&mut self) -> Self::Output {
        self.visit_cast(ValType::F64, None)
    }

    fn visit_i32_reinterpret_f32(&mut self) -> Self::Output {
        let opnd = self.pop_stack();
        let result = self.new_var(ValType::I32);
        self.push_stack(result);

        self.float_to_bits(opnd, result);
        Ok(())
    }

    fn visit_i64_reinterpret_f64(&mut self) -> Self::Output {
        let opnd = self.pop_stack();
        let result = self.new_var(ValType::I64);
        self.push_stack(result);

        self.float_to_bits(opnd, result);
        Ok(())
    }

    fn visit_f32_reinterpret_i32(&mut self) -> Self::Output {
        let opnd = self.pop_stack();
        let result = self.new_var(ValType::F32);
        self.push_stack(result);

        self.bits_to_float(opnd, result);
        Ok(())
    }

    fn visit_f64_reinterpret_i64(&mut self) -> Self::Output {
        let opnd = self.pop_stack();
        let result = self.new_var(ValType::F64);
        self.push_stack(result);

        self.bits_to_float(opnd, result);
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum OperatorError {
    #[error("`{0}` proposal is not implemented")]
    NotSupported(&'static str),
}

fn ty_f_to_i(ty: ValType) -> ValType {
    match ty {
        ValType::F32 => ValType::I32,
        ValType::F64 => ValType::I64,
        _ => panic!("Specify float type as argument"),
    }
}

fn get_int_bits(ty: ValType) -> u32 {
    match ty {
        ValType::I32 => 32,
        ValType::I64 => 64,
        _ => panic!("Specify integer type as argument"),
    }
}

fn get_frac_bits(ty: ValType) -> u32 {
    match ty {
        ValType::F32 => f32::MANTISSA_DIGITS - 1,
        ValType::F64 => f64::MANTISSA_DIGITS - 1,
        _ => panic!("Specify float type as argument"),
    }
}
