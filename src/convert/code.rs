use std::{cell::RefCell, fmt, rc::Rc};

use anyhow::Result;
use num_traits::float::Float;
use wasmparser::{
    for_each_operator, BlockType, BrTable, FuncType, FunctionBody, Ieee32, Ieee64, MemArg,
    StorageType, ValType, VisitOperator,
};

use crate::{
    ir::{
        func::{Code, Func, Instr, Var},
        module::Module,
        trap,
        ty::CsType,
        BREAK_DEPTH, LOOP, PAGE_SIZE,
    },
    util::bit_mask,
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

pub struct CodeConverter<'input, 'module> {
    module: &'module Module<'input>,
    func: Rc<RefCell<Func>>,
    blocks: Vec<Block>,
    code: Code,
    /// brの後など、到達不可能なコードの処理時に1加算。
    /// unreachableが1以上の場合のブロックの出現ごとに1加算。
    /// ブロックの終了時に1減算。
    unreachable: i32,
}

#[derive(Debug)]
struct Block {
    stack: Vec<Var>,
    result: Option<Var>,
    loop_var: Option<usize>,
}

impl<'input, 'module> CodeConverter<'input, 'module> {
    pub(super) fn new(module: &'module Module<'input>, func: Rc<RefCell<Func>>) -> Self {
        let code = Code::new(&func.borrow().header);
        Self {
            module,
            func,
            blocks: Vec::new(),
            code,
            unreachable: 0,
        }
    }

    fn push_line(&mut self, line: String) {
        self.code.instrs.push(Instr::Line(line));
    }

    pub fn convert(mut self, body: FunctionBody<'_>) -> Result<Code> {
        for local in body.get_locals_reader()? {
            let (count, ty) = local?;
            for _ in 0..count {
                self.code.new_var(CsType::get(ty));
            }
        }

        let blockty = {
            let func = self.func.borrow();
            let results = func.header.ty.results();
            match results.len() {
                0 => BlockType::Empty,
                1 => BlockType::Type(results[0]),
                _ => panic!("multi value is not supported"),
            }
        };

        // 関数の最上位のブロック
        self.visit_block(blockty)?;
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
            self.push_line(format!("return {res};"));
        }

        Ok(self.code)
    }

    fn new_block(&mut self, blockty: BlockType, is_loop: bool) -> &Block {
        let result = match blockty {
            BlockType::Empty => None,
            BlockType::Type(ty) => Some(self.code.new_var(CsType::get(ty))),
            BlockType::FuncType(..) => panic!("func type blocks are not supported"),
        };

        let loop_var = if is_loop {
            Some({
                self.code.loop_var_count += 1;
                self.code.loop_var_count - 1
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

    /// ブロックに戻り値があれば、戻り値を代入する命令を追加する
    fn block_result(&mut self, relative_depth: u32, is_br: bool) {
        let upper_block = &self.blocks[self.blocks.len() - 1 - relative_depth as usize];

        // brでループを再開する場合は戻り値を取らない
        if is_br && upper_block.loop_var.is_some() {
            return;
        }

        if let Some(result) = upper_block.result {
            let current_block = &self.blocks[self.blocks.len() - 1];
            let rhs = current_block.stack.last().unwrap();
            self.push_line(format!("{result} = {rhs};"));
        }
    }

    /// relative_depthが0より大きければBREAK_DEPTHに代入する処理を追加する
    fn set_break_depth(&mut self, relative_depth: u32) {
        if relative_depth > 0 {
            self.push_line(format!("{BREAK_DEPTH} = {relative_depth};"));
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
        var_type: CsType,
        mem_type: StorageType,
        signed: bool,
    ) -> <Self as VisitOperator<'_>>::Output {
        let idx = self.pop_stack();
        let var = self.code.new_var(var_type);
        self.push_stack(var);

        self.int_load(memarg, var_type, mem_type, signed, idx, var);

        Ok(())
    }

    fn int_load(
        &mut self,
        memarg: MemArg,
        var_type: CsType,
        mem_type: StorageType,
        signed: bool,
        idx: Var,
        var: Var,
    ) {
        use {StorageType::*, ValType::*};

        let mut load = format!("{var} = ");

        let memory = &self.module.memory.as_ref().unwrap().name;
        let cast_var = cast(var_type);
        match mem_type {
            I8 => load += &format!("{memory}[{0}+{1}];", idx, memarg.offset),
            I16 => load += &format!("{memory}[{0}+{1}] | {2}({memory}[{0}+{1}+1])<<8;", idx, memarg.offset, cast_var),
            Val(I32) => load += &format!(
                "{memory}[{0}+{1}] | {2}({memory}[{0}+{1}+1])<<8 | {2}({memory}[{0}+{1}+2])<<16 | {2}({memory}[{0}+{1}+3])<<24;",
                idx, memarg.offset, cast_var),
            Val(I64) => load += &format!(
                "{memory}[{0}+{1}] | {2}({memory}[{0}+{1}+1])<<8 | {2}({memory}[{0}+{1}+2])<<16 | {2}({memory}[{0}+{1}+3])<<24 | {2}({memory}[{0}+{1}+4])<<32 | {2}({memory}[{0}+{1}+5])<<40 | {2}({memory}[{0}+{1}+6])<<48 | {2}({memory}[{0}+{1}+7])<<56;",
                idx, memarg.offset, cast_var),
            _ => unreachable!(),
        }
        self.push_line(load);

        if signed {
            let mut sign;
            match mem_type {
                I8 => sign = format!("if ({var} >= 0x80) "),
                I16 => sign = format!("if ({var} >= 0x8000) "),
                Val(I32) => sign = format!("if ({var} >= 0x80000000) "),
                _ => unreachable!(),
            }

            match (var_type, mem_type) {
                (CsType::Int, I8) => sign += &format!("{var} |= -0x100;"),
                (CsType::Int, I16) => sign += &format!("{var} |= -0x10000;"),
                (CsType::Long, I8) => sign += &format!("{var} |= -0x100;"),
                (CsType::Long, I16) => sign += &format!("{var} |= -0x10000;"),
                (CsType::Long, Val(I32)) => sign += &format!("{var} |= -0x100000000;"),
                _ => unreachable!(),
            }

            self.push_line(sign);
        }
    }

    fn visit_float_load(
        &mut self,
        memarg: MemArg,
        var_type: CsType,
    ) -> <Self as VisitOperator<'_>>::Output {
        let idx = self.pop_stack();
        let i_ty = ty_f_to_i(var_type);
        let i_var = self.code.new_var(i_ty);

        let var = self.code.new_var(var_type);
        self.push_stack(var);

        self.int_load(
            memarg,
            i_ty,
            StorageType::Val(i_ty.val_type()),
            false,
            idx,
            i_var,
        );

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
        let class = self.math_class(f_ty);

        self.push_line("{".to_string());

        // ビット列の各データを抽出
        self.push_line(format!("var sign = ({i_var} >> {}) & 1;", bits - 1));
        self.push_line(format!(
            "var expo = ({i_var} >> {frac_bits}) & {expo_bit_mask};",
        ));
        self.push_line(format!(
            "var frac = {i_var} & {};",
            bit_mask(frac_bits as u64)
        ));

        let sign = format!("(1 - {}(sign) * 2)", cast(f_ty));

        self.push_line("if (expo == 0) {".to_string());
        {
            self.push_line("if (frac == 0) {".to_string());
            // 0の場合
            self.push_line(format!("{f_var} = sign == 0 ? 0f : -0f;"));
            self.push_line("} else {".to_string());
            // 非正規化数の場合
            self.push_line(format!("{f_var} = ({f_ty})frac * {f_ty}.Epsilon * {sign};"));
            self.push_line("}".to_string());
        }
        self.push_line(format!("}} else if (expo == {expo_bit_mask}) {{"));
        {
            self.push_line("if (frac == 0) {".to_string());
            // 無限大の場合
            self.push_line(format!(
                "{f_var} = sign == 0 ? {f_ty}.PositiveInfinity : {f_ty}.NegativeInfinity;"
            ));
            self.push_line("} else {".to_string());
            // NaNの場合
            self.push_line(format!("{f_var} = {f_ty}.NaN;"));
            self.push_line("}".to_string());
        }
        self.push_line("} else {".to_string());
        {
            // 浮動小数点数の変数に代入
            let expo = format!(
                "{class}.Pow(2, {}(expo) - {expo_offset})",
                cast(CsType::Int)
            );
            let frac = format!("({}(frac) / {} + 1)", cast(f_ty), 1u64 << frac_bits);
            self.push_line(format!("{f_var} = {frac} * {expo} * {sign};"));
        }
        self.push_line("}".to_string());

        self.push_line("}".to_string());
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

        let memory = &self.module.memory.as_ref().unwrap().name;
        let cast_byte = cast(CsType::Byte);
        self.push_line(match mem_type {
            I8 => format!("{memory}[{0}+{1}]={cast_byte}({2}&0xff);", idx, memarg.offset, var),
            I16 => {
                format!(
                    "{memory}[{0}+{1}]={cast_byte}({2}&0xff); {memory}[{0}+{1}+1]={cast_byte}(({2}>>8)&0xff);",
                    idx, memarg.offset, var
                )
            }
            Val(I32) => {
                format!(
                    "{memory}[{0}+{1}]={cast_byte}({2}&0xff); {memory}[{0}+{1}+1]={cast_byte}(({2}>>8)&0xff); {memory}[{0}+{1}+2]={cast_byte}(({2}>>16)&0xff); {memory}[{0}+{1}+3]={cast_byte}(({2}>>24)&0xff);",
                    idx, memarg.offset, var
                )
            }
            Val(I64) => {
                format!(
                    "{memory}[{0}+{1}]={cast_byte}({2}&0xff); {memory}[{0}+{1}+1]={cast_byte}(({2}>>8)&0xff); {memory}[{0}+{1}+2]={cast_byte}(({2}>>16)&0xff); {memory}[{0}+{1}+3]={cast_byte}(({2}>>24)&0xff); {memory}[{0}+{1}+4]={cast_byte}(({2}>>32)&0xff); {memory}[{0}+{1}+5]={cast_byte}(({2}>>40)&0xff); {memory}[{0}+{1}+6]={cast_byte}(({2}>>48)&0xff); {memory}[{0}+{1}+7]={cast_byte}(({2}>>56)&0xff);",
                    idx, memarg.offset, var
                )
            }
            _ => unreachable!(),
        });
    }

    fn visit_float_store(
        &mut self,
        memarg: MemArg,
        mem_type: CsType,
    ) -> <Self as VisitOperator<'_>>::Output {
        let var = self.pop_stack();
        let idx = self.pop_stack();
        let i_ty = ty_f_to_i(mem_type);
        let i_var = self.code.new_var(i_ty);

        self.float_to_bits(var, i_var);

        self.int_store(memarg, StorageType::Val(i_ty.val_type()), idx, i_var);

        Ok(())
    }

    fn float_to_bits(&mut self, f_var: Var, i_var: Var) {
        let f_ty = f_var.ty;
        let i_ty = i_var.ty;
        let bits = get_int_bits(i_ty);
        let frac_bits = get_frac_bits(f_ty);
        let frac_bits_mask = bit_mask(frac_bits as u64);
        let expo_bits = bits - 1 - frac_bits;
        let expo_bit_mask = bit_mask(expo_bits as u64);
        // LSBに寄せた後のビットマスク
        let expo_offset = (1 << (expo_bits - 1)) - 1;
        let class = self.math_class(f_ty);
        let subnormal_bound = match f_ty {
            CsType::Float => "1.1754944e-38f",
            CsType::Double => "2.2250738585072014e-308",
            _ => unreachable!(),
        };

        self.push_line("{".to_string());
        self.push_line("bool sign;".to_string());
        self.push_line(format!("{i_ty} expo;"));
        self.push_line(format!("{i_ty} frac;"));
        self.push_line(format!("var absVar = {class}.Abs({f_var});"));

        let cast_i_ty = cast(i_ty);

        self.push_line(format!("if ({f_var} == 0) {{"));
        {
            // 0の場合
            // 1/0を計算することで+0と-0を区別
            self.push_line(format!("sign = 1 / {f_var} < 0;"));
            self.push_line("expo = 0;".to_string());
            self.push_line("frac = 0;".to_string());
        }
        self.push_line(format!("}} else if ({f_ty}.IsInfinity({f_var})) {{"));
        {
            // 無限大の場合
            self.push_line(format!("sign = {f_var} < 0;"));
            self.push_line(format!("expo = {expo_bit_mask};"));
            self.push_line("frac = 0;".to_string());
        }
        self.push_line(format!("}} else if ({f_ty}.IsNaN({f_var})) {{"));
        {
            // NaNの場合
            self.push_line("sign = true;".to_string());
            self.push_line(format!("expo = {expo_bit_mask};"));
            // MSBだけが1
            self.push_line(format!("frac = {};", 1u64 << (frac_bits - 1)));
        }
        self.push_line(format!("}} else if (absVar < {subnormal_bound}) {{",));
        {
            // 非正規化数の場合
            self.push_line(format!("sign = {f_var} < 0;"));
            self.push_line("expo = 0;".to_string());
            self.push_line(format!(
                "frac = {cast_i_ty}({class}.Abs({f_var}) / {f_ty}.Epsilon);"
            ));
        }
        self.push_line("} else {".to_string());
        {
            self.push_line(format!("sign = {f_var} < 0;"));
            self.push_line(format!(
                "var expoF = {class}.Floor({class}.Log(absVar, 2));"
            ));
            self.push_line(format!("if (expoF >= {}) {{", expo_offset + 1));
            {
                // Log2の誤差で無限大になった場合
                self.push_line(format!("expo = {};", expo_bit_mask - 1));
                self.push_line(format!("frac = {frac_bits_mask};"));
            }
            self.push_line("} else {".to_string());
            {
                // 通常の浮動小数点数の場合
                self.push_line(format!("expo = {cast_i_ty}(expoF) + {expo_offset};"));
                self.push_line(format!(
                    "frac = {cast_i_ty}((absVar / {class}.Pow(2, expoF) - 1) * {});",
                    1u64 << frac_bits
                ));
            }
            self.push_line("}".to_string());
        }
        self.push_line("}".to_string());

        self.push_line(format!(
            "{i_var} = ({cast_i_ty}(sign) << {}) | (expo << {frac_bits}) | frac;",
            bits - 1
        ));

        self.push_line("}".to_string());
    }

    fn visit_const(
        &mut self,
        ty: CsType,
        value: impl fmt::Display,
    ) -> <Self as VisitOperator<'_>>::Output {
        let var = self.code.new_var(ty);
        self.push_stack(var);

        self.push_line(format!("{var} = {value};"));
        Ok(())
    }

    fn visit_float_const<T, F>(
        &mut self,
        value: T,
        ty: CsType,
        normal_display: F,
    ) -> std::result::Result<(), anyhow::Error>
    where
        T: Float,
        F: FnOnce() -> String,
    {
        let literal = if value.is_infinite() {
            if value.is_sign_positive() {
                format!("{ty}.PositiveInfinity")
            } else {
                format!("{ty}.NegativeInfinity")
            }
        } else if value.is_nan() {
            format!("{ty}.NaN")
        } else {
            normal_display()
        };

        self.visit_const(ty, literal)
    }

    /// (opnd, result)を返す
    fn un_op_vars(&mut self, result_ty: CsType) -> (Var, Var) {
        let opnd = self.pop_stack();
        let result = self.code.new_var(result_ty);
        self.push_stack(result);

        (opnd, result)
    }

    /// (opnd, result)を返し、opndの型をresultの型とする
    fn un_op_vars_auto_ty(&mut self) -> (Var, Var) {
        let opnd = self.pop_stack();
        let result = self.code.new_var(opnd.ty);
        self.push_stack(result);

        (opnd, result)
    }

    /// (lhs, rhs, result)を返し、lhsの型をresultの型とする
    fn bin_op_vars_auto_ty(&mut self) -> (Var, Var, Var) {
        let rhs = self.pop_stack();
        let lhs = self.pop_stack();
        let result = self.code.new_var(lhs.ty);
        self.push_stack(result);

        (lhs, rhs, result)
    }

    fn visit_eqz(&mut self) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars(CsType::Int);

        self.push_line(format!("{result} = {}({opnd} == 0);", cast(result.ty)));
        Ok(())
    }

    fn visit_un_op(&mut self, op: &str) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars_auto_ty();

        self.push_line(format!("{result} = {op}{opnd};"));
        Ok(())
    }

    fn visit_bin_op(
        &mut self,
        op: &str,
        logical: bool,
        signed: bool,
    ) -> <Self as VisitOperator<'_>>::Output {
        let rhs = self.pop_stack();
        let lhs = self.pop_stack();
        let result = self
            .code
            .new_var(if logical { CsType::Int } else { lhs.ty });
        self.push_stack(result);

        if signed {
            if logical {
                self.push_line(format!("{result} = {}({lhs} {op} {rhs});", cast(result.ty)));
            } else {
                self.push_line(format!("{result} = {lhs} {op} {rhs};"));
            }
        } else {
            let lhs_u = self.code.new_var(lhs.ty.to_unsigned());
            let rhs_u = self.code.new_var(rhs.ty.to_unsigned());
            let tmp = self
                .code
                .new_var(if logical { CsType::Bool } else { lhs_u.ty });

            self.cast_sign(lhs, lhs_u);
            self.cast_sign(rhs, rhs_u);
            self.push_line(format!("{tmp} = {lhs_u} {op} {rhs_u};"));

            if logical {
                self.push_line(format!("{result} = {}({tmp});", cast(result.ty)));
            } else {
                self.cast_sign(tmp, result);
            }
        }

        Ok(())
    }

    fn visit_rem_op(&mut self, signed: bool) -> <Self as VisitOperator<'_>>::Output {
        let (lhs, rhs, result) = self.bin_op_vars_auto_ty();

        if signed {
            self.push_line(format!("{result} = {lhs} - {lhs} / {rhs} * {rhs};"));
        } else {
            let lhs_u = self.code.new_var(lhs.ty.to_unsigned());
            let rhs_u = self.code.new_var(rhs.ty.to_unsigned());
            let tmp = self.code.new_var(lhs_u.ty);

            self.cast_sign(lhs, lhs_u);
            self.cast_sign(rhs, rhs_u);
            self.push_line(format!("{tmp} = {lhs_u} - {lhs_u} / {rhs_u} * {rhs_u};"));
            self.cast_sign(tmp, result);
        };

        Ok(())
    }

    fn cast_sign(&mut self, opnd: Var, result: Var) {
        let bits = get_int_bits(result.ty);
        let only_msb = 1u64 << (bits - 1);
        let except_msb = only_msb - 1;
        let cast_ty = cast(result.ty);

        if result.ty.signed() {
            self.push_line(format!("{result} = {opnd} >= {only_msb} ? {cast_ty}({opnd} - {only_msb}) | (-{except_msb} - 1) : {cast_ty}({opnd});"));
        } else {
            self.push_line(format!("{result} = {opnd} < 0 ? {cast_ty}({opnd} & {except_msb}) | {only_msb} : {cast_ty}({opnd});"));
        }
    }

    fn visit_shift_op(&mut self, op: &str, signed: bool) -> <Self as VisitOperator<'_>>::Output {
        let (lhs, rhs, result) = self.bin_op_vars_auto_ty();

        let rhs_int = if rhs.ty == CsType::Int {
            rhs
        } else {
            let rhs_int = self.code.new_var(CsType::Int);
            self.wrap(rhs, rhs_int);
            rhs_int
        };

        if signed {
            self.push_line(format!("{result} = {lhs} {op} {rhs_int};"));
        } else {
            self.shr_u(lhs, rhs_int, result);
        }
        Ok(())
    }

    fn shr_u(&mut self, lhs: Var, rhs_int: Var, result: impl fmt::Display) {
        let bits = get_int_bits(lhs.ty);
        let only_msb = 1u64 << (bits - 1);
        let except_msb = only_msb - 1;

        self.push_line(format!(
            "{result} = {lhs} < 0 ? (({lhs} & {except_msb}) >> {rhs_int}) | ({} << (-1 - {rhs_int})) : {lhs} >> {rhs_int};",
            if lhs.ty == CsType::Long { "1L" } else { "1" }
        ));
    }

    fn visit_rot_op(&mut self, right: bool) -> <Self as VisitOperator<'_>>::Output {
        let (lhs, rhs, result) = self.bin_op_vars_auto_ty();

        let bits = get_int_bits(lhs.ty);

        let rhs_int = if rhs.ty == CsType::Int {
            rhs
        } else {
            let rhs_int = self.code.new_var(CsType::Int);
            self.wrap(rhs, rhs_int);
            rhs_int
        };

        let bits_m_rhs = self.code.new_var(CsType::Int);
        self.push_line(format!("{bits_m_rhs} = {bits} - {rhs_int};"));

        let shr = self.code.new_var(lhs.ty);
        if right {
            self.shr_u(lhs, rhs_int, shr);
            self.push_line(format!("{result} = {shr} | ({lhs} << {bits_m_rhs});"));
        } else {
            self.shr_u(lhs, bits_m_rhs, shr);
            self.push_line(format!("{result} = ({lhs} << {rhs_int}) | {shr};"));
        }
        Ok(())
    }

    fn visit_clz(&mut self) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars_auto_ty();

        let bits = get_int_bits(opnd.ty);

        self.push_line(format!("if ({opnd} == 0) {result} = {bits};"));
        let cast_ty = cast_from(CsType::Int, opnd.ty);
        // 2進で文字列化して文字数を数える
        self.push_line(format!(
            "else {result} = {cast_ty}({bits} - Convert.ToString({opnd}, 2).Length);",
        ));
        Ok(())
    }

    fn visit_ctz(&mut self) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars_auto_ty();

        let bits = get_int_bits(opnd.ty);

        self.push_line(format!("if ({opnd} == 0) {result} = {bits};"));
        let cast_ty = cast_from(CsType::Int, opnd.ty);

        // 符号付き整数の最小値のリテラルはUdonSharpではエラーとなるので
        // `-最大値 - 1` で表現する
        let or_opnd = match opnd.ty {
            CsType::Int => (1i32 << (bits - 1)) as i64,
            CsType::Long => 1i64 << (bits - 1),
            _ => unreachable!(),
        } + 1;

        // 1. 文字数を揃えるため、MSBだけが1の数とopndのORをとる
        // 2. 2進で文字列化する
        // 3. 最後に1が出現するインデックスを求める
        self.push_line(format!(
            "else {result} = {} - {cast_ty}(Convert.ToString(({opnd} | ({or_opnd} - 1)), 2).LastIndexOf('1'));",
            bits - 1,
        ));
        Ok(())
    }

    fn visit_popcnt(&mut self) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars_auto_ty();

        let cast_ty = cast_from(CsType::Int, opnd.ty);
        // 2進で文字列化して、0を除去した後の文字数を数える
        self.push_line(format!(
            "{result} = {cast_ty}(Convert.ToString({opnd}, 2).Replace(\"0\", \"\").Length);"
        ));
        Ok(())
    }

    fn visit_math_un_op(&mut self, func: &str) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars_auto_ty();

        self.push_line(format!(
            "{result} = {}.{func}({opnd});",
            self.math_class(opnd.ty)
        ));
        Ok(())
    }

    fn visit_math_bin_op(&mut self, func: &str) -> <Self as VisitOperator<'_>>::Output {
        let (lhs, rhs, result) = self.bin_op_vars_auto_ty();

        self.push_line(format!(
            "{result} = {}.{func}({lhs}, {rhs});",
            self.math_class(lhs.ty)
        ));
        Ok(())
    }

    fn visit_copysign_op(&mut self) -> <Self as VisitOperator<'_>>::Output {
        let (lhs, rhs, result) = self.bin_op_vars_auto_ty();

        self.push_line(format!(
            "{result} = {0}.Abs({lhs}) * (({rhs} == 0 ? 1 / {rhs} : {rhs}) > 0 ? 1 : -1);",
            self.math_class(lhs.ty)
        ));
        Ok(())
    }

    fn visit_cast(&mut self, result_ty: CsType) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars(result_ty);
        let cast_ty = cast(result_ty);

        self.push_line(format!("{result} = {cast_ty}({opnd});"));
        Ok(())
    }

    fn wrap(&mut self, opnd: Var, result: Var) {
        self.push_line(format!(
            "{result} = {}({opnd} & 0x7fffffff);",
            cast(CsType::Int)
        ));
        self.push_line(format!(
            "if (({opnd} & 0x80000000) != 0) {result} |= -0x7fffffff - 1;"
        ));
    }

    fn visit_cast_trunc(
        &mut self,
        result_ty: CsType,
        signed: bool,
    ) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars(result_ty);
        let tmp = self.code.new_var(CsType::Double);

        self.push_line(format!(
            "{tmp} = Math.Truncate({}({opnd}));",
            cast_from(opnd.ty, CsType::Double)
        ));

        if signed {
            let cast_ty = cast(result_ty);
            self.push_line(format!("{result} = {cast_ty}({tmp});"));
        } else {
            self.cast_sign(tmp, result);
        }
        Ok(())
    }

    fn visit_cast_extend(&mut self, signed: bool) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars(CsType::Long);
        let cast_ty = cast(CsType::Long);

        if signed {
            self.push_line(format!("{result} = {cast_ty}({opnd});"));
        } else {
            self.push_line(format!("{result} = {opnd} < 0 ? {cast_ty}({opnd} & 0x7fffffff) | 0x80000000 : {cast_ty}({opnd});"));
        }
        Ok(())
    }

    fn visit_cast_convert(
        &mut self,
        result_ty: CsType,
        signed: bool,
    ) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars(result_ty);

        let cast_ty = cast(result_ty);
        if signed {
            self.push_line(format!("{result} = {cast_ty}({opnd});"));
        } else {
            let bits = get_int_bits(opnd.ty);
            let only_msb = 1u64 << (bits - 1);

            self.push_line(format!(
                "{result} = {opnd} < 0 ? {cast_ty}({opnd} & {}) + {only_msb} : {cast_ty}({opnd});",
                only_msb - 1
            ));
        }
        Ok(())
    }

    fn get_result(&mut self, ty: &FuncType) -> Option<Var> {
        match ty.results().len() {
            0 => None,
            1 => {
                let var = self.code.new_var(CsType::get(ty.results()[0]));
                self.push_stack(var);
                Some(var)
            }
            _ => {
                panic!("Multiple return values are not supported")
            }
        }
    }

    fn math_class(&self, ty: CsType) -> &'static str {
        match ty {
            CsType::Float => {
                if self.module.test {
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

impl<'a, 'input, 'module> VisitOperator<'a> for CodeConverter<'input, 'module> {
    type Output = Result<()>;

    for_each_operator!(define_visit_operator);

    fn visit_unreachable(&mut self) -> Self::Output {
        self.unreachable = 1;
        self.push_line(trap(self.module, "unreachable"));
        Ok(())
    }

    fn visit_nop(&mut self) -> Self::Output {
        self.push_line("// nop".to_string());
        Ok(())
    }

    fn visit_block(&mut self, blockty: BlockType) -> Self::Output {
        if self.unreachable > 0 {
            self.unreachable += 1;
            return Ok(());
        }
        self.new_block(blockty, false);
        self.push_line("do {".to_string());
        Ok(())
    }

    fn visit_loop(&mut self, blockty: BlockType) -> Self::Output {
        if self.unreachable > 0 {
            self.unreachable += 1;
            return Ok(());
        }
        let block = self.new_block(blockty, true);
        let loop_var = block.loop_var.unwrap();
        self.push_line(format!("{LOOP}{loop_var} = true;"));
        self.push_line("do {".to_string());
        self.push_line("do {".to_string());
        Ok(())
    }

    fn visit_if(&mut self, blockty: BlockType) -> Self::Output {
        if self.unreachable > 0 {
            self.unreachable += 1;
            return Ok(());
        }
        let var = self.pop_stack();
        self.new_block(blockty, false);
        self.push_line(format!("do if ({var} != 0) {{"));
        Ok(())
    }

    fn visit_else(&mut self) -> Self::Output {
        match self.unreachable {
            x if x == 0 => self.block_result(0, false),
            x if x == 1 => self.unreachable -= 1,
            _ => return Ok(()),
        }

        self.blocks.last_mut().unwrap().stack.clear();

        self.push_line("} else {".to_string());
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
                self.push_line(format!("{LOOP}{} = false;", loop_var));
                self.push_line("} while (false);".to_string());
                self.push_line(format!("if ({BREAK_DEPTH} > 0) break;"));
                self.push_line(format!("}} while ({LOOP}{});", loop_var));
            }
            None => {
                self.push_line("} while (false);".to_string());
            }
        }

        // 最も外側のブロックのendでない場合のみ
        if !self.blocks.is_empty() {
            if let Some(result) = block.result {
                self.push_stack(result);
            }

            // 多重break
            self.push_line(format!(
                "if ({BREAK_DEPTH} > 0) {{ {BREAK_DEPTH}--; break; }}"
            ));
        }

        Ok(())
    }

    fn visit_br(&mut self, relative_depth: u32) -> Self::Output {
        self.block_result(relative_depth, true);
        self.set_break_depth(relative_depth);
        self.unreachable = 1;

        self.push_line("break;".to_string());
        Ok(())
    }

    fn visit_br_if(&mut self, relative_depth: u32) -> Self::Output {
        let var = self.pop_stack();
        self.push_line(format!("if ({var} != 0) {{"));

        self.block_result(relative_depth, true);
        self.set_break_depth(relative_depth);

        self.push_line("break;".to_string());

        self.push_line("}".to_string());
        Ok(())
    }

    fn visit_br_table(&mut self, targets: BrTable<'a>) -> Self::Output {
        self.unreachable = 1;
        let var = self.pop_stack();

        self.push_line(format!("switch ({var}) {{"));

        for (i, target) in targets.targets().enumerate() {
            let target = target?;

            self.push_line(format!("case {i}:"));
            self.block_result(target, true);
            self.set_break_depth(target);
            self.push_line("break;".to_string());
        }

        self.push_line("default:".to_string());
        self.block_result(targets.default(), true);
        self.set_break_depth(targets.default());
        self.push_line("break;".to_string());

        self.push_line("}".to_string());
        self.push_line("break;".to_string());

        Ok(())
    }

    fn visit_return(&mut self) -> Self::Output {
        self.unreachable = 1;

        let results_len = self.func.borrow().header.ty.results().len();
        match results_len {
            0 => self.push_line("return;".to_string()),
            1 => {
                let var = self.last_stack();
                self.push_line(format!("return {var};"))
            }
            _ => {
                panic!("Multiple return values are not supported")
            }
        }
        Ok(())
    }

    fn visit_call(&mut self, function_index: u32) -> Self::Output {
        let func = Rc::clone(&self.module.wasm_funcs[function_index as usize]);
        let ty = func.borrow().header.ty.clone();

        // mapは遅延評価であるため、mapの後にrevを呼んだ場合ではpop_stackで得る値が逆順にならない
        // そのため、一度collectでVecにした後、reverseで逆順にしている
        let mut params: Vec<Var> = ty.params().iter().map(|_| self.pop_stack()).collect();
        params.reverse();

        let result = self.get_result(&ty);

        self.code.instrs.push(Instr::Call {
            func,
            params,
            result,
        });
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

        let func = Rc::clone(&self.module.call_indirects[type_index as usize]);
        let ty = self.module.types[type_index as usize].clone();

        // Iteratorのrevを使わない理由はvisit_callのコメントを参照
        let mut params: Vec<Var> = (0..ty.params().len() + 1)
            .map(|_| self.pop_stack())
            .collect();
        params.reverse();

        let result = self.get_result(&ty);

        self.code.instrs.push(Instr::Call {
            func,
            params,
            result,
        });
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

        let select = self.code.new_var(val1.ty);
        self.push_stack(select);

        self.push_line(format!("{select} = {c} != 0 ? {val1} : {val2};"));

        Ok(())
    }

    fn visit_local_get(&mut self, local_index: u32) -> Self::Output {
        let local = self.code.vars[local_index as usize];
        let var = self.code.new_var(local.ty);
        self.push_stack(var);

        self.push_line(format!("{var} = {local};"));
        Ok(())
    }

    fn visit_local_set(&mut self, local_index: u32) -> Self::Output {
        let local = self.code.vars[local_index as usize];
        let var = self.pop_stack();
        self.push_line(format!("{local} = {var};"));
        Ok(())
    }

    fn visit_local_tee(&mut self, local_index: u32) -> Self::Output {
        let local = self.code.vars[local_index as usize];
        self.push_line(format!("{local} = {};", self.last_stack()));
        Ok(())
    }

    fn visit_global_get(&mut self, global_index: u32) -> Self::Output {
        let global = &self.module.globals[global_index as usize];
        let var = self.code.new_var(CsType::get(global.ty.content_type));
        self.push_stack(var);

        self.push_line(format!("{var} = {};", global.name));
        Ok(())
    }

    fn visit_global_set(&mut self, global_index: u32) -> Self::Output {
        let global = &self.module.globals[global_index as usize];
        let var = self.pop_stack();

        self.push_line(format!("{} = {var};", global.name));
        Ok(())
    }

    fn visit_i32_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, CsType::Int, StorageType::Val(ValType::I32), false)
    }

    fn visit_i64_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, CsType::Long, StorageType::Val(ValType::I64), false)
    }

    fn visit_f32_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_float_load(memarg, CsType::Float)
    }

    fn visit_f64_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_float_load(memarg, CsType::Double)
    }

    fn visit_i32_load8_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, CsType::Int, StorageType::I8, true)
    }

    fn visit_i32_load8_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, CsType::Int, StorageType::I8, false)
    }

    fn visit_i32_load16_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, CsType::Int, StorageType::I16, true)
    }

    fn visit_i32_load16_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, CsType::Int, StorageType::I16, false)
    }

    fn visit_i64_load8_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, CsType::Long, StorageType::I8, true)
    }

    fn visit_i64_load8_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, CsType::Long, StorageType::I8, false)
    }

    fn visit_i64_load16_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, CsType::Long, StorageType::I16, true)
    }

    fn visit_i64_load16_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, CsType::Long, StorageType::I16, false)
    }

    fn visit_i64_load32_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, CsType::Long, StorageType::Val(ValType::I32), true)
    }

    fn visit_i64_load32_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_load(memarg, CsType::Long, StorageType::Val(ValType::I32), false)
    }

    fn visit_i32_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_store(memarg, StorageType::Val(ValType::I32))
    }

    fn visit_i64_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_int_store(memarg, StorageType::Val(ValType::I64))
    }

    fn visit_f32_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_float_store(memarg, CsType::Float)
    }

    fn visit_f64_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_float_store(memarg, CsType::Double)
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

        let var = self.code.new_var(CsType::Int);
        self.push_stack(var);

        let memory = &self.module.memory.as_ref().unwrap().name;

        self.push_line(format!("{var} = {memory}.Length / {PAGE_SIZE};"));
        Ok(())
    }

    fn visit_memory_grow(&mut self, mem: u32, mem_byte: u8) -> Self::Output {
        if mem != 0 {
            panic!("Multi memory is not supported")
        }
        assert!(mem_byte == 0);

        let size = self.pop_stack();
        let var = self.code.new_var(CsType::Int);
        self.push_stack(var);

        let memory = &self.module.memory.as_ref().unwrap().name;
        let max = self
            .module
            .memory
            .as_ref()
            .unwrap()
            .ty
            .maximum
            .unwrap_or(0x10000);

        self.push_line(format!(
            "if ({memory}.Length / {PAGE_SIZE} + {size} > {max}) {{"
        ));
        {
            // 新しいメモリサイズが最大値を超えていれば-1を返す
            self.push_line(format!("{var} = -1;"));
        }
        self.push_line("} else {".to_string());
        {
            // 前のサイズを返す
            self.push_line(format!("{var} = {memory}.Length / {PAGE_SIZE};"));

            // メモリをsizeだけ拡張
            self.push_line(format!("var old = {memory};"));
            self.push_line(format!(
                "{memory} = new byte[old.Length + {size} * {PAGE_SIZE}];"
            ));
            self.push_line(format!("Array.Copy(old, {memory}, old.Length);"));
        }
        self.push_line("}".to_string());
        Ok(())
    }

    fn visit_i32_const(&mut self, value: i32) -> Self::Output {
        match value {
            i32::MIN => self.visit_const(CsType::Int, format!("{} - 1", i32::MIN + 1)),
            _ => self.visit_const(CsType::Int, value),
        }
    }

    fn visit_i64_const(&mut self, value: i64) -> Self::Output {
        match value {
            i64::MIN => self.visit_const(CsType::Long, format!("{} - 1", i64::MIN + 1)),
            _ => self.visit_const(CsType::Long, value),
        }
    }

    fn visit_f32_const(&mut self, value: Ieee32) -> Self::Output {
        let value = f32::from_bits(value.bits());
        self.visit_float_const(value, CsType::Float, || format!("{:e}f", value))
    }

    fn visit_f64_const(&mut self, value: Ieee64) -> Self::Output {
        let value = f64::from_bits(value.bits());
        self.visit_float_const(value, CsType::Double, || format!("{:e}", value))
    }

    fn visit_i32_eqz(&mut self) -> Self::Output {
        self.visit_eqz()
    }

    fn visit_i32_eq(&mut self) -> Self::Output {
        self.visit_bin_op("==", true, true)
    }

    fn visit_i32_ne(&mut self) -> Self::Output {
        self.visit_bin_op("!=", true, true)
    }

    fn visit_i32_lt_s(&mut self) -> Self::Output {
        self.visit_bin_op("<", true, true)
    }

    fn visit_i32_lt_u(&mut self) -> Self::Output {
        self.visit_bin_op("<", true, false)
    }

    fn visit_i32_gt_s(&mut self) -> Self::Output {
        self.visit_bin_op(">", true, true)
    }

    fn visit_i32_gt_u(&mut self) -> Self::Output {
        self.visit_bin_op(">", true, false)
    }

    fn visit_i32_le_s(&mut self) -> Self::Output {
        self.visit_bin_op("<=", true, true)
    }

    fn visit_i32_le_u(&mut self) -> Self::Output {
        self.visit_bin_op("<=", true, false)
    }

    fn visit_i32_ge_s(&mut self) -> Self::Output {
        self.visit_bin_op(">=", true, true)
    }

    fn visit_i32_ge_u(&mut self) -> Self::Output {
        self.visit_bin_op(">=", true, false)
    }

    fn visit_i64_eqz(&mut self) -> Self::Output {
        self.visit_eqz()
    }

    fn visit_i64_eq(&mut self) -> Self::Output {
        self.visit_bin_op("==", true, true)
    }

    fn visit_i64_ne(&mut self) -> Self::Output {
        self.visit_bin_op("!=", true, true)
    }

    fn visit_i64_lt_s(&mut self) -> Self::Output {
        self.visit_bin_op("<", true, true)
    }

    fn visit_i64_lt_u(&mut self) -> Self::Output {
        self.visit_bin_op("<", true, false)
    }

    fn visit_i64_gt_s(&mut self) -> Self::Output {
        self.visit_bin_op(">", true, true)
    }

    fn visit_i64_gt_u(&mut self) -> Self::Output {
        self.visit_bin_op(">", true, false)
    }

    fn visit_i64_le_s(&mut self) -> Self::Output {
        self.visit_bin_op("<=", true, true)
    }

    fn visit_i64_le_u(&mut self) -> Self::Output {
        self.visit_bin_op("<=", true, false)
    }

    fn visit_i64_ge_s(&mut self) -> Self::Output {
        self.visit_bin_op(">=", true, true)
    }

    fn visit_i64_ge_u(&mut self) -> Self::Output {
        self.visit_bin_op(">=", true, false)
    }

    fn visit_f32_eq(&mut self) -> Self::Output {
        self.visit_bin_op("==", true, true)
    }

    fn visit_f32_ne(&mut self) -> Self::Output {
        self.visit_bin_op("!=", true, true)
    }

    fn visit_f32_lt(&mut self) -> Self::Output {
        self.visit_bin_op("<", true, true)
    }

    fn visit_f32_gt(&mut self) -> Self::Output {
        self.visit_bin_op(">", true, true)
    }

    fn visit_f32_le(&mut self) -> Self::Output {
        self.visit_bin_op("<=", true, true)
    }

    fn visit_f32_ge(&mut self) -> Self::Output {
        self.visit_bin_op(">=", true, true)
    }

    fn visit_f64_eq(&mut self) -> Self::Output {
        self.visit_bin_op("==", true, true)
    }

    fn visit_f64_ne(&mut self) -> Self::Output {
        self.visit_bin_op("!=", true, true)
    }

    fn visit_f64_lt(&mut self) -> Self::Output {
        self.visit_bin_op("<", true, true)
    }

    fn visit_f64_gt(&mut self) -> Self::Output {
        self.visit_bin_op(">", true, true)
    }

    fn visit_f64_le(&mut self) -> Self::Output {
        self.visit_bin_op("<=", true, true)
    }

    fn visit_f64_ge(&mut self) -> Self::Output {
        self.visit_bin_op(">=", true, true)
    }

    fn visit_i32_clz(&mut self) -> Self::Output {
        self.visit_clz()
    }

    fn visit_i32_ctz(&mut self) -> Self::Output {
        self.visit_ctz()
    }

    fn visit_i32_popcnt(&mut self) -> Self::Output {
        self.visit_popcnt()
    }

    fn visit_i32_add(&mut self) -> Self::Output {
        self.visit_bin_op("+", false, true)
    }

    fn visit_i32_sub(&mut self) -> Self::Output {
        self.visit_bin_op("-", false, true)
    }

    fn visit_i32_mul(&mut self) -> Self::Output {
        self.visit_bin_op("*", false, true)
    }

    fn visit_i32_div_s(&mut self) -> Self::Output {
        self.visit_bin_op("/", false, true)
    }

    fn visit_i32_div_u(&mut self) -> Self::Output {
        self.visit_bin_op("/", false, false)
    }

    fn visit_i32_rem_s(&mut self) -> Self::Output {
        self.visit_bin_op("%", false, true)
    }

    fn visit_i32_rem_u(&mut self) -> Self::Output {
        self.visit_rem_op(false)
    }

    fn visit_i32_and(&mut self) -> Self::Output {
        self.visit_bin_op("&", false, true)
    }

    fn visit_i32_or(&mut self) -> Self::Output {
        self.visit_bin_op("|", false, true)
    }

    fn visit_i32_xor(&mut self) -> Self::Output {
        self.visit_bin_op("^", false, true)
    }

    fn visit_i32_shl(&mut self) -> Self::Output {
        self.visit_shift_op("<<", true)
    }

    fn visit_i32_shr_s(&mut self) -> Self::Output {
        self.visit_shift_op(">>", true)
    }

    fn visit_i32_shr_u(&mut self) -> Self::Output {
        self.visit_shift_op(">>", false)
    }

    fn visit_i32_rotl(&mut self) -> Self::Output {
        self.visit_rot_op(false)
    }

    fn visit_i32_rotr(&mut self) -> Self::Output {
        self.visit_rot_op(true)
    }

    fn visit_i64_clz(&mut self) -> Self::Output {
        self.visit_clz()
    }

    fn visit_i64_ctz(&mut self) -> Self::Output {
        self.visit_ctz()
    }

    fn visit_i64_popcnt(&mut self) -> Self::Output {
        self.visit_popcnt()
    }

    fn visit_i64_add(&mut self) -> Self::Output {
        self.visit_bin_op("+", false, true)
    }

    fn visit_i64_sub(&mut self) -> Self::Output {
        self.visit_bin_op("-", false, true)
    }

    fn visit_i64_mul(&mut self) -> Self::Output {
        self.visit_bin_op("*", false, true)
    }

    fn visit_i64_div_s(&mut self) -> Self::Output {
        self.visit_bin_op("/", false, true)
    }

    fn visit_i64_div_u(&mut self) -> Self::Output {
        self.visit_bin_op("/", false, false)
    }

    fn visit_i64_rem_s(&mut self) -> Self::Output {
        self.visit_rem_op(true)
    }

    fn visit_i64_rem_u(&mut self) -> Self::Output {
        self.visit_rem_op(false)
    }

    fn visit_i64_and(&mut self) -> Self::Output {
        self.visit_bin_op("&", false, true)
    }

    fn visit_i64_or(&mut self) -> Self::Output {
        self.visit_bin_op("|", false, true)
    }

    fn visit_i64_xor(&mut self) -> Self::Output {
        self.visit_bin_op("^", false, true)
    }

    fn visit_i64_shl(&mut self) -> Self::Output {
        self.visit_shift_op("<<", true)
    }

    fn visit_i64_shr_s(&mut self) -> Self::Output {
        self.visit_shift_op(">>", true)
    }

    fn visit_i64_shr_u(&mut self) -> Self::Output {
        self.visit_shift_op(">>", false)
    }

    fn visit_i64_rotl(&mut self) -> Self::Output {
        self.visit_rot_op(false)
    }

    fn visit_i64_rotr(&mut self) -> Self::Output {
        self.visit_rot_op(true)
    }

    fn visit_f32_abs(&mut self) -> Self::Output {
        self.visit_math_un_op("Abs")
    }

    fn visit_f32_neg(&mut self) -> Self::Output {
        self.visit_un_op("-")
    }

    fn visit_f32_ceil(&mut self) -> Self::Output {
        self.visit_math_un_op("Ceiling")
    }

    fn visit_f32_floor(&mut self) -> Self::Output {
        self.visit_math_un_op("Floor")
    }

    fn visit_f32_trunc(&mut self) -> Self::Output {
        let (opnd, result) = self.un_op_vars_auto_ty();
        self.push_line(format!(
            "{result} = {}(Math.Truncate({}({opnd})));",
            cast(CsType::Float),
            cast(CsType::Double)
        ));
        Ok(())
    }

    fn visit_f32_nearest(&mut self) -> Self::Output {
        self.visit_math_un_op("Round")
    }

    fn visit_f32_sqrt(&mut self) -> Self::Output {
        self.visit_math_un_op("Sqrt")
    }

    fn visit_f32_add(&mut self) -> Self::Output {
        self.visit_bin_op("+", false, true)
    }

    fn visit_f32_sub(&mut self) -> Self::Output {
        self.visit_bin_op("-", false, true)
    }

    fn visit_f32_mul(&mut self) -> Self::Output {
        self.visit_bin_op("*", false, true)
    }

    fn visit_f32_div(&mut self) -> Self::Output {
        self.visit_bin_op("/", false, true)
    }

    fn visit_f32_min(&mut self) -> Self::Output {
        self.visit_math_bin_op("Min")
    }

    fn visit_f32_max(&mut self) -> Self::Output {
        self.visit_math_bin_op("Max")
    }

    fn visit_f32_copysign(&mut self) -> Self::Output {
        self.visit_copysign_op()
    }

    fn visit_f64_abs(&mut self) -> Self::Output {
        self.visit_math_un_op("Abs")
    }

    fn visit_f64_neg(&mut self) -> Self::Output {
        self.visit_un_op("-")
    }

    fn visit_f64_ceil(&mut self) -> Self::Output {
        self.visit_math_un_op("Ceiling")
    }

    fn visit_f64_floor(&mut self) -> Self::Output {
        self.visit_math_un_op("Floor")
    }

    fn visit_f64_trunc(&mut self) -> Self::Output {
        self.visit_math_un_op("Truncate")
    }

    fn visit_f64_nearest(&mut self) -> Self::Output {
        self.visit_math_un_op("Round")
    }

    fn visit_f64_sqrt(&mut self) -> Self::Output {
        self.visit_math_un_op("Sqrt")
    }

    fn visit_f64_add(&mut self) -> Self::Output {
        self.visit_bin_op("+", false, true)
    }

    fn visit_f64_sub(&mut self) -> Self::Output {
        self.visit_bin_op("-", false, true)
    }

    fn visit_f64_mul(&mut self) -> Self::Output {
        self.visit_bin_op("*", false, true)
    }

    fn visit_f64_div(&mut self) -> Self::Output {
        self.visit_bin_op("/", false, true)
    }

    fn visit_f64_min(&mut self) -> Self::Output {
        self.visit_math_bin_op("Min")
    }

    fn visit_f64_max(&mut self) -> Self::Output {
        self.visit_math_bin_op("Max")
    }

    fn visit_f64_copysign(&mut self) -> Self::Output {
        self.visit_copysign_op()
    }

    fn visit_i32_wrap_i64(&mut self) -> Self::Output {
        let (opnd, result) = self.un_op_vars(CsType::Int);
        self.wrap(opnd, result);
        self.push_line(format!(
            "{result} = {}({opnd} & 0x7fffffff);",
            cast(CsType::Int)
        ));
        self.push_line(format!(
            "if (({opnd} & 0x80000000) != 0) {result} |= -0x7fffffff - 1;"
        ));
        Ok(())
    }

    fn visit_i32_trunc_f32_s(&mut self) -> Self::Output {
        self.visit_cast_trunc(CsType::Int, true)
    }

    fn visit_i32_trunc_f32_u(&mut self) -> Self::Output {
        self.visit_cast_trunc(CsType::Int, false)
    }

    fn visit_i32_trunc_f64_s(&mut self) -> Self::Output {
        self.visit_cast_trunc(CsType::Int, true)
    }

    fn visit_i32_trunc_f64_u(&mut self) -> Self::Output {
        self.visit_cast_trunc(CsType::Int, false)
    }

    fn visit_i64_extend_i32_s(&mut self) -> Self::Output {
        self.visit_cast_extend(true)
    }

    fn visit_i64_extend_i32_u(&mut self) -> Self::Output {
        self.visit_cast_extend(false)
    }

    fn visit_i64_trunc_f32_s(&mut self) -> Self::Output {
        self.visit_cast_trunc(CsType::Long, true)
    }

    fn visit_i64_trunc_f32_u(&mut self) -> Self::Output {
        self.visit_cast_trunc(CsType::Long, false)
    }

    fn visit_i64_trunc_f64_s(&mut self) -> Self::Output {
        self.visit_cast_trunc(CsType::Long, true)
    }

    fn visit_i64_trunc_f64_u(&mut self) -> Self::Output {
        self.visit_cast_trunc(CsType::Long, false)
    }

    fn visit_f32_convert_i32_s(&mut self) -> Self::Output {
        self.visit_cast_convert(CsType::Float, true)
    }

    fn visit_f32_convert_i32_u(&mut self) -> Self::Output {
        self.visit_cast_convert(CsType::Float, false)
    }

    fn visit_f32_convert_i64_s(&mut self) -> Self::Output {
        self.visit_cast_convert(CsType::Float, true)
    }

    fn visit_f32_convert_i64_u(&mut self) -> Self::Output {
        self.visit_cast_convert(CsType::Float, false)
    }

    fn visit_f32_demote_f64(&mut self) -> Self::Output {
        self.visit_cast(CsType::Float)
    }

    fn visit_f64_convert_i32_s(&mut self) -> Self::Output {
        self.visit_cast_convert(CsType::Double, true)
    }

    fn visit_f64_convert_i32_u(&mut self) -> Self::Output {
        self.visit_cast_convert(CsType::Double, false)
    }

    fn visit_f64_convert_i64_s(&mut self) -> Self::Output {
        self.visit_cast_convert(CsType::Double, true)
    }

    fn visit_f64_convert_i64_u(&mut self) -> Self::Output {
        self.visit_cast_convert(CsType::Double, false)
    }

    fn visit_f64_promote_f32(&mut self) -> Self::Output {
        self.visit_cast(CsType::Double)
    }

    fn visit_i32_reinterpret_f32(&mut self) -> Self::Output {
        let (opnd, result) = self.un_op_vars(CsType::Int);
        self.float_to_bits(opnd, result);
        Ok(())
    }

    fn visit_i64_reinterpret_f64(&mut self) -> Self::Output {
        let (opnd, result) = self.un_op_vars(CsType::Long);
        self.float_to_bits(opnd, result);
        Ok(())
    }

    fn visit_f32_reinterpret_i32(&mut self) -> Self::Output {
        let (opnd, result) = self.un_op_vars(CsType::Float);
        self.bits_to_float(opnd, result);
        Ok(())
    }

    fn visit_f64_reinterpret_i64(&mut self) -> Self::Output {
        let (opnd, result) = self.un_op_vars(CsType::Double);
        self.bits_to_float(opnd, result);
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum OperatorError {
    #[error("`{0}` proposal is not implemented")]
    NotSupported(&'static str),
}

fn ty_f_to_i(ty: CsType) -> CsType {
    match ty {
        CsType::Float => CsType::Int,
        CsType::Double => CsType::Long,
        _ => panic!("Specify float type as argument"),
    }
}

fn get_int_bits(ty: CsType) -> u32 {
    match ty {
        CsType::Int | CsType::UInt => i32::BITS,
        CsType::Long | CsType::ULong => i64::BITS,
        _ => panic!("Specify integer type as argument"),
    }
}

fn get_frac_bits(ty: CsType) -> u32 {
    match ty {
        CsType::Float => f32::MANTISSA_DIGITS - 1,
        CsType::Double => f64::MANTISSA_DIGITS - 1,
        _ => panic!("Specify float type as argument"),
    }
}

fn cast(ty: CsType) -> &'static str {
    match ty {
        CsType::Void => panic!("Unsupported conversion"),
        CsType::Bool => "Convert.ToBoolean",
        CsType::Byte => "Convert.ToByte",
        CsType::Int => "Convert.ToInt32",
        CsType::UInt => "Convert.ToUInt32",
        CsType::Long => "Convert.ToInt64",
        CsType::ULong => "Convert.ToUInt64",
        CsType::Float => "Convert.ToSingle",
        CsType::Double => "Convert.ToDouble",
    }
}

fn cast_from(from: CsType, to: CsType) -> &'static str {
    if from == to {
        ""
    } else {
        cast(to)
    }
}
