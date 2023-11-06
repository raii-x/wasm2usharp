use anyhow::Result;
use wasmparser::{
    for_each_operator, BlockType, BrTable, FuncType, FunctionBody, Ieee32, Ieee64, MemArg,
    VisitOperator,
};

use crate::ir::{
    func::{Code, Const, Expr, Instr, Var},
    module::Module,
    trap,
    ty::CsType,
    BREAK_DEPTH, LOOP, PAGE_SIZE,
};

use super::builtin_func;

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
    func: usize,
    blocks: Vec<Block>,
    code: Code,
    /// brの後など、到達不可能なコードの処理時に1加算。
    /// unreachableが1以上の場合のブロックの出現ごとに1加算。
    /// ブロックの終了時に1減算。
    unreachable: i32,
}

#[derive(Debug)]
struct Block {
    stack: Vec<Expr>,
    result: Option<Var>,
    loop_var: Option<usize>,
}

impl<'input, 'module> CodeConverter<'input, 'module> {
    pub(super) fn new(module: &'module Module<'input>, func: usize) -> Self {
        let code = Code::new(&module.all_funcs[func].header);
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
            let func = &self.module.all_funcs[self.func];
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

    fn push_stack(&mut self, val: Expr) {
        self.blocks.last_mut().unwrap().stack.push(val)
    }

    fn pop_stack(&mut self) -> Expr {
        self.blocks.last_mut().unwrap().stack.pop().unwrap()
    }

    fn last_stack(&self) -> Expr {
        *self.blocks.last().unwrap().stack.last().unwrap()
    }

    fn visit_load(
        &mut self,
        memarg: MemArg,
        ty: CsType,
        name: &str,
    ) -> <Self as VisitOperator<'_>>::Output {
        let idx = self.pop_stack();
        let result = self.code.new_var(ty);
        self.push_stack(result.into());

        self.push_line(if memarg.offset == 0 {
            format!("{result} = {name}({idx});")
        } else {
            format!("{result} = {name}({idx} + {});", memarg.offset as i32)
        });
        Ok(())
    }

    fn visit_store(&mut self, memarg: MemArg, name: &str) -> <Self as VisitOperator<'_>>::Output {
        let var = self.pop_stack();
        let idx = self.pop_stack();

        self.push_line(if memarg.offset == 0 {
            format!("{name}({idx}, {var});")
        } else {
            format!("{name}({idx} + {}, {var});", memarg.offset as i32)
        });
        Ok(())
    }

    /// (opnd, result)を返す
    fn un_op_vars(&mut self, result_ty: CsType) -> (Expr, Var) {
        let opnd = self.pop_stack();
        let result = self.code.new_var(result_ty);
        self.push_stack(result.into());

        (opnd, result)
    }

    /// (opnd, result)を返し、opndの型をresultの型とする
    fn un_op_vars_auto_ty(&mut self) -> (Expr, Var) {
        let opnd = self.pop_stack();
        let result = self.code.new_var(opnd.ty());
        self.push_stack(result.into());

        (opnd, result)
    }

    /// (lhs, rhs, result)を返し、lhsの型をresultの型とする
    fn bin_op_vars_auto_ty(&mut self) -> (Expr, Expr, Var) {
        let rhs = self.pop_stack();
        let lhs = self.pop_stack();
        let result = self.code.new_var(lhs.ty());
        self.push_stack(result.into());

        (lhs, rhs, result)
    }

    fn visit_eqz(&mut self) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars(CsType::Int);

        self.push_line(format!("{result} = {}({opnd} == 0);", result.ty.cast()));
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
            .new_var(if logical { CsType::Int } else { lhs.ty() });
        self.push_stack(result.into());

        if signed {
            if logical {
                self.push_line(format!(
                    "{result} = {}({lhs} {op} {rhs});",
                    result.ty.cast()
                ));
            } else {
                self.push_line(format!("{result} = {lhs} {op} {rhs};"));
            }
        } else {
            let lhs_u = self.code.new_var(lhs.ty().to_unsigned());
            let rhs_u = self.code.new_var(rhs.ty().to_unsigned());
            let tmp = self
                .code
                .new_var(if logical { CsType::Bool } else { lhs_u.ty });

            self.cast_sign(lhs, lhs_u);
            self.cast_sign(rhs, rhs_u);
            self.push_line(format!("{tmp} = {lhs_u} {op} {rhs_u};"));

            if logical {
                self.push_line(format!("{result} = {}({tmp});", result.ty.cast()));
            } else {
                self.cast_sign(tmp.into(), result);
            }
        }

        Ok(())
    }

    fn visit_rem_op(&mut self, signed: bool) -> <Self as VisitOperator<'_>>::Output {
        let (lhs, rhs, result) = self.bin_op_vars_auto_ty();

        if signed {
            self.push_line(format!("{result} = {lhs} - {lhs} / {rhs} * {rhs};"));
        } else {
            let lhs_u = self.code.new_var(lhs.ty().to_unsigned());
            let rhs_u = self.code.new_var(rhs.ty().to_unsigned());
            let tmp = self.code.new_var(lhs_u.ty);

            self.cast_sign(lhs, lhs_u);
            self.cast_sign(rhs, rhs_u);
            self.push_line(format!("{tmp} = {lhs_u} - {lhs_u} / {rhs_u} * {rhs_u};"));
            self.cast_sign(tmp.into(), result);
        };

        Ok(())
    }

    fn cast_sign(&mut self, opnd: Expr, result: Var) {
        let bits = result.ty.int_bits();
        let only_msb = 1u64 << (bits - 1);
        let except_msb = only_msb - 1;
        let cast_ty = result.ty.cast();

        if result.ty.signed() {
            self.push_line(format!("{result} = {opnd} >= {only_msb} ? {cast_ty}({opnd} - {only_msb}) | (-{except_msb} - 1) : {cast_ty}({opnd});"));
        } else {
            self.push_line(format!("{result} = {opnd} < 0 ? {cast_ty}({opnd} & {except_msb}) | {only_msb} : {cast_ty}({opnd});"));
        }
    }

    fn visit_shift_op(&mut self, op: &str, signed: bool) -> <Self as VisitOperator<'_>>::Output {
        let (lhs, rhs, result) = self.bin_op_vars_auto_ty();

        let rhs_int = if rhs.ty() == CsType::Int {
            rhs
        } else {
            let rhs_int = self.code.new_var(CsType::Int);
            self.wrap(rhs, rhs_int);
            rhs_int.into()
        };

        if signed {
            self.push_line(format!("{result} = {lhs} {op} {rhs_int};"));
        } else {
            self.shr_u(lhs, rhs_int, result);
        }
        Ok(())
    }

    fn shr_u(&mut self, lhs: Expr, rhs_int: Expr, result: Var) {
        let bits = lhs.ty().int_bits();
        let only_msb = 1u64 << (bits - 1);
        let except_msb = only_msb - 1;

        self.push_line(format!(
            "{result} = {lhs} < 0 ? (({lhs} & {except_msb}) >> {rhs_int}) | ({} << (-1 - {rhs_int})) : {lhs} >> {rhs_int};",
            if lhs.ty() == CsType::Long { "1L" } else { "1" }
        ));
    }

    fn visit_rot_op(&mut self, right: bool) -> <Self as VisitOperator<'_>>::Output {
        let (lhs, rhs, result) = self.bin_op_vars_auto_ty();

        let bits = lhs.ty().int_bits();

        let rhs_int = if rhs.ty() == CsType::Int {
            rhs
        } else {
            let rhs_int = self.code.new_var(CsType::Int);
            self.wrap(rhs, rhs_int);
            rhs_int.into()
        };

        let bits_m_rhs = self.code.new_var(CsType::Int);
        self.push_line(format!("{bits_m_rhs} = {bits} - {rhs_int};"));

        let shr = self.code.new_var(lhs.ty());
        if right {
            self.shr_u(lhs, rhs_int, shr);
            self.push_line(format!("{result} = {shr} | ({lhs} << {bits_m_rhs});"));
        } else {
            self.shr_u(lhs, bits_m_rhs.into(), shr);
            self.push_line(format!("{result} = ({lhs} << {rhs_int}) | {shr};"));
        }
        Ok(())
    }

    fn visit_clz(&mut self) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars_auto_ty();

        let bits = opnd.ty().int_bits();

        self.push_line(format!("if ({opnd} == 0) {result} = {bits};"));
        let cast_ty = cast_from(CsType::Int, opnd.ty());
        // 2進で文字列化して文字数を数える
        self.push_line(format!(
            "else {result} = {cast_ty}({bits} - Convert.ToString({opnd}, 2).Length);",
        ));
        Ok(())
    }

    fn visit_ctz(&mut self) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars_auto_ty();

        let bits = opnd.ty().int_bits();

        self.push_line(format!("if ({opnd} == 0) {result} = {bits};"));
        let cast_ty = cast_from(CsType::Int, opnd.ty());

        // 符号付き整数の最小値のリテラルはUdonSharpではエラーとなるので
        // `-最大値 - 1` で表現する
        let or_opnd = match opnd.ty() {
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

        let cast_ty = cast_from(CsType::Int, opnd.ty());
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
            self.module.math_class(opnd.ty())
        ));
        Ok(())
    }

    fn visit_math_bin_op(&mut self, func: &str) -> <Self as VisitOperator<'_>>::Output {
        let (lhs, rhs, result) = self.bin_op_vars_auto_ty();

        self.push_line(format!(
            "{result} = {}.{func}({lhs}, {rhs});",
            self.module.math_class(lhs.ty())
        ));
        Ok(())
    }

    fn visit_copysign_op(&mut self) -> <Self as VisitOperator<'_>>::Output {
        let (lhs, rhs, result) = self.bin_op_vars_auto_ty();

        self.push_line(format!(
            "{result} = {0}.Abs({lhs}) * (({rhs} == 0 ? 1 / {rhs} : {rhs}) > 0 ? 1 : -1);",
            self.module.math_class(lhs.ty())
        ));
        Ok(())
    }

    fn visit_cast(&mut self, result_ty: CsType) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars(result_ty);
        let cast_ty = result_ty.cast();

        self.push_line(format!("{result} = {cast_ty}({opnd});"));
        Ok(())
    }

    fn wrap(&mut self, opnd: Expr, result: Var) {
        self.push_line(format!(
            "{result} = {}({opnd} & 0x7fffffff);",
            CsType::Int.cast()
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
            cast_from(opnd.ty(), CsType::Double)
        ));

        if signed {
            let cast_ty = result_ty.cast();
            self.push_line(format!("{result} = {cast_ty}({tmp});"));
        } else {
            self.cast_sign(tmp.into(), result);
        }
        Ok(())
    }

    fn visit_cast_extend(&mut self, signed: bool) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars(CsType::Long);
        let cast_ty = CsType::Long.cast();

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

        let cast_ty = result_ty.cast();
        if signed {
            self.push_line(format!("{result} = {cast_ty}({opnd});"));
        } else {
            let bits = opnd.ty().int_bits();
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
                self.push_stack(var.into());
                Some(var)
            }
            _ => {
                panic!("Multiple return values are not supported")
            }
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
                self.push_stack(result.into());
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

        let results_len = self.module.all_funcs[self.func].header.ty.results().len();
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
        let index = function_index as usize;
        let func = &self.module.all_funcs[index];
        let ty = func.header.ty.clone();

        // mapは遅延評価であるため、mapの後にrevを呼んだ場合ではpop_stackで得る値が逆順にならない
        // そのため、一度collectでVecにした後、reverseで逆順にしている
        let mut params: Vec<Expr> = ty.params().iter().map(|_| self.pop_stack()).collect();
        params.reverse();

        let result = self.get_result(&ty);

        self.code.instrs.push(Instr::Call {
            func: index,
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

        let index = self.module.call_indirects[type_index as usize];
        let ty = self.module.types[type_index as usize].clone();

        // Iteratorのrevを使わない理由はvisit_callのコメントを参照
        let mut params: Vec<Expr> = (0..ty.params().len() + 1)
            .map(|_| self.pop_stack())
            .collect();
        params.reverse();

        let result = self.get_result(&ty);

        self.code.instrs.push(Instr::Call {
            func: index,
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

        let select = self.code.new_var(val1.ty());
        self.push_stack(select.into());

        self.push_line(format!("{select} = {c} != 0 ? {val1} : {val2};"));

        Ok(())
    }

    fn visit_local_get(&mut self, local_index: u32) -> Self::Output {
        let local = self.code.vars[local_index as usize];
        let var = self.code.new_var(local.ty);
        self.push_stack(var.into());

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
        self.push_stack(var.into());

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
        self.visit_load(memarg, CsType::Int, builtin_func::I32_LOAD)
    }

    fn visit_i64_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Long, builtin_func::I64_LOAD)
    }

    fn visit_f32_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Float, builtin_func::F32_LOAD)
    }

    fn visit_f64_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Double, builtin_func::F64_LOAD)
    }

    fn visit_i32_load8_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Int, builtin_func::I32_LOAD8_S)
    }

    fn visit_i32_load8_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Int, builtin_func::I32_LOAD8_U)
    }

    fn visit_i32_load16_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Int, builtin_func::I32_LOAD16_S)
    }

    fn visit_i32_load16_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Int, builtin_func::I32_LOAD16_U)
    }

    fn visit_i64_load8_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Long, builtin_func::I64_LOAD8_S)
    }

    fn visit_i64_load8_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Long, builtin_func::I64_LOAD8_U)
    }

    fn visit_i64_load16_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Long, builtin_func::I64_LOAD16_S)
    }

    fn visit_i64_load16_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Long, builtin_func::I64_LOAD16_U)
    }

    fn visit_i64_load32_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Long, builtin_func::I64_LOAD32_S)
    }

    fn visit_i64_load32_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Long, builtin_func::I64_LOAD32_U)
    }

    fn visit_i32_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, builtin_func::I32_STORE)
    }

    fn visit_i64_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, builtin_func::I64_STORE)
    }

    fn visit_f32_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, builtin_func::F32_STORE)
    }

    fn visit_f64_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, builtin_func::F64_STORE)
    }

    fn visit_i32_store8(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, builtin_func::I32_STORE8)
    }

    fn visit_i32_store16(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, builtin_func::I32_STORE16)
    }

    fn visit_i64_store8(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, builtin_func::I64_STORE8)
    }

    fn visit_i64_store16(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, builtin_func::I64_STORE16)
    }

    fn visit_i64_store32(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, builtin_func::I64_STORE32)
    }

    fn visit_memory_size(&mut self, mem: u32, mem_byte: u8) -> Self::Output {
        if mem != 0 {
            panic!("Multi memory is not supported")
        }
        assert!(mem_byte == 0);

        let var = self.code.new_var(CsType::Int);
        self.push_stack(var.into());

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
        self.push_stack(var.into());

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
        self.push_stack(Const::Int(value).into());
        Ok(())
    }

    fn visit_i64_const(&mut self, value: i64) -> Self::Output {
        self.push_stack(Const::Long(value).into());
        Ok(())
    }

    fn visit_f32_const(&mut self, value: Ieee32) -> Self::Output {
        self.push_stack(Const::Float(f32::from_bits(value.bits())).into());
        Ok(())
    }

    fn visit_f64_const(&mut self, value: Ieee64) -> Self::Output {
        self.push_stack(Const::Double(f64::from_bits(value.bits())).into());
        Ok(())
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
            CsType::Float.cast(),
            CsType::Double.cast()
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
        self.push_line(format!(
            "{result} = {}({opnd});",
            builtin_func::I32_REINTERPRET_F32
        ));
        Ok(())
    }

    fn visit_i64_reinterpret_f64(&mut self) -> Self::Output {
        let (opnd, result) = self.un_op_vars(CsType::Long);
        self.push_line(format!(
            "{result} = {}({opnd});",
            builtin_func::I64_REINTERPRET_F64
        ));
        Ok(())
    }

    fn visit_f32_reinterpret_i32(&mut self) -> Self::Output {
        let (opnd, result) = self.un_op_vars(CsType::Float);
        self.push_line(format!(
            "{result} = {}({opnd});",
            builtin_func::F32_REINTERPRET_I32
        ));
        Ok(())
    }

    fn visit_f64_reinterpret_i64(&mut self) -> Self::Output {
        let (opnd, result) = self.un_op_vars(CsType::Double);
        self.push_line(format!(
            "{result} = {}({opnd});",
            builtin_func::F64_REINTERPRET_I64
        ));
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum OperatorError {
    #[error("`{0}` proposal is not implemented")]
    NotSupported(&'static str),
}

fn cast_from(from: CsType, to: CsType) -> &'static str {
    if from == to {
        ""
    } else {
        to.cast()
    }
}
