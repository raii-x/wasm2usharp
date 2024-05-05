use std::{collections::HashSet, vec};

use anyhow::Result;
use wasmparser::{
    for_each_operator, BlockType, BrTable, FuncType, FunctionBody, Ieee32, Ieee64, MemArg,
    StorageType, ValType, VisitOperator,
};

use crate::ir::{
    builder::Builder,
    code::{Breakable, Call, Code, Inst, InstKind},
    module::Module,
    trap,
    ty::{Const, CsType},
    var::{Primary, Var, VarId},
    PAGE_SIZE,
};

use super::pool::{PoolPrimary, PoolVar, VarPool};

macro_rules! define_single_visit_operator {
    ( @mvp $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident) => {};
    ( @sign_extension $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident) => {};
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

pub struct CodeParser<'input, 'module> {
    module: &'module Module<'input>,
    func: usize,
    blocks: Vec<Block>,
    builder: Builder,
    /// brの後など、到達不可能なコードの処理時に1加算。
    /// unreachableが1以上の場合のブロックの出現ごとに1加算。
    /// ブロックの終了時に1減算。
    unreachable: i32,
    var_pool: VarPool,
}

struct Block {
    stack: Vec<PoolPrimary>,
    result: Option<PoolVar>,
    loop_var: Option<usize>,
}

impl<'input, 'module> CodeParser<'input, 'module> {
    pub(super) fn new(module: &'module Module<'input>, func: usize) -> Self {
        let header = &module.all_funcs[func].header;
        Self {
            module,
            func,
            blocks: Vec::new(),
            builder: Builder::new(header.ty.params()),
            unreachable: 0,
            var_pool: VarPool::new(),
        }
    }

    pub fn parse(mut self, body: FunctionBody<'_>) -> Result<Code> {
        for local in body.get_locals_reader()? {
            let (count, ty) = local?;
            for _ in 0..count {
                let cs_ty = CsType::get(ty);
                self.builder.new_var(Var {
                    ty: cs_ty,
                    local: true,
                    default: Some(cs_ty.default()),
                    ..Default::default()
                });
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
        let result_var = self.blocks[0].result.as_ref().map(|x| x.id());

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
            self.builder.push_return(Some(res.into()));
        }

        assert!(self.blocks.is_empty());

        #[cfg(debug_assertions)]
        self.var_pool.validate();

        Ok(self.builder.build())
    }

    fn new_block(&mut self, blockty: BlockType, is_loop: bool) -> &Block {
        let result = match blockty {
            BlockType::Empty => None,
            BlockType::Type(ty) => {
                let cs_ty = CsType::get(ty);
                Some(
                    self.var_pool
                        .take(&mut self.builder, cs_ty, Some(cs_ty.default())),
                )
            }
            BlockType::FuncType(..) => panic!("func type blocks are not supported"),
        };

        let loop_var = if is_loop {
            Some({
                self.builder.code.loop_var_count += 1;
                self.builder.code.loop_var_count - 1
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

        if let Some(result) = &upper_block.result {
            let current_block = &self.blocks[self.blocks.len() - 1];
            let rhs = current_block.stack.last().unwrap();
            self.builder.push_set(result.id(), rhs.into());
        }
    }

    fn push_stack(&mut self, val: PoolPrimary) {
        self.blocks.last_mut().unwrap().stack.push(val)
    }

    fn pop_stack(&mut self) -> PoolPrimary {
        let prim = self.blocks.last_mut().unwrap().stack.pop().unwrap();
        prim
    }

    fn last_stack(&self) -> &PoolPrimary {
        self.blocks.last().unwrap().stack.last().unwrap()
    }

    fn ty(&self, primary: &PoolPrimary) -> CsType {
        Primary::from(primary).ty(&self.builder.code.vars)
    }

    fn call(&mut self, index: usize, ty: FuncType, params: Vec<PoolPrimary>) {
        let save_vars = self.get_save_vars();
        let save_loop_vars = self.get_save_loop_vars();

        let result = self.get_result(&ty);

        let call = Call {
            func: index,
            recursive: false,
            save_vars,
            save_loop_vars,
        };

        let params = params.into_iter().map(|x| x.into()).collect();
        self.builder.push_call(call, params, result);
    }

    fn get_save_vars(&self) -> HashSet<VarId> {
        let locals = self
            .builder
            .code
            .vars
            .iter()
            .filter(|(_, var)| var.local)
            .map(|(id, _)| id);

        let stack_vars = self.blocks.iter().flat_map(|block| {
            block.stack.iter().filter_map(|prim| match prim {
                PoolPrimary::Pool(x) => Some(x.id()),
                PoolPrimary::Local(x) => Some(*x),
                PoolPrimary::Const(_) => None,
            })
        });

        locals.chain(stack_vars).collect()
    }

    fn get_save_loop_vars(&self) -> HashSet<usize> {
        self.blocks
            .iter()
            .filter_map(|block| block.loop_var)
            .collect()
    }

    fn get_result(&mut self, ty: &FuncType) -> Option<VarId> {
        match ty.results().len() {
            0 => None,
            1 => {
                let var = self
                    .var_pool
                    .take(&mut self.builder, CsType::get(ty.results()[0]), None);
                let id = var.id();
                self.push_stack(var.into());
                Some(id)
            }
            _ => {
                panic!("Multiple return values are not supported")
            }
        }
    }

    fn visit_load(
        &mut self,
        memarg: MemArg,
        result_ty: CsType,
        storage_ty: StorageType,
        signed: Option<bool>,
    ) -> <Self as VisitOperator<'_>>::Output {
        use {StorageType::*, ValType::*};

        let idx = self.pop_stack();
        let result = self.var_pool.take(&mut self.builder, result_ty, None);

        let memory = &self.module.memory.as_ref().unwrap().name;
        let idx_pat = index_pattern(memarg.offset);

        let mut kind = InstKind::Expr;

        let pattern = match (storage_ty, signed) {
            (I8, Some(false)) => format!("{memory}[{idx_pat}]"),
            (I8, Some(true)) => {
                kind = InstKind::Stmt;
                format!("$r = {memory}[{idx_pat}]; if ($r >= 0x80) $r |= -0x100;")
            }
            (I16, Some(false)) => format!("BitConverter.ToUInt16({memory}, {idx_pat})"),
            (I16, Some(true)) => format!("BitConverter.ToInt16({memory}, {idx_pat})"),
            (Val(I32), Some(false)) => format!("BitConverter.ToUInt32({memory}, {idx_pat})"),
            (Val(I32), Some(true) | None) => format!("BitConverter.ToInt32({memory}, {idx_pat})"),
            (Val(I64), None) => format!("BitConverter.ToInt64({memory}, {idx_pat})"),
            (Val(F32), None) => format!("BitConverter.ToSingle({memory}, {idx_pat})"),
            (Val(F64), None) => format!("BitConverter.ToDouble({memory}, {idx_pat})"),
            _ => unreachable!(),
        };

        self.builder.push(Inst {
            kind,
            pattern,
            params: vec![idx.into()],
            result: Some(result.id()),
            ..Default::default()
        });

        self.push_stack(result.into());
        Ok(())
    }

    fn visit_store(
        &mut self,
        memarg: MemArg,
        storage_ty: StorageType,
    ) -> <Self as VisitOperator<'_>>::Output {
        use {StorageType::*, ValType::*};

        let var = self.pop_stack();
        let idx = self.pop_stack();

        let memory = &self.module.memory.as_ref().unwrap().name;
        let idx_pat = index_pattern(memarg.offset);

        let pattern = match storage_ty {
            I8 => format!("{memory}[{idx_pat}] = {}($p1 & 0xff);", CsType::Byte.cast()),
            I16 => format!("Array.Copy(BitConverter.GetBytes(Convert.ToUInt16($p1 & 0xffff)), 0, {memory}, {idx_pat}, 2);"),
            Val(I32) => match self.ty(&var) {
                CsType::Int => format!("Array.Copy(BitConverter.GetBytes($p1), 0, {memory}, {idx_pat}, 4);"),
                CsType::Long => format!("Array.Copy(BitConverter.GetBytes({}($p1 & 0xffffffff)), 0, {memory}, {idx_pat}, 4);", CsType::UInt.cast()),
                _ => unreachable!(),
            },
            Val(I64) => format!("Array.Copy(BitConverter.GetBytes($p1), 0, {memory}, {idx_pat}, 8);"),
            Val(F32) => format!("Array.Copy(BitConverter.GetBytes($p1), 0, {memory}, {idx_pat}, 4);"),
            Val(F64) => format!("Array.Copy(BitConverter.GetBytes($p1), 0, {memory}, {idx_pat}, 8);"),
            _ => unreachable!(),
        };

        self.builder.push(Inst {
            kind: InstKind::Stmt,
            pattern,
            params: vec![idx.into(), var.into()],
            ..Default::default()
        });
        Ok(())
    }

    /// (opnd, result)を返す
    fn un_op_vars(&mut self, result_ty: CsType) -> (PoolPrimary, VarId) {
        let opnd = self.pop_stack();
        let result = self.var_pool.take(&mut self.builder, result_ty, None);
        let result_id = result.id();
        self.push_stack(result.into());

        (opnd, result_id)
    }

    /// (opnd, result)を返し、opndの型をresultの型とする
    fn un_op_vars_auto_ty(&mut self) -> (PoolPrimary, VarId) {
        let opnd = self.pop_stack();
        let ty = self.ty(&opnd);
        let result = self.var_pool.take(&mut self.builder, ty, None);
        let result_id = result.id();
        self.push_stack(result.into());

        (opnd, result_id)
    }

    /// (lhs, rhs, result)を返し、lhsの型をresultの型とする
    fn bin_op_vars_auto_ty(&mut self) -> (PoolPrimary, PoolPrimary, VarId) {
        let rhs = self.pop_stack();
        let lhs = self.pop_stack();
        let ty = self.ty(&lhs);
        let result = self.var_pool.take(&mut self.builder, ty, None);
        let result_id = result.id();
        self.push_stack(result.into());

        (lhs, rhs, result_id)
    }

    fn visit_eqz(&mut self) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars(CsType::Int);

        self.builder.push_set_pattern(
            result,
            format!("{}($p0 == 0)", self.builder.code.vars[result].ty.cast()),
            vec![opnd.into()],
        );
        Ok(())
    }

    fn visit_un_op(&mut self, op: &str) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars_auto_ty();

        self.builder
            .push_set_pattern(result, format!("{op}$p0"), vec![opnd.into()]);
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
        let result_ty = if logical { CsType::Int } else { self.ty(&lhs) };
        let result = self.var_pool.take(&mut self.builder, result_ty, None);

        if signed {
            if logical {
                self.builder.push_set_pattern(
                    result.id(),
                    format!(
                        "{}($p0 {op} $p1)",
                        self.builder.code.vars[result.id()].ty.cast()
                    ),
                    vec![lhs.into(), rhs.into()],
                );
            } else {
                self.builder.push_set_pattern(
                    result.id(),
                    format!("$p0 {op} $p1"),
                    vec![lhs.into(), rhs.into()],
                );
            }
        } else {
            let lhs_u_ty = self.ty(&lhs).to_unsigned();
            let lhs_u = self.var_pool.take(&mut self.builder, lhs_u_ty, None);
            let rhs_u_ty = self.ty(&rhs).to_unsigned();
            let rhs_u = self.var_pool.take(&mut self.builder, rhs_u_ty, None);
            let tmp = self.var_pool.take(
                &mut self.builder,
                if logical { CsType::Bool } else { lhs_u_ty },
                None,
            );

            self.cast_sign(lhs, lhs_u.id());
            self.cast_sign(rhs, rhs_u.id());
            self.builder.push_set_pattern(
                tmp.id(),
                format!("$p0 {op} $p1"),
                vec![lhs_u.id().into(), rhs_u.id().into()],
            );

            if logical {
                self.builder.push_set_pattern(
                    result.id(),
                    format!("{}($p0)", self.builder.code.vars[result.id()].ty.cast()),
                    vec![tmp.id().into()],
                );
            } else {
                self.cast_sign(tmp.into(), result.id());
            }
        }

        self.push_stack(result.into());
        Ok(())
    }

    fn visit_rem_op(&mut self, signed: bool) -> <Self as VisitOperator<'_>>::Output {
        let (lhs, rhs, result) = self.bin_op_vars_auto_ty();

        if signed {
            self.builder.push_set_pattern(
                result,
                "$p0 - $p0 / $p1 * $p1",
                vec![lhs.into(), rhs.into()],
            );
        } else {
            let lhs_u_ty = self.ty(&lhs).to_unsigned();
            let lhs_u = self.var_pool.take(&mut self.builder, lhs_u_ty, None);
            let rhs_u_ty = self.ty(&rhs).to_unsigned();
            let rhs_u = self.var_pool.take(&mut self.builder, rhs_u_ty, None);
            let tmp = self.var_pool.take(&mut self.builder, lhs_u_ty, None);

            self.cast_sign(lhs, lhs_u.id());
            self.cast_sign(rhs, rhs_u.id());
            self.builder.push_set_pattern(
                tmp.id(),
                "$p0 - $p0 / $p1 * $p1",
                vec![lhs_u.id().into(), rhs_u.id().into()],
            );
            self.cast_sign(tmp.into(), result);
        };

        Ok(())
    }

    fn cast_sign(&mut self, opnd: PoolPrimary, result: VarId) {
        let result_ty = self.builder.code.vars[result].ty;
        let bits = result_ty.int_bits();
        let only_msb = 1u64 << (bits - 1);
        let except_msb = only_msb - 1;
        let cast_ty = result_ty.cast();

        if result_ty.signed() {
            self.builder.push_set_pattern(result,
                format!("$p0 >= {only_msb} ? {cast_ty}($p0 - {only_msb}) | (-{except_msb} - 1) : {cast_ty}($p0)"),
                vec![opnd.into()],
            );
        } else {
            self.builder.push_set_pattern(
                result,
                format!("$p0 < 0 ? {cast_ty}($p0 & {except_msb}) | {only_msb} : {cast_ty}($p0)"),
                vec![opnd.into()],
            );
        }
    }

    fn visit_shift_op(&mut self, op: &str, signed: bool) -> <Self as VisitOperator<'_>>::Output {
        let (lhs, rhs, result) = self.bin_op_vars_auto_ty();

        let rhs_int = if self.ty(&rhs) == CsType::Int {
            rhs
        } else {
            let rhs_int = self.var_pool.take(&mut self.builder, CsType::Int, None);
            self.wrap(rhs, rhs_int.id());
            rhs_int.into()
        };

        if signed {
            self.builder.push_set_pattern(
                result,
                format!("$p0 {op} $p1"),
                vec![lhs.into(), rhs_int.into()],
            );
        } else {
            self.shr_u(&lhs, &rhs_int, result);
        }
        Ok(())
    }

    fn shr_u(&mut self, lhs: &PoolPrimary, rhs_int: &PoolPrimary, result: VarId) {
        let bits = self.ty(lhs).int_bits();
        let only_msb = 1u64 << (bits - 1);
        let except_msb = only_msb - 1;

        self.builder.push_set_pattern(
            result,
            format!(
                "$p0 < 0 ? (($p0 & {except_msb}) >> $p1) | ({} << (-1 - $p1)) : $p0 >> $p1",
                if self.ty(lhs) == CsType::Long {
                    "1L"
                } else {
                    "1"
                }
            ),
            vec![lhs.into(), rhs_int.into()],
        );
    }

    fn visit_rot_op(&mut self, right: bool) -> <Self as VisitOperator<'_>>::Output {
        let (lhs, rhs, result) = self.bin_op_vars_auto_ty();

        let bits = self.ty(&lhs).int_bits();

        let rhs_int = if self.ty(&rhs) == CsType::Int {
            rhs
        } else {
            let rhs_int = self.var_pool.take(&mut self.builder, CsType::Int, None);
            self.wrap(rhs, rhs_int.id());
            rhs_int.into()
        };

        let bits_m_rhs = self.var_pool.take(&mut self.builder, CsType::Int, None);
        self.builder.push_set_pattern(
            bits_m_rhs.id(),
            format!("{bits} - $p0"),
            vec![(&rhs_int).into()],
        );

        let shr_ty = self.ty(&lhs);
        let shr = self.var_pool.take(&mut self.builder, shr_ty, None);
        if right {
            self.shr_u(&lhs, &rhs_int, shr.id());
            self.builder.push_set_pattern(
                result,
                "$p0 | ($p1 << $p2)",
                vec![shr.id().into(), lhs.into(), bits_m_rhs.id().into()],
            );
        } else {
            self.shr_u(&lhs, &bits_m_rhs.into(), shr.id());
            self.builder.push_set_pattern(
                result,
                "($p0 << $p1) | $p2",
                vec![lhs.into(), rhs_int.into(), shr.id().into()],
            );
        }
        Ok(())
    }

    fn visit_clz(&mut self) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars_auto_ty();

        let bits = self.ty(&opnd).int_bits();

        let mut pattern = format!("$p0 == 0 ? {bits} : ");
        let cast_ty = cast_from(CsType::Int, self.ty(&opnd));
        // 2進で文字列化して文字数を数える
        pattern += &format!("{cast_ty}({bits} - Convert.ToString($p0, 2).Length)",);

        self.builder
            .push_set_pattern(result, pattern, vec![opnd.into()]);
        Ok(())
    }

    fn visit_ctz(&mut self) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars_auto_ty();

        let bits = self.ty(&opnd).int_bits();

        let mut pattern = format!("$p0 == 0 ? {bits} : ");
        let cast_ty = cast_from(CsType::Int, self.ty(&opnd));

        // 符号付き整数の最小値のリテラルはUdonSharpではエラーとなるので
        // `-最大値 - 1` で表現する
        let or_opnd = match self.ty(&opnd) {
            CsType::Int => (1i32 << (bits - 1)) as i64,
            CsType::Long => 1i64 << (bits - 1),
            _ => unreachable!(),
        } + 1;

        // 1. 文字数を揃えるため、MSBだけが1の数とopndのORをとる
        // 2. 2進で文字列化する
        // 3. 最後に1が出現するインデックスを求める
        pattern += &format!(
            "{} - {cast_ty}(Convert.ToString(($p0 | ({or_opnd} - 1)), 2).LastIndexOf('1'))",
            bits - 1,
        );

        self.builder
            .push_set_pattern(result, pattern, vec![opnd.into()]);
        Ok(())
    }

    fn visit_popcnt(&mut self) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars_auto_ty();

        let cast_ty = cast_from(CsType::Int, self.ty(&opnd));
        // 2進で文字列化して、0を除去した後の文字数を数える
        self.builder.push_set_pattern(
            result,
            format!("{cast_ty}(Convert.ToString($p0, 2).Replace(\"0\", \"\").Length)"),
            vec![opnd.into()],
        );
        Ok(())
    }

    fn visit_math_un_op(&mut self, func: &str) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars_auto_ty();

        self.builder.push_set_pattern(
            result,
            format!("{}.{func}($p0)", self.module.math_class(self.ty(&opnd))),
            vec![opnd.into()],
        );
        Ok(())
    }

    fn visit_math_bin_op(&mut self, func: &str) -> <Self as VisitOperator<'_>>::Output {
        let (lhs, rhs, result) = self.bin_op_vars_auto_ty();

        self.builder.push_set_pattern(
            result,
            format!("{}.{func}($p0, $p1)", self.module.math_class(self.ty(&lhs))),
            vec![lhs.into(), rhs.into()],
        );
        Ok(())
    }

    fn visit_copysign_op(&mut self) -> <Self as VisitOperator<'_>>::Output {
        let (lhs, rhs, result) = self.bin_op_vars_auto_ty();

        self.builder.push_set_pattern(
            result,
            format!(
                "{0}.Abs($p0) * (($p1 == 0 ? 1 / $p1 : $p1) > 0 ? 1 : -1)",
                self.module.math_class(self.ty(&lhs))
            ),
            vec![lhs.into(), rhs.into()],
        );
        Ok(())
    }

    fn visit_cast(&mut self, result_ty: CsType) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars(result_ty);
        let cast_ty = result_ty.cast();

        self.builder
            .push_set_pattern(result, format!("{cast_ty}($p0)"), vec![opnd.into()]);
        Ok(())
    }

    fn wrap(&mut self, opnd: PoolPrimary, result: VarId) {
        let mut pattern = format!("$r = {}($p0 & 0x7fffffff); ", CsType::Int.cast());
        pattern += "if (($p0 & 0x80000000) != 0) $r |= -0x7fffffff - 1;";

        self.builder.push(Inst {
            kind: InstKind::Stmt,
            pattern,
            params: vec![opnd.into()],
            result: Some(result),
            ..Default::default()
        })
    }

    fn visit_cast_trunc(
        &mut self,
        result_ty: CsType,
        signed: bool,
    ) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars(result_ty);
        let tmp = self.var_pool.take(&mut self.builder, CsType::Double, None);

        self.builder.push_set_pattern(
            tmp.id(),
            format!(
                "Math.Truncate({}($p0))",
                cast_from(self.ty(&opnd), CsType::Double)
            ),
            vec![opnd.into()],
        );

        if signed {
            let cast_ty = result_ty.cast();
            self.builder
                .push_set_pattern(result, format!("{cast_ty}($p0)"), vec![tmp.id().into()]);
        } else {
            self.cast_sign(tmp.into(), result);
        }
        Ok(())
    }

    fn visit_cast_extend(&mut self, signed: bool) -> <Self as VisitOperator<'_>>::Output {
        let (opnd, result) = self.un_op_vars(CsType::Long);
        let cast_ty = CsType::Long.cast();

        if signed {
            self.builder
                .push_set_pattern(result, format!("{cast_ty}($p0)"), vec![opnd.into()]);
        } else {
            self.builder.push_set_pattern(
                result,
                format!("$p0 < 0 ? {cast_ty}($p0 & 0x7fffffff) | 0x80000000 : {cast_ty}($p0)"),
                vec![opnd.into()],
            );
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
            self.builder
                .push_set_pattern(result, format!("{cast_ty}($p0)"), vec![opnd.into()]);
        } else {
            let bits = self.ty(&opnd).int_bits();
            let only_msb = 1u64 << (bits - 1);

            if matches!(result_ty, CsType::Float | CsType::Double) {
                let opnd_ty_u = self.ty(&opnd).to_unsigned();
                self.builder.push_set_pattern(
                    result,
                    format!(
                    "$p0 < 0 ? {cast_ty}(({opnd_ty_u})($p0 & {}) + {only_msb}) : {cast_ty}($p0)",
                    only_msb - 1
                ),
                    vec![opnd.into()],
                );
            } else {
                self.builder.push_set_pattern(
                    result,
                    format!(
                        "$p0 < 0 ? {cast_ty}($p0 & {}) + {only_msb} : {cast_ty}($p0)",
                        only_msb - 1
                    ),
                    vec![opnd.into()],
                );
            }
        }
        Ok(())
    }

    fn visit_extend(&mut self, ty: StorageType) -> <Self as VisitOperator<'_>>::Output {
        let opnd = self.pop_stack();
        let result_ty = self.ty(&opnd);
        let result = self.var_pool.take(&mut self.builder, result_ty, None);

        use {StorageType::*, ValType::*};
        let (and, ge, or) = match ty {
            I8 => ("0xff", "0x80", "-0x100"),
            I16 => ("0xffff", "0x8000", "-0x10000"),
            Val(I32) => ("0xffffffff", "0x80000000", "-0x100000000"),
            _ => unreachable!(),
        };

        let mut pattern = format!("$r = $p0 & {and}; ");
        pattern += &format!("if ($r >= {ge}) $r |= {or};");

        self.builder.push(Inst {
            kind: InstKind::Stmt,
            pattern,
            params: vec![opnd.into()],
            result: Some(result.id()),
            ..Default::default()
        });

        self.push_stack(result.into());
        Ok(())
    }

    fn visit_br_table_case(&mut self, case: Option<usize>, target: u32) {
        self.builder.start_block();
        match case {
            Some(case) => self.builder.push_case(Const::Int(case as i32).into()),
            None => self.builder.push_default(),
        }
        self.block_result(target, true);
        // targetが0の場合、switchブロックがbreakされた後に続きの外のブロックが
        // そのまま実行されるので再度breakする必要はない。
        // targetが1以上の場合、Wasmでは存在しないswitchブロックをbreakするため、
        // targetにswitchブロックの分の1を足している
        self.builder
            .push_br(if target == 0 { 0 } else { target + 1 });
        self.builder.end_block();
    }
}

fn index_pattern(offset: u64) -> String {
    if offset == 0 {
        "$p0".to_string()
    } else {
        format!("$p0 + {}", offset as i32)
    }
}

impl<'a, 'input, 'module> VisitOperator<'a> for CodeParser<'input, 'module> {
    type Output = Result<()>;

    for_each_operator!(define_visit_operator);

    fn visit_unreachable(&mut self) -> Self::Output {
        self.unreachable = 1;
        self.builder.push_line(trap(self.module, "unreachable"));
        Ok(())
    }

    fn visit_nop(&mut self) -> Self::Output {
        self.builder.push_line("// nop");
        Ok(())
    }

    fn visit_block(&mut self, blockty: BlockType) -> Self::Output {
        if self.unreachable > 0 {
            self.unreachable += 1;
            return Ok(());
        }

        self.new_block(blockty, false);

        self.builder.push_block(Breakable::Multi);
        self.builder.start_block();

        Ok(())
    }

    fn visit_loop(&mut self, blockty: BlockType) -> Self::Output {
        if self.unreachable > 0 {
            self.unreachable += 1;
            return Ok(());
        }

        let block = self.new_block(blockty, true);
        let loop_var = block.loop_var.unwrap();

        self.builder.push_loop(loop_var, Breakable::Multi);
        self.builder.start_block();

        Ok(())
    }

    fn visit_if(&mut self, blockty: BlockType) -> Self::Output {
        if self.unreachable > 0 {
            self.unreachable += 1;
            return Ok(());
        }

        let opnd = self.pop_stack();
        self.new_block(blockty, false);

        self.builder.push_if(opnd.into(), Breakable::Multi);
        self.builder.start_block();

        Ok(())
    }

    fn visit_else(&mut self) -> Self::Output {
        match self.unreachable {
            0 => self.block_result(0, false),
            1 => self.unreachable -= 1,
            _ => return Ok(()),
        }

        self.blocks.last_mut().unwrap().stack.clear();

        self.builder.end_block();
        self.builder.start_block();

        Ok(())
    }

    fn visit_end(&mut self) -> Self::Output {
        match self.unreachable {
            0 => self.block_result(0, false),
            1 => self.unreachable -= 1,
            _ => {
                self.unreachable -= 1;
                return Ok(());
            }
        }

        let block = self.blocks.pop().unwrap();

        // 最も外側のブロックのendでない場合のみ
        if !self.blocks.is_empty() {
            if let Some(result) = block.result {
                self.push_stack(result.into());
            }
        }

        self.builder.end_block();
        Ok(())
    }

    fn visit_br(&mut self, relative_depth: u32) -> Self::Output {
        self.block_result(relative_depth, true);
        self.unreachable = 1;

        self.builder.push_br(relative_depth);
        Ok(())
    }

    fn visit_br_if(&mut self, relative_depth: u32) -> Self::Output {
        let opnd = self.pop_stack();

        self.builder.push_if(opnd.into(), Breakable::No);
        self.builder.start_block();
        self.block_result(relative_depth, true);
        self.builder.push_br(relative_depth);
        self.builder.end_block();
        Ok(())
    }

    fn visit_br_table(&mut self, targets: BrTable<'a>) -> Self::Output {
        self.unreachable = 1;
        let opnd = self.pop_stack();

        self.builder.push_switch(opnd.into(), Breakable::Multi);

        for (i, target) in targets.targets().enumerate() {
            // case i:
            self.visit_br_table_case(Some(i), target?);
        }

        // default:
        self.visit_br_table_case(None, targets.default());

        Ok(())
    }

    fn visit_return(&mut self) -> Self::Output {
        self.unreachable = 1;

        let results_len = self.module.all_funcs[self.func].header.ty.results().len();
        match results_len {
            0 => self.builder.push_return(None),
            1 => {
                let var = self.last_stack();
                self.builder.push_return(Some(var.into()));
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
        let mut params: Vec<PoolPrimary> = ty.params().iter().map(|_| self.pop_stack()).collect();
        params.reverse();

        self.call(index, ty, params);
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

        let index = *self
            .module
            .call_indirects
            .get(&(type_index as usize))
            .unwrap();
        let ty = self.module.types[type_index as usize].clone();

        // Iteratorのrevを使わない理由はvisit_callのコメントを参照
        let mut params: Vec<PoolPrimary> = (0..ty.params().len() + 1)
            .map(|_| self.pop_stack())
            .collect();
        params.reverse();

        self.call(index, ty, params);
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

        let result_ty = self.ty(&val1);
        let result = self.var_pool.take(&mut self.builder, result_ty, None);

        self.builder.push_set_pattern(
            result.id(),
            "$p2 != 0 ? $p0 : $p1",
            vec![val1.into(), val2.into(), c.into()],
        );

        self.push_stack(result.into());
        Ok(())
    }

    fn visit_local_get(&mut self, local_index: u32) -> Self::Output {
        let local_id = VarId::from_u32(local_index);
        let local = self.builder.code.vars[local_id];
        let var = self.var_pool.take(&mut self.builder, local.ty, None);

        self.builder.push_set(var.id(), local_id.into());

        self.push_stack(var.into());
        Ok(())
    }

    fn visit_local_set(&mut self, local_index: u32) -> Self::Output {
        let local_id = VarId::from_u32(local_index);
        let var = self.pop_stack();
        self.builder.push_set(local_id, var.into());
        Ok(())
    }

    fn visit_local_tee(&mut self, local_index: u32) -> Self::Output {
        let local_id = VarId::from_u32(local_index);
        self.builder.push_set(local_id, self.last_stack().into());
        Ok(())
    }

    fn visit_global_get(&mut self, global_index: u32) -> Self::Output {
        let global = &self.module.globals[global_index as usize];
        let var = self
            .var_pool
            .take(&mut self.builder, CsType::get(global.ty.content_type), None);

        self.builder
            .push_set_pattern(var.id(), global.name.clone(), vec![]);

        self.push_stack(var.into());
        Ok(())
    }

    fn visit_global_set(&mut self, global_index: u32) -> Self::Output {
        let global = &self.module.globals[global_index as usize];
        let var = self.pop_stack();

        self.builder.push(Inst {
            kind: InstKind::Stmt,
            pattern: format!("{} = $p0;", global.name),
            params: vec![var.into()],
            ..Default::default()
        });
        Ok(())
    }

    fn visit_i32_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Int, StorageType::Val(ValType::I32), None)
    }

    fn visit_i64_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Long, StorageType::Val(ValType::I64), None)
    }

    fn visit_f32_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Float, StorageType::Val(ValType::F32), None)
    }

    fn visit_f64_load(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Double, StorageType::Val(ValType::F64), None)
    }

    fn visit_i32_load8_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Int, StorageType::I8, Some(true))
    }

    fn visit_i32_load8_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Int, StorageType::I8, Some(false))
    }

    fn visit_i32_load16_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Int, StorageType::I16, Some(true))
    }

    fn visit_i32_load16_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Int, StorageType::I16, Some(false))
    }

    fn visit_i64_load8_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Long, StorageType::I8, Some(true))
    }

    fn visit_i64_load8_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Long, StorageType::I8, Some(false))
    }

    fn visit_i64_load16_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Long, StorageType::I16, Some(true))
    }

    fn visit_i64_load16_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(memarg, CsType::Long, StorageType::I16, Some(false))
    }

    fn visit_i64_load32_s(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(
            memarg,
            CsType::Long,
            StorageType::Val(ValType::I32),
            Some(true),
        )
    }

    fn visit_i64_load32_u(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_load(
            memarg,
            CsType::Long,
            StorageType::Val(ValType::I32),
            Some(false),
        )
    }

    fn visit_i32_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, StorageType::Val(ValType::I32))
    }

    fn visit_i64_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, StorageType::Val(ValType::I64))
    }

    fn visit_f32_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, StorageType::Val(ValType::F32))
    }

    fn visit_f64_store(&mut self, memarg: MemArg) -> Self::Output {
        self.visit_store(memarg, StorageType::Val(ValType::F64))
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

        let result = self.var_pool.take(&mut self.builder, CsType::Int, None);
        let memory = &self.module.memory.as_ref().unwrap().name;

        self.builder.push_set_pattern(
            result.id(),
            format!("{memory}.Length / {PAGE_SIZE}"),
            vec![],
        );

        self.push_stack(result.into());
        Ok(())
    }

    fn visit_memory_grow(&mut self, mem: u32, mem_byte: u8) -> Self::Output {
        if mem != 0 {
            panic!("Multi memory is not supported")
        }
        assert!(mem_byte == 0);

        let size = self.pop_stack();
        let result = self.var_pool.take(&mut self.builder, CsType::Int, None);

        let memory = &self.module.memory.as_ref().unwrap().name;
        let max = self
            .module
            .memory
            .as_ref()
            .unwrap()
            .ty
            .maximum
            .unwrap_or(0x10000);

        let mut pattern = format!("if ({memory}.Length / {PAGE_SIZE} + $p0 > {max}) {{\n");
        {
            // 新しいメモリサイズが最大値を超えていれば-1を返す
            pattern += "$r = -1;\n";
        }
        pattern += "} else {\n";
        {
            // 前のサイズを返す
            pattern += &format!("$r = {memory}.Length / {PAGE_SIZE};\n");

            // メモリをsizeだけ拡張
            pattern += &format!("var old = {memory};\n");
            pattern += &format!("{memory} = new byte[old.Length + $p0 * {PAGE_SIZE}];\n");
            pattern += &format!("Array.Copy(old, {memory}, old.Length);\n");
        }
        pattern += "}";

        self.builder.push(Inst {
            kind: InstKind::Stmt,
            pattern,
            params: vec![size.into()],
            result: Some(result.id()),
            ..Default::default()
        });

        self.push_stack(result.into());
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
        self.push_stack(Const::Float(value.bits()).into());
        Ok(())
    }

    fn visit_f64_const(&mut self, value: Ieee64) -> Self::Output {
        self.push_stack(Const::Double(value.bits()).into());
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
        self.builder.push_set_pattern(
            result,
            format!(
                "{}(Math.Truncate({}($p0)))",
                CsType::Float.cast(),
                CsType::Double.cast()
            ),
            vec![opnd.into()],
        );
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
        self.builder.push_set_pattern(
            result,
            "BitConverter.SingleToInt32Bits($p0)",
            vec![opnd.into()],
        );
        Ok(())
    }

    fn visit_i64_reinterpret_f64(&mut self) -> Self::Output {
        let (opnd, result) = self.un_op_vars(CsType::Long);
        self.builder.push_set_pattern(
            result,
            "BitConverter.DoubleToInt64Bits($p0)",
            vec![opnd.into()],
        );
        Ok(())
    }

    fn visit_f32_reinterpret_i32(&mut self) -> Self::Output {
        let (opnd, result) = self.un_op_vars(CsType::Float);
        self.builder.push_set_pattern(
            result,
            "BitConverter.Int32BitsToSingle($p0)",
            vec![opnd.into()],
        );
        Ok(())
    }

    fn visit_f64_reinterpret_i64(&mut self) -> Self::Output {
        let (opnd, result) = self.un_op_vars(CsType::Double);
        self.builder.push_set_pattern(
            result,
            "BitConverter.Int64BitsToDouble($p0)",
            vec![opnd.into()],
        );
        Ok(())
    }

    fn visit_i32_extend8_s(&mut self) -> Self::Output {
        self.visit_extend(StorageType::I8)
    }

    fn visit_i32_extend16_s(&mut self) -> Self::Output {
        self.visit_extend(StorageType::I16)
    }

    fn visit_i64_extend8_s(&mut self) -> Self::Output {
        self.visit_extend(StorageType::I8)
    }

    fn visit_i64_extend16_s(&mut self) -> Self::Output {
        self.visit_extend(StorageType::I16)
    }

    fn visit_i64_extend32_s(&mut self) -> Self::Output {
        self.visit_extend(StorageType::Val(ValType::I32))
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
