use wasmparser::{FuncType, StorageType, ValType};

use crate::{
    ir::{
        func::{Code, Func, FuncHeader, Instr, Var},
        module::Module,
        ty::CsType,
    },
    util::bit_mask,
};

pub const I32_LOAD: &str = "w2us_i32_load";
pub const I64_LOAD: &str = "w2us_i64_load";
pub const F32_LOAD: &str = "w2us_f32_load";
pub const F64_LOAD: &str = "w2us_f64_load";
pub const I32_LOAD8_S: &str = "w2us_i32_load8_s";
pub const I32_LOAD8_U: &str = "w2us_i32_load8_u";
pub const I32_LOAD16_S: &str = "w2us_i32_load16_s";
pub const I32_LOAD16_U: &str = "w2us_i32_load16_u";
pub const I64_LOAD8_S: &str = "w2us_i64_load8_s";
pub const I64_LOAD8_U: &str = "w2us_i64_load8_u";
pub const I64_LOAD16_S: &str = "w2us_i64_load16_s";
pub const I64_LOAD16_U: &str = "w2us_i64_load16_u";
pub const I64_LOAD32_S: &str = "w2us_i64_load32_s";
pub const I64_LOAD32_U: &str = "w2us_i64_load32_u";

pub const I32_STORE: &str = "w2us_i32_store";
pub const I64_STORE: &str = "w2us_i64_store";
pub const F32_STORE: &str = "w2us_f32_store";
pub const F64_STORE: &str = "w2us_f64_store";
pub const I32_STORE8: &str = "w2us_i32_store8";
pub const I32_STORE16: &str = "w2us_i32_store16";
pub const I64_STORE8: &str = "w2us_i64_store8";
pub const I64_STORE16: &str = "w2us_i64_store16";
pub const I64_STORE32: &str = "w2us_i64_store32";

pub const I32_REINTERPRET_F32: &str = "w2us_i32_reinterpret_f32";
pub const I64_REINTERPRET_F64: &str = "w2us_i64_reinterpret_f64";
pub const F32_REINTERPRET_I32: &str = "w2us_f32_reinterpret_i32";
pub const F64_REINTERPRET_I64: &str = "w2us_f64_reinterpret_i64";

pub fn add_builtin_funcs(module: &mut Module<'_>) {
    if module.memory.is_some() {
        add_funcs_load(module);
        add_funcs_store(module);
    }

    add_funcs_reinterpret(module);
}

fn add_funcs_load(module: &mut Module<'_>) {
    use {StorageType::*, ValType::*};

    add_func(module, I32_LOAD, vec![I32], vec![I32], &|m, c, p, r| {
        func_int_load(m, c, p, r, Val(I32), false)
    });
    add_func(module, I64_LOAD, vec![I32], vec![I64], &|m, c, p, r| {
        func_int_load(m, c, p, r, Val(I64), false)
    });
    add_func(module, F32_LOAD, vec![I32], vec![F32], &func_float_load);
    add_func(module, F64_LOAD, vec![I32], vec![F64], &func_float_load);
    add_func(module, I32_LOAD8_S, vec![I32], vec![I32], &|m, c, p, r| {
        func_int_load(m, c, p, r, I8, true)
    });
    add_func(module, I32_LOAD8_U, vec![I32], vec![I32], &|m, c, p, r| {
        func_int_load(m, c, p, r, I8, false)
    });
    add_func(module, I32_LOAD16_S, vec![I32], vec![I32], &|m, c, p, r| {
        func_int_load(m, c, p, r, I16, true)
    });
    add_func(module, I32_LOAD16_U, vec![I32], vec![I32], &|m, c, p, r| {
        func_int_load(m, c, p, r, I16, false)
    });
    add_func(module, I64_LOAD8_S, vec![I32], vec![I64], &|m, c, p, r| {
        func_int_load(m, c, p, r, I8, true)
    });
    add_func(module, I64_LOAD8_U, vec![I32], vec![I64], &|m, c, p, r| {
        func_int_load(m, c, p, r, I8, false)
    });
    add_func(module, I64_LOAD16_S, vec![I32], vec![I64], &|m, c, p, r| {
        func_int_load(m, c, p, r, I16, true)
    });
    add_func(module, I64_LOAD16_U, vec![I32], vec![I64], &|m, c, p, r| {
        func_int_load(m, c, p, r, I16, false)
    });
    add_func(module, I64_LOAD32_S, vec![I32], vec![I64], &|m, c, p, r| {
        func_int_load(m, c, p, r, Val(I32), true)
    });
    add_func(module, I64_LOAD32_U, vec![I32], vec![I64], &|m, c, p, r| {
        func_int_load(m, c, p, r, Val(I32), false)
    });
}

fn add_funcs_store(module: &mut Module<'_>) {
    use {StorageType::*, ValType::*};

    add_func(module, I32_STORE, vec![I32, I32], vec![], &|m, c, p, r| {
        func_int_store(m, c, p, r, Val(I32))
    });
    add_func(module, I64_STORE, vec![I32, I64], vec![], &|m, c, p, r| {
        func_int_store(m, c, p, r, Val(I64))
    });
    add_func(module, F32_STORE, vec![I32, F32], vec![], &func_float_store);
    add_func(module, F64_STORE, vec![I32, F64], vec![], &func_float_store);
    add_func(module, I32_STORE8, vec![I32, I32], vec![], &|m, c, p, r| {
        func_int_store(m, c, p, r, I8)
    });
    add_func(
        module,
        I32_STORE16,
        vec![I32, I32],
        vec![],
        &|m, c, p, r| func_int_store(m, c, p, r, I16),
    );
    add_func(module, I64_STORE8, vec![I32, I64], vec![], &|m, c, p, r| {
        func_int_store(m, c, p, r, I8)
    });
    add_func(
        module,
        I64_STORE16,
        vec![I32, I64],
        vec![],
        &|m, c, p, r| func_int_store(m, c, p, r, I16),
    );
    add_func(
        module,
        I64_STORE32,
        vec![I32, I64],
        vec![],
        &|m, c, p, r| func_int_store(m, c, p, r, Val(I32)),
    );
}

fn add_funcs_reinterpret(module: &mut Module<'_>) {
    use ValType::*;

    add_func(
        module,
        I32_REINTERPRET_F32,
        vec![F32],
        vec![I32],
        &func_reinterpret,
    );
    add_func(
        module,
        I64_REINTERPRET_F64,
        vec![F64],
        vec![I64],
        &func_reinterpret,
    );
    add_func(
        module,
        F32_REINTERPRET_I32,
        vec![I32],
        vec![F32],
        &func_reinterpret,
    );
    add_func(
        module,
        F64_REINTERPRET_I64,
        vec![I64],
        vec![F64],
        &func_reinterpret,
    );
}

type FnCode<'a> = dyn Fn(&Module<'a>, &mut Code, Vec<ValType>, Vec<ValType>);

fn add_func<'a>(
    module: &mut Module<'a>,
    name: &str,
    params: Vec<ValType>,
    results: Vec<ValType>,
    fn_code: &FnCode<'a>,
) {
    let header = FuncHeader {
        name: name.to_string(),
        ty: FuncType::new(params.clone(), results.clone()),
        import: false,
        export: false,
    };

    let mut code = Code::new(&header);
    fn_code(module, &mut code, params, results);

    let func = Func {
        header,
        code: Some(code),
        in_table: false,
    };

    module.all_funcs.push(func);
}

fn func_int_load(
    module: &Module<'_>,
    code: &mut Code,
    _params: Vec<ValType>,
    results: Vec<ValType>,
    mem_type: StorageType,
    signed: bool,
) {
    let result = code.new_var(CsType::get(results[0]), None);
    int_load(
        module,
        code,
        mem_type,
        signed,
        code.var_decls[0].var,
        result,
    );
    push_line(code, format!("return {};", result));
}

fn func_float_load(
    module: &Module<'_>,
    code: &mut Code,
    _params: Vec<ValType>,
    results: Vec<ValType>,
) {
    let i_result = code.new_var(CsType::get(results[0]).to_i(), None);
    let f_result = code.new_var(CsType::get(results[0]), None);
    int_load(
        module,
        code,
        StorageType::Val(i_result.ty.val_type()),
        false,
        code.var_decls[0].var,
        i_result,
    );
    bits_to_float(module, code, i_result, f_result);
    push_line(code, format!("return {};", f_result));
}

fn func_int_store(
    module: &Module<'_>,
    code: &mut Code,
    _params: Vec<ValType>,
    _results: Vec<ValType>,
    mem_type: StorageType,
) {
    int_store(
        module,
        code,
        mem_type,
        code.var_decls[0].var,
        code.var_decls[1].var,
    );
}

fn func_float_store(
    module: &Module<'_>,
    code: &mut Code,
    _params: Vec<ValType>,
    _results: Vec<ValType>,
) {
    let i_var = code.new_var(code.var_decls[1].var.ty.to_i(), None);
    float_to_bits(module, code, code.var_decls[1].var, i_var);
    int_store(
        module,
        code,
        StorageType::Val(i_var.ty.val_type()),
        code.var_decls[0].var,
        i_var,
    );
}

fn func_reinterpret(
    module: &Module<'_>,
    code: &mut Code,
    params: Vec<ValType>,
    results: Vec<ValType>,
) {
    let result = code.new_var(CsType::get(results[0]), None);
    match params[0] {
        ValType::I32 | ValType::I64 => bits_to_float(module, code, code.var_decls[0].var, result),
        ValType::F32 | ValType::F64 => float_to_bits(module, code, code.var_decls[0].var, result),
        _ => unreachable!(),
    }
    push_line(code, format!("return {};", result));
}

fn int_load(
    module: &Module<'_>,
    code: &mut Code,
    mem_type: StorageType,
    signed: bool,
    idx: Var,
    result: Var,
) {
    use {StorageType::*, ValType::*};

    let mut load = format!("{result} = ");

    let memory = &module.memory.as_ref().unwrap().name;
    let cast_result = result.ty.cast();
    match mem_type {
            I8 => load += &format!("{0}[{1}];", memory, idx),
            I16 => load += &format!("{0}[{1}] | {2}({0}[{1}+1])<<8;", memory, idx, cast_result),
            Val(I32) => load += &format!(
                "{0}[{1}] | {2}({0}[{1}+1])<<8 | {2}({0}[{1}+2])<<16 | {2}({0}[{1}+3])<<24;",
                memory, idx, cast_result),
            Val(I64) => load += &format!(
                "{0}[{1}] | {2}({0}[{1}+1])<<8 | {2}({0}[{1}+2])<<16 | {2}({0}[{1}+3])<<24 | {2}({0}[{1}+4])<<32 | {2}({0}[{1}+5])<<40 | {2}({0}[{1}+6])<<48 | {2}({0}[{1}+7])<<56;",
                memory, idx, cast_result),
            _ => unreachable!(),
        }
    push_line(code, load);

    if signed {
        let (ge, or) = match mem_type {
            I8 => ("0x80", "-0x100"),
            I16 => ("0x8000", "-0x10000"),
            Val(I32) => ("0x80000000", "-0x100000000"),
            _ => unreachable!(),
        };

        push_line(code, format!("if ({result} >= {ge}) {result} |= {or};"));
    }
}

fn int_store(module: &Module<'_>, code: &mut Code, mem_type: StorageType, idx: Var, var: Var) {
    use {StorageType::*, ValType::*};

    let memory = &module.memory.as_ref().unwrap().name;
    let cast_byte = CsType::Byte.cast();
    push_line(
        code,
        match mem_type {
            I8 => format!("{0}[{1}]={2}({3}&0xff);", memory, idx, cast_byte, var),
            I16 => {
                format!(
                    "{0}[{1}]={2}({3}&0xff); {0}[{1}+1]={2}(({3}>>8)&0xff);",
                    memory, idx, cast_byte, var
                )
            }
            Val(I32) => {
                format!(
                    "{0}[{1}]={2}({3}&0xff); {0}[{1}+1]={2}(({3}>>8)&0xff); {0}[{1}+2]={2}(({3}>>16)&0xff); {0}[{1}+3]={2}(({3}>>24)&0xff);",
                    memory, idx, cast_byte, var
                )
            }
            Val(I64) => {
                format!(
                    "{0}[{1}]={2}({3}&0xff); {0}[{1}+1]={2}(({3}>>8)&0xff); {0}[{1}+2]={2}(({3}>>16)&0xff); {0}[{1}+3]={2}(({3}>>24)&0xff); {0}[{1}+4]={2}(({3}>>32)&0xff); {0}[{1}+5]={2}(({3}>>40)&0xff); {0}[{1}+6]={2}(({3}>>48)&0xff); {0}[{1}+7]={2}(({3}>>56)&0xff);",
                    memory, idx, cast_byte, var
                )
            }
            _ => unreachable!(),
        },
    );
}

fn bits_to_float(module: &Module<'_>, code: &mut Code, i_var: Var, f_var: Var) {
    let i_ty = i_var.ty;
    let f_ty = f_var.ty;
    let bits = i_ty.int_bits();
    let frac_bits = f_ty.frac_bits();
    let expo_bits = bits - 1 - frac_bits;
    let expo_bit_mask = bit_mask(expo_bits as u64);
    // LSBに寄せた後のビットマスク
    let expo_offset = (1 << (expo_bits - 1)) - 1;
    let class = module.math_class(f_ty);

    push_line(code, "{".to_string());

    // ビット列の各データを抽出
    push_line(code, format!("var sign = ({i_var} >> {}) & 1;", bits - 1));
    push_line(
        code,
        format!("var expo = ({i_var} >> {frac_bits}) & {expo_bit_mask};",),
    );
    push_line(
        code,
        format!("var frac = {i_var} & {};", bit_mask(frac_bits as u64)),
    );

    let sign = format!("(1 - {}(sign) * 2)", f_ty.cast());

    push_line(code, "if (expo == 0) {".to_string());
    {
        push_line(code, "if (frac == 0) {".to_string());
        // 0の場合
        push_line(code, format!("{f_var} = sign == 0 ? 0f : -0f;"));
        push_line(code, "} else {".to_string());
        // 非正規化数の場合
        push_line(
            code,
            format!("{f_var} = ({f_ty})frac * {f_ty}.Epsilon * {sign};"),
        );
        push_line(code, "}".to_string());
    }
    push_line(code, format!("}} else if (expo == {expo_bit_mask}) {{"));
    {
        push_line(code, "if (frac == 0) {".to_string());
        // 無限大の場合
        push_line(
            code,
            format!("{f_var} = sign == 0 ? {f_ty}.PositiveInfinity : {f_ty}.NegativeInfinity;"),
        );
        push_line(code, "} else {".to_string());
        // NaNの場合
        push_line(code, format!("{f_var} = {f_ty}.NaN;"));
        push_line(code, "}".to_string());
    }
    push_line(code, "} else {".to_string());
    {
        // 浮動小数点数の変数に代入
        let expo = format!(
            "{class}.Pow(2, {}(expo) - {expo_offset})",
            CsType::Int.cast()
        );
        let frac = format!("({}(frac) / {} + 1)", f_ty.cast(), 1u64 << frac_bits);
        push_line(code, format!("{f_var} = {frac} * {expo} * {sign};"));
    }
    push_line(code, "}".to_string());

    push_line(code, "}".to_string());
}

fn float_to_bits(module: &Module<'_>, code: &mut Code, f_var: Var, i_var: Var) {
    let f_ty = f_var.ty;
    let i_ty = i_var.ty;
    let bits = i_ty.int_bits();
    let frac_bits = f_ty.frac_bits();
    let frac_bits_mask = bit_mask(frac_bits as u64);
    let expo_bits = bits - 1 - frac_bits;
    let expo_bit_mask = bit_mask(expo_bits as u64);
    // LSBに寄せた後のビットマスク
    let expo_offset = (1 << (expo_bits - 1)) - 1;
    let class = module.math_class(f_ty);
    let subnormal_bound = match f_ty {
        CsType::Float => "1.1754944e-38f",
        CsType::Double => "2.2250738585072014e-308",
        _ => unreachable!(),
    };

    push_line(code, "{".to_string());
    push_line(code, "bool sign;".to_string());
    push_line(code, format!("{i_ty} expo;"));
    push_line(code, format!("{i_ty} frac;"));
    push_line(code, format!("var absVar = {class}.Abs({f_var});"));

    let cast_i_ty = i_ty.cast();

    push_line(code, format!("if ({f_var} == 0) {{"));
    {
        // 0の場合
        // 1/0を計算することで+0と-0を区別
        push_line(code, format!("sign = 1 / {f_var} < 0;"));
        push_line(code, "expo = 0;".to_string());
        push_line(code, "frac = 0;".to_string());
    }
    push_line(code, format!("}} else if ({f_ty}.IsInfinity({f_var})) {{"));
    {
        // 無限大の場合
        push_line(code, format!("sign = {f_var} < 0;"));
        push_line(code, format!("expo = {expo_bit_mask};"));
        push_line(code, "frac = 0;".to_string());
    }
    push_line(code, format!("}} else if ({f_ty}.IsNaN({f_var})) {{"));
    {
        // NaNの場合
        push_line(code, "sign = true;".to_string());
        push_line(code, format!("expo = {expo_bit_mask};"));
        // MSBだけが1
        push_line(code, format!("frac = {};", 1u64 << (frac_bits - 1)));
    }
    push_line(code, format!("}} else if (absVar < {subnormal_bound}) {{",));
    {
        // 非正規化数の場合
        push_line(code, format!("sign = {f_var} < 0;"));
        push_line(code, "expo = 0;".to_string());
        push_line(
            code,
            format!("frac = {cast_i_ty}({class}.Abs({f_var}) / {f_ty}.Epsilon);"),
        );
    }
    push_line(code, "} else {".to_string());
    {
        push_line(code, format!("sign = {f_var} < 0;"));
        push_line(
            code,
            format!("var expoF = {class}.Floor({class}.Log(absVar, 2));"),
        );
        push_line(code, format!("if (expoF >= {}) {{", expo_offset + 1));
        {
            // Log2の誤差で無限大になった場合
            push_line(code, format!("expo = {};", expo_bit_mask - 1));
            push_line(code, format!("frac = {frac_bits_mask};"));
        }
        push_line(code, "} else {".to_string());
        {
            // 通常の浮動小数点数の場合
            push_line(code, format!("expo = {cast_i_ty}(expoF) + {expo_offset};"));
            push_line(
                code,
                format!(
                    "frac = {cast_i_ty}((absVar / {class}.Pow(2, expoF) - 1) * {});",
                    1u64 << frac_bits
                ),
            );
        }
        push_line(code, "}".to_string());
    }
    push_line(code, "}".to_string());

    push_line(
        code,
        format!(
            "{i_var} = ({cast_i_ty}(sign) << {}) | (expo << {frac_bits}) | frac;",
            bits - 1
        ),
    );

    push_line(code, "}".to_string());
}

fn push_line(code: &mut Code, line: String) {
    code.instrs.push(Instr::Line(line));
}
