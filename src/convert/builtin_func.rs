use wasmparser::{FuncType, StorageType, ValType};

use crate::ir::{
    func::{Code, Func, FuncHeader, Var},
    instr::Instr,
    module::Module,
    ty::CsType,
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
        func_int_load(m, c, p, r, Val(I32), true)
    });
    add_func(module, I64_LOAD, vec![I32], vec![I64], &|m, c, p, r| {
        func_int_load(m, c, p, r, Val(I64), true)
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
        true,
        code.var_decls[0].var,
        i_result,
    );
    bits_to_float(code, i_result, f_result);
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
    float_to_bits(code, code.var_decls[1].var, i_var);
    int_store(
        module,
        code,
        StorageType::Val(i_var.ty.val_type()),
        code.var_decls[0].var,
        i_var,
    );
}

fn func_reinterpret(
    _module: &Module<'_>,
    code: &mut Code,
    params: Vec<ValType>,
    results: Vec<ValType>,
) {
    let result = code.new_var(CsType::get(results[0]), None);
    match params[0] {
        ValType::I32 | ValType::I64 => bits_to_float(code, code.var_decls[0].var, result),
        ValType::F32 | ValType::F64 => float_to_bits(code, code.var_decls[0].var, result),
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
    match (mem_type, signed) {
        (I8, _) => load += &format!("{memory}[{idx}];"),
        (I16, false) => load += &format!("BitConverter.ToUInt16({memory}, {idx});"),
        (I16, true) => load += &format!("BitConverter.ToInt16({memory}, {idx});"),
        (Val(I32), false) => load += &format!("BitConverter.ToUInt32({memory}, {idx});"),
        (Val(I32), true) => load += &format!("BitConverter.ToInt32({memory}, {idx});"),
        (Val(I64), true) => load += &format!("BitConverter.ToInt64({memory}, {idx});"),
        _ => unreachable!(),
    }
    push_line(code, load);

    if mem_type == I8 && signed {
        push_line(code, format!("if ({result} >= 0x80) {result} |= -0x100;"));
    }
}

fn int_store(module: &Module<'_>, code: &mut Code, mem_type: StorageType, idx: Var, var: Var) {
    use {StorageType::*, ValType::*};

    let memory = &module.memory.as_ref().unwrap().name;
    push_line(
        code,
        match mem_type {
            I8 => format!("{memory}[{idx}] = {}({var} & 0xff);", CsType::Byte.cast()),
            I16 => format!("Array.Copy(BitConverter.GetBytes(Convert.ToUInt16({var} & 0xffff)), 0, {memory}, {idx}, 2);"),
            Val(I32) => match var.ty {
                CsType::Int => format!("Array.Copy(BitConverter.GetBytes({var}), 0, {memory}, {idx}, 4);"),
                CsType::Long => format!("Array.Copy(BitConverter.GetBytes({}({var} & 0xffffffff)), 0, {memory}, {idx}, 4);", CsType::UInt.cast()),
                _ => unreachable!(),
            },
            Val(I64) => format!("Array.Copy(BitConverter.GetBytes({var}), 0, {memory}, {idx}, 8);"),
            _ => unreachable!(),
        },
    );
}

fn bits_to_float(code: &mut Code, i_var: Var, f_var: Var) {
    push_line(
        code,
        match f_var.ty {
            CsType::Float => format!("{f_var} = BitConverter.Int32BitsToSingle({i_var});"),
            CsType::Double => format!("{f_var} = BitConverter.Int64BitsToDouble({i_var});"),
            _ => unreachable!(),
        },
    );
}

fn float_to_bits(code: &mut Code, f_var: Var, i_var: Var) {
    push_line(
        code,
        match f_var.ty {
            CsType::Float => format!("{i_var} = BitConverter.SingleToInt32Bits({f_var});"),
            CsType::Double => format!("{i_var} = BitConverter.DoubleToInt64Bits({f_var});"),
            _ => unreachable!(),
        },
    );
}

fn push_line(code: &mut Code, line: String) {
    code.instrs.push(Instr::Line(line));
}
