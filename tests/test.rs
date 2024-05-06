#![warn(rust_2018_idioms)]

mod cs_proj;
mod value;

use std::{fs::read_to_string, path::PathBuf};

use cs_proj::{CsProj, CsProjExec};
use wasm2usharp::parse::convert_to_ident;
use wast::{
    core::{Module, WastArgCore},
    lexer::Lexer,
    parser::{self, ParseBuffer},
    token::{Float32, Float64, Id},
    QuoteWat, Wast, WastArg, WastDirective, WastExecute, WastInvoke, WastRet, Wat,
};

use crate::value::wast_ret_core_contains;

macro_rules! test (
    ($func_name:ident, $name:expr) => {
        #[test]
        fn $func_name() {
            test_wast($name, None);
        }
    };
    ($func_name:ident, $name:expr, $filter:expr) => {
        #[test]
        fn $func_name() {
            test_wast($name, Some($filter));
        }
    };
);

macro_rules! test_ignore {
    ($func_name:ident, $name:expr) => {
        #[test]
        #[ignore]
        fn $func_name() {
            test_wast($name, None);
        }
    };
}

test!(test_address, "address");
test!(test_align, "align");
test!(test_binary_leb128, "binary-leb128");
test!(test_binary, "binary");
test!(test_block, "block");
test!(test_br, "br");
test!(test_br_if, "br_if");
test_ignore!(test_br_table, "br_table");
test!(test_break_drop, "break-drop");
test!(test_call, "call");
test!(test_call_indirect, "call_indirect");
test!(test_comments, "comments");
test!(test_const, "const");
test!(test_conversions, "conversions");
test!(test_custom, "custom");
test!(test_data, "data");
test!(test_elem, "elem");
test!(test_endianness, "endianness");
test!(test_exports, "exports");
test!(test_f32, "f32");
test!(
    test_f32_bitwise,
    "f32_bitwise",
    &deny_float_nan_case(&["copysign"])
);
test!(test_f32_cmp, "f32_cmp");
test!(test_f64, "f64");
test!(
    test_f64_bitwise,
    "f64_bitwise",
    &deny_float_nan_case(&["copysign"])
);
test!(test_f64_cmp, "f64_cmp");
test!(test_fac, "fac");
test!(test_float_exprs, "float_exprs");
test!(test_float_literals, "float_literals");
test!(test_float_memory, "float_memory");
test!(
    test_float_misc,
    "float_misc",
    &deny_float_nan_case(&["f32.copysign", "f64.copysign"])
);
test!(test_forward, "forward");
test!(test_func, "func");
test!(test_func_ptrs, "func_ptrs");
test!(test_globals, "globals");
test!(test_if, "if");
// test!(test_imports, "imports");
// test!(test_inline_module, "inline-module");
test!(test_int_exprs, "int_exprs");
test!(test_int_literals, "int_literals");
test!(test_labels, "labels");
test!(test_left_to_right, "left-to-right");
// test!(test_linking, "linking");
test!(test_load, "load");
test!(test_local_get, "local_get");
test!(test_local_set, "local_set");
test!(test_local_tee, "local_tee");
test!(test_loop, "loop");
test!(test_memory, "memory");
test!(test_memory_grow, "memory_grow");
test!(test_memory_redundancy, "memory_redundancy");
test!(test_memory_size, "memory_size");
test!(test_memory_trap, "memory_trap");
// test!(test_names, "names");
test!(test_nop, "nop");
test!(test_return, "return");
test!(test_select, "select");
test!(test_skip_stack_guard_page, "skip-stack-guard-page");
test!(test_stack, "stack");
test!(test_start, "start");
test!(test_store, "store");
test!(test_switch, "switch");
test!(test_table, "table");
test!(test_token, "token");
test!(test_traps, "traps");
test!(test_type, "type");
test!(test_typecheck, "typecheck");
test!(test_unreachable, "unreachable");
test!(test_unreached_invalid, "unreached-invalid");
test!(test_unwind, "unwind");
test!(test_utf8_custom_section_id, "utf8-custom-section-id");
test!(test_utf8_import_field, "utf8-import-field");
test!(test_utf8_import_module, "utf8-import-module");
test!(test_utf8_invalid_encoding, "utf8-invalid-encoding");

// Non-trapping float-to-int conversions
test!(
    test_nontrapping_float_to_int_conversions,
    "proposals/nontrapping-float-to-int-conversions/conversions"
);

// Sign-extension operators
test!(
    test_sign_extension_ops_i32,
    "proposals/sign-extension-ops/i32",
    &deny_int_neg_max_case(&["rem_s"])
);
test!(
    test_sign_extension_ops_i64,
    "proposals/sign-extension-ops/i64",
    &deny_int_neg_max_case(&["rem_s"])
);

// Bulk memory operations
test!(
    test_memory_copy,
    "proposals/bulk-memory-operations/memory_copy"
);
test!(
    test_memory_fill,
    "proposals/bulk-memory-operations/memory_fill"
);

// traitのエイリアス
trait Filter: Fn(&WastDirective<'_>) -> bool {}
impl<F> Filter for F where F: Fn(&WastDirective<'_>) -> bool {}

/// AssertReturnで、指定された関数名で、引数で与えられた関数の結果がfalseのものを処理しない
fn deny_assert_return<F>(names: &'static [&'static str], filter: F) -> impl Filter
where
    F: Fn(&WastInvoke<'_>, &Vec<WastRet<'_>>) -> bool,
{
    move |directive| match directive {
        WastDirective::AssertReturn {
            exec: WastExecute::Invoke(invoke),
            results,
            ..
        } => !names.contains(&invoke.name) || filter(invoke, results),
        _ => true,
    }
}

// いずれかの引数がNaNなら処理しない
fn deny_float_nan_case(names: &'static [&'static str]) -> impl Filter {
    deny_assert_return(names, move |invoke, _| {
        !(invoke.args.iter().any(|x| match x {
            WastArg::Core(x) => match x {
                WastArgCore::F32(x) => f32::from_bits(x.bits).is_nan(),
                WastArgCore::F64(x) => f64::from_bits(x.bits).is_nan(),
                _ => false,
            },
            _ => false,
        }))
    })
}

// 最初の引数が符号付き整数型の負の最大値なら処理しない
fn deny_int_neg_max_case(names: &'static [&'static str]) -> impl Filter {
    deny_assert_return(names, move |invoke, _| {
        !(match &invoke.args[0] {
            WastArg::Core(x) => match x {
                WastArgCore::I32(x) => *x as u32 == 0x80000000,
                WastArgCore::I64(x) => *x as u64 == 0x8000000000000000,
                _ => false,
            },
            _ => false,
        })
    })
}

fn test_wast(name: &str, filter: Option<&dyn Filter>) {
    let buf = read_to_string("tests/spectest.wast").unwrap();

    let mut wast_path: PathBuf = PathBuf::from("tests/testsuite/");
    wast_path.push(name);
    wast_path.set_extension("wast");

    let buf = buf + &read_to_string(&wast_path).unwrap();

    let adjust_wast = |mut err: wast::Error| {
        err.set_path(&wast_path);
        err.set_text(&buf);
        err
    };

    let lexer = Lexer::new(&buf);
    let buf: ParseBuffer<'_> = ParseBuffer::new_with_lexer(lexer)
        .map_err(adjust_wast)
        .unwrap();
    let mut ast = parser::parse::<Wast<'_>>(&buf)
        .map_err(adjust_wast)
        .unwrap();

    let mut cs_proj = CsProj::new();

    // 各モジュールのクラスファイルの生成
    for directive in ast.directives.iter_mut() {
        match directive {
            WastDirective::Wat(wat) => {
                // Wasmとして読み込んでC#に変換
                cs_proj.add_module(get_wat_id(wat), &wat.encode().unwrap());
            }
            WastDirective::Register { name, module, .. } => {
                cs_proj.register(name.to_string(), module);
            }
            _ => (),
        }
    }

    let mut cs_proj_exec = cs_proj.run();

    // モジュールの実行
    for directive in ast.directives {
        use wast::WastDirective::*;
        if let Some(filter) = &filter {
            if !filter(&directive) {
                println!("Directive is skipped");
                continue;
            }
        }
        match directive {
            Wat(wat) => match wat {
                QuoteWat::Wat(wast::Wat::Module(Module { id, .. })) => {
                    cs_proj_exec.next_module(id);
                }
                _ => unreachable!(),
            },
            Invoke(invoke) => {
                invoke_func(invoke, &mut cs_proj_exec);
            }
            AssertReturn { exec, results, .. } => {
                let actual = match exec {
                    WastExecute::Invoke(invoke) => invoke_func(invoke, &mut cs_proj_exec),
                    WastExecute::Get { module, global } => {
                        get_global(module, global, &mut cs_proj_exec)
                    }
                    _ => panic!(),
                };

                let expected = results
                    .into_iter()
                    .map(|x| match x {
                        WastRet::Core(x) => x,
                        WastRet::Component(_) => panic!("component-model is not supported"),
                    })
                    .collect::<Vec<_>>();

                assert_eq!(
                    actual.len(),
                    expected.len(),
                    "assertion failed\n   actual = {:?}\n expected = {:?}",
                    actual,
                    expected
                );
                assert!(
                    expected
                        .iter()
                        .zip(actual.iter())
                        .all(|(e, a)| wast_ret_core_contains(e, a)),
                    "assertion failed\n   actual = {:?}\n expected = {:?}",
                    actual,
                    expected
                );
            }
            AssertMalformed { .. }
            | AssertInvalid { .. }
            | AssertTrap { .. }
            | AssertExhaustion { .. }
            | AssertUnlinkable { .. }
            | AssertException { .. } => {
                println!("Failure case is skipped")
            }
            Register { .. } => {}
        }
    }
}

fn get_wat_id<'a>(wat: &QuoteWat<'a>) -> Option<Id<'a>> {
    match wat {
        QuoteWat::Wat(Wat::Module(Module { id, .. })) => *id,
        QuoteWat::QuoteModule(..) => panic!("QuoteModule is not supported"),
        QuoteWat::Wat(Wat::Component(_)) | QuoteWat::QuoteComponent(..) => {
            panic!("component-model is not supported")
        }
    }
}

fn invoke_func<'input>(
    invoke: WastInvoke<'input>,
    exec: &mut CsProjExec<'input, '_>,
) -> Vec<WastArgCore<'static>> {
    use wast::core::WastArgCore::*;
    println!("Invoking: {} {:?}", invoke.name, invoke.args);

    // 最初のコマンド引数に呼び出す関数名を指定
    let mut args = vec![convert_to_ident(invoke.name)];
    // 2番目以降のコマンド引数に関数の引数を指定
    args.extend(invoke.args.iter().flat_map(|arg| match arg {
        WastArg::Core(arg) => match arg {
            I32(x) => ["i32".to_string(), x.to_string()],
            I64(x) => ["i64".to_string(), x.to_string()],
            F32(x) => ["f32".to_string(), x.bits.to_string()],
            F64(x) => ["f64".to_string(), x.bits.to_string()],
            V128(_) => panic!("simd is not supported"),
            RefNull(_) | RefExtern(_) | RefHost(_) => {
                panic!("reference-type is not supported")
            }
        },
        WastArg::Component(_) => panic!("component-model is not supported"),
    }));

    let args = args.join(" ");
    let line = exec.invoke(invoke.module, &args);

    parse_results(&line)
}

fn get_global<'input>(
    module: Option<Id<'input>>,
    global: &'input str,
    exec: &mut CsProjExec<'input, '_>,
) -> Vec<WastArgCore<'static>> {
    println!("Get: {global}");

    let line = exec.get_global(module, global);

    parse_results(&line)
}

fn parse_results(line: &str) -> Vec<WastArgCore<'static>> {
    let mut results = Vec::new();

    // スペースで区切った1つ目が型で、2つ目が値のビットを数値で表現した文字列
    let mut sp = line.split_whitespace();
    let ty = sp.next();
    if let Some(ty) = ty {
        let val = sp.next().unwrap();
        assert!(sp.next().is_none());

        results.push(match ty {
            "i32" => WastArgCore::I32(val.parse::<i32>().unwrap()),
            "i64" => WastArgCore::I64(val.parse::<i64>().unwrap()),
            "f32" => WastArgCore::F32(Float32 {
                bits: val.parse::<u32>().unwrap(),
            }),
            "f64" => WastArgCore::F64(Float64 {
                bits: val.parse::<u64>().unwrap(),
            }),
            _ => panic!("unknown return type"),
        });
    }

    results
}
