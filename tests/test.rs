mod value;

extern crate wasm2usharp;

use std::{
    fs::{read_to_string, File},
    io::{BufRead, BufReader, Write},
    path::PathBuf,
    process::{Child, Command, Stdio},
};

use tempfile::{tempdir, TempDir};
use wasm2usharp::convert_to_ident;
use wast::{
    core::WastRetCore,
    lexer::Lexer,
    parser::{self, ParseBuffer},
    QuoteWat, Wast, WastArg, WastExecute, WastInvoke, WastRet, Wat,
};

use crate::value::{f32_to_wasm_ret_core, f64_to_wasm_ret_core, WastRetEq};

macro_rules! test (
    ($func_name:ident, $name:expr) => {
        #[test]
        fn $func_name() {
            test_wast($name);
        }
    };
);

test!(test_address, "address");
test!(test_align, "align");
test!(test_binary_leb128, "binary-leb128");
test!(test_block, "block");
// test!(test_br_table, "br_table");
// test!(test_br_if, "br_if");

struct DirChild {
    child: Child,
    _dir: TempDir,
}

fn test_wast(name: &str) {
    let mut wast_path: PathBuf = PathBuf::from("tests/testsuite/");
    wast_path.push(name);
    wast_path.set_extension("wast");

    let buf = read_to_string(&wast_path).unwrap();

    let adjust_wast = |mut err: wast::Error| {
        err.set_path(&wast_path);
        err.set_text(&buf);
        err
    };

    let lexer = Lexer::new(&buf);
    let buf: ParseBuffer<'_> = ParseBuffer::new_with_lexer(lexer)
        .map_err(adjust_wast)
        .unwrap();
    let ast = parser::parse::<Wast>(&buf).map_err(adjust_wast).unwrap();

    let mut dir_child = None;

    for directive in ast.directives {
        use wast::WastDirective::*;
        match directive {
            Wat(module) => dir_child = Some(run_wat(module)),
            Invoke(invoke) => {
                invoke_func(invoke, &mut dir_child.as_mut().unwrap().child);
            }
            AssertReturn { exec, results, .. } => match exec {
                WastExecute::Invoke(invoke) => {
                    assert_eq!(
                        &invoke_func(invoke, &mut dir_child.as_mut().unwrap().child),
                        &results
                            .into_iter()
                            .map(WastRetEq)
                            .map(|x| x.into_canonical_nan())
                            .collect::<Vec<_>>()
                    );
                }
                _ => panic!(),
            },
            AssertMalformed { .. }
            | AssertInvalid { .. }
            | AssertTrap { .. }
            | AssertExhaustion { .. }
            | AssertUnlinkable { .. }
            | AssertException { .. } => {
                println!("Failure case is skipped")
            }
            Register { .. } => {
                println!("Register is skipped")
            }
        }
    }
}

fn run_wat(mut wat: QuoteWat<'_>) -> DirChild {
    println!("Running wat");

    match &wat {
        QuoteWat::Wat(Wat::Module(_)) | QuoteWat::QuoteModule(..) => (),
        QuoteWat::Wat(Wat::Component(_)) | QuoteWat::QuoteComponent(..) => {
            panic!("component-model is not supported")
        }
    };

    let dir = tempdir().unwrap();
    let dir_path = dir.path();

    // .NETプロジェクトを作成
    Command::new("dotnet")
        .args(["new", "console", "-o", dir_path.to_str().unwrap()])
        .status()
        .unwrap();

    let bytes = wat.encode().unwrap();
    let mut conv = wasm2usharp::Converter::new(&bytes, true);

    {
        // Program.csファイルに書き込み
        let mut cs_file = File::create(dir_path.join("Program.cs")).unwrap();
        conv.convert(&mut cs_file).unwrap();
    }

    // .NETプロジェクトをビルド
    let status = Command::new("dotnet")
        .args(["build", dir_path.to_str().unwrap()])
        .status()
        .unwrap();
    assert!(status.success());

    // .NETプロジェクトを実行
    let child = Command::new("dotnet")
        .args(["run", "--project", dir_path.to_str().unwrap(), "--no-build"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    DirChild { _dir: dir, child }
}

fn invoke_func<'a>(invoke: WastInvoke, child: &mut Child) -> Vec<WastRetEq<'a>> {
    use wast::core::WastArgCore::*;
    println!("Invoking: {} {:?}", invoke.name, invoke.args);

    // 最初のコマンド引数に呼び出す関数名を指定
    let mut args = vec![convert_to_ident(invoke.name)];
    // 2番目以降のコマンド引数に関数の引数を指定
    args.extend(invoke.args.iter().flat_map(|arg| match arg {
        WastArg::Core(arg) => match arg {
            I32(x) => ["i32".to_string(), (*x as u32).to_string()],
            I64(x) => ["i64".to_string(), (*x as u64).to_string()],
            F32(x) => ["f32".to_string(), x.bits.to_string()],
            F64(x) => ["f64".to_string(), x.bits.to_string()],
            V128(_) => panic!("simd is not supported"),
            RefNull(_) | RefExtern(_) | RefHost(_) => {
                panic!("reference-type is not supported")
            }
        },
        WastArg::Component(_) => panic!("component-model is not supported"),
    }));

    let args = args.join(" ") + "\n";
    let stdin = child.stdin.as_mut().unwrap();
    stdin.write_all(args.as_bytes()).unwrap();

    let stdout = child.stdout.as_mut().unwrap();
    let mut stdout = BufReader::new(stdout);
    let mut line = String::new();
    stdout.read_line(&mut line).unwrap();

    let mut results = Vec::new();

    // スペースで区切った1つ目が型で、2つ目が値のビットを数値で表現した文字列
    let mut sp = line.split_whitespace();
    let ty = sp.next();
    if let Some(ty) = ty {
        let val = sp.next().unwrap();
        assert!(sp.next().is_none());

        results.push(match ty {
            "i32" => WastRetCore::I32(val.parse::<u32>().unwrap() as i32),
            "i64" => WastRetCore::I64(val.parse::<u64>().unwrap() as i64),
            "f32" => f32_to_wasm_ret_core(val.parse::<u32>().unwrap()),
            "f64" => f64_to_wasm_ret_core(val.parse::<u64>().unwrap()),
            _ => panic!("unknown return type"),
        });
    }

    results
        .into_iter()
        .map(WastRet::Core)
        .map(WastRetEq)
        .collect()
}
