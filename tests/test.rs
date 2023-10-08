extern crate wasm2usharp;

use std::{
    fs::{read_to_string, File},
    io::{BufRead, BufReader, Write},
    path::{Path, PathBuf},
    process::{Child, Command, Stdio},
};

use tempfile::tempdir;
use wasm2usharp::convert_to_ident;
use wast::{
    core::{NanPattern, WastRetCore},
    lexer::Lexer,
    parser::{self, ParseBuffer},
    token::{Float32, Float64},
    QuoteWat, Wast, WastArg, WastExecute, WastRet, Wat,
};

macro_rules! test (
    ($func_name:ident, $name:expr) => {
        #[test]
        fn $func_name() {
            test_wast($name);
        }
    };
);

test!(test_block, "block");
// test!(test_br_table, "br_table");
// test!(test_br_if, "br_if");

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

    let out_dir = tempdir().unwrap();
    let mut child = None;

    for directive in ast.directives {
        use wast::WastDirective::*;
        match directive {
            Wat(module) => child = Some(run_wat(module, out_dir.path())),
            Invoke(invoke) => {}
            AssertReturn { exec, results, .. } => {
                assert_eq!(
                    &execute(exec, child.as_mut().unwrap()),
                    &results.into_iter().map(WastRetEq).collect::<Vec<_>>()
                );
            }
            AssertInvalid => (),
            TypeUse => {}
            AssertMalformed => {}
        }
    }
}

fn run_wat(mut wat: QuoteWat<'_>, out_dir: &Path) -> Child {
    println!("Running wat");

    match &wat {
        QuoteWat::Wat(Wat::Module(_)) | QuoteWat::QuoteModule(..) => (),
        QuoteWat::Wat(Wat::Component(_)) | QuoteWat::QuoteComponent(..) => {
            panic!("component-model is not supported")
        }
    };

    // .NETプロジェクトを作成
    Command::new("dotnet")
        .args(["new", "console", "-o", out_dir.to_str().unwrap()])
        .status()
        .unwrap();

    let bytes = wat.encode().unwrap();
    let mut conv = wasm2usharp::Converter::new(&bytes, false);

    {
        // Program.csファイルに書き込み
        let mut cs_file = File::create(out_dir.join("Program.cs")).unwrap();
        conv.convert(&mut cs_file).unwrap();
    }

    // .NETプロジェクトをビルド
    Command::new("dotnet")
        .args(["build", out_dir.to_str().unwrap()])
        .status()
        .unwrap();

    // .NETプロジェクトを実行
    Command::new("dotnet")
        .args(["run", "--project", out_dir.to_str().unwrap(), "--no-build"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap()
}

fn execute<'a>(exec: WastExecute, child: &mut Child) -> Vec<WastRetEq<'a>> {
    match exec {
        wast::WastExecute::Invoke(invoke) => {
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

            // if !output.status.success() {
            //     panic!("{}", String::from_utf8(output.stderr).unwrap());
            // }

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
        _ => panic!(),
    }
}

fn f32_to_wasm_ret_core<'a>(bits: u32) -> WastRetCore<'a> {
    WastRetCore::F32(match f_to_wasm_ret_core(bits as u64, 32, 23) {
        NanPattern::CanonicalNan => NanPattern::CanonicalNan,
        NanPattern::ArithmeticNan => NanPattern::ArithmeticNan,
        NanPattern::Value(bits) => NanPattern::Value(Float32 { bits: bits as u32 }),
    })
}

fn f64_to_wasm_ret_core<'a>(bits: u64) -> WastRetCore<'a> {
    WastRetCore::F64(match f_to_wasm_ret_core(bits, 64, 52) {
        NanPattern::CanonicalNan => NanPattern::CanonicalNan,
        NanPattern::ArithmeticNan => NanPattern::ArithmeticNan,
        NanPattern::Value(bits) => NanPattern::Value(Float64 { bits }),
    })
}

fn f_to_wasm_ret_core(bits: u64, size_bits: u64, frac_bits: u64) -> NanPattern<u64> {
    let expo_bits = size_bits - 1 - frac_bits;
    let expo_mask = bit_mask(expo_bits) << frac_bits;

    if bits & expo_mask == expo_mask {
        let frac = bits & bit_mask(frac_bits);
        if frac == 1 << (frac_bits - 1) {
            NanPattern::CanonicalNan
        } else {
            NanPattern::ArithmeticNan
        }
    } else {
        NanPattern::Value(bits)
    }
}

/// 与えられた数のLSBからのビットが全て1の数を返す
fn bit_mask(bits: u64) -> u64 {
    (0..bits).fold(0, |acc, x| acc | (1 << x))
}

#[derive(Debug)]
struct WastRetEq<'a>(WastRet<'a>);

impl<'a> PartialEq for WastRetEq<'a> {
    fn eq(&self, other: &Self) -> bool {
        wast_ret_eq(&self.0, &other.0)
    }
}

fn wast_ret_eq(lhs: &WastRet, rhs: &WastRet) -> bool {
    use WastRet::*;
    match (lhs, rhs) {
        (Core(l0), Core(r0)) => wast_ret_core_eq(l0, r0),
        (Component(_), Component(_)) => {
            panic!("component-model is not supported")
        }
        _ => false,
    }
}

fn wast_ret_core_eq(lhs: &WastRetCore, rhs: &WastRetCore) -> bool {
    use WastRetCore::*;
    match (lhs, rhs) {
        (I32(l0), I32(r0)) => l0 == r0,
        (I64(l0), I64(r0)) => l0 == r0,
        (F32(l0), F32(r0)) => nan_pattern_eq(l0, r0),
        (F64(l0), F64(r0)) => nan_pattern_eq(l0, r0),
        _ => false,
    }
}

fn nan_pattern_eq<T: PartialEq>(
    lhs: &NanPattern<impl FloatBits<T>>,
    rhs: &NanPattern<impl FloatBits<T>>,
) -> bool {
    use NanPattern::*;
    match (lhs, rhs) {
        (CanonicalNan, CanonicalNan) => true,
        (ArithmeticNan, ArithmeticNan) => true,
        (Value(l0), Value(r0)) => l0.float_bits() == r0.float_bits(),
        _ => false,
    }
}

trait FloatBits<T> {
    fn float_bits(&self) -> T;
}

impl FloatBits<u32> for Float32 {
    fn float_bits(&self) -> u32 {
        self.bits
    }
}

impl FloatBits<u64> for Float64 {
    fn float_bits(&self) -> u64 {
        self.bits
    }
}
