mod converter;

use std::{env, process::ExitCode};

use wasmparser::validate;

use crate::converter::Converter;

fn main() -> ExitCode {
    let mut file_path = None;
    let mut test = false;

    let args = env::args().collect::<Vec<_>>();
    for arg in &args[1..] {
        match arg.as_str() {
            "--test" => test = true,
            _ => match file_path {
                Some(_) => {
                    // 入力ファイルが2個以上指定されている場合はエラー
                    show_usage(&args[0]);
                    return ExitCode::FAILURE;
                }
                None => file_path = Some(arg),
            },
        }
    }

    let file_path = match file_path {
        Some(x) => x,
        None => {
            // 入力ファイルが指定されていない場合はエラー
            show_usage(&args[0]);
            return ExitCode::FAILURE;
        }
    };

    let buf: Vec<u8> = std::fs::read(file_path).unwrap();
    validate(&buf).unwrap();
    let mut converter = Converter::new(&buf, test);
    converter.convert(&mut std::io::stdout()).unwrap();

    ExitCode::SUCCESS
}

fn show_usage(program: &str) {
    println!("Usage: {} [--test] file", program);
}
