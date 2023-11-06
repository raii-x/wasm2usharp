#![warn(rust_2018_idioms, clippy::clone_on_ref_ptr)]

pub mod convert;
pub mod ir;
pub mod pass;
pub mod util;

use std::{
    fs,
    io::{BufWriter, Write},
    path::Path,
    process::ExitCode,
};

use clap::Parser;
use wasmparser::validate;

use convert::{convert, convert_to_ident};

#[derive(Parser, Debug)]
struct Args {
    /// Input file
    input: String,
    /// Write output to file
    #[arg(short)]
    output: Option<String>,
    /// Convert to C# for test
    #[arg(long)]
    test: bool,
}

pub fn lib_main() -> ExitCode {
    let args = Args::parse();

    let class_name = match &args.output {
        Some(x) => Path::new(x).file_stem().unwrap().to_str().unwrap(),
        None => "Wasm2USharp",
    };

    let buf: Vec<u8> = std::fs::read(args.input).unwrap();
    validate(&buf).unwrap();

    let mut out_file = BufWriter::new(match &args.output {
        Some(x) => Box::new(fs::File::create(x).unwrap()) as Box<dyn Write>,
        None => Box::new(std::io::stdout()) as Box<dyn Write>,
    });

    let import_map = |module: &_| format!("class_{}", convert_to_ident(module));

    convert(&buf, class_name, args.test, &mut out_file, &import_map).unwrap();

    ExitCode::SUCCESS
}
