#![warn(rust_2018_idioms)]

pub mod converter;
pub mod util;

use std::{
    fs,
    io::{BufWriter, Write},
    path::Path,
    process::ExitCode,
};

use clap::Parser;
use wasmparser::validate;

use converter::{convert_to_ident, Converter};

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
    let mut converter = Converter::new(&buf, class_name, args.test);

    let mut out_file = BufWriter::new(match &args.output {
        Some(x) => Box::new(fs::File::create(x).unwrap()) as Box<dyn Write>,
        None => Box::new(std::io::stdout()) as Box<dyn Write>,
    });

    converter
        .convert(&mut out_file, |module| {
            format!("class_{}", convert_to_ident(module))
        })
        .unwrap();

    ExitCode::SUCCESS
}
