#![warn(rust_2018_idioms, clippy::clone_on_ref_ptr)]

pub mod codegen;
pub mod ir;
pub mod parse;
pub mod pass;
pub mod util;

use std::{
    collections::HashSet,
    fs,
    io::{BufWriter, Write},
    path::PathBuf,
};

use anyhow::{Context, Result};
use clap::Parser;
use codegen::codegen_module;
use ir::module::Module;
use pass::run_passes;
use wasmparser::validate;

use parse::{convert_to_ident, parse_module};

#[derive(Parser, Debug)]
struct Args {
    /// Input file
    input: PathBuf,
    /// Write output to file
    #[arg(short)]
    output: Option<PathBuf>,
    /// Convert to C# for test
    #[arg(long)]
    test: bool,
}

pub fn run() -> anyhow::Result<()> {
    let args = Args::parse();

    let class_name = match &args.output {
        Some(x) => x
            .file_stem()
            .context("The output path has no file name")?
            .to_string_lossy(),
        None => "Wasm2USharp".into(),
    };

    let buf: Vec<u8> = std::fs::read(args.input)?;
    validate(&buf)?;

    let mut out_file = BufWriter::new(match &args.output {
        Some(x) => Box::new(fs::File::create(x)?) as Box<dyn Write>,
        None => Box::new(std::io::stdout()) as Box<dyn Write>,
    });

    let import_map = |module: &_| format!("class_{}", convert_to_ident(module));

    convert(
        &buf,
        class_name.to_string(),
        args.test,
        &mut out_file,
        &import_map,
    )?;

    Ok(())
}

pub fn convert<'input>(
    buf: &'input [u8],
    class_name: String,
    test: bool,
    out_file: &mut dyn Write,
    import_map: &dyn Fn(&str) -> String,
) -> Result<HashSet<&'input str>> {
    let mut module = Module::new(buf, class_name, test);

    let ret = parse_module(&mut module, import_map)?;
    run_passes(&mut module);
    codegen_module(&module, out_file)?;

    Ok(ret)
}
