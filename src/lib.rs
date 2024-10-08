#![warn(rust_2018_idioms, clippy::clone_on_ref_ptr)]

pub mod codegen;
pub mod ir;
pub mod parse;
pub mod pass;
pub mod util;

use std::{
    collections::HashSet,
    fs::File,
    io::{BufWriter, Read, Write},
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use clap::Parser;
use codegen::codegen_module;
use ir::module::Module;
use pass::run_passes;
use wasmparser::{Validator, WasmFeatures};

use parse::{convert_to_ident, parse_module};

#[derive(Parser, Debug)]
#[command(version, about)]
struct Args {
    /// Input file. If not provided or if this is `-` then stdin is used.
    input: Option<PathBuf>,
    /// Where to place output. If not provided then stdout is used.
    #[arg(short, long)]
    output: Option<PathBuf>,
    /// Namespace to use for the generated class.
    #[arg(short, long)]
    namespace: Option<String>,
    /// Convert to C# instead of UdonSharp for testing.
    #[arg(long)]
    test: bool,
}

pub fn run() -> Result<()> {
    let args = Args::parse();

    let class_name = match &args.output {
        Some(x) => x
            .file_stem()
            .with_context(|| format!("the output path `{}` has no file name", x.to_string_lossy()))?
            .to_string_lossy(),
        None => "Wasm2USharp".into(),
    };

    let buf: Vec<u8> = match &args.input {
        Some(path) if path != Path::new("-") => std::fs::read(path)
            .with_context(|| format!("failed to read from `{}`", path.to_string_lossy()))?,
        _ => {
            let mut buf = Vec::new();
            std::io::stdin()
                .read_to_end(&mut buf)
                .context("failed to read <stdin>")?;
            buf
        }
    };

    let mut out_file = BufWriter::new(match &args.output {
        Some(x) => {
            let file = File::create(x)
                .with_context(|| format!("failed to write `{}`", x.to_string_lossy()))?;
            Box::new(file) as Box<dyn Write>
        }
        None => Box::new(std::io::stdout()) as Box<dyn Write>,
    });

    let import_map = |module: &_| format!("class_{}", convert_to_ident(module));

    convert(
        &buf,
        class_name.to_string(),
        args.namespace,
        args.test,
        &mut out_file,
        &import_map,
    )
    .context("failed to convert to UdonSharp")?;

    Ok(())
}

pub fn convert<'input>(
    buf: &'input [u8],
    class_name: String,
    namespace: Option<String>,
    test: bool,
    out_file: &mut dyn Write,
    import_map: &dyn Fn(&str) -> String,
) -> Result<HashSet<&'input str>> {
    Validator::new_with_features(WasmFeatures {
        mutable_global: true,
        saturating_float_to_int: true,
        sign_extension: true,
        bulk_memory: true,
        floats: true,
        reference_types: false,
        multi_value: false,
        simd: false,
        relaxed_simd: false,
        threads: false,
        tail_call: false,
        multi_memory: false,
        exceptions: false,
        memory64: false,
        extended_const: false,
        component_model: false,
        function_references: false,
        memory_control: false,
        gc: false,
        component_model_values: false,
    })
    .validate_all(buf)?;

    let mut module = Module::new(buf, class_name, namespace, test);

    let ret = parse_module(&mut module, import_map)?;
    run_passes(&mut module);
    codegen_module(&module, out_file)?;

    Ok(ret)
}
