mod converter;

use std::env;

use anyhow::Result;
use wasmparser::validate;

use crate::converter::Converter;

fn main() -> Result<()> {
    let args = env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        println!("Usage: {} in.wasm", args[0]);
        return Ok(());
    }

    let buf: Vec<u8> = std::fs::read(&args[1])?;
    validate(&buf)?;
    let mut converter = Converter::new(&buf);
    converter.convert()?;

    Ok(())
}
