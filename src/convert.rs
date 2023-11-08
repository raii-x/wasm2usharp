mod builtin_func;
mod code;
mod module;

use std::{collections::HashSet, io::Write};

use anyhow::Result;
use once_cell::sync::Lazy;
use regex::Regex;

use crate::{ir::module::Module, pass::run_passes};

use self::module::ModuleConverter;

pub fn convert<'input>(
    buf: &'input [u8],
    class_name: String,
    test: bool,
    out_file: &mut dyn Write,
    import_map: &dyn Fn(&str) -> String,
) -> Result<HashSet<&'input str>> {
    let mut module = Module::new(buf, class_name, test);
    let mut conv = ModuleConverter::new(&mut module);

    let ret = conv.convert(import_map)?;
    run_passes(&mut module);

    module.write(out_file)?;

    Ok(ret)
}

pub fn convert_to_ident(name: &str) -> String {
    static KEYWORDS: Lazy<HashSet<&str>> = Lazy::new(|| {
        HashSet::from([
            "abstract",
            "as",
            "base",
            "bool",
            "break",
            "byte",
            "case",
            "catch",
            "char",
            "checked",
            "class",
            "const",
            "continue",
            "decimal",
            "default",
            "delegate",
            "do",
            "double",
            "else",
            "enum",
            "event",
            "explicit",
            "extern",
            "false",
            "finally",
            "fixed",
            "float",
            "for",
            "foreach",
            "goto",
            "if",
            "implicit",
            "in",
            "int",
            "interface",
            "internal",
            "is",
            "lock",
            "long",
            "namespace",
            "new",
            "null",
            "object",
            "operator",
            "out",
            "override",
            "params",
            "private",
            "protected",
            "public",
            "readonly",
            "ref",
            "return",
            "sbyte",
            "sealed",
            "short",
            "sizeof",
            "stackalloc",
            "static",
            "string",
            "struct",
            "switch",
            "this",
            "throw",
            "true",
            "try",
            "typeof",
            "uint",
            "ulong",
            "unchecked",
            "unsafe",
            "ushort",
            "using",
            "virtual",
            "void",
            "volatile",
            "while",
        ])
    });

    let prefix = if name.chars().next().unwrap().is_ascii_digit() || KEYWORDS.contains(name) {
        "_"
    } else {
        ""
    };

    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\W").unwrap());
    let ident = RE.replace_all(name, "_");

    prefix.to_string() + ident.as_ref()
}
