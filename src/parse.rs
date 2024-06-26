mod code;
mod module;
mod pool;

use std::collections::HashSet;

use anyhow::Result;
use once_cell::sync::Lazy;
use regex::Regex;

use crate::ir::{module::Module, START};

use self::module::ModuleParser;

pub fn parse_module<'input>(
    module: &mut Module<'input>,
    import_map: &dyn Fn(&str) -> String,
) -> Result<HashSet<&'input str>> {
    let mut conv = ModuleParser::new(module);
    conv.parse(import_map)
}

pub fn convert_to_ident(mut name: &str) -> String {
    if name == "_start" {
        name = START;
    }

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

#[derive(Debug, thiserror::Error)]
pub enum NotSupportedError {
    #[error("`{0}` instrunction is not supported")]
    Instruction(&'static str),
    #[error("{0} is not supported")]
    Feature(&'static str),
}
