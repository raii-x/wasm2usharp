extern crate wasm2usharp;

use std::{fs::read_to_string, path::Path};

use wast::{
    lexer::Lexer,
    parser::{self, ParseBuffer},
    QuoteWat, Wast, WastDirective, Wat,
};

#[test]
fn block() {
    test_wast(Path::new("tests/testsuite/block.wast"));
}

fn test_wast(path: &Path) {
    let buf = read_to_string(path).unwrap();

    let adjust_wast = |mut err: wast::Error| {
        err.set_path(path.as_ref());
        err.set_text(&buf);
        err
    };

    let lexer = Lexer::new(&buf);
    let buf = ParseBuffer::new_with_lexer(lexer)
        .map_err(adjust_wast)
        .unwrap();
    let ast = parser::parse::<Wast>(&buf).map_err(adjust_wast).unwrap();

    for directive in ast.directives {
        match directive {
            WastDirective::Wat(module) => compile_wat(module),
            _ => (),
        }
        // println!("{:?}", directive);
    }
}

fn compile_wat(mut wat: QuoteWat<'_>) {
    match &wat {
        QuoteWat::Wat(Wat::Module(_)) | QuoteWat::QuoteModule(..) => (),
        QuoteWat::Wat(Wat::Component(_)) | QuoteWat::QuoteComponent(..) => {
            panic!("component-model support not enabled")
        }
    };

    let bytes = wat.encode().unwrap();
    let mut conv = wasm2usharp::Converter::new(&bytes, false);
    conv.convert(&mut std::io::stdout()).unwrap();
}
