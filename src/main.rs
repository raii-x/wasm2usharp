use std::process;

fn main() {
    if let Err(e) = wasm2usharp::run() {
        eprint!("error: {}", e);
        if let Some(source) = e.source() {
            eprint!(": {}", source);
        }
        eprintln!();
        process::exit(1);
    }
}
