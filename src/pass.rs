mod breakable;
mod recursive;

use crate::ir::module::Module;

use self::{breakable::breakable, recursive::recursive};

pub fn run_passes(module: &mut Module<'_>) {
    for func in module.all_funcs.iter_mut() {
        let Some(code) = func.code.as_mut() else {
            continue;
        };

        breakable(code);
    }

    recursive(module);
}
