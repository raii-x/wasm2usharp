mod breakable;
mod copy_propagation;
mod recursive;
mod var;

use crate::ir::module::Module;

use self::{
    breakable::breakable,
    copy_propagation::copy_propagation,
    recursive::recursive,
    var::{remove_unused_break_depth, remove_unused_vars},
};

pub fn run_passes(module: &mut Module<'_>) {
    recursive(module);

    for func in module.all_funcs.iter_mut() {
        let Some(code) = func.code.as_mut() else {
            continue;
        };

        breakable(code);
        copy_propagation(code);
        remove_unused_vars(code);
        remove_unused_break_depth(code);
    }
}
