mod breakable;
mod copy_propagation;
mod fix_param_overwrite;
mod recursive;
mod var;

use crate::{ir::module::Module, pass::fix_param_overwrite::fix_param_overwrite};

use self::{
    breakable::breakable,
    copy_propagation::copy_propagation,
    recursive::recursive,
    var::{remove_unused_break_depth, remove_unused_vars},
};

pub fn run_passes(module: &mut Module<'_>) {
    recursive(module);

    for (i, func) in module.all_funcs.iter_mut().enumerate() {
        let func_ty = &func.header.ty;
        let Some(code) = func.code.as_mut() else {
            continue;
        };

        breakable(code);
        copy_propagation(func_ty, code);
        remove_unused_vars(code);
        remove_unused_break_depth(code);
        fix_param_overwrite(code, i);
    }
}
