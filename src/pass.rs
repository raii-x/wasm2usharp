mod recursive;

use crate::ir::module::Module;

use self::recursive::recursive;

pub fn run_passes(module: &mut Module<'_>) {
    recursive(module);
}
