use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::ir::{
    builder::Builder,
    ty::{Const, CsType},
    var::{Primary, Var, VarId},
};

/// コードの変数を再利用するためのプール
pub struct VarPool(Rc<RefCell<Pool>>, #[cfg(debug_assertions)] Pool);

type Pool = HashMap<(CsType, Option<Const>), Vec<VarId>>;

impl VarPool {
    pub fn new() -> Self {
        Self(
            Rc::new(RefCell::new(HashMap::new())),
            #[cfg(debug_assertions)]
            HashMap::new(),
        )
    }

    /// プールから変数を取り出す。取り出せなければ新しい変数を作って返す。
    pub fn take(&mut self, builder: &mut Builder, ty: CsType, default: Option<Const>) -> PoolVar {
        let id = self
            .0
            .borrow_mut()
            .get_mut(&(ty, default))
            .and_then(|x| x.pop())
            .unwrap_or_else(|| {
                let id = builder.new_var(Var {
                    ty,
                    default,
                    ..Default::default()
                });
                #[cfg(debug_assertions)]
                self.1.entry((ty, default)).or_default().push(id);
                id
            });

        PoolVar {
            pool: Rc::clone(&self.0),
            key: (ty, default),
            id,
        }
    }

    #[cfg(debug_assertions)]
    pub fn validate(&mut self) {
        self.0.borrow_mut().iter_mut().for_each(|(_, v)| v.sort());
        self.1.iter_mut().for_each(|(_, v)| v.sort());
        assert_eq!(*self.0.borrow(), self.1);
    }
}

pub struct PoolVar {
    pool: Rc<RefCell<Pool>>,
    key: (CsType, Option<Const>),
    id: VarId,
}

impl PoolVar {
    pub fn id(&self) -> VarId {
        self.id
    }
}

impl Drop for PoolVar {
    fn drop(&mut self) {
        // 変数をプールに戻す
        self.pool
            .borrow_mut()
            .entry(self.key)
            .or_default()
            .push(self.id);
    }
}

pub enum PoolPrimary {
    Pool(PoolVar),
    Local(VarId),
    Const(Const),
}

impl From<PoolVar> for PoolPrimary {
    fn from(value: PoolVar) -> Self {
        Self::Pool(value)
    }
}

impl From<Const> for PoolPrimary {
    fn from(value: Const) -> Self {
        Self::Const(value)
    }
}

impl From<&PoolPrimary> for Primary {
    fn from(value: &PoolPrimary) -> Self {
        match value {
            PoolPrimary::Pool(value) => Primary::Var(value.id),
            PoolPrimary::Local(value) => Primary::Var(*value),
            PoolPrimary::Const(value) => Primary::Const(*value),
        }
    }
}

impl From<PoolPrimary> for Primary {
    fn from(value: PoolPrimary) -> Self {
        Primary::from(&value)
    }
}

pub struct LoopVarPool(Rc<RefCell<Vec<u32>>>, #[cfg(debug_assertions)] Vec<u32>);

impl LoopVarPool {
    pub fn new() -> Self {
        Self(
            Rc::new(RefCell::new(Vec::new())),
            #[cfg(debug_assertions)]
            Vec::new(),
        )
    }

    /// プールから変数を取り出す。取り出せなければ新しい変数を作って返す。
    pub fn take(&mut self, builder: &mut Builder) -> PoolLoopVar {
        let index = self.0.borrow_mut().pop().unwrap_or_else(|| {
            let index = builder.code.loop_var_count;
            builder.code.loop_var_count += 1;
            #[cfg(debug_assertions)]
            self.1.push(index);
            index
        });

        PoolLoopVar {
            pool: Rc::clone(&self.0),
            index,
        }
    }

    #[cfg(debug_assertions)]
    pub fn validate(&mut self) {
        self.0.borrow_mut().sort();
        self.1.sort();
        assert_eq!(*self.0.borrow(), self.1);
    }
}

pub struct PoolLoopVar {
    pool: Rc<RefCell<Vec<u32>>>,
    index: u32,
}

impl PoolLoopVar {
    pub fn index(&self) -> u32 {
        self.index
    }
}

impl Drop for PoolLoopVar {
    fn drop(&mut self) {
        // 変数をプールに戻す
        self.pool.borrow_mut().push(self.index);
    }
}
