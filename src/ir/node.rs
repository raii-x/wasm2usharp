use cranelift_entity::{EntityRef, SecondaryMap};

pub trait Node<T, P, C> {
    fn parent(&self) -> Option<P>;
    fn prev(&self) -> Option<T>;
    fn next(&self) -> Option<T>;
    fn first_child(&self) -> Option<C>;
    fn last_child(&self) -> Option<C>;

    fn iter(&self) -> Iter<C> {
        Iter(self.first_child())
    }
}

macro_rules! node_def {
    ($name:ident, $t:ty, $p:ty, $c:ty) => {
        #[derive(Clone, Default)]
        pub struct $name {
            pub parent: PackedOption<$c>,
            pub prev: PackedOption<$t>,
            pub next: PackedOption<$t>,
            pub first_child: PackedOption<$c>,
            pub last_child: PackedOption<$c>,
        }

        impl Node<$t, $p, $c> for $name {
            fn parent(&self) -> Option<$p> {
                self.parent.expand()
            }
            fn prev(&self) -> Option<$t> {
                self.prev.expand()
            }
            fn next(&self) -> Option<$t> {
                self.next.expand()
            }
            fn first_child(&self) -> Option<$c> {
                self.first_child.expand()
            }
            fn last_child(&self) -> Option<$c> {
                self.last_child.expand()
            }
        }
    };
}
pub(crate) use node_def;

pub struct Iter<T>(Option<T>);

impl<T> Iter<T> {
    pub fn next<N, P, C>(&mut self, nodes: &SecondaryMap<T, N>) -> Option<T>
    where
        T: EntityRef,
        N: Node<T, P, C> + Clone,
    {
        let current = self.0;
        if let Some(id) = self.0 {
            self.0 = nodes[id].next();
        }
        current
    }
}
