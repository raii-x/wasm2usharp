use cranelift_entity::{
    packed_option::{PackedOption, ReservedValue},
    EntityRef, SecondaryMap,
};

#[derive(Clone)]
pub struct Node<T, P, C>
where
    T: ReservedValue,
    P: ReservedValue,
    C: ReservedValue,
{
    pub parent: PackedOption<P>,
    pub prev: PackedOption<T>,
    pub next: PackedOption<T>,
    pub first_child: PackedOption<C>,
    pub last_child: PackedOption<C>,
}

impl<T, P, C> Node<T, P, C>
where
    T: ReservedValue,
    P: ReservedValue,
    C: ReservedValue + Copy,
{
    pub fn iter(&self) -> Iter<C> {
        Iter(self.first_child.expand())
    }
}

impl<T, P, C> Default for Node<T, P, C>
where
    T: ReservedValue,
    P: ReservedValue,
    C: ReservedValue,
{
    fn default() -> Self {
        Self {
            parent: Default::default(),
            prev: Default::default(),
            next: Default::default(),
            first_child: Default::default(),
            last_child: Default::default(),
        }
    }
}

pub struct Iter<T>(Option<T>);

impl<T> Iter<T> {
    pub fn next<P, C>(&mut self, nodes: &SecondaryMap<T, Node<T, P, C>>) -> Option<T>
    where
        T: ReservedValue + EntityRef,
        P: ReservedValue + Clone,
        C: ReservedValue + Clone,
    {
        let current = self.0;
        if let Some(id) = self.0 {
            self.0 = nodes[id].next.expand();
        }
        current
    }
}
