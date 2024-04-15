use super::instr::{Instr, InstrChild, InstrKind, InstrNode};

pub struct Builder {
    blocks: Vec<Vec<InstrNode>>,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            blocks: vec![Vec::new()],
        }
    }

    pub fn push(&mut self, instr: Instr) {
        let breakable = match instr.kind {
            InstrKind::Block | InstrKind::Loop { .. } | InstrKind::If { .. } => Some(true),
            InstrKind::Switch(_) => Some(false),
            _ => None,
        };

        self.blocks.last_mut().unwrap().push(InstrNode {
            instr,
            child: breakable.map(|breakable| InstrChild {
                blocks: Vec::new(),
                breakable,
            }),
        });
    }

    pub fn start_block(&mut self) {
        self.blocks.push(Vec::new());
    }

    pub fn end_block(&mut self) {
        let block = self.blocks.pop().unwrap();
        let instr = self.blocks.last_mut().unwrap().last_mut().unwrap();

        instr.child.as_mut().unwrap().blocks.push(block);
    }

    pub fn build(self) -> Vec<InstrNode> {
        assert!(self.blocks.len() == 1);
        self.blocks.into_iter().next().unwrap()
    }
}

impl Default for Builder {
    fn default() -> Self {
        Self::new()
    }
}
