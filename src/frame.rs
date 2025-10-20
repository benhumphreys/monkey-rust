use crate::code::Instructions;

pub struct Frame {
    func: Instructions,
    pub ip: usize,
}

impl Frame {
    pub fn new(func: Instructions) -> Self {
        Self {
            func,
            ip: 0,
        }
    }

    pub fn instructions(&self) -> &Instructions {
        &self.func
    }
}