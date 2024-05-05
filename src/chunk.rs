use crate::{value::ValueArray, OpCode, OP_RETURN};

pub struct Chunk {
    pub code: Vec<OpCode>,
    pub constants: ValueArray,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: ValueArray::new(),
        }
    }

    pub fn write(&mut self, byte: OpCode) {
        self.code.push(byte);
    }

    pub fn add_constant(&mut self, value: f64) -> usize {
        self.constants.write(value);
        self.constants.count() - 1
    }
}
