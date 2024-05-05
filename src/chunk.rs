use crate::{value::ValueArray, OpCode};

pub struct Chunk {
    pub code: Vec<OpCode>,
    pub lines: Vec<(usize, usize)>,  // Changed to store (line number, count)
    pub constants: ValueArray,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            lines: Vec::new(),
            constants: ValueArray::new(),
        }
    }

    pub fn write(&mut self, byte: OpCode, line: usize) {
        if let Some(last) = self.lines.last_mut() {
            if last.0 == line {
                last.1 += 1;
            } else {
                self.lines.push((line, 1));
            }
        } else {
            self.lines.push((line, 1));
        }
        self.code.push(byte);
    }

    pub fn add_constant(&mut self, value: f64) -> usize {
        self.constants.write(value);
        self.constants.count() - 1
    }

    pub fn write_constant(&mut self, value: f64, line: usize) -> usize {
        let index = self.add_constant(value);
        if index < 256 {
            self.write(OpCode::Constant, line);
            self.code.push(OpCode::from_usize(index).unwrap());
        } else {
            self.write(OpCode::ConstantLong, line);
            self.code.push(OpCode::from_usize((index >> 16) & 0xFF).unwrap());
            self.code.push(OpCode::from_usize((index >> 8) & 0xFF).unwrap());
            self.code.push(OpCode::from_usize(index & 0xFF).unwrap());
        }
        index
    }
}
