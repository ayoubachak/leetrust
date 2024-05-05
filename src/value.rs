pub struct ValueArray {
    values: Vec<f64>,
}

impl ValueArray {
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
        }
    }

    pub fn write(&mut self, value: f64) {
        self.values.push(value);
    }

    pub fn count(&self) -> usize {
        self.values.len()
    }
}
