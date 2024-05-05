pub mod chunk;
pub mod value;
pub mod memory;
pub mod debug;

pub type OpCode = u8;

// OpCode constants
pub const OP_RETURN: OpCode = 0;
