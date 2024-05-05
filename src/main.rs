use leetrust::chunk::Chunk;
use leetrust::debug::disassemble_chunk;
use leetrust::*;
mod vm;
use vm::VM;
use std::rc::Rc;


fn main() {
    let mut vm = VM::new();

    let mut chunk = Chunk::new();

    // Add a constant and its usage to the chunk
    let mut constant_index = chunk.add_constant(1.2);
    chunk.write(OpCode::Constant, 123);  // Opcode to push constant onto the stack
    chunk.write(OpCode::from_usize(constant_index).unwrap(), 123);  // The index of the constant in the array

    constant_index = chunk.add_constant(3.4);
    chunk.write(OpCode::Constant, 123);  // Opcode to push constant onto the stack
    chunk.write(OpCode::from_usize(constant_index).unwrap(), 123);  // The index of the constant in the array
    chunk.write(OpCode::Add, 123);  // Opcode to push constant onto the stack

    constant_index = chunk.add_constant(5.6);
    chunk.write(OpCode::Constant, 123);  // Opcode to push constant onto the stack
    chunk.write(OpCode::from_usize(constant_index).unwrap(), 123);  // The index of the constant in the array
    chunk.write(OpCode::Divide, 123);  // Opcode to push constant onto the stack

    chunk.write(OpCode::Negate, 123);  // The index of the constant in the array
    // Add return operation
    chunk.write(OpCode::Return, 123);

    // Disassemble the chunk for debug purposes
    disassemble_chunk(&chunk, "Test Chunk");
    print!("Debugging...\n");
    // Wrap the chunk in a Rc<Chunk> to manage references and ownership
    let rc_chunk = Rc::new(chunk);

    // Interpret the chunk using the VM
    vm.interpret(rc_chunk.clone());  // Clone Rc for passing without giving ownership
}