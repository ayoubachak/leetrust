use std::rc::Rc;

use leetrust::{chunk::Chunk, OpCode};
use crate::compiler::compile;

pub struct VM {
    chunk: Option<Rc<Chunk>>,
    ip: usize,  // Instruction pointer
    stack: Vec<f64>,  // Stack to hold values
}

impl VM {
    pub fn new() -> Self {
        VM {
            chunk: None,
            ip: 0,
            stack: Vec::new(),
        }
    }

    fn reset_stack(&mut self) {
        self.stack.clear();
    }

    pub fn push(&mut self, value: f64) {
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> f64 {
        self.stack.pop().expect("Stack underflow!")
    }
}


impl VM {
    pub fn interpret(&mut self, chunk: Rc<Chunk>) -> InterpretResult {
        self.chunk = Some(chunk);
        self.ip = 0;
        self.run()
    }

    pub fn compile_and_run(&mut self, source: &str) -> InterpretResult {
        // Reset the VM state
        self.reset_stack();
        self.chunk = None; // Reset the chunk
        
        // Assume compile function returns a Result<Option<Chunk>, CompileError>
        compile(source);
        return InterpretResult::Ok;
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            // Debugging: Output the current state of the stack
            if cfg!(feature = "debug_trace_execution") {
                self.debug_instruction(); // Debug the next instruction to execute
            }

            if self.ip >= self.chunk.as_ref().unwrap().code.len() {
                return InterpretResult::CompileError;
            }

            let instruction = self.read_byte();

            match OpCode::from_u8(instruction) {
                Some(OpCode::Constant) => {
                    let constant_idx = self.read_byte() as usize;
                    let value = self.read_constant(constant_idx);
                    self.push(value);
                },
                Some(OpCode::ConstantLong) => {
                    let constant_idx = self.read_long();
                    let value = self.read_constant(constant_idx);
                    self.push(value);
                },
                Some(OpCode::Negate) => {
                    let value = self.pop();
                    self.push(-value);
                },
                Some(OpCode::Add) => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a + b);
                },
                Some(OpCode::Subtract) => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a - b);
                },
                Some(OpCode::Multiply) => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a * b);
                },
                Some(OpCode::Divide) => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a / b);
                },
                Some(OpCode::Return) => {
                    let value = self.pop();
                    println!("{}", value); // Assuming we have Display for f64
                    if cfg!(feature = "debug_trace_execution") {
                        self.debug_stack(); // Output the stack's state after popping
                    }
                    return InterpretResult::Ok;
                },
                _ => return InterpretResult::RuntimeError,
            }

            // Debugging: Output the current state of the stack after the instruction
            if cfg!(feature = "debug_trace_execution") {
                self.debug_stack();
            }
        }
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.chunk.as_ref().unwrap().code[self.ip];
        self.ip += 1;
        byte.to_u8()
    }

    fn read_long(&mut self) -> usize {
        let b1 = self.read_byte() as usize;
        let b2 = self.read_byte() as usize;
        let b3 = self.read_byte() as usize;
        (b1 << 16) | (b2 << 8) | b3
    }

    fn read_constant(&self, index: usize) -> f64 {
        self.chunk.as_ref().unwrap().constants.values[index]
    }
}

impl VM {
    fn debug_stack(&self) {
        print!(" ");
        for value in &self.stack {
            print!("[ {} ]", value);
        }
        println!();
    }

    fn debug_instruction(&self) {
        // print!("{:04} ", self.ip); // Print the instruction pointer before disassembling
        crate::debug::disassemble_instruction(self.chunk.as_ref().unwrap(), self.ip);
    }
}
#[derive(Debug, PartialEq)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}
