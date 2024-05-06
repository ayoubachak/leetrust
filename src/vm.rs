use std::rc::Rc;

use leetrust::{chunk::Chunk, OpCode};
use crate::compiler::Compiler;

pub struct VM {
    chunk: Option<Chunk>,
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

    pub fn pop(&mut self) -> Option<f64> {
        self.stack.pop()
    }
}


impl VM {
    pub fn interpret(&mut self) -> InterpretResult {
        if self.chunk.is_some() {
            self.ip = 0;
            self.run()  // Use `run` without passing `chunk`
        } else {
            InterpretResult::CompileError
        }
    }

    pub fn compile_and_run(&mut self, source: &str) -> InterpretResult {
        // Reset the VM state
        self.reset_stack();
        self.chunk = None; // Reset the chunk
        let mut new_chunk = Chunk::new();
        let compiler = Compiler::new(source, &mut new_chunk);
        // I can't be bothered to fix this, sorry me from tomorrow
        match compiler.compile() {
            Ok(_) => {
                self.chunk = Some(new_chunk);  // Move the compiled chunk into VM
                self.interpret()
            },
            Err(_) => InterpretResult::CompileError,
        }
    }
    fn run(&mut self) -> InterpretResult {
        loop {
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
                    let value = self.pop().unwrap();
                    self.push(-value);
                },
                Some(OpCode::Add) => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();
                    self.push(a + b);
                },
                Some(OpCode::Subtract) => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();
                    self.push(a - b);
                },
                Some(OpCode::Multiply) => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();
                    self.push(a * b);
                },
                Some(OpCode::Divide) => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();
                    self.push(a / b);
                },
                Some(OpCode::Return) => {
                    let value =  match self.pop() {
                        Some(value) => {
                            println!("{}", value);
                            return InterpretResult::Ok;
                        },
                        None => return InterpretResult::RuntimeError,
                    };
                },
                _ => return InterpretResult::RuntimeError,
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
