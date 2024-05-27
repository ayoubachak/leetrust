// vm.rs
use std::{collections::HashMap, rc::Rc};

use leetrust::{chunk::Chunk, value::Value, OpCode};
use crate::compiler::Compiler;

pub struct VM {
    chunk: Option<Chunk>,
    ip: usize,  // Instruction pointer
    stack: Vec<Value>,  // Stack to hold values
    globals: HashMap<Rc<String>, Value>, 
}

impl VM {
    pub fn new() -> Self {
        VM {
            chunk: None,
            ip: 0,
            stack: Vec::new(),
            globals: HashMap::new(),
        }
    }

    fn define_global(&mut self, name: Rc<String>, value: Value) {
        self.globals.insert(name, value);
    }

    fn get_global(&self, name: &Rc<String>) -> Option<Value> {
        self.globals.get(name).cloned()
    }

    fn set_global(&mut self, name: Rc<String>, value: Value) -> bool {
        if self.globals.contains_key(&name) {
            self.globals.insert(name, value);
            true
        } else {
            false
        }
    }

    fn reset_stack(&mut self) {
        self.stack.clear();
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> Option<Value> {
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
                // Globals 
                Some(OpCode::DefineGlobal) => {
                    let name_idx = self.read_byte() as usize;
                    let name = self.read_constant(name_idx).as_string();
                    let value = self.pop().unwrap();
                    self.define_global(name, value);
                }
                Some(OpCode::GetGlobal) => {
                    let name_idx = self.read_byte() as usize;
                    let name = self.read_constant(name_idx).as_string();
                    match self.get_global(&name) {
                        Some(value) => self.push(value),
                        None => return InterpretResult::RuntimeError,
                    }
                }
                Some(OpCode::SetGlobal) => {
                    let name_idx = self.read_byte() as usize;
                    let name = self.read_constant(name_idx).as_string();
                    let value = self.pop().unwrap();
                    if !self.set_global(name.clone(), value.clone()) {
                        self.define_global(name, value);
                    }
                }
                Some(OpCode::Print) => {
                    let value = self.pop().unwrap();
                    println!("{}", value);
                },
                // End Globals
                Some(OpCode::Pop) => {
                    self.pop();
                }
                Some(OpCode::Nil) => self.push(Value::None),  // Using 0.0 to represent nil
                Some(OpCode::True) => self.push(Value::Bool(true)), // Using 1.0 to represent true
                Some(OpCode::False) => self.push(Value::Bool(false)),// Using 0.0 to represent false
                
                Some(OpCode::Negate) => {
                    let value = self.pop().unwrap();
                    match -value {
                        result => self.push(result.unwrap()),
                        _ => return InterpretResult::RuntimeError,
                    }
                },
                Some(OpCode::Not) => {
                    let value = self.pop().unwrap_or(Value::Number(0.0));
                    match !value {
                        result => self.push(result.unwrap()),
                        _ => return InterpretResult::RuntimeError,
                    }
                },
                Some(OpCode::Add) => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();
                    match a + b {
                        result => self.push(result.unwrap()),
                        _ => return InterpretResult::RuntimeError,
                    }
                },
                Some(OpCode::Subtract) => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();
                    match a - b {
                        result => self.push(result.unwrap()),
                        _ => return InterpretResult::RuntimeError,
                    }
                },
                Some(OpCode::Multiply) => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();
                    match a * b {
                        result => self.push(result.unwrap()),
                        _ => return InterpretResult::RuntimeError,
                    }
                },
                Some(OpCode::Divide) => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();
                    match a / b {
                        result => self.push(result.unwrap()),
                        _ => return InterpretResult::RuntimeError,
                    }
                },
                Some(OpCode::Equal) => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();
                    match a == b {
                        true => self.push(Value::Bool(true)),
                        false => self.push(Value::Bool(false)),
                    }
                },
                Some(OpCode::Greater) => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();
                    match a > b {
                        true => self.push(Value::Bool(true)),
                        false => self.push(Value::Bool(false)),
                    }
                },
                Some(OpCode::Less) => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();
                    match a < b {
                        true => self.push(Value::Bool(true)),
                        false => self.push(Value::Bool(false)),
                    }
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

    fn read_constant(&self, index: usize) -> Value {
        self.chunk.as_ref().unwrap().constants.values[index].clone()
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

impl VM {
    fn concatenate(&mut self) {
        let b = self.pop().unwrap().as_string();
        let a = self.pop().unwrap().as_string();
        let result = Rc::new((*a).clone() + &(*b));
        self.push(Value::String(result));
    }
}

#[derive(Debug, PartialEq)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}
