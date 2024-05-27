# Code
## chunk.rs
```
use crate::value::Value;
// chunk.rs
use crate::{value::ValueArray, OpCode};
use crate::debug::disassemble_chunk;

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

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.write(value);
        self.constants.count() - 1
    }

    pub fn write_constant(&mut self, value: Value, line: usize) -> usize {
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

    pub fn disassemble(&self, name: &str) {
        disassemble_chunk(self, name)
    }
}

```
## compiler.rs
```
// compiler.rs
use std::collections::HashMap;

use leetrust::value::Value;
use leetrust::{chunk::Chunk, OpCode};

use crate::scanner::{Scanner, TokenType, Token};
use crate::debug::disassemble_chunk;
use crate::vm::InterpretResult;
use std::rc::Rc;

#[derive(Debug)]
pub enum CompileError {
    ScanError { message: String, line: usize },
    UnexpectedCharacter,
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            CompileError::ScanError { ref message, line } => write!(f, "Error at line {}: {}", line, message),
            CompileError::UnexpectedCharacter => write!(f, "Unexpected character encountered."),
        }
    }
}

impl std::error::Error for CompileError {}

#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
enum Precedence {
    None = 0,
    Assignment,  // 1
    Or,          // 2
    And,         // 3
    Equality,    // 4
    Comparison,  // 5
    Term,        // 6
    Factor,      // 7
    Unary,       // 8
    Call,        // 9
    Primary      // 10
}

impl Precedence {
    fn next(self) -> Self {
        use Precedence::*;
        match self {
            None => Assignment,
            Assignment => Or,
            Or => And,
            And => Equality,
            Equality => Comparison,
            Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Call,
            Call => Primary,
            Primary => Primary, // No higher precedence exists, so return self
        }
    }
}
type ParseFn = Rc<dyn Fn(&mut Compiler)>;

#[derive(Clone)]
struct ParseRule {
    prefix: Option<Rc<dyn Fn(&mut Compiler)>>,
    infix: Option<Rc<dyn Fn(&mut Compiler)>>,
    precedence: Precedence,
}
pub struct Compiler<'a> {
    scanner: Scanner<'a>,
    current_token: Token<'a>,
    previous_token: Token<'a>,
    had_error: bool,
    panic_mode: bool,
    chunk: &'a mut Chunk,
    rules: HashMap<TokenType, ParseRule>,  // Add this line
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str, chunk: &'a mut Chunk) -> Self {
        let mut compiler = Compiler {
            scanner: Scanner::new(source),
            current_token: Token::default(),
            previous_token: Token::default(),
            had_error: false,
            panic_mode: false,
            chunk,
            rules: HashMap::new(),
        };
        compiler.setup_rules();
        compiler.advance(); // Load the first token
        compiler
    }

    fn advance(&mut self) {
        std::mem::swap(&mut self.previous_token, &mut self.current_token);
        loop {
            self.current_token = self.scanner.scan_token();
            if self.current_token.type_ != TokenType::Error {
                break;
            }
            self.error_at_current("Error at token.");
        }
    }

    fn consume(&mut self, type_: TokenType, message: &str) {
        if self.current_token.type_ == type_ {
            self.advance();
        } else {
            self.error(message);
        }
    }

    fn emit_byte(&mut self, byte: OpCode) {
        self.chunk.write(byte, self.previous_token.line);
    }

    fn emit_bytes(&mut self, byte1: OpCode, byte2: OpCode) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return);
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        let constant = self.chunk.add_constant(value);
        if constant > u8::MAX as usize {
            self.error("Too many constants in one chunk.");
            0
        } else {
            constant as u8
        }
    }

    fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_bytes(OpCode::Constant, OpCode::from_u8(constant).unwrap());
    }

    fn end_compiler(&mut self) {
        self.emit_return();
        if !self.had_error {
            self.chunk.disassemble("code");
        }
    }

    // Instead of panicking or failing, log an error and continue.
    fn error_at_current(&mut self, message: &str) {
        eprintln!("[line {}] Error at '{}': {}", self.current_token.line, self.current_token.start, message);
        self.had_error = true;
    }


    fn error(&mut self, message: &str) {
        self.error_at(self.previous_token.clone(), message);
    }

    fn error_at(&mut self, token : Token, message: &str) {
        if self.panic_mode { return; }
        self.panic_mode = true;
        eprint!("[line {}] Error", token.line);
        if token.type_ == TokenType::Eof {
            eprint!(" at end");
        } else if token.type_ == TokenType::Error {
            // Nothing
        } else {
            eprint!(" at '{}'", token.start);
        }
        eprintln!(": {}", message);
        self.had_error = true;
    }

    // Placeholder for parser rules (as in C version)
    // Placeholder for expression(), number(), unary(), binary(), grouping(), and parse_precedence()
    pub fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn binary(&mut self) {
        let operator_type = self.previous_token.type_;
        let rule = self.get_rule(operator_type);
        self.parse_precedence(rule.unwrap().precedence.next()); // Use the next precedence level for right-hand side of the operator
    
        match operator_type {
            TokenType::Plus => self.emit_byte(OpCode::Add),
            TokenType::Minus => self.emit_byte(OpCode::Subtract),
            TokenType::Star => self.emit_byte(OpCode::Multiply),
            TokenType::Slash => self.emit_byte(OpCode::Divide),
            TokenType::BangEqual => { 
                self.emit_bytes(OpCode::Equal, OpCode::Not);  // Evaluate equality then negate the result
            }
            TokenType::EqualEqual => self.emit_byte(OpCode::Equal),
            TokenType::Greater => self.emit_byte(OpCode::Greater),
            TokenType::GreaterEqual => {
                self.emit_bytes(OpCode::Less, OpCode::Not);  // Evaluate less-than then negate the result
            }
            TokenType::Less => self.emit_byte(OpCode::Less),
            TokenType::LessEqual => {
                self.emit_bytes(OpCode::Greater, OpCode::Not);  // Evaluate greater-than then negate the result
            }
            _ => {}
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn number(&mut self) {
        let value: Value = Value::from_f64(self.previous_token.start.parse().unwrap_or(0.0));
        self.emit_constant(value);
    }

    fn unary(&mut self) {
        let operator_type = self.previous_token.type_;

        // Compile the operand.
        self.parse_precedence(Precedence::Unary);

        // Emit the operator instruction.
        match operator_type {
            TokenType::Minus => self.emit_byte(OpCode::Negate),
            TokenType::Bang => self.emit_byte(OpCode::Not),
            _ => {}
        }
    }

    fn string(&mut self) {
        let value = self.previous_token.start;
        let constant_index = self.make_constant(Value::from_string(Rc::new(value.to_owned())));
        self.emit_bytes(OpCode::Constant, OpCode::from_u8(constant_index).unwrap());
    }
    
    pub fn compile(mut self) -> Result<&'a mut Chunk, InterpretResult> {
        self.expression();
        self.consume(TokenType::Eof, "Expect end of expression.");
        self.end_compiler();
        if self.had_error {
            Err(InterpretResult::CompileError) // we return the type of the compile error here (there might be many later)
        } else {
            Ok(self.chunk) // if no error was found we return the compiled chunk
        }
    }
}


impl Compiler<'_> {
    fn literal(&mut self) {
        match self.previous_token.type_ {
            TokenType::False => self.emit_byte(OpCode::False),
            TokenType::Nil => self.emit_byte(OpCode::Nil),
            TokenType::True => self.emit_byte(OpCode::True),
            _ => self.error("Unhandled literal"),
        }
    }
    fn setup_rules(&mut self) {
        println!("Setting up rules");
        self.rules.insert(TokenType::Plus, ParseRule {
            prefix: None,
            infix: Some(Rc::new(|comp: &mut Compiler| comp.binary())),
            precedence: Precedence::Term,
        });
        self.rules.insert(TokenType::Minus, ParseRule {
            prefix: Some(Rc::new(|comp: &mut Compiler| comp.unary())),
            infix: Some(Rc::new(|comp: &mut Compiler| comp.binary())),
            precedence: Precedence::Term,
        });
        self.rules.insert(TokenType::Star, ParseRule {
            prefix: None,
            infix: Some(Rc::new(|comp: &mut Compiler| comp.binary())),
            precedence: Precedence::Factor,
        });
        self.rules.insert(TokenType::Slash, ParseRule {
            prefix: None,
            infix: Some(Rc::new(|comp: &mut Compiler| comp.binary())),
            precedence: Precedence::Factor,
        });
        self.rules.insert(TokenType::Number, ParseRule {
            prefix: Some(Rc::new(|comp: &mut Compiler| comp.number())),
            infix: None,
            precedence: Precedence::Primary,
        });
        self.rules.insert(TokenType::LeftParen, ParseRule {
            prefix: Some(Rc::new(|comp: &mut Compiler| comp.grouping())),
            infix: None,
            precedence: Precedence::Primary,
        });
        self.rules.insert(TokenType::False, ParseRule {
            prefix: Some(Rc::new(|comp: &mut Compiler| comp.literal())),
            infix: None,
            precedence: Precedence::None,
        });
        self.rules.insert(TokenType::Nil, ParseRule {
            prefix: Some(Rc::new(|comp: &mut Compiler| comp.literal())),
            infix: None,
            precedence: Precedence::None,
        });
        self.rules.insert(TokenType::True, ParseRule {
            prefix: Some(Rc::new(|comp: &mut Compiler| comp.literal())),
            infix: None,
            precedence: Precedence::None,
        });
        self.rules.insert(TokenType::Bang, ParseRule {
            prefix: Some(Rc::new(|comp: &mut Compiler| comp.unary())),
            infix: None,
            precedence: Precedence::Unary,  // Make sure the precedence is set appropriately
        });
        self.rules.insert(TokenType::EqualEqual, ParseRule {
            prefix: None,
            infix: Some(Rc::new(|comp: &mut Compiler| comp.binary())),
            precedence: Precedence::Equality,
        });
        self.rules.insert(TokenType::BangEqual, ParseRule {
            prefix: None,
            infix: Some(Rc::new(|comp: &mut Compiler| comp.binary())),
            precedence: Precedence::Equality,
        });
        self.rules.insert(TokenType::Greater, ParseRule {
            prefix: None,
            infix: Some(Rc::new(|comp: &mut Compiler| comp.binary())),
            precedence: Precedence::Comparison,
        });
        self.rules.insert(TokenType::GreaterEqual, ParseRule {
            prefix: None,
            infix: Some(Rc::new(|comp: &mut Compiler| comp.binary())),
            precedence: Precedence::Comparison,
        });
        self.rules.insert(TokenType::Less, ParseRule {
            prefix: None,
            infix: Some(Rc::new(|comp: &mut Compiler| comp.binary())),
            precedence: Precedence::Comparison,
        });
        self.rules.insert(TokenType::LessEqual, ParseRule {
            prefix: None,
            infix: Some(Rc::new(|comp: &mut Compiler| comp.binary())),
            precedence: Precedence::Comparison,
        });
        self.rules.insert(TokenType::String, ParseRule {
            prefix: Some(Rc::new(|comp: &mut Compiler| comp.string())),
            infix: None,
            precedence: Precedence::Primary,
        });
        
        self.rules.insert(TokenType::Eof, ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None, // EOF typically doesn't participate in expressions
        });

        
    }

 


    fn get_rule(&self, type_: TokenType) -> Option<ParseRule> {
        self.rules.get(&type_).cloned()  // Clone the ParseRule here
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let current_type = self.previous_token.type_;
        let rule = self.get_rule(current_type);  // Clone the rule to separate its lifecycle from `self`
    
        if let Some(rule) = rule {
            if let Some(prefix) = &rule.prefix {
                prefix(self);
            } else {
                self.error("Error: Expect expression.");
                return;
            }
        } else {
            self.error("Error: Expect expression.");
            return;
        }
    
        while let Some(next_rule) = self.get_rule(self.current_token.type_) {  // Clone each time to avoid borrowing issues
            if precedence <= next_rule.precedence {
                self.advance();
                if let Some(infix) = next_rule.infix {
                    infix(self);
                } else {
                    self.error("Expected infix operation.");
                    break;
                }
            } else {
                break;
            }
        }
    }
    
}

fn print_rules(rules : HashMap<TokenType, ParseRule>) {
    for (token_type, rule) in rules.iter() {
        println!("Inserting rule for {:?}: prefix = {:?}, infix = {:?}, precedence = {:?}",
                 token_type,
                 rule.prefix.is_some(),
                 rule.infix.is_some(),
                 rule.precedence);
    }
}
```
## debug.rs
```
// debug.rs
use crate::chunk::Chunk;
use crate::OpCode;

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    let mut offset = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(chunk, offset);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:04} ", offset);

    let current_line = chunk.lines.iter()
        .scan(0, |state, &(line, count)| {
            *state += count;
            Some((line, *state))
        })
        .find(|&(_, count)| count > offset)
        .map(|(line, _)| line)
        .unwrap_or(0);

    // Retrieve previous line number based on the previous opcode's offset to compare effectively
    let previous_line = if offset > 0 {
        let prev_offset = offset - 1;
        chunk.lines.iter()
            .scan(0, |state, &(line, count)| {
                *state += count;
                Some((line, *state))
            })
            .find(|&(_, count)| count > prev_offset)
            .map(|(line, _)| line)
            .unwrap_or(0)
    } else {
        0 // Default to 0 or a suitable default for the first opcode
    };
    
    if offset > 0 && current_line == previous_line {
        print!("   | ");
    } else {
        print!("{:4} ", current_line);
    }

    match chunk.code[offset] {
        OpCode::Constant => {
            let constant = chunk.code[offset + 1] as usize;
            println!("{:<16} {:4} '{}'", "OP_CONSTANT", constant, chunk.constants.values[constant]);
            offset + 2
        },
        OpCode::ConstantLong => {
            if offset + 3 < chunk.code.len() {
                let constant = ((chunk.code[offset + 1] as usize) << 16)
                             | ((chunk.code[offset + 2] as usize) << 8)
                             | (chunk.code[offset + 3] as usize);
                if constant < chunk.constants.values.len() {
                    println!("{:<16} {:4} '{}'", "OP_CONSTANT_LONG", constant, chunk.constants.values[constant]);
                } else {
                    println!("Constant index {} out of bounds", constant);
                }
                offset + 4
            } else {
                println!("Not enough bytes after OP_CONSTANT_LONG");
                offset + 1
            }
        },
        OpCode::Negate => simple_instruction("OP_NEGATE", offset),
        OpCode::Add => simple_instruction("OP_ADD", offset),
        OpCode::Subtract => simple_instruction("OP_SUBTRACT", offset),
        OpCode::Multiply => simple_instruction("OP_MULTIPLY", offset),
        OpCode::Divide => simple_instruction("OP_DIVIDE", offset),
        OpCode::Return => simple_instruction("OP_RETURN", offset),
        _ => {
            println!("Unknown opcode {:?}", chunk.code[offset]);
            offset + 1
        }
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}



```
## lib.rs
```
// lib.rs
pub mod chunk;
pub mod value;
pub mod memory;
pub mod debug;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpCode {
    Return,
    Constant,
    ConstantLong,
    Nil,
    True,
    False,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    Jump,
    JumpIfFalse,
    Loop,
    Call,
    Closure,
    CloseUpvalue,
    GetUpvalue,
    SetUpvalue,
    Class,
    Method,
    Invoke,
    Inherit,
    GetProperty,
    SetProperty,
    GetSuper,
    SuperInvoke,
    Import,
    EndModule,
    ReturnModule,
    ImportVariable,
    ImportVariableAs,
    ImportModule,
    ImportModuleAs,
    ImportMethod,
    ImportMethodAs,
    ImportClass,
    ImportClassAs,
    ImportSuperclass,
    ImportSuperclassAs,
    ImportProperty,
    ImportPropertyAs,
    ImportMethodProperty,
    ImportMethodPropertyAs,
    ImportClassProperty,
    ImportClassPropertyAs,
    ImportSuperclassProperty,
    ImportSuperclassPropertyAs,
    String,
}

impl OpCode {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0 => Some(OpCode::Return),
            1 => Some(OpCode::Constant),
            2 => Some(OpCode::ConstantLong),
            3 => Some(OpCode::Nil),
            4 => Some(OpCode::True),
            5 => Some(OpCode::False),
            6 => Some(OpCode::Negate),
            7 => Some(OpCode::Add),
            8 => Some(OpCode::Subtract),
            9 => Some(OpCode::Multiply),
            10 => Some(OpCode::Divide),
            11 => Some(OpCode::Not),
            12 => Some(OpCode::Equal),
            13 => Some(OpCode::Greater),
            14 => Some(OpCode::Less),
            15 => Some(OpCode::Print),
            16 => Some(OpCode::Pop),
            17 => Some(OpCode::DefineGlobal),
            18 => Some(OpCode::GetGlobal),
            19 => Some(OpCode::SetGlobal),
            20 => Some(OpCode::GetLocal),
            21 => Some(OpCode::SetLocal),
            22 => Some(OpCode::Jump),
            23 => Some(OpCode::JumpIfFalse),
            24 => Some(OpCode::Loop),
            25 => Some(OpCode::Call),
            26 => Some(OpCode::Closure),
            27 => Some(OpCode::CloseUpvalue),
            28 => Some(OpCode::GetUpvalue),
            29 => Some(OpCode::SetUpvalue),
            30 => Some(OpCode::Class),
            31 => Some(OpCode::Method),
            32 => Some(OpCode::Invoke),
            33 => Some(OpCode::Inherit),
            34 => Some(OpCode::GetProperty),
            35 => Some(OpCode::SetProperty),
            36 => Some(OpCode::GetSuper),
            37 => Some(OpCode::SuperInvoke),
            38 => Some(OpCode::Import),
            39 => Some(OpCode::EndModule),
            40 => Some(OpCode::ReturnModule),
            41 => Some(OpCode::ImportVariable),
            42 => Some(OpCode::ImportVariableAs),
            43 => Some(OpCode::ImportModule),
            44 => Some(OpCode::ImportModuleAs),
            45 => Some(OpCode::ImportMethod),
            46 => Some(OpCode::ImportMethodAs),
            47 => Some(OpCode::ImportClass),
            48 => Some(OpCode::ImportClassAs),
            49 => Some(OpCode::ImportSuperclass),
            50 => Some(OpCode::ImportSuperclassAs),
            51 => Some(OpCode::ImportProperty),
            52 => Some(OpCode::ImportPropertyAs),
            53 => Some(OpCode::ImportMethodProperty),
            54 => Some(OpCode::ImportMethodPropertyAs),
            55 => Some(OpCode::ImportClassProperty),
            56 => Some(OpCode::ImportClassPropertyAs),
            57 => Some(OpCode::ImportSuperclassProperty),
            58 => Some(OpCode::ImportSuperclassPropertyAs),
            59 => Some(OpCode::String),
            _ => None,
        }
    }

    pub fn to_u8(self) -> u8 {
        match self {
            OpCode::Return => 0,
            OpCode::Constant => 1,
            OpCode::ConstantLong => 2,
            OpCode::Nil => 3,
            OpCode::True => 4,
            OpCode::False => 5,
            OpCode::Negate => 6,
            OpCode::Add => 7,
            OpCode::Subtract => 8,
            OpCode::Multiply => 9,
            OpCode::Divide => 10,
            OpCode::Not => 11,
            OpCode::Equal => 12,
            OpCode::Greater => 13,
            OpCode::Less => 14,
            OpCode::Print => 15,
            OpCode::Pop => 16,
            OpCode::DefineGlobal => 17,
            OpCode::GetGlobal => 18,
            OpCode::SetGlobal => 19,
            OpCode::GetLocal => 20,
            OpCode::SetLocal => 21,
            OpCode::Jump => 22,
            OpCode::JumpIfFalse => 23,
            OpCode::Loop => 24,
            OpCode::Call => 25,
            OpCode::Closure => 26,
            OpCode::CloseUpvalue => 27,
            OpCode::GetUpvalue => 28,
            OpCode::SetUpvalue => 29,
            OpCode::Class => 30,
            OpCode::Method => 31,
            OpCode::Invoke => 32,
            OpCode::Inherit => 33,
            OpCode::GetProperty => 34,
            OpCode::SetProperty => 35,
            OpCode::GetSuper => 36,
            OpCode::SuperInvoke => 37,
            OpCode::Import => 38,
            OpCode::EndModule => 39,
            OpCode::ReturnModule => 40,
            OpCode::ImportVariable => 41,
            OpCode::ImportVariableAs => 42,
            OpCode::ImportModule => 43,
            OpCode::ImportModuleAs => 44,
            OpCode::ImportMethod => 45,
            OpCode::ImportMethodAs => 46,
            OpCode::ImportClass => 47,
            OpCode::ImportClassAs => 48,
            OpCode::ImportSuperclass => 49,
            OpCode::ImportSuperclassAs => 50,
            OpCode::ImportProperty => 51,
            OpCode::ImportPropertyAs => 52,
            OpCode::ImportMethodProperty => 53,
            OpCode::ImportMethodPropertyAs => 54,
            OpCode::ImportClassProperty => 55,
            OpCode::ImportClassPropertyAs => 56,
            OpCode::ImportSuperclassProperty => 57,
            OpCode::ImportSuperclassPropertyAs => 58,
            OpCode::String => 59,
        }
    }

    pub fn from_usize(index: usize) -> Option<Self> {
        match index {
            0 => Some(OpCode::Return),
            1 => Some(OpCode::Constant),
            2 => Some(OpCode::ConstantLong),
            3 => Some(OpCode::Nil),
            4 => Some(OpCode::True),
            5 => Some(OpCode::False),
            6 => Some(OpCode::Negate),
            7 => Some(OpCode::Add),
            8 => Some(OpCode::Subtract),
            9 => Some(OpCode::Multiply),
            10 => Some(OpCode::Divide),
            11 => Some(OpCode::Not),
            12 => Some(OpCode::Equal),
            13 => Some(OpCode::Greater),
            14 => Some(OpCode::Less),
            15 => Some(OpCode::Print),
            16 => Some(OpCode::Pop),
            17 => Some(OpCode::DefineGlobal),
            18 => Some(OpCode::GetGlobal),
            19 => Some(OpCode::SetGlobal),
            20 => Some(OpCode::GetLocal),
            21 => Some(OpCode::SetLocal),
            22 => Some(OpCode::Jump),
            23 => Some(OpCode::JumpIfFalse),
            24 => Some(OpCode::Loop),
            25 => Some(OpCode::Call),
            26 => Some(OpCode::Closure),
            27 => Some(OpCode::CloseUpvalue),
            28 => Some(OpCode::GetUpvalue),
            29 => Some(OpCode::SetUpvalue),
            30 => Some(OpCode::Class),
            31 => Some(OpCode::Method),
            32 => Some(OpCode::Invoke),
            33 => Some(OpCode::Inherit),
            34 => Some(OpCode::GetProperty),
            35 => Some(OpCode::SetProperty),
            36 => Some(OpCode::GetSuper),
            37 => Some(OpCode::SuperInvoke),
            38 => Some(OpCode::Import),
            39 => Some(OpCode::EndModule),
            40 => Some(OpCode::ReturnModule),
            41 => Some(OpCode::ImportVariable),
            42 => Some(OpCode::ImportVariableAs),
            43 => Some(OpCode::ImportModule),
            44 => Some(OpCode::ImportModuleAs),
            45 => Some(OpCode::ImportMethod),
            46 => Some(OpCode::ImportMethodAs),
            47 => Some(OpCode::ImportClass),
            48 => Some(OpCode::ImportClassAs),
            49 => Some(OpCode::ImportSuperclass),
            50 => Some(OpCode::ImportSuperclassAs),
            51 => Some(OpCode::ImportProperty),
            52 => Some(OpCode::ImportPropertyAs),
            53 => Some(OpCode::ImportMethodProperty),
            54 => Some(OpCode::ImportMethodPropertyAs),
            55 => Some(OpCode::ImportClassProperty),
            56 => Some(OpCode::ImportClassPropertyAs),
            57 => Some(OpCode::ImportSuperclassProperty),
            58 => Some(OpCode::ImportSuperclassPropertyAs),
            59 => Some(OpCode::String),
            _ => None,
        }
    }
}

impl std::fmt::Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Return => write!(f, "OP_RETURN"),
            OpCode::Constant => write!(f, "OP_CONSTANT"),
            OpCode::ConstantLong => write!(f, "OP_CONSTANT_LONG"),
            OpCode::Nil => write!(f, "OP_NIL"),
            OpCode::True => write!(f, "OP_TRUE"),
            OpCode::False => write!(f, "OP_FALSE"),
            OpCode::Negate => write!(f, "OP_NEGATE"),
            OpCode::Add => write!(f, "OP_ADD"),
            OpCode::Subtract => write!(f, "OP_SUBTRACT"),
            OpCode::Multiply => write!(f, "OP_MULTIPLY"),
            OpCode::Divide => write!(f, "OP_DIVIDE"),
            OpCode::Not => write!(f, "OP_NOT"),
            OpCode::Equal => write!(f, "OP_EQUAL"),
            OpCode::Greater => write!(f, "OP_GREATER"),
            OpCode::Less => write!(f, "OP_LESS"),
            OpCode::Print => write!(f, "OP_PRINT"),
            OpCode::Pop => write!(f, "OP_POP"),
            OpCode::DefineGlobal => write!(f, "OP_DEFINE_GLOBAL"),
            OpCode::GetGlobal => write!(f, "OP_GET_GLOBAL"),
            OpCode::SetGlobal => write!(f, "OP_SET_GLOBAL"),
            OpCode::GetLocal => write!(f, "OP_GET_LOCAL"),
            OpCode::SetLocal => write!(f, "OP_SET_LOCAL"),
            OpCode::Jump => write!(f, "OP_JUMP"),
            OpCode::JumpIfFalse => write!(f, "OP_JUMP_IF_FALSE"),
            OpCode::Loop => write!(f, "OP_LOOP"),
            OpCode::Call => write!(f, "OP_CALL"),
            OpCode::Closure => write!(f, "OP_CLOSURE"),
            OpCode::CloseUpvalue => write!(f, "OP_CLOSE_UPVALUE"),
            OpCode::GetUpvalue => write!(f, "OP_GET_UPVALUE"),
            OpCode::SetUpvalue => write!(f, "OP_SET_UPVALUE"),
            OpCode::Class => write!(f, "OP_CLASS"),
            OpCode::Method => write!(f, "OP_METHOD"),
            OpCode::Invoke => write!(f, "OP_INVOKE"),
            OpCode::Inherit => write!(f, "OP_INHERIT"),
            OpCode::GetProperty => write!(f, "OP_GET_PROPERTY"),
            OpCode::SetProperty => write!(f, "OP_SET_PROPERTY"),
            OpCode::GetSuper => write!(f, "OP_GET_SUPER"),
            OpCode::SuperInvoke => write!(f, "OP_SUPER_INVOKE"),
            OpCode::Import => write!(f, "OP_IMPORT"),
            OpCode::EndModule => write!(f, "OP_END_MODULE"),
            OpCode::ReturnModule => write!(f, "OP_RETURN_MODULE"),
            OpCode::ImportVariable => write!(f, "OP_IMPORT_VARIABLE"),
            OpCode::ImportVariableAs => write!(f, "OP_IMPORT_VARIABLE_AS"),
            OpCode::ImportModule => write!(f, "OP_IMPORT_MODULE"),
            OpCode::ImportModuleAs => write!(f, "OP_IMPORT_MODULE_AS"),
            OpCode::ImportMethod => write!(f, "OP_IMPORT_METHOD"),
            OpCode::ImportMethodAs => write!(f, "OP_IMPORT_METHOD_AS"),
            OpCode::ImportClass => write!(f, "OP_IMPORT_CLASS"),
            OpCode::ImportClassAs => write!(f, "OP_IMPORT_CLASS_AS"),
            OpCode::ImportSuperclass => write!(f, "OP_IMPORT_SUPERCLASS"),
            OpCode::ImportSuperclassAs => write!(f, "OP_IMPORT_SUPERCLASS_AS"),
            OpCode::ImportProperty => write!(f, "OP_IMPORT_PROPERTY"),
            OpCode::ImportPropertyAs => write!(f, "OP_IMPORT_PROPERTY_AS"),
            OpCode::ImportMethodProperty => write!(f, "OP_IMPORT_METHOD_PROPERTY"),
            OpCode::ImportMethodPropertyAs => write!(f, "OP_IMPORT_METHOD_PROPERTY_AS"),
            OpCode::ImportClassProperty => write!(f, "OP_IMPORT_CLASS_PROPERTY"),
            OpCode::ImportClassPropertyAs => write!(f, "OP_IMPORT_CLASS_PROPERTY_AS"),
            OpCode::ImportSuperclassProperty => write!(f, "OP_IMPORT_SUPERCLASS_PROPERTY"),
            OpCode::ImportSuperclassPropertyAs => write!(f, "OP_IMPORT_SUPERCLASS_PROPERTY_AS"),
            OpCode::String => write!(f, "OP_STRING"),
            _ => write!(f, "Unknown opcode"),
        }
    }
}

// OpCode constants
pub const OP_RETURN: OpCode = OpCode::Return;
pub const OP_CONSTANT: OpCode = OpCode::Constant;
pub const OP_CONSTANT_LONG: OpCode = OpCode::ConstantLong;
pub const OP_NIL: OpCode = OpCode::Nil;
pub const OP_TRUE: OpCode = OpCode::True;
pub const OP_FALSE: OpCode = OpCode::False;
pub const OP_NEGATE: OpCode = OpCode::Negate;
pub const OP_ADD: OpCode = OpCode::Add;
pub const OP_SUBTRACT: OpCode = OpCode::Subtract;
pub const OP_MULTIPLY: OpCode = OpCode::Multiply;
pub const OP_DIVIDE: OpCode = OpCode::Divide;
pub const OP_NOT: OpCode = OpCode::Not;
pub const OP_EQUAL: OpCode = OpCode::Equal;
pub const OP_GREATER: OpCode = OpCode::Greater;
pub const OP_LESS: OpCode = OpCode::Less;
pub const OP_PRINT: OpCode = OpCode::Print;
pub const OP_POP: OpCode = OpCode::Pop;
pub const OP_DEFINE_GLOBAL: OpCode = OpCode::DefineGlobal;
pub const OP_GET_GLOBAL: OpCode = OpCode::GetGlobal;
pub const OP_SET_GLOBAL: OpCode = OpCode::SetGlobal;
pub const OP_GET_LOCAL: OpCode = OpCode::GetLocal;
pub const OP_SET_LOCAL: OpCode = OpCode::SetLocal;
pub const OP_JUMP: OpCode = OpCode::Jump;
pub const OP_JUMP_IF_FALSE: OpCode = OpCode::JumpIfFalse;
pub const OP_LOOP: OpCode = OpCode::Loop;
pub const OP_CALL: OpCode = OpCode::Call;
pub const OP_CLOSURE: OpCode = OpCode::Closure;
pub const OP_CLOSE_UPVALUE: OpCode = OpCode::CloseUpvalue;
pub const OP_GET_UPVALUE: OpCode = OpCode::GetUpvalue;
pub const OP_SET_UPVALUE: OpCode = OpCode::SetUpvalue;
pub const OP_CLASS: OpCode = OpCode::Class;
pub const OP_METHOD: OpCode = OpCode::Method;
pub const OP_INVOKE: OpCode = OpCode::Invoke;
pub const OP_INHERIT: OpCode = OpCode::Inherit;
pub const OP_GET_PROPERTY: OpCode = OpCode::GetProperty;
pub const OP_SET_PROPERTY: OpCode = OpCode::SetProperty;
pub const OP_GET_SUPER: OpCode = OpCode::GetSuper;
pub const OP_SUPER_INVOKE: OpCode = OpCode::SuperInvoke;
pub const OP_IMPORT: OpCode = OpCode::Import;
pub const OP_END_MODULE: OpCode = OpCode::EndModule;
pub const OP_RETURN_MODULE: OpCode = OpCode::ReturnModule;
pub const OP_IMPORT_VARIABLE: OpCode = OpCode::ImportVariable;
pub const OP_IMPORT_VARIABLE_AS: OpCode = OpCode::ImportVariableAs;
pub const OP_IMPORT_MODULE: OpCode = OpCode::ImportModule;
pub const OP_IMPORT_MODULE_AS: OpCode = OpCode::ImportModuleAs;
pub const OP_IMPORT_METHOD: OpCode = OpCode::ImportMethod;
pub const OP_IMPORT_METHOD_AS: OpCode = OpCode::ImportMethodAs;
pub const OP_IMPORT_CLASS: OpCode = OpCode::ImportClass;
pub const OP_IMPORT_CLASS_AS: OpCode = OpCode::ImportClassAs;
pub const OP_IMPORT_SUPERCLASS: OpCode = OpCode::ImportSuperclass;
pub const OP_IMPORT_SUPERCLASS_AS: OpCode = OpCode::ImportSuperclassAs;
pub const OP_IMPORT_PROPERTY: OpCode = OpCode::ImportProperty;
pub const OP_IMPORT_PROPERTY_AS: OpCode = OpCode::ImportPropertyAs;
pub const OP_IMPORT_METHOD_PROPERTY: OpCode = OpCode::ImportMethodProperty;
pub const OP_IMPORT_METHOD_PROPERTY_AS: OpCode = OpCode::ImportMethodPropertyAs;
pub const OP_IMPORT_CLASS_PROPERTY: OpCode = OpCode::ImportClassProperty;
pub const OP_IMPORT_CLASS_PROPERTY_AS: OpCode = OpCode::ImportClassPropertyAs;
pub const OP_IMPORT_SUPERCLASS_PROPERTY: OpCode = OpCode::ImportSuperclassProperty;
pub const OP_IMPORT_SUPERCLASS_PROPERTY_AS: OpCode = OpCode::ImportSuperclassPropertyAs;
pub const OP_STRING: OpCode = OpCode::String;

```

## main.rs
```
// main.rs
mod vm;
mod compiler;
mod scanner;
mod object;
use leetrust::*;
use vm::{InterpretResult, VM};
use std::rc::Rc;
use std::io::{self, Write, BufRead};
use std::fs;
use std::path::Path;
use std::env;

fn repl(vm: &mut VM) {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();

    loop {
        print!("> ");
        io::stdout().flush().unwrap(); // Ensure the prompt is printed immediately

        if let Some(Ok(line)) = lines.next() {
            // Here you would need to compile and interpret the line
            // Assuming you have a function compile_and_run in VM that handles source strings
            vm.compile_and_run(&line);
        } else {
            break;
        }
    }
}

fn run_file(vm: &mut VM, path: &Path) {
    let source = fs::read_to_string(path)
        .expect("Could not read file");

    // Assuming compile_and_run interprets the entire source code from a string
    let result = vm.compile_and_run(&source);
    match result {
        InterpretResult::CompileError => std::process::exit(65),
        InterpretResult::RuntimeError => std::process::exit(70),
        _ => {}
    }
}

fn main() {
    let mut vm = VM::new();
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => repl(&mut vm),
        2 => run_file(&mut vm, Path::new(&args[1])),
        _ => {
            eprintln!("Usage: clox [path]");
            std::process::exit(64);
        }
    }
}
```
## memory.rs
```

```
## object.rs
```
// object.rs

```
## scanner.rs
```
// scanner.rs
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
    // Single-character tokens
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
    // One or two character tokens
    Bang, BangEqual, Equal, EqualEqual,
    Greater, GreaterEqual, Less, LessEqual,
    // Literals
    Identifier, String, Number,
    // String Interpolation
    InterpolationStart,
    InterpolationEnd,
    // Keywords
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,
    // Error and EOF
    Error, Eof
}

#[derive(Clone, Copy, Debug)]
pub struct Token<'a> {
    pub type_: TokenType,
    pub start: &'a str,
    pub length: usize,
    pub line: usize,
}

impl<'a> Default for Token<'a> {
    fn default() -> Self {
        Token {
            type_: TokenType::Eof,  // Or any other default type
            start: "",
            length: 0,
            line: 0,
        }
    }
}

pub struct Scanner<'a> {
    source: &'a str,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner { source, start: 0, current: 0, line: 1 }
    }

    fn advance(&mut self) -> char {
        let c = self.source.as_bytes()[self.current] as char;
        self.current += 1;
        c
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source.as_bytes()[self.current] as char
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source.as_bytes()[self.current + 1] as char
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn is_alpha(c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    fn is_digit(c: char) -> bool {
        c.is_ascii_digit()
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.source.as_bytes()[self.current] as char != expected {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();
            match c {
                ' ' | '\r' | '\t' => { self.advance(); },
                '\n' => { self.line += 1; self.advance(); },
                '/' => {
                    if self.peek_next() == '/' {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        break;
                    }
                },
                _ => break,
            }
        }
    }

    fn check_keyword(&self, start: usize, length: usize, rest: &str, type_: TokenType) -> TokenType {
        if self.current - self.start == start + length && &self.source[self.start + start..self.start + start + length] == rest {
            type_
        } else {
            TokenType::Identifier
        }
    }

    fn identifier_type(&self) -> TokenType {
        match self.source.as_bytes()[self.start] as char {
            'a' => self.check_keyword(1, 2, "nd", TokenType::And),
            'c' => self.check_keyword(1, 4, "lass", TokenType::Class),
            'e' => self.check_keyword(1, 3, "lse", TokenType::Else),
            'f' => if self.current - self.start > 1 {
                match self.source.as_bytes()[self.start + 1] as char {
                    'a' => self.check_keyword(2, 3, "lse", TokenType::False),
                    'o' => self.check_keyword(2, 1, "r", TokenType::For),
                    'u' => self.check_keyword(2, 1, "n", TokenType::Fun),
                    _ => TokenType::Identifier,
                }
            } else {
                TokenType::Identifier
            },
            'i' => self.check_keyword(1, 1, "f", TokenType::If),
            'n' => self.check_keyword(1, 2, "il", TokenType::Nil),
            'o' => self.check_keyword(1, 1, "r", TokenType::Or),
            'p' => self.check_keyword(1, 4, "rint", TokenType::Print),
            'r' => self.check_keyword(1, 5, "eturn", TokenType::Return),
            's' => self.check_keyword(1, 4, "uper", TokenType::Super),
            't' => if self.current - self.start > 1 {
                match self.source.as_bytes()[self.start + 1] as char {
                    'h' => self.check_keyword(2, 2, "is", TokenType::This),
                    'r' => self.check_keyword(2, 2, "ue", TokenType::True),
                    _ => TokenType::Identifier,
                }
            } else {
                TokenType::Identifier
            },
            'v' => self.check_keyword(1, 2, "ar", TokenType::Var),
            'w' => self.check_keyword(1, 4, "hile", TokenType::While),
            _ => TokenType::Identifier,
        }
    }

    fn identifier(&mut self) -> Token<'a> {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        // Here we would use identifierType() to determine if it is a keyword or just an identifier
        self.make_token(self.identifier_type())
    }

    fn number(&mut self) -> Token<'a> {
        while self.peek().is_digit(10) {
            self.advance();
        }

        // Handle decimals
        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();  // consume the '.'
            while self.peek().is_digit(10) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn string(&mut self) -> Token<'a> {
        let start = self.current;
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
    
        // Ensure string is terminated correctly
        if self.is_at_end() {
            return self.error_token("Unterminated string.");
        }
    
        // Create a string slice from the start to the current position minus the quote
        let token = Token {
            type_: TokenType::String,
            start: &self.source[start..self.current],
            length: self.current - start,
            line: self.line,
        };
    
        self.advance();  // Skip the closing quote.
        token
    }

    fn make_token(&self, type_: TokenType) -> Token<'a> {
        Token {
            type_,
            start: &self.source[self.start..self.current],
            length: self.current - self.start,
            line: self.line,
        }
    }

    fn error_token(&self, message: &'static str) -> Token<'a> {
        Token {
            type_: TokenType::Error,
            start: message,
            length: message.len(),
            line: self.line,
        }
    }

    pub fn scan_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        self.start = self.current;
    
        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }
    
        let c = self.advance();
        match c {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            ';' => self.make_token(TokenType::Semicolon),
            '*' => self.make_token(TokenType::Star),
            '!' => if self.match_char('=') { self.make_token(TokenType::BangEqual) } else { self.make_token(TokenType::Bang) },
            '=' => if self.match_char('=') { self.make_token(TokenType::EqualEqual) } else { self.make_token(TokenType::Equal) },
            '<' => if self.match_char('=') { self.make_token(TokenType::LessEqual) } else { self.make_token(TokenType::Less) },
            '>' => if self.match_char('=') { self.make_token(TokenType::GreaterEqual) } else { self.make_token(TokenType::Greater) },
            '/' => {
                if self.match_char('/') {
                    // A comment goes until the end of the line.
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    self.scan_token() // Ignore the comment and scan the next token
                } else {
                    self.make_token(TokenType::Slash)
                }
            },
            '"' => self.string(),
            _ => if c.is_digit(10) {
                self.number()
            } else if c.is_alphabetic() || c == '_' {
                self.identifier()
            } else {
                self.error_token("Unexpected character.")
            },
        }
    }

    

}

```
## value.rs
```
// value.rs

use std::cmp::Ordering;
use std::ops::{Add, Sub, Mul, Div, Neg, Not};
use std::rc::Rc;

pub struct ValueArray {
    pub values: Vec<Value>,
}

impl ValueArray {
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
        }
    }

    pub fn write(&mut self, value: Value) {
        self.values.push(value);
    }

    pub fn count(&self) -> usize {
        self.values.len()
    }
}


#[derive(Clone)]
pub enum Value {
    Number(f64),
    Bool(bool),
    String(Rc<String>),
    None,
    Error(String),
}

impl Value {
    pub fn from_f64(n: f64) -> Self {
        Value::Number(n)
    }

    pub fn from_bool(b: bool) -> Self {
        Value::Bool(b)
    }

    pub fn from_string(s: Rc<String>) -> Self {
        Value::String(s)
    }

    pub fn as_number(&self) -> f64 {
        if let Value::Number(n) = self {
            *n
        } else {
            panic!("Value is not a number");
        }
    }

    pub fn as_bool(&self) -> bool {
        if let Value::Bool(b) = self {
            *b
        } else {
            panic!("Value is not a bool");
        }
    }

    pub fn as_string(&self) -> Rc<String> {
        if let Value::String(s) = self {
            Rc::clone(s)
        } else {
            panic!("Value is not a string");
        }
    }
}

impl Add for Value {
    type Output = Result<Self, String>;

    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (Value::String(a), Value::String(b)) => Ok(Value::from_string(Rc::new((*a).clone() + &(*b)))),
            _ => Err("Add operation type mismatch".to_string()),
        }
    }
}

impl Sub for Value {
    type Output = Result<Self, String>;


    fn sub(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            _ => Err("Subtract operation type mismatch or unsupported".to_string()),
        }
    }
}

impl Neg for Value {
    type Output = Result<Self, String>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(a) => Ok(Value::Number(-a)),
            _ => Err("Negate operation type mismatch or unsupported".to_string()),
        }
    }
}

impl Mul for Value {
    type Output = Result<Self, String>;

    fn mul(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
            _ => Err("Multiply operation type mismatch or unsupported".to_string()),
        }
    }
}

impl Div for Value {
    type Output = Result<Self, String>;

    fn div(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => {
                if b == 0.0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Number(a / b))
                }
            },
            _ => Err("Divide operation type mismatch or unsupported".to_string()),
        }
    }
}

impl Not for Value {
    type Output = Result<Self, String>;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(b) => Ok(Value::Bool(!b)),
            Value::Number(n) => Ok(Value::Bool(n == 0.0)),
            _ => Err("Not operation type mismatch or unsupported".to_string()),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            _ => false,
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::None => write!(f, "None"),
            Value::Error(e) => write!(f, "Error: {}", e),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a.partial_cmp(b),
            (Value::String(a), Value::String(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}


impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::None => write!(f, "None"),
            Value::Error(e) => write!(f, "Error: {}", e),
        }
    }
}
```
## vm.rs
```
// vm.rs
use std::rc::Rc;

use leetrust::{chunk::Chunk, value::Value, OpCode};
use crate::compiler::Compiler;

pub struct VM {
    chunk: Option<Chunk>,
    ip: usize,  // Instruction pointer
    stack: Vec<Value>,  // Stack to hold values
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

```
