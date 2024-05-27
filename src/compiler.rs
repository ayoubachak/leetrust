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

    fn define_global(&mut self, name: Rc<String>) {
        let name_constant = self.make_constant(Value::from_string(name.clone()));
        self.emit_bytes(OpCode::DefineGlobal, OpCode::from_u8(name_constant).unwrap());
    }

    fn get_global(&mut self, name: Rc<String>) {
        let name_constant = self.make_constant(Value::from_string(name.clone()));
        self.emit_bytes(OpCode::GetGlobal, OpCode::from_u8(name_constant).unwrap());
    }

    fn set_global(&mut self, name: Rc<String>) {
        let name_constant = self.make_constant(Value::from_string(name.clone()));
        self.emit_bytes(OpCode::SetGlobal, OpCode::from_u8(name_constant).unwrap());
    }

    // Placeholder for parsing variable usage
    fn variable(&mut self, can_assign: bool) {
        let name = Rc::new(self.previous_token.start.to_string());

        if can_assign && self.current_token.type_ == TokenType::Equal {
            self.advance(); // Consume the '='
            self.expression(); // Compile the right-hand side.
            self.set_global(name); // Emit code to set the variable.
        } else {
            self.get_global(name); // Emit code to get the variable.
        }
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

    // Add the print statement handling method
    fn print_statement(&mut self) {
        self.advance();  // Move past the 'print' token
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_byte(OpCode::Print);
    }

    // Add the expression statement handling method
    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_byte(OpCode::Pop);  // Discard the result of the expression
    }

    // Implement the statement method to handle different kinds of statements
    fn statement(&mut self) {
        match self.current_token.type_ {
            TokenType::Print => self.print_statement(),
            _ => self.expression_statement(),
        }
    }

    // Add the var declaration handling method
    fn var_declaration(&mut self) {
        let global_name = Rc::new(self.previous_token.start.to_string());
        self.consume(TokenType::Equal, "Expect '=' after variable name.");
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after variable declaration.");
        self.define_global(global_name);
    }

    // Implement the declaration method to handle different kinds of declarations
    fn declaration(&mut self) {
        println!("Declaration type: {:?}", self.current_token.type_);
        match self.current_token.type_ {
            TokenType::Var => {
                self.advance();  // Move past the 'var' token
                self.var_declaration();
            },
            _ => self.statement(),
        }
    }
    
    // Update the compile method to handle variable declarations
    pub fn compile(mut self) -> Result<&'a mut Chunk, InterpretResult> {
        self.advance();  // Move past the initial token

        while self.current_token.type_ != TokenType::Eof {
            self.declaration();
            self.advance();
        }

        self.end_compiler();
        if self.had_error {
            Err(InterpretResult::CompileError)
        } else {
            Ok(self.chunk)
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
        self.rules.insert(TokenType::Identifier, ParseRule {
            prefix: Some(Rc::new(|comp: &mut Compiler| comp.variable(true))),
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
            precedence: Precedence::Unary,
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
            precedence: Precedence::None,
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