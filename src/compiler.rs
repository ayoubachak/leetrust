use leetrust::{chunk::Chunk, OpCode};

use crate::scanner::{Scanner, TokenType};


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


pub fn compile(source: &str) {
    let mut scanner = Scanner::new(source);
    let mut line = -1isize;

    loop {
        let token = scanner.scan_token();
        if token.line != line as usize {
            print!("{:4} ", token.line);
            line = token.line as isize;
        } else {
            print!(" | ");
        }
        println!("{:2} '{}'", token.type_ as usize, token.start);
        if token.type_ == TokenType::Eof {
            break;
        }
    }
}