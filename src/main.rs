mod vm;
mod compiler;
mod scanner;
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