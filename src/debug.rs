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


