use leetrust::chunk::Chunk;
use leetrust::debug::disassemble_chunk;
use leetrust::OP_RETURN;

fn main() {
    let mut chunk = Chunk::new();
    chunk.write(OP_RETURN);

    disassemble_chunk(&chunk, "Test Chunk");
}
