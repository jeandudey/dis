use falcon::error::Result;
use falcon::translator::{BlockTranslationResult, Translator};
use falcon::il::ControlFlowGraph;

use dis_xtensa_lx6::{self, Opcode, Instruction};

mod semantics;

#[derive(Debug)]
pub struct XtensaLx6Le;

impl Translator for XtensaLx6Le {
    fn translate_block(&self, bytes: &[u8], address: u64) -> Result<BlockTranslationResult> {
        translate_block(bytes, address)
    }
}

fn translate_block(bytes: &[u8], address: u64) -> Result<BlockTranslationResult> {
    // A vec which holds each lifted instruction in this block.
    let mut block_graphs: Vec<(u64, ControlFlowGraph)> = Vec::new();

    // the length of this block in bytes.
    let mut length: usize = 0;

    // The successors which exit this block.
    //let mut successors = Vec::new();

    // Offset in bytes to the next instruction from the address given at entry.
    let mut offset: usize = 0;

    loop {
        let disassemble_range = (offset)..bytes.len();
        let disassemble_bytes = bytes.get(disassemble_range).unwrap();

        let lookahead = disassemble_bytes.len() >= 3;
        let op: Opcode;
        if lookahead {
            op = Opcode([disassemble_bytes[0], disassemble_bytes[1], disassemble_bytes[3]]);
        } else {
            op = Opcode([disassemble_bytes[0], disassemble_bytes[1], 0]);
        }

        let ins = match dis_xtensa_lx6::match_opcode(op) {
            Some(ins) => ins,
            None => {
                offset += 3;
                continue;
            }
        };

        let mut instruction_graph = ControlFlowGraph::new();

        match ins {
            Instruction::ILL => semantics::ill(&mut instruction_graph)?,
            _ => {},
        }

        if ins.is_narrow() {
            offset += 2;
        } else {
            offset += 3;
        }
        unimplemented!();
    }
}
