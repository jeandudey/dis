use falcon::error::Result;
use falcon::il::{ControlFlowGraph, Expression};
use falcon::translator::{BlockTranslationResult, Translator};

use dis_xtensa_lx6::{self, Id};

mod semantics;

#[derive(Debug)]
pub struct Mcu {
    pub nareg: u8,
}

impl Mcu {
    pub fn esp32() -> Mcu {
        Mcu {
            nareg: 64,
        }
    }

    pub fn window_base_size(&self) -> usize {
        // log2(NAREG / 4)
        match self.nareg {
            32 => 3,
            64 => 4,
            _ => f64::from(self.nareg / 4).log2() as usize,
        }
    }
}

#[derive(Debug)]
pub struct XtensaLx6 {
    pub mcu: Mcu,
}

impl Translator for XtensaLx6 {
    fn translate_block(&self, bytes: &[u8], address: u64) -> Result<BlockTranslationResult> {
        translate_block(&self.mcu, bytes, address)
    }
}

fn translate_block(mcu: &Mcu, bytes: &[u8], address: u64) -> Result<BlockTranslationResult> {
    // A vec which holds each lifted instruction in this block.
    let mut block_graphs: Vec<(u64, ControlFlowGraph)> = Vec::new();

    // the length of this block in bytes.
    let mut length: usize = 0;

    // The successors which exit this block.
    let successors: Vec<(u64, Option<Expression>)> = Vec::new();

    // Offset in bytes to the next instruction from the address given at entry.
    let mut offset: usize = 0;

    loop {
        let disassemble_range = (offset)..bytes.len();
        let disassemble_bytes = bytes.get(disassemble_range).unwrap();

        let lookahead = disassemble_bytes.len() >= 3;
        let op;
        if lookahead {
            op = (
                disassemble_bytes[0],
                disassemble_bytes[1],
                disassemble_bytes[2],
            );
        } else {
            op = (disassemble_bytes[0], disassemble_bytes[1], 0);
        }

        let instruction = dis_xtensa_lx6::match_opcode(op).unwrap();

        let mut instruction_graph = ControlFlowGraph::new();

        match instruction.id {
            Id::ILL => semantics::ill(&mut instruction_graph)?,
            Id::ENTRY => semantics::entry(&mut instruction_graph, &instruction, mcu)?,
            Id::MOV_N => semantics::mov_n(&mut instruction_graph, &instruction)?,
            _ => {}
        }

        instruction_graph.set_address(Some(address + offset as u64));
        block_graphs.push((address + offset as u64, instruction_graph));

        offset += instruction.size();
        length += instruction.size();

        break;
    }

    Ok(BlockTranslationResult::new(
        block_graphs,
        address,
        length,
        successors,
    ))
}
