use falcon::error::Result;
use falcon::il::{ControlFlowGraph, Expression};
use falcon::translator::{BlockTranslationResult, Translator};

use crate::architecture::Mcu;

mod semantics;
mod syntax;

#[derive(Debug)]
pub struct XtensaLx6 {
    pub mcu: Mcu,
}

impl XtensaLx6 {
    pub fn new(mcu: Mcu) -> XtensaLx6 {
        XtensaLx6 { mcu }
    }
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

        let instruction = syntax::match_opcode(op).unwrap();

        let mut instruction_graph = ControlFlowGraph::new();

        match instruction.id {
            syntax::Id::ILL => semantics::ill(&mut instruction_graph)?,
            syntax::Id::ENTRY => semantics::entry(&mut instruction_graph, &instruction, mcu)?,
            syntax::Id::L32R => semantics::l32r(&mut instruction_graph, &instruction, &mcu)?,
            syntax::Id::MOV_N => semantics::mov_n(&mut instruction_graph, &instruction)?,
            _ => {}
        }

        instruction_graph.set_address(Some(address + offset as u64));
        block_graphs.push((address + offset as u64, instruction_graph));

        offset += instruction.size();
        length += instruction.size();

        // XXX: for testing.
        if offset >= 8 {
            break;
        }
    }

    Ok(BlockTranslationResult::new(
        block_graphs,
        address,
        length,
        successors,
    ))
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use falcon::translator::Translator;

    #[test]
    fn transalte_main() {
        const BIN: &'static [u8] = include_bytes!("../../examples/data/main.bin");
        let translator = XtensaLx6 {
            mcu: Mcu::esp32(),
        };
        let result = translator.translate_block(BIN, 0).unwrap();
        for instruction in result.instructions() {
            println!("{}", instruction.1.graph().dot_graph());
        }

        panic!();
    }
}
