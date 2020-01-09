// p8n-xtensa-lx6 - A libre Xtensa LX6 disassmbler
// Copyright (C) 2020  Jean Pierre Dudey
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

use smallvec::SmallVec;
use p8n_types::{Endianess, Match, Region, Result, Value, Guard, Atom, Variable, Constant, Statement};

const PC_BITS: u16 = 32;

use crate::disassembler::*;
use crate::semantic;

fn decode_register(s: Constant) -> Result<Variable> {
    Variable::new2(format!("a{}", s.value), None, 32)
}

fn decode_rrrn(opcode: u16) -> Result<(Variable, Variable)> {
    let sbits = (opcode & 0b0000_1111_0000_0000) >> 8;
    let tbits = (opcode & 0b0000_0000_1111_0000) >> 4;
    let s = Variable::new2(format!("a{}", sbits), None, 32)?;
    let t = Variable::new2(format!("a{}", tbits), None, 32)?;
    Ok((s, t))
}

fn decode_bri12(opcode: u32) -> Result<(Constant, Constant)> {
    let imm12bits = (opcode & 0b111111111111_0000_00_00_0000) >> 12;
    let sbits =     (opcode & 0b000000000000_1111_00_00_0000) >> 8;
    let imm12 = Constant::new(imm12bits as u64, 12)?;
    let s = Constant::new(sbits as u64, 4)?;
    Ok((imm12, s))
}

fn decode_ri6(opcode: u32) -> Result<(Constant, Variable)> {
    let imm16bits = (opcode & 0b1111111111111111_0000_0000) >> 8;
    let tbits =     (opcode & 0b0000000000000000_1111_0000) >> 4;
    let imm16 = Constant::new(imm16bits as u64, 16)?;
    let t = Variable::new2(format!("a{}", tbits), None, 32)?;
    Ok((imm16, t))
}

fn decode_call(opcode: u32) -> Result<Constant> {
    let imm18bits = (opcode & 0b111111111111111111_00_0000) >> 6;
    let imm18 = Constant::new(imm18bits as u64, 16)?;
    Ok(imm18)
}

fn narrow_nonary(name: &str, address: u64) -> Result<Match> {
    Ok(Match {
        area: address..(address + 2),
        opcode: Atom::from(name),
        operands: SmallVec::new(),
        instructions: Vec::new(),
        jumps: SmallVec::from_vec(vec![(address, Value::val(address + 2, PC_BITS)?, Guard::True)]),
    })
}

fn narrow_binary(name: &str, a: Value, b: Value, address: u64) -> Result<Match> {
    Ok(Match {
        area: address..(address + 2),
        opcode: Atom::from(name),
        operands: SmallVec::from_vec(vec![a, b]),
        instructions: Vec::new(),
        jumps: SmallVec::from_vec(vec![(address, Value::val(address + 2, PC_BITS)?, Guard::True)]),
    })
}

fn nonary(name: &str, address: u64) -> Result<Match> {
    Ok(Match {
        area: address..(address + 3),
        opcode: Atom::from(name),
        operands: SmallVec::new(),
        instructions: Vec::new(),
        jumps: SmallVec::from_vec(vec![(address, Value::val(address + 3, PC_BITS)?, Guard::True)]),
    })
}

fn binary(name: &str, a: Value, b: Value, stms: Vec<Statement>, address: u64) -> Result<Match> {
    Ok(Match {
        area: address..(address + 3),
        opcode: Atom::from(name),
        operands: SmallVec::from_vec(vec![a, b]),
        instructions: stms,
        jumps: SmallVec::from_vec(vec![(address, Value::val(address + 3, PC_BITS)?, Guard::True)]),
    })
}

fn call(name: &str, label: Constant, address: u64) -> Result<Match> {
    let new_addr = ((address as i64) + (label.value as i64)) as u64;
    Ok(Match {
        area: address..(address + 3),
        opcode: Atom::from(name),
        operands: SmallVec::from_vec(vec![label.into()]),
        instructions: Vec::new(),
        jumps: SmallVec::from_vec(vec![(address, Value::val(new_addr, PC_BITS)?, Guard::True)]),
    })
}

pub fn match_opcode(
    code: &Region,
    mut address: u64,
    _config: &Mcu,
    matches: &mut Vec<Match>,
) -> Result<()> {
    while let Ok(opcode) = code.read_integer(address, Endianess::Little, 2) {
        info!("disasm @ {:#X}", address);
        let opcode = (opcode & 0x00FFFFFF) as u32;

        let op0 = (opcode & 0x0000000F) as u8;
        match op0 {
            // Check if it's a narrow instruction
            0b1000 | 0b1001 | 0b1010 | 0b1011 | 0b1100 | 0b1101 => {
                let compact_opcode = (opcode & 0x0000FFFF) as u16;
                match compact_opcode {
                    // MOV.N at, as
                    0b0000_0000_0000_1101..=0b0000_1111_1111_1101 => {
                        let (as_, at) = decode_rrrn(opcode as u16)?;
                        matches.push(narrow_binary("mov.n", at.into(), as_.into(), address)?);
                        address += 2;
                        continue;
                    }
                    // NOP.N
                    0b1111_0000_0011_1101 => {
                        matches.push(narrow_nonary("nop.n", address)?);
                        address += 2;
                        continue;
                    }
                    // RET.N
                    0b1111_0000_0000_1101 => {
                        matches.push(narrow_nonary("ret.n", address)?);
                        address += 2;
                        continue;
                    }
                    // RETW.N
                    0b1111_0000_0001_1101 => {
                        matches.push(narrow_nonary("retw.n", address)?);
                        address += 2;
                        continue;
                    }
                    _ => unimplemented!(),
                }
            }
            // Not a narrow instruction
            _ => {}
        }

        if let Ok(opcode_rest) = code.read_integer(address + 2, Endianess::Little, 1) {
            let opcode = ((opcode as u64 | opcode_rest << 24) & 0x00FFFFFF) as u32;
            match opcode {
                // ILL
                0b0000_0000_0000_0000_0000_0000 => {
                    matches.push(nonary("ill", address)?);
                    address += 3;
                    continue;
                }
                // ENTRY as, 0..32760
                0b000000000000_0000_00_11_0110..=0b111111111111_1111_00_11_0110 => {
                    let (imm12, s) = decode_bri12(opcode)?;
                    let as_ = decode_register(s.clone())?;
                    let stms = semantic::entry(as_.clone(), s.clone(), imm12.clone())?;
                    matches.push(binary("entry", as_.into(), imm12.into(), stms, address)?);
                    address += 3;
                    continue;
                }
                // CALL8 label
                0b000000000000000000_10_0101..=0b111111111111111111_10_0101 => {
                    let label = decode_call(opcode)?;
                    matches.push(call("call8", label, address)?);
                    address += 3;
                    continue;
                }
                // L32R at, label
                0b0000000000000000_0000_0001..=0b1111111111111111_1111_0001 => {
                    let (label, at) = decode_ri6(opcode)?;
                    let stms = semantic::l32r()?;
                    matches.push(binary("l32r", at.into(), label.into(), stms, address)?);
                    address += 3;
                    continue;
                }
                _ => unimplemented!(),
            }
        }
    }

    Ok(())
}
