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

use std::sync::Arc;
use std::fmt::Write;

use p8n_types::{Architecture, Region, Value};
use p8n_xtensa_lx6::{Mcu, XtensaLx6};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let _ = simple_logger::init();
    let buf = vec![
        0x36, 0x41, 0x00, 0x7d, 0x01, 0x21, 0x00, 0x00, 0xad, 0x02, 0x25, 0x00, 0x00, 0x3d, 0xf0,
        0x1d, 0xf0,
    ];
    let reg = Region::from_buf(".text", 32, Arc::from(buf), 0, None);
    let mcu = Mcu;
    let mut matches = vec![];

    XtensaLx6::decode(&reg, 0, &mcu, &mut matches).unwrap();

    for m in matches {
        let mut ins = String::new();
        write!(&mut ins, "{:08X}\t{}\t", m.area.start, m.opcode)?;
        match m.operands.len() {
            1 => {
                let a = match m.operands[0] {
                    Value::Undefined => format!("undefined!"),
                    Value::Variable(ref var) => format!("{}", var.name.base()),
                    Value::Constant(ref constant) => format!("{:#X}", constant.value),
                };

                write!(&mut ins, "{}", a)?;
            },
            2 => {
                let a = match m.operands[0] {
                    Value::Undefined => format!("undefined!"),
                    Value::Variable(ref var) => format!("{}", var.name.base()),
                    Value::Constant(ref constant) => format!("{:#X}", constant.value),
                };

                let b = match m.operands[1] {
                    Value::Undefined => format!("undefined!"),
                    Value::Variable(ref var) => format!("{}", var.name.base()),
                    Value::Constant(ref constant) => format!("{:#X}", constant.value),
                };

                write!(&mut ins, "{},\t{}", a, b)?;
            },
            _ => {}
        }

        println!("{} -> {:?}", ins, m.instructions);
    }

    Ok(())
}
