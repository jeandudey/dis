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

use std::borrow::Cow;

use p8n_types::{Architecture, Match, Region, Result};

use crate::syntax;

#[derive(Clone, Debug)]
pub enum XtensaLx6 {}

impl Architecture for XtensaLx6 {
    type Configuration = Mcu;

    fn prepare(_: &Region, _: &Mcu) -> Result<Cow<'static, [(&'static str, u64, &'static str)]>> {
        Ok(Cow::Borrowed(&[]))
    }

    fn decode(code: &Region, start: u64, config: &Mcu, matches: &mut Vec<Match>) -> Result<()> {
        syntax::match_opcode(code, start, config, matches)
    }
}

#[derive(Clone)]
pub struct Mcu;
