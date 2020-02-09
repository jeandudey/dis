//! # Falcon Xtensa LX6 instruction decoder

mod syntax;
#[cfg(feature = "translator")]
mod translator;

pub use syntax::{match_opcode, Id, Instruction};
