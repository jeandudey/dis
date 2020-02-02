#![no_main]
use libfuzzer_sys::fuzz_target;

use falcon_xtensa_lx6::match_opcode;

fuzz_target!(|data: &[u8]| {
    if data.len() == 2 {
        match_opcode((data[0], data[1], 0)).ok();
    } else if data.len() > 3 {
        match_opcode((data[0], data[1], data[3])).ok();
    }
});
