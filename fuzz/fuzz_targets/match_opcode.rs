#![no_main]
use libfuzzer_sys::fuzz_target;

use falcon_xtensa_lx6::match_opcode;

fuzz_target!(|data: &[u8]| {
    let mut bytes = (0, 0, 0);
    if data.len() == 2 {
        bytes = (data[0], data[1], 0);
    } else if data.len() > 3 {
        bytes = (data[0], data[1], data[3]);
    }

    match match_opcode(bytes) {
        Ok(_) => {},
        Err(e) => println!("Failed: {}", e),
    }
});
