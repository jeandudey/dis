/*#[macro_use]
extern crate log;*/

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Id {
    ILL,
    RET,
    RETW,
    JX,
    CALLX0,
    CALLX4,
    CALLX8,
    CALLX12,
    MOVSP,
    ISYNC,
    RSYNC,
    ESYNC,
    DSYNC,
    EXCW,
    MEMW,
    EXTW,
    RFE,
    RFUE,
    RFDE,
    RFWO,
    RFWU,
    RFI,
    RFME,
    BREAK,
    SYSCALL,
    RSIL,
    WAITI,
    ANY4,
    ALL4,
    ANY8,
    ALL8,
    AND,
    OR,
    XOR,
    SSR,
    SSL,
    SSA8L,
    SSA8B,
    SSAI,
    RER,
    WER,
    ROTW,
    NSA,
    NSAU,
    RITLB0,
    IITLB,
    PITLB,
    WITLB,
    RITLB1,
    RDTLB0,
    IDTLB,
    PDTLB,
    WDTLB,
    RDTLB1,
    NEG,
    ABS,
    ADD,
    ADDX2,
    ADDX4,
    ADDX8,
    SUB,
    SUBX2,
    SUBX4,
    SUBX8,
    SLLI,
    SRAI,
    SRLI,
    XSR,
    SRC,
    SLL,
    SRA,
    MUL16U,
    MUL16S,
    LICT,
    SICT,
    LICW,
    SICW,
    LDCT,
    SDCT,
    RFDO,
    RFDD,

    L32R,

    ENTRY,

    RESERVED,
    RESERVED_N,
}

impl Id {
    #[rustfmt::skip]
    pub fn mnemonic(&self) -> &'static str {
        match *self {
            Id::ILL     => "ill",
            Id::RET     => "ret",
            Id::RETW    => "retw",
            Id::JX      => "jx",
            Id::CALLX0  => "callx0",
            Id::CALLX4  => "callx4",
            Id::CALLX8  => "callx8",
            Id::CALLX12 => "callx12",
            Id::MOVSP   => "movsp",
            Id::ISYNC   => "isync",
            Id::RSYNC   => "rsync",
            Id::ESYNC   => "esync",
            Id::DSYNC   => "dsync",
            Id::EXCW    => "excw",
            Id::MEMW    => "memw",
            Id::EXTW    => "extw",
            Id::RFE     => "rfe",
            Id::RFUE    => "rfue",
            Id::RFDE    => "rfde",
            Id::RFWO    => "rfwo",
            Id::RFWU    => "rfwu",
            Id::RFI     => "rfi",
            Id::RFME    => "rfme",
            Id::BREAK   => "break",
            Id::SYSCALL => "syscall",
            Id::RSIL    => "syscall",
            Id::WAITI   => "waiti",
            Id::ANY4    => "any4",
            Id::ALL4    => "all4",
            Id::ANY8    => "any8",
            Id::ALL8    => "all8",
            Id::AND     => "and",
            Id::OR      => "or",
            Id::XOR     => "xor",
            Id::SSR     => "ssr",
            Id::SSL     => "ssl",
            Id::SSA8L   => "ssa8l",
            Id::SSA8B   => "ssa8b",
            Id::SSAI    => "ssai",
            Id::RER     => "rer",
            Id::WER     => "wer",
            Id::ROTW    => "rotw",
            Id::NSA     => "nsa",
            Id::NSAU    => "nsau",
            Id::RITLB0  => "ritlb0",
            Id::IITLB   => "iitlb",
            Id::PITLB   => "pitlb",
            Id::WITLB   => "witlb",
            Id::RITLB1  => "ritlb1",
            Id::RDTLB0  => "rdtlb0",
            Id::IDTLB   => "idtlb",
            Id::PDTLB   => "pdtlb",
            Id::WDTLB   => "wdtlb",
            Id::RDTLB1  => "rdtlb1",
            Id::NEG     => "neg",
            Id::ABS     => "abs",
            Id::ADD     => "add",
            Id::ADDX2   => "addx2",
            Id::ADDX4   => "addx4",
            Id::ADDX8   => "addx8",
            Id::SUB     => "sub",
            Id::SUBX2   => "subx2",
            Id::SUBX4   => "subx4",
            Id::SUBX8   => "subx8",
            Id::SLLI    => "slli",
            Id::SRAI    => "srai",
            Id::SRLI    => "srli",
            Id::XSR     => "xsr",
            Id::SRC     => "src",
            Id::SLL     => "sll",
            Id::SRA     => "sra",
            Id::MUL16U  => "mul16u",
            Id::MUL16S  => "mul16s",
            Id::LICT    => "lict",
            Id::SICT    => "sict",
            Id::LICW    => "licw",
            Id::SICW    => "sicw",
            Id::LDCT    => "ldct",
            Id::SDCT    => "sdct",
            Id::RFDO    => "rfdo",
            Id::RFDD    => "rfdd",

            Id::L32R    => "l32r",

            Id::ENTRY   => "entry",

            Id::RESERVED | Id::RESERVED_N => "reserved",
        }
    }

    pub fn is_narrow(&self) -> bool {
        match *self {
            _ => false,
        }
    }
}

macro_rules! full {
    ($opcode:expr, $next_byte:expr) => {
        match $next_byte {
            Some(b) => (u32::from(b) << 16) | $opcode,
            None => return Err("expected one more byte"),
        }
    }
}

pub fn match_opcode(bytes: (u8, u8, Option<u8>)) -> Result<Id, &'static str> {
    let opcode = (u32::from(bytes.1) << 8) | u32::from(bytes.0);

    println!("opcode = {:b}", opcode);

    let op0 = opcode & 0b1111;
    match op0 {
        0b0000 => qrst(full!(opcode, bytes.2)),
        0b0001 => Ok(Id::L32R), 
        0b0010 => lsai(full!(opcode, bytes.2)),
        0b0011 => { unimplemented!(); }

        0b0100 => { unimplemented!(); }
        0b0101 => { unimplemented!(); }
        0b0110 => si(full!(opcode, bytes.2)),
        0b0111 => { unimplemented!(); }

        0b1000 => { unimplemented!(); }
        0b1001 => { unimplemented!(); }
        0b1010 => { unimplemented!(); }
        0b1011 => { unimplemented!(); }

        0b1100 => { unimplemented!(); }
        0b1101 => { unimplemented!(); }
        0b1110 => { unimplemented!(); }
        0b1111 => { unimplemented!(); }

        _ => unreachable!(),
    }
}

fn qrst(_: u32) -> Result<Id, &'static str> { unimplemented!(); }
fn lsai(_: u32) -> Result<Id, &'static str> { unimplemented!(); }

fn si(opcode: u32) -> Result<Id, &'static str> {
    let n = (opcode & (0b11 << 4)) >> 4;

    match n {
        0b00 => { unimplemented!(); },
        0b01 => { unimplemented!(); },
        0b10 => { unimplemented!(); },
        0b11 => { bi1(opcode) },
        _ => unreachable!(),
    }
}

fn bi1(opcode: u32) -> Result<Id, &'static str> {
    let m = (opcode & (0b11 << 6)) >> 4;

    match m {
        0b00 => { Ok(Id::ENTRY) },
        0b01 => { unimplemented!(); },
        0b10 => { unimplemented!(); },
        0b11 => { unimplemented!() },
        _ => unreachable!(),
    }
}

#[cfg(test)]
pub mod tests {
    use super::match_opcode;

    #[test]
    fn test_app_main() {
        const BIN: &'static [u8] = include_bytes!("../examples/data/main.bin");
        panic!("{:?}", match_opcode((BIN[0], BIN[1], Some(BIN[2]))).unwrap());
    }
}
