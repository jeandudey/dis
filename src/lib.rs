/*#[macro_use]
extern crate log;*/

#[derive(Debug, Clone, Copy)]
pub struct Opcode(pub [u8; 3]);

impl Opcode {
    pub fn new() -> Opcode {
        Opcode([0u8; 3])
    }

    #[rustfmt::skip]
    fn to_u32(self) -> u32 {
        let b = (u32::from(self.0[0]) << 16) |
        (u32::from(self.0[1]) << 8) |
        (u32::from(self.0[2]) << 0);
        println!("{:b}", b);
        b
    }

    /*#[rustfmt::skip]
    fn to_u16(self) -> u32 {
        (u32::from(self.0[0]) << 8) |
        (u32::from(self.0[1]) << 0)
    }*/
}

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

/*fn decode_rrr(opcode: u32) -> (u8, u8, u8) {
    let r = ((opcode & 0b0000_0000_1111_0000_0000_0000) >> 12) as u8;
    let s = ((opcode & 0b0000_0000_0000_1111_0000_0000) >> 8) as u8;
    let t = ((opcode & 0b0000_0000_0000_0000_1111_0000) >> 4) as u8;

    (r, s, t)
}

fn decode_callx(opcode: u32) -> (u8, u8, u8, u8) {
    let r = ((opcode & 0b0000_0000_1111_0000_00_00_0000) >> 12) as u8;
    let s = ((opcode & 0b0000_0000_0000_1111_00_00_0000) >> 8) as u8;
    let m = ((opcode & 0b0000_0000_0000_0000_11_00_0000) >> 6) as u8;
    let n = ((opcode & 0b0000_0000_0000_0000_00_11_0000) >> 4) as u8;

    (r, s, m, n)
}*/

#[rustfmt::skip]
const MATCH_OP0: [fn(Opcode) -> Id; 16] = [
    qrst,   l32r,   lsai,     lsci,
    mac16,  calln,  si,       b,
    l32i_n, s32i_n, add_n,    addi_n,
    st2,    st3,    reserved, reserved,
];

fn qrst(opcode: Opcode) -> Id {
    #[rustfmt::skip]
    const MATCH_OP1: [fn(u32) -> Id; 16] = [
        rst0,         rst1,         rst2,         rst3,
        extui0,       extui1,       cust0,        cust1,
        lscx,         lsc4,         fp0,          fp1,
        reserved_u32, reserved_u32, reserved_u32, reserved_u32,
    ];

    fn rst0(opcode: u32) -> Id {
        #[rustfmt::skip]
        const MATCH_OP2: [fn(u32) -> Id; 16] = [
            st0, and,   or,    xor,
            st1, tlb,   rt0,   reserved_u32,
            add, addx2, addx4, addx8,
            sub, subx2, subx4, subx8,
        ];

        fn st0(opcode: u32) -> Id {
            #[rustfmt::skip]
            const MATCH_R: [fn(u32) -> Id; 16] = [
                snm0,         movsp,        sync,         rfei,
                r#break,      syscall,      rsil,         waiti,
                any4,         all4,         any8,         all8,
                reserved_u32, reserved_u32, reserved_u32, reserved_u32,
            ];

            fn snm0(opcode: u32) -> Id {
                #[rustfmt::skip]
                const MATCH_M: [fn(u32) -> Id; 4] = [
                    ill, reserved_u32, jr, callx,
                ];

                fn ill(_: u32) -> Id {
                    Id::ILL
                }

                fn jr(opcode: u32) -> Id {
                    #[rustfmt::skip]
                    const MATCH_N: [Id; 4] = [
                        Id::RET, Id::RETW, Id::JX, Id::RESERVED,
                    ];

                    let n = ((opcode & 0b0000_0000_0000_0000_00_11_0000) >> 4) as usize;
                    MATCH_N[n]
                }

                fn callx(opcode: u32) -> Id {
                    #[rustfmt::skip]
                    const MATCH_N: [Id; 4] = [
                        Id::CALLX0, Id::CALLX4, Id::CALLX8, Id::CALLX12,
                    ];

                    let n = ((opcode & 0b0000_0000_0000_0000_00_11_0000) >> 4) as usize;
                    MATCH_N[n]
                }

                let m = ((opcode & 0b0000_0000_0000_0000_11_00_0000) >> 6) as usize;
                let match_m = MATCH_M[m];
                match_m(opcode)
            }

            fn movsp(_: u32) -> Id {
                Id::MOVSP
            }

            fn sync(opcode: u32) -> Id {
                #[rustfmt::skip]
                const MATCH_T: [Id; 16] = [
                    Id::ISYNC,    Id::RSYNC,    Id::ESYNC,    Id::DSYNC,
                    Id::RESERVED, Id::RESERVED, Id::RESERVED, Id::RESERVED,
                    Id::EXCW,     Id::RESERVED, Id::RESERVED, Id::RESERVED,
                    Id::MEMW,     Id::EXTW,     Id::RESERVED, Id::RESERVED,
                ];

                let t = ((opcode & 0b0000_0000_0000_0000_1111_0000) >> 4) as usize;
                MATCH_T[t]
            }

            fn rfei(opcode: u32) -> Id {
                #[rustfmt::skip]
                const MATCH_T: [fn(u32) -> Id; 16] = [
                    rfet,         rfi,         rfme,         reserved_u32,
                    reserved_u32, reserved_u32, reserved_u32, reserved_u32,
                    reserved_u32, reserved_u32, reserved_u32, reserved_u32,
                    reserved_u32, reserved_u32, reserved_u32, reserved_u32,
                ];

                fn rfet(opcode: u32) -> Id {
                    #[rustfmt::skip]
                    const MATCH_S: [Id; 16] = [
                        Id::RFE,      Id::RFUE,     Id::RFDE,     Id::RESERVED,
                        Id::RFWO,     Id::RFWU,     Id::RESERVED, Id::RESERVED,
                        Id::RESERVED, Id::RESERVED, Id::RESERVED, Id::RESERVED,
                        Id::RESERVED, Id::RESERVED, Id::RESERVED, Id::RESERVED,
                    ];

                    let s = ((opcode & 0b0000_0000_0000_1111_0000_0000) >> 8) as usize;
                    MATCH_S[s]
                }

                fn rfi(_: u32) -> Id {
                    Id::RFI
                }

                fn rfme(_: u32) -> Id {
                    Id::RFME
                }

                let t = ((opcode & 0b0000_0000_0000_0000_1111_0000) >> 4) as usize;
                let match_t = MATCH_T[t];
                match_t(opcode)
            }

            fn r#break(_: u32) -> Id {
                Id::BREAK
            }

            fn syscall(_: u32) -> Id {
                Id::SYSCALL
            }

            fn rsil(_: u32) -> Id {
                Id::RSIL
            }

            fn waiti(_: u32) -> Id {
                Id::WAITI
            }

            fn any4(_: u32) -> Id {
                Id::ANY4
            }

            fn all4(_: u32) -> Id {
                Id::ANY4
            }

            fn any8(_: u32) -> Id {
                Id::ANY8
            }

            fn all8(_: u32) -> Id {
                Id::ALL8
            }

            let r = ((opcode & 0b0000_0000_1111_0000_0000_0000) >> 12) as usize;
            let match_r = MATCH_R[r];
            match_r(opcode)
        }

        fn and(_: u32) -> Id {
            Id::AND
        }
        fn or(_: u32) -> Id {
            Id::OR
        }
        fn xor(_: u32) -> Id {
            Id::XOR
        }

        fn st1(opcode: u32) -> Id {
            #[rustfmt::skip]
            const MATCH_R: [Id; 16] = [
                Id::SSR,      Id::SSL,      Id::SSA8L,    Id::SSA8B,
                Id::SSAI,     Id::RESERVED, Id::RER,      Id::WER,
                Id::ROTW,     Id::RESERVED, Id::RESERVED, Id::RESERVED,
                Id::RESERVED, Id::RESERVED, Id::NSA,      Id::NSAU,
            ];

            let r = ((opcode & 0b0000_0000_1111_0000_0000_0000) >> 12) as usize;
            MATCH_R[r]
        }

        fn tlb(opcode: u32) -> Id {
            #[rustfmt::skip]
            const MATCH_R: [Id; 16] = [
                Id::RESERVED, Id::RESERVED, Id::RESERVED, Id::RITLB0,
                Id::IITLB,    Id::PITLB,    Id::WITLB,    Id::RITLB1,
                Id::RESERVED, Id::RESERVED, Id::RESERVED, Id::RDTLB0,
                Id::IDTLB,    Id::PDTLB,    Id::WDTLB,    Id::RDTLB1,
            ];

            let r = ((opcode & 0b0000_0000_1111_0000_0000_0000) >> 12) as usize;
            MATCH_R[r]
        }

        fn rt0(opcode: u32) -> Id {
            #[rustfmt::skip]
            const MATCH_S: [Id; 16] = [
                Id::NEG,      Id::ABS,      Id::RESERVED, Id::RESERVED,
                Id::RESERVED, Id::RESERVED, Id::RESERVED, Id::RESERVED,
                Id::RESERVED, Id::RESERVED, Id::RESERVED, Id::RESERVED,
                Id::RESERVED, Id::RESERVED, Id::RESERVED, Id::RESERVED,
            ];

            let s = ((opcode & 0b0000_0000_0000_1111_0000_0000) >> 12) as usize;
            MATCH_S[s]
        }

        fn add(_: u32) -> Id {
            Id::ADD
        }
        fn addx2(_: u32) -> Id {
            Id::ADDX2
        }
        fn addx4(_: u32) -> Id {
            Id::ADDX4
        }
        fn addx8(_: u32) -> Id {
            Id::ADDX8
        }

        fn sub(_: u32) -> Id {
            Id::SUB
        }
        fn subx2(_: u32) -> Id {
            Id::SUBX2
        }
        fn subx4(_: u32) -> Id {
            Id::SUBX4
        }
        fn subx8(_: u32) -> Id {
            Id::SUBX8
        }

        let op2 = ((opcode & 0b1111_0000_0000_0000_0000_0000) >> 20) as usize;
        let match_op2 = MATCH_OP2[op2];
        match_op2(opcode)
    }

    fn rst1(opcode: u32) -> Id {
        #[rustfmt::skip]
        const MATCH_OP2: [fn(u32) -> Id; 16] = [
            slli,   slli,         srai,         srai,
            srli,   reserved_u32, xsr,          accer,
            src,    srl,          sll,          sra,
            mul16u, mul16s,       reserved_u32, imp,
        ];

        fn slli(_: u32) -> Id {
            Id::SLLI
        }

        fn srai(_: u32) -> Id {
            Id::SRAI
        }

        fn srli(_: u32) -> Id {
            Id::SRLI
        }

        fn xsr(_: u32) -> Id {
            Id::XSR
        }

        fn accer(_: u32) -> Id {
            // TODO: Xtensa ISA is unclear about ACCER, just return reserved.
            Id::RESERVED
        }

        fn src(_: u32) -> Id {
            Id::SRAI
        }

        fn srl(_: u32) -> Id {
            Id::SRC
        }

        fn sll(_: u32) -> Id {
            Id::SLL
        }

        fn sra(_: u32) -> Id {
            Id::SRA
        }

        fn mul16u(_: u32) -> Id {
            Id::MUL16U
        }

        fn mul16s(_: u32) -> Id {
            Id::MUL16S
        }

        fn imp(opcode: u32) -> Id {
            #[rustfmt::skip]
            const MATCH_R: [fn(u32) -> Id; 16] = [
                lict,         sict,         licw,         sicw,
                reserved_u32, reserved_u32, reserved_u32, reserved_u32,
                ldct,         sdct,         reserved_u32, reserved_u32,
                reserved_u32, reserved_u32, rfdx,         reserved_u32,
            ];

            fn lict(_: u32) -> Id {
                Id::LICT
            }

            fn sict(_: u32) -> Id {
                Id::SICT
            }

            fn licw(_: u32) -> Id {
                Id::LICW
            }

            fn sicw(_: u32) -> Id {
                Id::SICW
            }

            fn ldct(_: u32) -> Id {
                Id::LDCT
            }

            fn sdct(_: u32) -> Id {
                Id::SDCT
            }

            fn rfdx(opcode: u32) -> Id {
                #[rustfmt::skip]
                const MATCH_T: [Id; 16] = [
                    Id::RFDO,     Id::RFDD,     Id::RESERVED, Id::RESERVED,
                    Id::RESERVED, Id::RESERVED, Id::RESERVED, Id::RESERVED,
                    Id::RESERVED, Id::RESERVED, Id::RESERVED, Id::RESERVED,
                    Id::RESERVED, Id::RESERVED, Id::RESERVED, Id::RESERVED,
                ];

                let t = ((opcode & 0b0000_0000_0000_0000_1111_0000) >> 4) as usize;
                MATCH_T[t]
            }

            let r = ((opcode & 0b0000_0000_1111_0000_0000_0000) >> 12) as usize;
            let match_r = MATCH_R[r];
            match_r(opcode)
        }

        let op2 = ((opcode & 0b1111_0000_0000_0000_0000_0000) >> 20) as usize;
        let match_op2 = MATCH_OP2[op2];
        match_op2(opcode)
    }

    fn rst2(_: u32) -> Id {
        Id::RESERVED
    }
    fn rst3(_: u32) -> Id {
        Id::RESERVED
    }

    fn extui0(_: u32) -> Id {
        Id::RESERVED
    }
    fn extui1(_: u32) -> Id {
        Id::RESERVED
    }
    fn cust0(_: u32) -> Id {
        Id::RESERVED
    }
    fn cust1(_: u32) -> Id {
        Id::RESERVED
    }

    fn lscx(_: u32) -> Id {
        Id::RESERVED
    }
    fn lsc4(_: u32) -> Id {
        Id::RESERVED
    }
    fn fp0(_: u32) -> Id {
        Id::RESERVED
    }
    fn fp1(_: u32) -> Id {
        Id::RESERVED
    }

    let opcode_u32 = opcode.to_u32();
    let op1 = ((opcode_u32 & 0b0000_1111_0000_0000_0000_0000) >> 16) as usize;
    let match_op1 = MATCH_OP1[op1];
    match_op1(opcode_u32)
}

fn l32r(_: Opcode) -> Id {
    Id::L32R
}
fn lsai(_: Opcode) -> Id {
    Id::RESERVED
}
fn lsci(_: Opcode) -> Id {
    Id::RESERVED
}

fn mac16(_: Opcode) -> Id {
    Id::RESERVED
}
fn calln(_: Opcode) -> Id {
    Id::RESERVED_N
}

fn si(opcode: Opcode) -> Id {
    #[rustfmt::skip]
    const MATCH_N: [fn(u32) -> Id; 4] = [
        j, bz, bi0, bi1,
    ];

    fn j(_: u32) -> Id { Id::RESERVED }
    fn bz(_: u32) -> Id { Id::RESERVED }
    fn bi0(_: u32) -> Id { Id::RESERVED }
    fn bi1(opcode: u32) -> Id {
        #[rustfmt::skip]
        const MATCH_M: [fn(u32) -> Id; 4] = [
            entry, b1, bltui, bgeui,
        ];

        fn entry(_: u32) -> Id { Id::ENTRY }
        fn b1(_: u32) -> Id { Id::RESERVED }
        fn bltui(_: u32) -> Id { Id::RESERVED }
        fn bgeui(_: u32) -> Id { Id::RESERVED }

        let m = ((opcode & 0b0000_0000_0000_0000_11_00_0000) >> 6) as usize;
        let match_m = MATCH_M[m];
        match_m(opcode)
    }

    let opcode_u32 = opcode.to_u32();
    let n = ((opcode_u32 & 0b0000_0000_0000_0000_00_11_0000) >> 4) as usize;
    let match_n = MATCH_N[n];
    match_n(opcode_u32)
}

fn b(_: Opcode) -> Id {
    Id::RESERVED
}

fn l32i_n(_: Opcode) -> Id {
    Id::RESERVED_N
}
fn s32i_n(_: Opcode) -> Id {
    Id::RESERVED_N
}
fn add_n(_: Opcode) -> Id {
    Id::RESERVED_N
}
fn addi_n(_: Opcode) -> Id {
    Id::RESERVED_N
}

fn st2(_: Opcode) -> Id {
    Id::RESERVED
}
fn st3(_: Opcode) -> Id {
    Id::RESERVED
}

fn reserved(_: Opcode) -> Id {
    Id::RESERVED
}

fn reserved_u32(_: u32) -> Id {
    Id::RESERVED
}

pub fn match_opcode(opcode: Opcode) -> Id {
    let op0 = (opcode.to_u32() & 0b0000_0000_0000_0000_0000_1111) as usize;
    let match_op0 = MATCH_OP0[op0];
    match_op0(opcode)
}

#[cfg(test)]
pub mod tests {
    use crate::{match_opcode, Opcode};

    const MAIN: &'static [u8] = include_bytes!("../examples/data/main.bin");

    #[test]
    fn test_match_addi() {
        let mut i = 0;

        while i < MAIN.len() {
            let lookahead = (MAIN.len() - i) >= 3;
            let op: Opcode;
            if lookahead {
                op = Opcode([MAIN[i + 0], MAIN[i + 1], MAIN[i + 3]]);
            } else {
                op = Opcode([MAIN[i + 0], MAIN[i + 1], 0]);
            }
            let ins = match_opcode(op);
            println!("{}", ins.mnemonic());
            if ins.is_narrow() {
                println!("is narrow");
                i += 2;
            } else {
                i += 3;
            }
        }

        unimplemented!();
    }
}
