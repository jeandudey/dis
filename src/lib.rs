#[macro_use]
extern crate log;

#[derive(Debug, Clone, Copy)]
pub struct Opcode(pub [u8; 3]);

impl Opcode {
    pub fn new() -> Opcode {
        Opcode([0u8; 3])
    }

    #[rustfmt::skip]
    fn to_u32(self) -> u32 {
        (u32::from(self.0[0]) << 16) |
        (u32::from(self.0[1]) << 8) |
        (u32::from(self.0[2]) << 0)
    }

    /*#[rustfmt::skip]
    fn to_u16(self) -> u32 {
        (u32::from(self.0[0]) << 8) |
        (u32::from(self.0[1]) << 0)
    }*/
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum InstructionId {
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

    RESERVED,
    RESERVED_N,
}

impl InstructionId {
    pub fn mnemonic(&self) -> &'static str {
        match *self {
            InstructionId::ILL => "ill",
            InstructionId::RET => "ret",
            InstructionId::RETW => "retw",
            InstructionId::JX => "jx",
            InstructionId::CALLX0 => "callx0",
            InstructionId::CALLX4 => "callx4",
            InstructionId::CALLX8 => "callx8",
            InstructionId::CALLX12 => "callx12",
            InstructionId::MOVSP => "movsp",
            InstructionId::ISYNC => "isync",
            InstructionId::RSYNC => "rsync",
            InstructionId::ESYNC => "esync",
            InstructionId::DSYNC => "dsync",
            InstructionId::EXCW => "excw",
            InstructionId::MEMW => "memw",
            InstructionId::EXTW => "extw",
            InstructionId::RFE => "rfe",
            InstructionId::RFUE => "rfue",
            InstructionId::RFDE => "rfde",
            InstructionId::RFWO => "rfwo",
            InstructionId::RFWU => "rfwu",
            InstructionId::RFI => "rfi",
            InstructionId::RFME => "rfme",
            InstructionId::BREAK => "break",
            InstructionId::SYSCALL => "syscall",
            InstructionId::RSIL => "syscall",
            InstructionId::WAITI => "waiti",
            InstructionId::ANY4 => "any4",
            InstructionId::ALL4 => "all4",
            InstructionId::ANY8 => "any8",
            InstructionId::ALL8 => "all8",
            InstructionId::RESERVED | InstructionId::RESERVED_N => "reserved",
        }
    }

    pub fn is_narrow(&self) -> bool {
        match *self {
            _ => false
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
const MATCH_OP0: [fn(Opcode) -> InstructionId; 16] = [
    qrst,   l32r,   lsai,     lsci,
    mac16,  calln,  si,       b,
    l32i_n, s32i_n, add_n,    addi_n,
    st2,    st3,    reserved, reserved,
];

fn qrst(opcode: Opcode) -> InstructionId {
    #[rustfmt::skip]
    const MATCH_OP1: [fn(u32) -> InstructionId; 16] = [
        rst0,         rst1,         rst2,         rst3,
        extui0,       extui1,       cust0,        cust1,
        lscx,         lsc4,         fp0,          fp1,
        reserved_u32, reserved_u32, reserved_u32, reserved_u32,
    ];

    fn rst0(opcode: u32) -> InstructionId {
        #[rustfmt::skip]
        const MATCH_OP2: [fn(u32) -> InstructionId; 16] = [
            st0, and,   or,    xor,
            st1, tlb,   rt0,   reserved_u32,
            add, addx2, addx4, addx8,
            sub, subx2, subx4, subx8,
        ];

        fn st0(opcode: u32) -> InstructionId {
            #[rustfmt::skip]
            const MATCH_R: [fn(u32) -> InstructionId; 16] = [
                snm0,         movsp,        sync,         rfei,
                r#break,      syscall,      rsil,         waiti,
                any4,         all4,         any8,         all8,
                reserved_u32, reserved_u32, reserved_u32, reserved_u32,
            ];

            fn snm0(opcode: u32) -> InstructionId {
                #[rustfmt::skip]
                const MATCH_M: [fn(u32) -> InstructionId; 4] = [
                    ill, reserved_u32, jr, callx,
                ];

                fn ill(_: u32) -> InstructionId {
                    InstructionId::ILL
                }

                fn jr(opcode: u32) -> InstructionId {
                    #[rustfmt::skip]
                    const MATCH_N: [InstructionId; 4] = [
                        InstructionId::RET, InstructionId::RETW, InstructionId::JX, InstructionId::RESERVED,
                    ];

                    let n = ((opcode & 0b0000_0000_0000_0000_00_11_0000) >> 4) as usize;
                    MATCH_N[n]
                }

                fn callx(opcode: u32) -> InstructionId {
                    #[rustfmt::skip]
                    const MATCH_N: [InstructionId; 4] = [
                        InstructionId::CALLX0, InstructionId::CALLX4, InstructionId::CALLX8, InstructionId::CALLX12,
                    ];

                    let n = ((opcode & 0b0000_0000_0000_0000_00_11_0000) >> 4) as usize;
                    MATCH_N[n]
                }

                let m = ((opcode & 0b0000_0000_0000_0000_11_00_0000) >> 6) as usize;
                let match_m = MATCH_M[m];
                match_m(opcode)
            }

            fn movsp(_: u32) -> InstructionId { InstructionId::MOVSP }

            fn sync(opcode: u32) -> InstructionId {
                #[rustfmt::skip]
                const MATCH_T: [InstructionId; 16] = [
                    InstructionId::ISYNC,    InstructionId::RSYNC,    InstructionId::ESYNC,    InstructionId::DSYNC,
                    InstructionId::RESERVED, InstructionId::RESERVED, InstructionId::RESERVED, InstructionId::RESERVED,
                    InstructionId::EXCW,     InstructionId::RESERVED, InstructionId::RESERVED, InstructionId::RESERVED,
                    InstructionId::MEMW,     InstructionId::EXTW,     InstructionId::RESERVED, InstructionId::RESERVED,
                ];

                let t = ((opcode & 0b0000_0000_0000_0000_1111_0000) >> 4) as usize;
                MATCH_T[t]
            }

            fn rfei(opcode: u32) -> InstructionId {
                #[rustfmt::skip]
                const MATCH_T: [fn(u32) -> InstructionId; 16] = [
                    rfet,         rfi,         rfme,         reserved_u32,
                    reserved_u32, reserved_u32, reserved_u32, reserved_u32,
                    reserved_u32, reserved_u32, reserved_u32, reserved_u32,
                    reserved_u32, reserved_u32, reserved_u32, reserved_u32,
                ];

                fn rfet(opcode: u32) -> InstructionId {
                    #[rustfmt::skip]
                    const MATCH_S: [InstructionId; 16] = [
                        InstructionId::RFE,      InstructionId::RFUE,     InstructionId::RFDE,     InstructionId::RESERVED,
                        InstructionId::RFWO,     InstructionId::RFWU,     InstructionId::RESERVED, InstructionId::RESERVED,
                        InstructionId::RESERVED, InstructionId::RESERVED, InstructionId::RESERVED, InstructionId::RESERVED,
                        InstructionId::RESERVED, InstructionId::RESERVED, InstructionId::RESERVED, InstructionId::RESERVED,
                    ];

                    let s = ((opcode & 0b0000_0000_0000_1111_0000_0000) >> 8) as usize;
                    MATCH_S[s]
                }

                fn rfi(_: u32) -> InstructionId {
                    InstructionId::RFI
                }

                fn rfme(_: u32) -> InstructionId {
                    InstructionId::RFME
                }

                let t = ((opcode & 0b0000_0000_0000_0000_1111_0000) >> 4) as usize;
                let match_t = MATCH_T[t];
                match_t(opcode)
            }

            fn r#break(_: u32) -> InstructionId {
                InstructionId::BREAK
            }

            fn syscall(_: u32) -> InstructionId {
                InstructionId::SYSCALL
            }

            fn rsil(_: u32) -> InstructionId {
                InstructionId::RSIL
            }

            fn waiti(_: u32) -> InstructionId {
                InstructionId::WAITI
            }

            fn any4(_: u32) -> InstructionId {
                InstructionId::ANY4
            }

            fn all4(_: u32) -> InstructionId {
                InstructionId::ANY4
            }

            fn any8(_: u32) -> InstructionId {
                InstructionId::ANY8
            }

            fn all8(_: u32) -> InstructionId {
                InstructionId::ALL8
            }

            let r = ((opcode & 0b0000_0000_1111_0000_0000_0000) >> 12) as usize;
            let match_r = MATCH_R[r];
            match_r(opcode)
        }

        fn and(_: u32) -> InstructionId { InstructionId::RESERVED }
        fn or(_: u32) -> InstructionId { InstructionId::RESERVED }
        fn xor(_: u32) -> InstructionId { InstructionId::RESERVED }

        fn st1(_: u32) -> InstructionId { InstructionId::RESERVED }
        fn tlb(_: u32) -> InstructionId { InstructionId::RESERVED }
        fn rt0(_: u32) -> InstructionId { InstructionId::RESERVED }

        fn add(_: u32) -> InstructionId { InstructionId::RESERVED }
        fn addx2(_: u32) -> InstructionId { InstructionId::RESERVED }
        fn addx4(_: u32) -> InstructionId { InstructionId::RESERVED }
        fn addx8(_: u32) -> InstructionId { InstructionId::RESERVED }

        fn sub(_: u32) -> InstructionId { InstructionId::RESERVED }
        fn subx2(_: u32) -> InstructionId { InstructionId::RESERVED }
        fn subx4(_: u32) -> InstructionId { InstructionId::RESERVED }
        fn subx8(_: u32) -> InstructionId { InstructionId::RESERVED }

        let op2 = ((opcode & 0b1111_0000_0000_0000_0000_0000) >> 20) as usize;
        let match_op2 = MATCH_OP2[op2];
        match_op2(opcode)
    }

    fn rst1(_: u32) -> InstructionId { InstructionId::RESERVED }
    fn rst2(_: u32) -> InstructionId { InstructionId::RESERVED }
    fn rst3(_: u32) -> InstructionId { InstructionId::RESERVED }

    fn extui0(_: u32) -> InstructionId { InstructionId::RESERVED }
    fn extui1(_: u32) -> InstructionId { InstructionId::RESERVED }
    fn cust0(_: u32) -> InstructionId { InstructionId::RESERVED }
    fn cust1(_: u32) -> InstructionId { InstructionId::RESERVED }

    fn lscx(_: u32) -> InstructionId { InstructionId::RESERVED }
    fn lsc4(_: u32) -> InstructionId { InstructionId::RESERVED }
    fn fp0(_: u32) -> InstructionId { InstructionId::RESERVED }
    fn fp1(_: u32) -> InstructionId { InstructionId::RESERVED }

    let opcode_u32 = opcode.to_u32();
    let op1 = ((opcode_u32 & 0b0000_1111_0000_0000_0000_0000) >> 16) as usize;
    let match_op1 = MATCH_OP1[op1];
    match_op1(opcode_u32)
}

fn l32r(_: Opcode) -> InstructionId { InstructionId::RESERVED }
fn lsai(_: Opcode) -> InstructionId { InstructionId::RESERVED }
fn lsci(_: Opcode) -> InstructionId { InstructionId::RESERVED }

fn mac16(_: Opcode) -> InstructionId { InstructionId::RESERVED }
fn calln(_: Opcode) -> InstructionId { InstructionId::RESERVED }
fn si(_: Opcode) -> InstructionId { InstructionId::RESERVED }
fn b(_: Opcode) -> InstructionId { InstructionId::RESERVED }

fn l32i_n(_: Opcode) -> InstructionId { InstructionId::RESERVED }
fn s32i_n(_: Opcode) -> InstructionId { InstructionId::RESERVED }
fn add_n(_: Opcode) -> InstructionId { InstructionId::RESERVED }
fn addi_n(_: Opcode) -> InstructionId { InstructionId::RESERVED }

fn st2(_: Opcode) -> InstructionId { InstructionId::RESERVED }
fn st3(_: Opcode) -> InstructionId { InstructionId::RESERVED }

fn reserved(_: Opcode) -> InstructionId { InstructionId::RESERVED }

fn reserved_u32(_: u32) -> InstructionId { InstructionId::RESERVED }

pub fn match_opcode(opcode: Opcode) -> InstructionId {
    let op0 = (opcode.to_u32() & 0b0000_0000_0000_0000_0000_1111) as usize;
    let match_op0 = MATCH_OP0[op0];
    match_op0(opcode)
}

#[cfg(test)]
pub mod tests {
    use crate::{Opcode, match_opcode};

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
            println!("{:?}", ins);
            if ins.unwrap().is_narrow() {
                i += 2;
            } else {
                i += 3;
            }
        }

        unimplemented!();
    }
}
